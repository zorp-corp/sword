/** Jam-based snapshotting
 *
 * This is a simple checkpoint system that should be safe but has (very) poor performance.  This is
 * intended as a working placeholder until the real PMA is hooked up.
 *
 * This keeps two files, .urb/chk/snapshot-0.jam and .urbit/chk/snapshot-1.jam.  Each of these
 * contains 64 bits for a mug checksum, then 64 bits for the event number, then a jam of the state.
 * We alternate between writing these two files, so that at least one is always valid.
 *
 * When we start up, we read both files and pick the one with the higher event number.  If either
 * is corrupted, we use the other.
 */
use super::Snapshot;
use crate::mem::NockStack;
use crate::mug::mug_u32;
use crate::noun::{IndirectAtom, Noun, D};
use crate::serialization::{cue, jam};
use either::Either;
use memmap::Mmap;
use memmap::MmapMut;
use std::fs::{File, OpenOptions};
use std::io;
use std::mem;
use std::path::Path;
use std::path::PathBuf;
use std::ptr::copy_nonoverlapping;
use std::ptr::write_bytes;

crate::gdb!();

pub struct DoubleJam {
    path: PathBuf,
    noun: Noun,
}

impl DoubleJam {
    pub fn new<P: AsRef<Path>>(path: P) -> Self {
        Self {
            path: path.as_ref().to_path_buf(),
            noun: D(0),
        }
    }

    fn latest_snapshot(&self, stack: &mut NockStack) -> io::Result<(u8, u64, IndirectAtom)> {
        let res0 = self.load_snapshot(stack, 0);
        let res1 = self.load_snapshot(stack, 1);

        match (res0, res1) {
            (Ok((event_number_0, state_0)), Ok((event_number_1, state_1))) => {
                if event_number_0 > event_number_1 {
                    Ok((0, event_number_0, state_0))
                } else {
                    Ok((1, event_number_1, state_1))
                }
            }
            (Ok((event_number_0, state_0)), Err(_)) => Ok((0, event_number_0, state_0)),
            (Err(_), Ok((event_number_1, state_1))) => Ok((1, event_number_1, state_1)),
            (Err(_), Err(_)) => Err(io::Error::new(
                io::ErrorKind::NotFound,
                "no valid snapshot found",
            )),
        }
    }

    fn load_snapshot(&self, stack: &mut NockStack, number: u8) -> io::Result<(u64, IndirectAtom)> {
        let path = self.path.join(format!("snapshot-{}.jam", number));

        eprintln!("\rload: snapshot at {:?}", path);

        let f = File::open(path)?;

        let in_len = f.metadata().unwrap().len() - 8;
        let word_len = (in_len + 7) >> 3;
        let (event_number, state) = unsafe {
            let in_map = Mmap::map(&f).unwrap();
            let in_ptr = in_map.as_ptr();
            let (mut state, dest) = IndirectAtom::new_raw_mut(stack, word_len as usize);
            let mugged = (*in_ptr.add(0) as u32)
                | ((*in_ptr.add(1) as u32) << 8)
                | ((*in_ptr.add(2) as u32) << 16)
                | ((*in_ptr.add(3) as u32) << 24);
            write_bytes(dest.add(word_len as usize - 1), 0, 8);
            copy_nonoverlapping(in_ptr.add(8), dest as *mut u8, in_len as usize);
            mem::drop(in_map);
            state.normalize(); // know it's not direct because first word is event number

            if mug_u32(stack, state.as_noun()) != mugged {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "snapshot checksum mismatch",
                ));
            }

            (*state.data_pointer(), state)
        };

        Ok((event_number, state))
    }
}

impl Snapshot for DoubleJam {
    fn save(&mut self, _stack: &mut NockStack, noun: Noun) {
        self.noun = noun;
    }

    fn sync(&mut self, stack: &mut NockStack, _epoch: u64, event_number: u64) {
        // Find the latest valid snapshot, and write to the other file.
        let prev_snap = if let Ok((prev_snap, _, _)) = self.latest_snapshot(stack) {
            prev_snap
        } else {
            0
        };
        let snap_number = if prev_snap == 0 { 1 } else { 0 };
        let path = self.path.join(format!("snapshot-{}.jam", snap_number));

        let jammed_arvo = jam(stack, self.noun);
        let state = unsafe {
            let (mut state, dest) = IndirectAtom::new_raw_mut(stack, jammed_arvo.size() + 1);
            dest.write(event_number);
            match jammed_arvo.as_either() {
                Either::Left(direct) => {
                    copy_nonoverlapping(&direct.data() as *const u64, dest.add(1), 1);
                }
                Either::Right(indirect) => {
                    copy_nonoverlapping(indirect.data_pointer(), dest.add(1), jammed_arvo.size());
                }
            };
            state.normalize_as_atom()
        };

        let mugged = mug_u32(stack, state.as_noun());

        let f = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(path)
            .unwrap();

        f.set_len(((state.size() + 1) << 3) as u64).unwrap();
        unsafe {
            let mut out_map = MmapMut::map_mut(&f).unwrap();
            let out_ptr = out_map.as_mut_ptr();
            out_ptr.add(0).write(mugged as u8);
            out_ptr.add(1).write((mugged >> 8) as u8);
            out_ptr.add(2).write((mugged >> 16) as u8);
            out_ptr.add(3).write((mugged >> 24) as u8);
            copy_nonoverlapping(
                state.data_pointer() as *mut u8,
                out_ptr.add(8),
                state.size() << 3,
            );
            out_map.flush().unwrap();

            // This appears to match c3/portable.h: fdatasync for linux, fcntl with F_FULLFSYNC for for
            // macos, and fsync for some other platforms.
            f.sync_data().unwrap();
        };
    }

    fn load(&mut self, stack: &mut NockStack) -> io::Result<(u64, u64, Noun)> {
        let (_num, event_number, state) = self.latest_snapshot(stack)?;

        let jammed_arvo =
            unsafe { IndirectAtom::new_raw(stack, state.size() - 1, state.data_pointer().add(1)) };

        let arvo = cue(stack, jammed_arvo.as_atom());

        Ok((0, event_number, arvo))
    }
}
