use crate::interpreter::Slogger;
/** Newt: IPC to the king
 *
 * This manages an IPC connection to the king over stdin and stdout.  The protocol is jammed nouns,
 * with the following schema:
 *
 * |%
 * ::  +writ: from king to serf
 * ::
 * +$  writ
 *   $%  $:  %live
 *           $%  [%cram eve=@]
 *               [%exit cod=@]
 *               [%save eve=@]
 *               [%meld ~]
 *               [%pack ~]
 *       ==  ==
 *       [%peek mil=@ sam=*]  :: gang (each path $%([%once @tas @tas path] [%beam @tas beam]))
 *       [%play eve=@ lit=(list ?((pair @da ovum) *))]
 *       [%work mil=@ job=(pair @da ovum)]
 *   ==
 * ::  +plea: from serf to king
 * ::
 * +$  plea
 *   $%  [%live ~]
 *       [%ripe [pro=%1 hon=@ nok=@] eve=@ mug=@]
 *       [%slog pri=@ tank]
 *       [%flog cord]
 *       $:  %peek
 *           $%  [%done dat=(unit (cask))]
 *               [%bail dud=goof]
 *       ==  ==
 *       $:  %play
 *           $%  [%done mug=@]
 *               [%bail eve=@ mug=@ dud=goof]
 *       ==  ==
 *       $:  %work
 *           $%  [%done eve=@ mug=@ fec=(list ovum)]
 *               [%swap eve=@ mug=@ job=(pair @da ovum) fec=(list ovum)]
 *               [%bail lud=(list goof)]
 *       ==  ==
 *   ==
 * --
 *
 * NB: stdin and stdout are generally buffered, and there's no officially supported way to work
 * around that: https://github.com/rust-lang/rust/issues/58326.
 *
 * We use stdin and stdout with File::from_raw_fd(0) and File::from_raw_fd(1), which seems to get
 * around this.  We tested that using io::Stdout requires flushing while this doesn't, but we
 * haven't tested the same for stdin.
 *
 * It's important to not use io::Stdin and io::Stdout directly.  All printfs should use stderr.
 */
use crate::mem::{AllocResult, NockStack};
use crate::noun::{IndirectAtom, Noun, D, T};
use crate::serialization::{cue, jam};
use either::Either;
use std::io::{Read, Write};
use std::os::unix::prelude::FromRawFd;
use std::pin::Pin;
use std::ptr::copy_nonoverlapping;
use std::slice::from_raw_parts_mut;
use sword_macros::tas;

crate::gdb!();

pub struct Newt {
    input: std::fs::File,
    output: std::fs::File,
}

impl Newt {
    pub fn new_from_files(input: std::fs::File, output: std::fs::File) -> Newt {
        Newt { input, output }
    }

    pub fn new() -> Newt {
        Newt {
            input: unsafe { std::fs::File::from_raw_fd(0) },
            output: unsafe { std::fs::File::from_raw_fd(1) },
        }
    }

    pub fn new_mock() -> Newt {
        Newt {
            input: std::fs::File::open("/dev/null").expect("newt: could not open /dev/null"),
            output: std::fs::File::options()
                .read(true)
                .write(true)
                .open("/dev/null")
                .expect("newt: could not open /dev/null"),
        }
    }

    /** Write a noun to the newt.
     *
     * NB: we write 64-bit words, while vere writes bytes.  The extra zero bytes shouldn't be a
     * problem.
     */
    fn write_noun(&mut self, stack: &mut NockStack, noun: Noun) -> AllocResult<()> {
        let atom = jam(stack, noun)?;
        let size = atom.size() << 3;
        // XX: checked add?
        let buf = unsafe { from_raw_parts_mut(stack.struct_alloc::<u8>(size + 5)?, size + 5) };
        buf[0] = 0u8;
        buf[1] = size as u8;
        buf[2] = (size >> 8) as u8;
        buf[3] = (size >> 16) as u8;
        buf[4] = (size >> 24) as u8;
        match atom.as_either() {
            Either::Left(direct) => unsafe {
                copy_nonoverlapping(
                    &direct.data() as *const u64 as *const u8,
                    buf.as_mut_ptr().add(5),
                    size,
                );
            },
            Either::Right(indirect) => unsafe {
                // REVIEW: is this safe/the right way to do this?
                copy_nonoverlapping(
                    indirect.data_pointer() as *const u8,
                    buf.as_mut_ptr().add(5),
                    size,
                );
            },
        };
        self.output.write_all(buf).unwrap();
        Ok(())
    }

    /** Send %ripe, the first event.
     *
     * eve  =   event number
     * mug  =   mug of Arvo after above event
     */
    pub fn ripe(&mut self, stack: &mut NockStack, eve: u64, mug: u64) -> AllocResult<()> {
        let version = T(
            stack,
            &[
                D(1),   // newt protocol
                D(139), // hoon kelvin
                D(4),   // nock kelvin
            ],
        )?;
        let ripe = T(stack, &[D(tas!(b"ripe")), version, D(eve), D(mug)])?;
        self.write_noun(stack, ripe);
        Ok(())
    }

    /** Send %live, acknowledging. */
    pub fn live(&mut self, stack: &mut NockStack) -> AllocResult<()> {
        let live = T(stack, &[D(tas!(b"live")), D(0)])?;
        self.write_noun(stack, live);
        Ok(())
    }

    /** Send %peek %done, successfully scried. */
    pub fn peek_done(&mut self, stack: &mut NockStack, dat: Noun) -> AllocResult<()> {
        let peek = T(stack, &[D(tas!(b"peek")), D(tas!(b"done")), dat])?;
        self.write_noun(stack, peek);
        Ok(())
    }

    /** Send %peek %bail, unsuccessfully scried.
     *
     * dud  =   goof
     */
    pub fn peek_bail(&mut self, stack: &mut NockStack, dud: Noun) -> AllocResult<()> {
        let peek = T(stack, &[D(tas!(b"peek")), D(tas!(b"bail")), dud])?;
        self.write_noun(stack, peek);
        Ok(())
    }

    /** Send %play %done, successfully replayed events.
     *
     * mug  =   mug of Arvo after full replay
     */
    pub fn play_done(&mut self, stack: &mut NockStack, mug: u64) -> AllocResult<()> {
        let play = T(stack, &[D(tas!(b"play")), D(tas!(b"done")), D(mug)])?;
        self.write_noun(stack, play);
        Ok(())
    }

    /** Send %play %bail, failed to replay events.
     *
     * eve  =   last good event number
     * mug  =   mug of Arvo after above event
     * dud  =   goof when trying next event
     */
    pub fn play_bail(&mut self, stack: &mut NockStack, eve: u64, mug: u64, dud: Noun) -> AllocResult<()> {
        let play = T(
            stack,
            &[D(tas!(b"play")), D(tas!(b"bail")), D(eve), D(mug), dud],
        )?;
        self.write_noun(stack, play);
        Ok(())
    }

    /** Send %work %done, successfully ran event.
     *
     * eve  =   new event number
     * mug  =   mug of Arvo after above event
     * fec  =   list of effects
     */
    pub fn work_done(&mut self, stack: &mut NockStack, eve: u64, mug: u64, fec: Noun) -> AllocResult<()> {
        let work = T(
            stack,
            &[D(tas!(b"work")), D(tas!(b"done")), D(eve), D(mug), fec],
        )?;
        self.write_noun(stack, work);
        Ok(())
    }

    /** Send %work %swap, successfully replaced failed event.
     *
     * eve  =   new event number
     * mug  =   mug of Arvo after above event
     * job  =   event performed instead of the one given to serf by king
     * fec  =   list of effects
     */
    pub fn work_swap(&mut self, stack: &mut NockStack, eve: u64, mug: u64, job: Noun, fec: Noun) -> AllocResult<()> {
        let work = T(
            stack,
            &[D(tas!(b"work")), D(tas!(b"swap")), D(eve), D(mug), job, fec],
        )?;
        self.write_noun(stack, work);
        Ok(())
    }

    pub fn slogger(&self) -> Result<Pin<Box<dyn Slogger + Unpin>>, std::io::Error> {
        let input = self.input.try_clone()?;
        let output = self.output.try_clone()?;
        Ok(std::boxed::Box::pin(NewtSlogger(Newt { input, output })))
    }

    /** Send %work %bail, failed to run event.
     *
     * lud  =   list of goof
     */
    pub fn work_bail(&mut self, stack: &mut NockStack, lud: Noun) -> AllocResult<()> {
        let work = T(stack, &[D(tas!(b"work")), D(tas!(b"bail")), lud])?;
        self.write_noun(stack, work);
        Ok(())
    }

    /** Fetch next message. */
    pub fn next(&mut self, stack: &mut NockStack) -> AllocResult<Option<Noun>> {
        let mut header: Vec<u8> = vec![0; 5];
        if let Err(err) = self.input.read_exact(&mut header) {
            if err.kind() == std::io::ErrorKind::UnexpectedEof {
                return Ok(None);
            } else {
                panic!("Newt::next: Error reading header: {}", err);
            }
        }

        let byte_len = u32::from_le_bytes([header[1], header[2], header[3], header[4]]) as usize;

        let atom = unsafe {
            let (mut atom, dest) = IndirectAtom::new_raw_mut_bytes(stack, byte_len)?;
            if let Err(err) = self.input.read_exact(dest) {
                if err.kind() == std::io::ErrorKind::UnexpectedEof {
                    return Ok(None);
                } else {
                    panic!("Newt::next: Error reading body: {}", err);
                }
            }
            atom.normalize_as_atom()
        };

        Ok(Some(cue(stack, atom).expect("Newt::next: bad jammed noun")))
    }
}

impl Slogger for Newt {
    fn slog(&mut self, stack: &mut NockStack, pri: u64, tank: Noun) -> AllocResult<()> {
        let slog = T(stack, &[D(tas!(b"slog")), D(pri), tank])?;
        self.write_noun(stack, slog);
        Ok(())
    }

    fn flog(&mut self, stack: &mut NockStack, cord: Noun) -> AllocResult<()> {
        let flog = T(stack, &[D(tas!(b"flog")), cord])?;
        self.write_noun(stack, flog);
        Ok(())
    }
}

impl Default for Newt {
    fn default() -> Self {
        Self::new()
    }
}

struct NewtSlogger(Newt);

impl Slogger for NewtSlogger {
    fn slog(&mut self, stack: &mut NockStack, pri: u64, tank: Noun) -> AllocResult<()> {
        self.0.slog(stack, pri, tank)?;
        Ok(())
    }

    fn flog(&mut self, stack: &mut NockStack, cord: Noun) -> AllocResult<()> {
        self.0.flog(stack, cord)?;
        Ok(())
    }
}
