use crate::interpreter::{interpret, raw_slot};
use crate::mem::NockStack;
use crate::mug::{mug, mug_u32};
use crate::newt::Newt;
use crate::noun::{Cell, IndirectAtom, Noun, D};
use crate::serialization::{cue, jam};
use ares_macros::tas;
use memmap::Mmap;
use memmap::MmapMut;
use std::fs::{create_dir_all, File, OpenOptions};
use std::io;
use std::mem;
use std::path::PathBuf;
use std::ptr::copy_nonoverlapping;
use std::ptr::write_bytes;

#[allow(dead_code)]
const LOAD_AXIS: u64 = 4;
const PEEK_AXIS: u64 = 22;
const POKE_AXIS: u64 = 23;
#[allow(dead_code)]
const WISH_AXIS: u64 = 10;

/**
 * This is suitable for talking to the king process.  To test, change the arg_c[0] line in
 * u3_lord_init in vere to point at this binary and start vere like normal.
 */
pub fn serf() -> io::Result<()> {
    let snap_path_string = std::env::args()
        .nth(2)
        .ok_or(io::Error::new(io::ErrorKind::Other, "no pier path"))?;
    let mut snap_path = PathBuf::from(snap_path_string);
    snap_path.push(".urb");
    snap_path.push("chk");
    create_dir_all(&snap_path)?;

    let ref mut stack = NockStack::new(8 << 10 << 10, 0);
    let ref mut newt = Newt::new();
    let mut snap_number;  // Last valid snapshot number.
    let mut event_number;
    let mut arvo;

    (event_number, arvo, snap_number) = load(stack, snap_path.clone()).unwrap_or((0, D(0), 0));
    let mug = mug_u32(stack, arvo);

    newt.ripe(stack, event_number, mug as u64);

    // Can't use for loop because it borrows newt
    loop {
        let writ = if let Some(writ) = newt.next(stack) {
            writ
        } else {
            break;
        };

        let tag = raw_slot(writ, 2).as_direct().unwrap();
        match tag.data() {
            tas!(b"live") => {
                let inner = raw_slot(writ, 6);
                match inner.as_direct().unwrap().data() {
                    tas!(b"cram") => eprintln!("cram"),
                    tas!(b"exit") => eprintln!("exit"),
                    tas!(b"save") => {
                        // XX what is eve for?
                        eprintln!("save");
                        snap_number = if snap_number == 0 { 1 } else { 0 };
                        save(stack, snap_path.clone(), snap_number, event_number, arvo);
                    }
                    tas!(b"meld") => eprintln!("meld"),
                    tas!(b"pack") => eprintln!("pack"),
                    _ => eprintln!("unknown live"),
                }
                newt.live(stack);
            }
            tas!(b"peek") => {
                let sam = raw_slot(writ, 7);
                let res = slam(stack, newt, arvo, PEEK_AXIS, sam);
                newt.peek_done(stack, res);
            }
            tas!(b"play") => {
                let run = if event_number == 0 {
                    // apply lifecycle to first batch
                    let lit = raw_slot(writ, 7);
                    let sub = Cell::new_tuple(stack, &[D(0), D(3)]).as_noun();
                    let lyf = Cell::new_tuple(stack, &[D(2), sub, D(0), D(2)]).as_noun();
                    let gat = interpret(stack, &mut Some(newt), lit, lyf);
                    arvo = raw_slot(gat, 7);
                    false
                } else {
                    true
                };

                // do we need to assert something here?
                // event_number = raw_slot(writ, 6).as_direct().unwrap().data();

                let mut lit = raw_slot(writ, 7);
                loop {
                    if let Ok(cell) = lit.as_cell() {
                        if run {
                            let ovo = cell.head();
                            let res = slam(stack, newt, arvo, POKE_AXIS, ovo).as_cell().unwrap();
                            arvo = res.tail();
                        }
                        event_number += 1;
                        lit = cell.tail();
                    } else {
                        break;
                    }
                }
                newt.play_done(stack, 0);
            }
            tas!(b"work") => {
                let ovo = raw_slot(writ, 7);
                let res = slam(stack, newt, arvo, POKE_AXIS, ovo).as_cell().unwrap();
                let fec = res.head();
                arvo = res.tail();

                event_number += 1;

                newt.work_done(stack, event_number, 0, fec);
            }
            _ => panic!("got message with unknown tag {:?}", tag),
        };
    }

    Ok(())
}

pub fn slam(stack: &mut NockStack, newt: &mut Newt, arvo: Noun, axis: u64, ovo: Noun) -> Noun {
    let pul = Cell::new_tuple(stack, &[D(9), D(axis), D(0), D(2)]).as_noun();
    let sam = Cell::new_tuple(stack, &[D(6), D(0), D(7)]).as_noun();
    let fol = Cell::new_tuple(stack, &[D(8), pul, D(9), D(2), D(10), sam, D(0), D(2)]).as_noun();
    let sub = Cell::new_tuple(stack, &[arvo, ovo]).as_noun();
    interpret(stack, &mut Some(newt), sub, fol)
}

fn save(
    stack: &mut NockStack,
    mut snap_path: PathBuf,
    snap_number: u8,
    event_number: u64,
    arvo: Noun,
) {
    snap_path.push(format!("snapshot-{}.snap", snap_number));

    let state = Cell::new(stack, D(event_number), arvo).as_noun();
    let mugged = mug(stack, state).as_noun();
    let snapshot = Cell::new(stack, mugged, state).as_noun();

    let jammed = jam(stack, snapshot);
    let f = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(snap_path)
        .unwrap();

    f.set_len((jammed.size() << 3) as u64).unwrap();
    unsafe {
        let mut out_map = MmapMut::map_mut(&f).unwrap();
        copy_nonoverlapping(
            jammed.data_pointer() as *mut u8,
            out_map.as_mut_ptr(),
            jammed.size() << 3,
        );
        out_map.flush().unwrap();
    };
}

fn load(stack: &mut NockStack, snap_path: PathBuf) -> io::Result<(u64, Noun, u8)> {
    let res0 = load_snapshot(stack, snap_path.clone(), 0);
    let res1 = load_snapshot(stack, snap_path.clone(), 1);

    match (res0, res1) {
        (Ok((event_number_0, arvo_0)), Ok((event_number_1, arvo_1))) => {
            if event_number_0 > event_number_1 {
                Ok((event_number_0, arvo_0, 0))
            } else {
                Ok((event_number_1, arvo_1, 1))
            }
        }
        (Ok((event_number_0, arvo_0)), Err(_)) => Ok((event_number_0, arvo_0, 0)),
        (Err(_), Ok((event_number_1, arvo_1))) => Ok((event_number_1, arvo_1, 1)),
        (Err(_), Err(_)) => Err(io::Error::new(
            io::ErrorKind::NotFound,
            "no valid snapshot found",
        )),
    }
}

fn load_snapshot(
    stack: &mut NockStack,
    mut snap_path: PathBuf,
    number: u8,
) -> io::Result<(u64, Noun)> {
    snap_path.push(format!("snapshot-{}.snap", number));

    eprintln!("\rload: snapshot at {:?}", snap_path);

    let f = File::open(snap_path)?;

    let in_len = f.metadata().unwrap().len();
    let jammed = unsafe {
        let in_map = Mmap::map(&f).unwrap();
        let word_len = (in_len + 7) >> 3;
        let (mut atom, dest) = IndirectAtom::new_raw_mut(stack, word_len as usize);
        write_bytes(dest.add(word_len as usize - 1), 0, 8);
        copy_nonoverlapping(in_map.as_ptr(), dest as *mut u8, in_len as usize);
        mem::drop(in_map);
        atom.normalize_as_atom()
    };

    let snapshot = cue(stack, jammed).as_cell().unwrap();
    let state = snapshot.tail().as_cell().unwrap();
    if !unsafe {
        snapshot
            .head()
            .raw_equals(mug(stack, state.as_noun()).as_noun())
    } {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "snapshot checksum failed",
        ))
    }
    let event_number = state.head().as_direct().unwrap().data();
    let arvo = state.tail();

    Ok((event_number, arvo))
}
