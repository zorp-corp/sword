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
    let pier_path_string = std::env::args()
        .nth(2)
        .ok_or(io::Error::new(io::ErrorKind::Other, "no pier path"))?;
    let mut pier_path = PathBuf::from(pier_path_string);
    pier_path.push(".urb");
    pier_path.push("chk");
    create_dir_all(&pier_path)?;
    pier_path.push("snapshot.jam");

    let ref mut stack = NockStack::new(8 << 10 << 10, 0);
    let ref mut newt = Newt::new();

    let (mut eve, mut cor) = load(stack, &pier_path).unwrap_or((0, D(0)));
    let mug = mug_u32(stack, cor);

    newt.ripe(stack, eve, mug as u64);

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
                        save(stack, &pier_path, eve, cor);
                    }
                    tas!(b"meld") => eprintln!("meld"),
                    tas!(b"pack") => eprintln!("pack"),
                    _ => eprintln!("unknown live"),
                }
                newt.live(stack);
            }
            tas!(b"peek") => {
                let sam = raw_slot(writ, 7);
                let res = slam(stack, newt, cor, PEEK_AXIS, sam);
                newt.peek_done(stack, res);
            }
            tas!(b"play") => {
                let run = if eve == 0 {
                    // apply lifecycle to first batch
                    let lit = raw_slot(writ, 7);
                    let sub = Cell::new_tuple(stack, &[D(0), D(3)]).as_noun();
                    let lyf = Cell::new_tuple(stack, &[D(2), sub, D(0), D(2)]).as_noun();
                    let gat = interpret(stack, &mut Some(newt), lit, lyf);
                    cor = raw_slot(gat, 7);
                    false
                } else {
                    true
                };

                // do we need to assert something here?
                // eve = raw_slot(writ, 6).as_direct().unwrap().data();

                let mut lit = raw_slot(writ, 7);
                loop {
                    if let Ok(cell) = lit.as_cell() {
                        if run {
                            let ovo = cell.head();
                            let res = slam(stack, newt, cor, POKE_AXIS, ovo).as_cell().unwrap();
                            cor = res.tail();
                        }
                        eve += 1;
                        lit = cell.tail();
                    } else {
                        break;
                    }
                }
                newt.play_done(stack, 0);
            }
            tas!(b"work") => {
                let ovo = raw_slot(writ, 7);
                let res = slam(stack, newt, cor, POKE_AXIS, ovo).as_cell().unwrap();
                let fec = res.head();
                cor = res.tail();

                eve += 1;

                newt.work_done(stack, eve, 0, fec);
            }
            _ => panic!("got message with unknown tag {:?}", tag),
        };
    }

    Ok(())
}

pub fn slam(stack: &mut NockStack, newt: &mut Newt, cor: Noun, axis: u64, ovo: Noun) -> Noun {
    let pul = Cell::new_tuple(stack, &[D(9), D(axis), D(0), D(2)]).as_noun();
    let sam = Cell::new_tuple(stack, &[D(6), D(0), D(7)]).as_noun();
    let fol = Cell::new_tuple(stack, &[D(8), pul, D(9), D(2), D(10), sam, D(0), D(2)]).as_noun();
    let sub = Cell::new_tuple(stack, &[cor, ovo]).as_noun();
    interpret(stack, &mut Some(newt), sub, fol)
}

fn save(stack: &mut NockStack, snap_path: &PathBuf, eve: u64, cor: Noun) {
    let state = Cell::new(stack, D(eve), cor).as_noun();
    let mugged = mug(stack, state).as_noun();
    let snapshot = Cell::new(stack, mugged, state).as_noun();

    let jammed = jam(stack, snapshot);
    let f_out = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(snap_path)
        .unwrap();

    f_out.set_len((jammed.size() << 3) as u64).unwrap();
    unsafe {
        let mut out_map = MmapMut::map_mut(&f_out).unwrap();
        copy_nonoverlapping(
            jammed.data_pointer() as *mut u8,
            out_map.as_mut_ptr(),
            jammed.size() << 3,
        );
        out_map.flush().unwrap();
    };
}

fn load(stack: &mut NockStack, snap_path: &PathBuf) -> io::Result<(u64, Noun)> {
    let f_in = File::open(snap_path)?;

    let in_len = f_in.metadata().unwrap().len();
    let jammed = unsafe {
        let in_map = Mmap::map(&f_in).unwrap();
        let word_len = (in_len + 7) >> 3;
        let (mut atom, dest) = IndirectAtom::new_raw_mut(stack, word_len as usize);
        write_bytes(dest.add(word_len as usize - 1), 0, 8);
        copy_nonoverlapping(in_map.as_ptr(), dest as *mut u8, in_len as usize);
        mem::drop(in_map);
        atom.normalize_as_atom()
    };

    let snapshot = cue(stack, jammed).as_cell().unwrap();
    let state = snapshot.tail().as_cell().unwrap();
    assert!(
        unsafe {
            mug(stack, state.as_noun())
                .as_noun()
                .raw_equals(snapshot.head())
        },
        "snapshot is corrupt"
    );
    let eve = state.head().as_direct().unwrap().data();
    let cor = state.tail();

    Ok((eve, cor))
}
