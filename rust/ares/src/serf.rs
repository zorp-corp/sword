use crate::interpreter::{interpret, raw_slot};
use crate::mem::NockStack;
use crate::mug::mug_u32;
use crate::newt::Newt;
use crate::noun::{Noun, D, T};
use crate::snapshot::{self, Snapshot};
use ares_macros::tas;
use std::fs::create_dir_all;
use std::io;
use std::path::PathBuf;
use std::thread::sleep;
use std::time;

crate::gdb!();

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
    sleep(time::Duration::from_secs(0));
    let snap_path_string = std::env::args()
        .nth(2)
        .ok_or(io::Error::new(io::ErrorKind::Other, "no pier path"))?;
    let mut snap_path = PathBuf::from(snap_path_string);
    snap_path.push(".urb");
    snap_path.push("chk");
    create_dir_all(&snap_path)?;
    // PMA is currently limited to ~650KB, use DoubleJam for anything bigger
    // let ref mut snap = snapshot::double_jam::DoubleJam::new(snap_path);
    let ref mut snap = snapshot::pma::Pma::new(snap_path);

    let stack = &mut NockStack::new(96 << 10 << 10, 0);
    let newt = &mut Newt::new();

    let (_epoch, mut event_number, mut arvo) = snap.load(stack).unwrap_or((0, 0, D(0)));
    let mug = mug_u32(stack, arvo);

    newt.ripe(stack, event_number, mug as u64);

    // Can't use for loop because it borrows newt
    while let Some(writ) = newt.next(stack) {
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
                        snap.sync(stack, 0, event_number);
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
                    let sub = T(stack, &[D(0), D(3)]);
                    let lyf = T(stack, &[D(2), sub, D(0), D(2)]);
                    let gat = interpret(stack, &mut Some(newt), lit, lyf);
                    arvo = raw_slot(gat, 7);
                    false
                } else {
                    true
                };

                // do we need to assert something here?
                // event_number = raw_slot(writ, 6).as_direct().unwrap().data();

                let mut lit = raw_slot(writ, 7);
                while let Ok(cell) = lit.as_cell() {
                    if run {
                        let ovo = cell.head();
                        let res = slam(stack, newt, arvo, POKE_AXIS, ovo).as_cell().unwrap();
                        arvo = res.tail();
                    }
                    event_number += 1;
                    lit = cell.tail();
                }

                snap.save(stack, &mut arvo);
                newt.play_done(stack, 0);
            }
            tas!(b"work") => {
                let ovo = raw_slot(writ, 7);
                let res = slam(stack, newt, arvo, POKE_AXIS, ovo).as_cell().unwrap();
                let fec = res.head();
                arvo = res.tail();
                snap.save(stack, &mut arvo);

                event_number += 1;

                newt.work_done(stack, event_number, 0, fec);
            }
            _ => panic!("got message with unknown tag {}", tag),
        };
    }

    Ok(())
}

pub fn slam(stack: &mut NockStack, newt: &mut Newt, core: Noun, axis: u64, ovo: Noun) -> Noun {
    let pul = T(stack, &[D(9), D(axis), D(0), D(2)]);
    let sam = T(stack, &[D(6), D(0), D(7)]);
    let fol = T(stack, &[D(8), pul, D(9), D(2), D(10), sam, D(0), D(2)]);
    let sub = T(stack, &[core, ovo]);
    interpret(stack, &mut Some(newt), sub, fol)
}
