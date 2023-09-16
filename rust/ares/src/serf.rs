use crate::interpreter::{interpret, NockErr};
use crate::jets::nock::util::mook;
use crate::jets::text::util::lent;
use crate::mem::NockStack;
use crate::mug::mug_u32;
use crate::newt::Newt;
use crate::noun::{Noun, Cell, Slots, D, T};
use crate::snapshot::{self, Snapshot};
use ares_macros::tas;
use std::fs::create_dir_all;
use std::io;
use std::path::PathBuf;
use std::result::Result;
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
    // TODO: switch to Pma when ready
    // let snap = &mut snapshot::pma::Pma::new(snap_path);
    let snap = &mut snapshot::double_jam::DoubleJam::new(snap_path);

    let stack = &mut NockStack::new(96 << 10 << 10, 0);
    let newt = &mut Newt::new();

    let (_epoch, loaded_event_num, mut arvo) = snap.load(stack).unwrap_or((0, 0, D(0)));
    let mut current_event_num = loaded_event_num;
    let loaded_mug = mug_u32(stack, arvo);
    let mut current_mug = loaded_mug;

    newt.ripe(stack, current_event_num, loaded_mug as u64);
    
    // Can't use for loop because it borrows newt
    while let Some(writ) = newt.next(stack) {
        let tag = slot(writ, 2)?.as_direct().unwrap();
        match tag.data() {
            tas!(b"live") => {
                let inner = slot(writ, 6)?.as_direct().unwrap();
                match inner.data() {
                    tas!(b"cram") => eprintln!("cram"),
                    tas!(b"exit") => eprintln!("exit"),
                    tas!(b"save") => {
                        // XX what is eve for?
                        eprintln!("save");
                        snap.sync(stack, 0, current_event_num);
                    }
                    tas!(b"meld") => eprintln!("meld"),
                    tas!(b"pack") => eprintln!("pack"),
                    _ => eprintln!("unknown live"),
                }
                newt.live(stack);
            }
            tas!(b"peek") => {
                let sam = slot(writ, 7)?;
                let res = slam(stack, newt, arvo, PEEK_AXIS, sam)
                    .expect("peek error handling unimplemented");
                newt.peek_done(stack, res);
            }
            tas!(b"play") => {
                // apply lifecycle to first batch
                if current_event_num == 0 {
                    let eve = slot(writ, 7)?;
                    let sub = T(stack, &[D(0), D(3)]);
                    let lyf = T(stack, &[D(2), sub, D(0), D(2)]);
                    //  XX: TODO
                    match interpret(stack, &mut Some(newt), eve, lyf) {
                        Ok(gat) => {
                            arvo = slot(gat, 7).expect("serf: play: lifecycle didn't return initial Arvo");
                            current_event_num = lent(eve).expect("serf: play: boot event number failure") as u64;
                            current_mug = mug_u32(stack, arvo);
                        }
                        Err(NockErr::Error(trace)) => {
                            let tone = Cell::new(stack, D(2), trace);
                            let tang = mook(stack, &mut Some(newt), tone, false)
                                .expect("serf: play: +mook crashed on bail")
                                .tail();
                            let goof = T(stack, &[D(tas!(b"exit")), tang]);
                            newt.play_bail(stack, 0, 0, goof);
                        }
                        Err(NockErr::Blocked(_)) => {
                            panic!("play: blocked err handling unimplemented")
                        }
                    }
                } else {
                    // do we need to assert something here?
                    // current_event_num = slot(writ, 6)?.as_direct().unwrap().data();

                    let mut lit = slot(writ, 7)?;
                    while let Ok(cell) = lit.as_cell() {
                        let ovo = cell.head();
                        
                        match slam(stack, newt, arvo, POKE_AXIS, ovo) {
                            Ok(res) => {
                                let cell = res.as_cell().expect("serf: play: +slam returned atom");
                                arvo = cell.tail();
                                current_mug = mug_u32(stack, arvo);
                                current_event_num += 1;
                            }
                            Err(NockErr::Error(trace)) => {
                                let tone = Cell::new(stack, D(2), trace);
                                let tang = mook(stack, &mut Some(newt), tone, false)
                                    .expect("serf: play: +mook crashed on bail")
                                    .tail();
                                let goof = T(stack, &[D(tas!(b"exit")), tang]);
                                newt.play_bail(stack, current_event_num, current_mug as u64, goof);
                            }
                            Err(NockErr::Blocked(_)) => {
                                panic!("play: blocked err handling unimplemented")
                            }
                        }

                        lit = cell.tail();
                    }
                };

                snap.save(stack, &mut arvo);
                newt.play_done(stack, current_mug as u64);
            }
            tas!(b"work") => {
                //  XX: what is in slot 6? it's mil_w in Vere Serf
                let job = slot(writ, 7)?;
                match slam(stack, newt, arvo, POKE_AXIS, job) {
                    Ok(res) => {
                        let cell = res.as_cell().expect("serf: work: +slam returned atom");
                        let fec = cell.head();
                        arvo = cell.tail();
                        snap.save(stack, &mut arvo);
                        
                        current_mug = mug_u32(stack, arvo);
                        current_event_num += 1;
        
                        newt.work_done(stack, current_event_num, current_mug as u64, fec);
                    }
                    Err(NockErr::Error(trace)) => {
                        //  XX: Our Arvo can't currently handle %crud, so just bail
                        let tone = Cell::new(stack, D(2), trace);
                        let tang = mook(stack, &mut Some(newt), tone, false)
                            .expect("serf: play: +mook crashed on bail")
                            .tail();
                        let goof = T(stack, &[D(tas!(b"exit")), tang]);
                        //  lud = (list goof)
                        let lud = T(stack, &[goof, D(0)]);
                        newt.work_bail(stack, lud);
                    }
                    Err(NockErr::Blocked(_)) => {
                        panic!("play: blocked err handling unimplemented")
                    }
                }
            }
            _ => panic!("got message with unknown tag {}", tag),
        };
    }

    Ok(())
}

pub fn slam(
    stack: &mut NockStack,
    newt: &mut Newt,
    core: Noun,
    axis: u64,
    ovo: Noun,
) -> Result<Noun, NockErr> {
    let pul = T(stack, &[D(9), D(axis), D(0), D(2)]);
    let sam = T(stack, &[D(6), D(0), D(7)]);
    let fol = T(stack, &[D(8), pul, D(9), D(2), D(10), sam, D(0), D(2)]);
    let sub = T(stack, &[core, ovo]);
    interpret(stack, &mut Some(newt), sub, fol)
}

fn slot(noun: Noun, axis: u64) -> io::Result<Noun> {
    noun.slot(axis)
        .map_err(|_e| io::Error::new(io::ErrorKind::InvalidInput, "Bad axis"))
}
