use crate::interpreter::{inc, interpret, Tone};
use crate::jets::nock::util::mook;
use crate::jets::text::util::lent;
use crate::mem::NockStack;
use crate::mug::mug_u32;
use crate::newt::Newt;
use crate::noun::{Cell, Noun, Slots, D, T};
use crate::snapshot::{self, Snapshot};
use ares_macros::tas;
use signal_hook;
use signal_hook::consts::SIGINT;
use std::fs::create_dir_all;
use std::io;
use std::path::PathBuf;
use std::result::Result;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

crate::gdb!();

#[allow(dead_code)]
const LOAD_AXIS: u64 = 4;
const PEEK_AXIS: u64 = 22;
const POKE_AXIS: u64 = 23;
#[allow(dead_code)]
const WISH_AXIS: u64 = 10;

// Necessary because Arc::new is not const
lazy_static! {
    pub static ref TERMINATOR: Arc<AtomicBool> = Arc::new(AtomicBool::new(false));
}

/**
 * This is suitable for talking to the king process.  To test, change the arg_c[0] line in
 * u3_lord_init in vere to point at this binary and start vere like normal.
 */
pub fn serf() -> io::Result<()> {
    // Register SIGTERM signal hook to set flag first time, shutdown second time
    signal_hook::flag::register_conditional_shutdown(SIGINT, 1, Arc::clone(&TERMINATOR))?;
    signal_hook::flag::register(SIGINT, Arc::clone(&TERMINATOR))?;

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

    let stack = &mut NockStack::new(256 << 10 << 10, 0);
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
                    match interpret(stack, &mut Some(newt), eve, lyf) {
                        Ok(gat) => {
                            arvo = slot(gat, 7)
                                .expect("serf: play: lifecycle didn't return initial Arvo");
                            current_event_num =
                                lent(eve).expect("serf: play: boot event number failure") as u64;
                            current_mug = mug_u32(stack, arvo);
                        }
                        Err(Tone::Error(_, trace)) => {
                            let tone = Cell::new(stack, D(2), trace);
                            let tang = mook(stack, &mut Some(newt), tone, false)
                                .expect("serf: play: +mook crashed on bail")
                                .tail();
                            let goof = T(stack, &[D(tas!(b"exit")), tang]);
                            newt.play_bail(stack, 0, 0, goof);
                        }
                        Err(Tone::Blocked(_)) => {
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
                            Err(Tone::Error(_, trace)) => {
                                let tone = Cell::new(stack, D(2), trace);
                                let tang = mook(stack, &mut Some(newt), tone, false)
                                    .expect("serf: play: +mook crashed on bail")
                                    .tail();
                                let goof = T(stack, &[D(tas!(b"exit")), tang]);
                                newt.play_bail(stack, current_event_num, current_mug as u64, goof);
                            }
                            Err(Tone::Blocked(_)) => {
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
                //  TODO: assert event numbers are continuous
                let job = slot(writ, 7)?;
                match soft(stack, newt, arvo, POKE_AXIS, job) {
                    Ok(res) => {
                        let cell = res.as_cell().expect("serf: work: +slam returned atom");
                        let fec = cell.head();
                        arvo = cell.tail();
                        snap.save(stack, &mut arvo);

                        current_mug = mug_u32(stack, arvo);
                        current_event_num += 1;

                        newt.work_done(stack, current_event_num, current_mug as u64, fec);
                    }
                    Err(goof) => {
                        //  TODO: on decryption failure in aes_siv, should bail as fast as
                        //  possible, without rendering stack trace or injecting crud event.  See
                        //  c3__evil in vere.

                        clear_interrupt();

                        //  crud = [+(now) [%$ %arvo ~] [%crud goof ovo]]
                        let job_cell = job.as_cell().expect("serf: work: job not a cell");
                        let now = inc(
                            stack,
                            job_cell.head().as_atom().expect("serf: work: now not atom"),
                        )
                        .as_noun();
                        let wire = T(stack, &[D(0), D(tas!(b"arvo")), D(0)]);
                        let crud = T(stack, &[now, wire, D(tas!(b"crud")), goof, job_cell.tail()]);

                        match soft(stack, newt, arvo, POKE_AXIS, crud) {
                            Ok(res) => {
                                let cell =
                                    res.as_cell().expect("serf: work: crud +slam returned atom");
                                let fec = cell.head();
                                arvo = cell.tail();
                                snap.save(stack, &mut arvo);

                                current_mug = mug_u32(stack, arvo);
                                current_event_num += 1;

                                newt.work_swap(
                                    stack,
                                    current_event_num,
                                    current_mug as u64,
                                    crud,
                                    fec,
                                );
                            }
                            Err(goof_crud) => {
                                let lud = T(stack, &[goof_crud, goof, D(0)]);
                                newt.work_bail(stack, lud);
                            }
                        }
                    }
                }
            }
            _ => panic!("got message with unknown tag {}", tag),
        };

        clear_interrupt();
    }

    Ok(())
}

pub fn slam(
    stack: &mut NockStack,
    newt: &mut Newt,
    core: Noun,
    axis: u64,
    ovo: Noun,
) -> Result<Noun, Tone> {
    let pul = T(stack, &[D(9), D(axis), D(0), D(2)]);
    let sam = T(stack, &[D(6), D(0), D(7)]);
    let fol = T(stack, &[D(8), pul, D(9), D(2), D(10), sam, D(0), D(2)]);
    let sub = T(stack, &[core, ovo]);
    interpret(stack, &mut Some(newt), sub, fol)
}

/** Run slam, process stack trace to tang if error */
pub fn soft(
    stack: &mut NockStack,
    newt: &mut Newt,
    core: Noun,
    axis: u64,
    ovo: Noun,
) -> Result<Noun, Noun> {
    match slam(stack, newt, core, axis, ovo) {
        Ok(res) => Ok(res),
        Err(Tone::Error(_, trace)) => {
            let tone = Cell::new(stack, D(2), trace);
            let tang = mook(stack, &mut Some(newt), tone, false)
                .expect("serf: soft: +mook crashed on bail")
                .tail();
            //  XX: noun::Tone or noun::NockErr should use a bail enum system similar to u3m_bail motes;
            //      might be able to replace NockErr with mote and map determinism to individual motes;
            //      for, always set to %exit
            let goof = T(stack, &[D(tas!(b"exit")), tang]);
            Err(goof)
        }
        Err(Tone::Blocked(_)) => panic!("soft: blocked err handling unimplemented"),
    }
}

fn slot(noun: Noun, axis: u64) -> io::Result<Noun> {
    noun.slot(axis)
        .map_err(|_e| io::Error::new(io::ErrorKind::InvalidInput, "Bad axis"))
}

fn clear_interrupt() {
    (*TERMINATOR).store(false, Ordering::Relaxed);
}
