use crate::hamt::Hamt;
use crate::interpreter;
use crate::interpreter::{inc, interpret, Error};
use crate::jets::cold::Cold;
use crate::jets::hot::{Hot, HotEntry};
use crate::jets::list::util::{lent, zing};
use crate::jets::nock::util::mook;
use crate::jets::warm::Warm;
use crate::mem::NockStack;
use crate::mug::*;
use crate::newt::Newt;
use crate::noun::{Atom, Cell, DirectAtom, Noun, Slots, D, T};
use crate::snapshot::double_jam::DoubleJam;
use crate::snapshot::Snapshot;
use crate::trace::*;
use ares_macros::tas;
use hw_exception;
use signal_hook;
use signal_hook::consts::SIGINT;
use std::fs::create_dir_all;
use std::io;
use std::path::PathBuf;
use std::result::Result;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;

crate::gdb!();

const FLAG_TRACE: u32 = 1 << 8;

struct Context {
    epoch: u64,
    event_num: u64,
    snapshot: DoubleJam,
    arvo: Noun,
    mug: u32,
    nock_context: interpreter::Context,
}

impl Context {
    pub fn new(
        snap_path: &PathBuf,
        trace_info: Option<TraceInfo>,
        constant_hot_state: &[HotEntry],
    ) -> Self {
        // TODO: switch to Pma when ready
        // let snap = &mut snapshot::pma::Pma::new(snap_path);
        let mut snapshot = DoubleJam::new(snap_path);
        let mut stack = NockStack::new(512 << 10 << 10, 0);

        let cold = Cold::new(&mut stack);
        let hot = Hot::init(&mut stack, constant_hot_state);

        let (epoch, event_num, arvo) = snapshot.load(&mut stack).unwrap_or((0, 0, D(0)));
        let mug = mug_u32(&mut stack, arvo);

        let nock_context = interpreter::Context {
            stack,
            newt: Newt::new(),
            cold,
            warm: Warm::new(),
            hot,
            cache: Hamt::<Noun>::new(),
            scry_stack: D(0),
            trace_info,
        };

        Context {
            epoch,
            event_num,
            snapshot,
            arvo,
            mug,
            nock_context,
        }
    }

    //
    // Setters
    //

    pub fn event_update(&mut self, new_event_num: u64, new_arvo: Noun) {
        //  XX: assert event numbers are continuous
        self.arvo = new_arvo;
        self.event_num = new_event_num;
        self.snapshot
            .save(&mut self.nock_context.stack, &mut self.arvo);
        self.mug = mug_u32(&mut self.nock_context.stack, self.arvo);
    }

    //
    // Snapshot functions
    //

    pub fn sync(&mut self) {
        self.snapshot
            .sync(&mut self.nock_context.stack, self.epoch, self.event_num);
    }

    //
    // Newt functions
    //

    pub fn next(&mut self) -> Option<Noun> {
        self.nock_context.newt.next(&mut self.nock_context.stack)
    }

    pub fn ripe(&mut self) {
        self.nock_context.newt.ripe(
            &mut self.nock_context.stack,
            self.event_num,
            self.mug as u64,
        );
    }

    pub fn live(&mut self) {
        self.nock_context.newt.live(&mut self.nock_context.stack);
    }

    pub fn peek_done(&mut self, dat: Noun) {
        self.nock_context
            .newt
            .peek_done(&mut self.nock_context.stack, dat);
    }

    pub fn play_done(&mut self) {
        self.nock_context
            .newt
            .play_done(&mut self.nock_context.stack, self.mug as u64);
    }

    pub fn play_bail(&mut self, dud: Noun) {
        self.nock_context.newt.play_bail(
            &mut self.nock_context.stack,
            self.event_num,
            self.mug as u64,
            dud,
        );
    }

    pub fn work_done(&mut self, fec: Noun) {
        self.nock_context.newt.work_done(
            &mut self.nock_context.stack,
            self.event_num,
            self.mug as u64,
            fec,
        );
    }

    pub fn work_swap(&mut self, job: Noun, fec: Noun) {
        self.nock_context.newt.work_swap(
            &mut self.nock_context.stack,
            self.event_num,
            self.mug as u64,
            job,
            fec,
        );
    }

    pub fn work_bail(&mut self, lud: Noun) {
        self.nock_context
            .newt
            .work_bail(&mut self.nock_context.stack, lud);
    }
}

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
pub fn serf(constant_hot_state: &[HotEntry]) -> io::Result<()> {
    // Register SIGINT signal hook to set flag first time, shutdown second time
    signal_hook::flag::register_conditional_shutdown(SIGINT, 1, Arc::clone(&TERMINATOR))?;
    signal_hook::flag::register(SIGINT, Arc::clone(&TERMINATOR))?;

    unsafe {
        // Register a hook for SIGSEGV, which captures and throws a backtrace.
        hw_exception::register_hook(&[hw_exception::Signo::SIGSEGV], |e| hw_exception::throw(e));
    }

    let pier_path_string = std::env::args()
        .nth(2)
        .ok_or(io::Error::new(io::ErrorKind::Other, "no pier path"))?;
    let pier_path = PathBuf::from(pier_path_string);
    let mut snap_path = pier_path.clone();
    snap_path.push(".urb");
    snap_path.push("chk");
    create_dir_all(&snap_path)?;

    let wag: u32 = std::env::args()
        .nth(4)
        .ok_or(io::Error::new(io::ErrorKind::Other, "no flag bitmap"))?
        .parse()
        .or(Err(io::Error::new(
            io::ErrorKind::Other,
            "flag bitmap is not integer",
        )))?;

    let mut trace_info = if wag & FLAG_TRACE != 0 {
        create_trace_file(pier_path).ok()
    } else {
        None
    };
    if let Some(ref mut info) = trace_info.as_mut() {
        if let Err(_e) = write_metadata(info) {
            //  XX: need NockStack allocated string interpolation
            // eprintln!("\rError initializing trace file: {:?}", e);
            trace_info = None;
        }
    }

    let mut context = Context::new(&snap_path, trace_info, constant_hot_state);
    context.ripe();

    // Can't use for loop because it borrows newt
    while let Some(writ) = context.next() {
        // Reset the local cache and scry handler stack
        context.nock_context.cache = Hamt::<Noun>::new();
        context.nock_context.scry_stack = D(0);
        context.nock_context.stack.frame_push(0);

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
                        context.sync();
                    }
                    tas!(b"meld") => eprintln!("meld"),
                    tas!(b"pack") => eprintln!("pack"),
                    _ => eprintln!("unknown live"),
                }
                context.live();
            }
            tas!(b"peek") => {
                let ovo = slot(writ, 7)?;
                let res = peek(&mut context, ovo);
                context.peek_done(res);
            }
            tas!(b"play") => {
                let lit = slot(writ, 7)?;
                if context.epoch == 0 && context.event_num == 0 {
                    // apply lifecycle to first batch
                    play_life(&mut context, lit);
                } else {
                    play_list(&mut context, lit);
                };
            }
            tas!(b"work") => {
                //  XX: what is in slot 6? it's mil_w in Vere Serf
                let job = slot(writ, 7)?;
                work(&mut context, job);
            }
            _ => panic!("got message with unknown tag {}", tag),
        };

        clear_interrupt();

        // Persist data that should survive between events
        //  XX: Such data should go in the PMA once that's available
        unsafe {
            let stack = &mut context.nock_context.stack;
            stack.preserve(&mut context.arvo);
            stack.preserve(&mut context.nock_context.cold);
            stack.preserve(&mut context.nock_context.warm);
            stack.frame_pop();
        }
    }

    Ok(())
}

fn slam(context: &mut Context, axis: u64, ovo: Noun) -> Result<Noun, Error> {
    let arvo = context.arvo;
    let stack = &mut context.nock_context.stack;
    let pul = T(stack, &[D(9), D(axis), D(0), D(2)]);
    let sam = T(stack, &[D(6), D(0), D(7)]);
    let fol = T(stack, &[D(8), pul, D(9), D(2), D(10), sam, D(0), D(2)]);
    let sub = T(stack, &[arvo, ovo]);
    interpret(&mut context.nock_context, sub, fol)
}

fn peek(context: &mut Context, ovo: Noun) -> Noun {
    if context.nock_context.trace_info.is_some() {
        //  XX: way too many cases in the input to pull the actual vane, care, and path out
        let trace_name = "peek";
        let start = Instant::now();
        let slam_res = slam(context, PEEK_AXIS, ovo);
        write_serf_trace_safe(&mut context.nock_context.trace_info, trace_name, start);

        slam_res.expect("peek error handling unimplemented")
    } else {
        slam(context, PEEK_AXIS, ovo).expect("peek error handling unimplemented")
    }
}

fn goof(context: &mut Context, traces: Noun) -> Noun {
    let trace = zing(&mut context.nock_context.stack, traces).unwrap();
    let tone = Cell::new(&mut context.nock_context.stack, D(2), trace);
    let tang = mook(&mut context.nock_context, tone, false)
        .expect("serf: goof: +mook crashed on bail")
        .tail();
    //  XX: interpreter::Error should use a bail enum system similar to u3m_bail motes;
    //      might be able to replace Deterministic / NonDeterministic with mote and map determinism
    //      to individual motes; for now, always use %exit
    T(&mut context.nock_context.stack, &[D(tas!(b"exit")), tang])
}

/** Run slam; process stack trace to tang if error.
 *  Generate tracing events, if JSON tracing enabled.
 */
fn soft(context: &mut Context, ovo: Noun, trace_name: Option<String>) -> Result<Noun, Noun> {
    let slam_res = if context.nock_context.trace_info.is_some() {
        let start = Instant::now();
        let slam_res = slam(context, POKE_AXIS, ovo);
        write_serf_trace_safe(
            &mut context.nock_context.trace_info,
            trace_name.as_ref().unwrap(),
            start,
        );

        slam_res
    } else {
        slam(context, POKE_AXIS, ovo)
    };

    match slam_res {
        Ok(res) => Ok(res),
        Err(error) => match error {
            Error::Deterministic(traces) | Error::NonDeterministic(traces) => {
                Err(goof(context, traces))
            }
            Error::ScryBlocked(_) | Error::ScryCrashed(_) => {
                panic!("serf: soft: .^ invalid outside of virtual Nock")
            }
        },
    }
}

fn play_life(context: &mut Context, eve: Noun) {
    let stack = &mut context.nock_context.stack;
    let sub = T(stack, &[D(0), D(3)]);
    let lyf = T(stack, &[D(2), sub, D(0), D(2)]);
    let res = if context.nock_context.trace_info.is_some() {
        let trace_name = "boot";
        let start = Instant::now();
        let boot_res = interpret(&mut context.nock_context, eve, lyf);
        write_serf_trace_safe(&mut context.nock_context.trace_info, trace_name, start);

        boot_res
    } else {
        interpret(&mut context.nock_context, eve, lyf)
    };

    match res {
        Ok(gat) => {
            let eved = lent(eve).expect("serf: play: boot event number failure") as u64;
            let arvo = slot(gat, 7).expect("serf: play: lifecycle didn't return initial Arvo");

            context.event_update(eved, arvo);
            context.play_done();
        }
        Err(error) => match error {
            Error::Deterministic(traces) | Error::NonDeterministic(traces) => {
                let goof = goof(context, traces);
                context.play_bail(goof);
            }
            Error::ScryBlocked(_) | Error::ScryCrashed(_) => {
                panic!("serf: play: .^ invalid outside of virtual Nock")
            }
        },
    }
}

fn play_list(context: &mut Context, mut lit: Noun) {
    let mut eve = context.event_num;
    while let Ok(cell) = lit.as_cell() {
        let ovo = cell.head();
        let trace_name = if context.nock_context.trace_info.is_some() {
            Some(format!("play [{}]", eve))
        } else {
            None
        };

        match soft(context, ovo, trace_name) {
            Ok(res) => {
                let arvo = res
                    .as_cell()
                    .expect("serf: work: +slam returned atom")
                    .tail();
                eve += 1;

                context.event_update(eve, arvo);
            }
            Err(goof) => {
                return context.play_bail(goof);
            }
        }
        lit = cell.tail();
    }
    context.play_done();
}

fn work(context: &mut Context, job: Noun) {
    let trace_name = if context.nock_context.trace_info.is_some() {
        //  XX: good luck making this safe AND rust idiomatic!
        let wire = job.slot(6).expect("serf: work: job missing wire");
        let vent = job
            .slot(14)
            .expect("serf: work: job missing event tag")
            .as_atom()
            .expect("serf: work: event tag not atom");

        Some(work_trace_name(&mut context.nock_context.stack, wire, vent))
    } else {
        None
    };

    match soft(context, job, trace_name) {
        Ok(res) => {
            let cell = res.as_cell().expect("serf: work: +slam returned atom");
            let fec = cell.head();
            let eve = context.event_num;

            context.event_update(eve + 1, cell.tail());
            context.work_done(fec);
        }
        Err(goof) => {
            work_swap(context, job, goof);
        }
    }
}

fn work_swap(context: &mut Context, job: Noun, goof: Noun) {
    //  TODO: on decryption failure in aes_siv, should bail as fast as
    //  possible, without rendering stack trace or injecting crud event.  See
    //  c3__evil in vere.

    clear_interrupt();

    let stack = &mut context.nock_context.stack;
    context.nock_context.cache = Hamt::<Noun>::new();
    //  crud ovo = [+(now) [%$ %arvo ~] [%crud goof ovo]]
    let job_cell = job.as_cell().expect("serf: work: job not a cell");
    let job_now = job_cell.head().as_atom().expect("serf: work: now not atom");
    let now = inc(stack, job_now).as_noun();
    let wire = T(stack, &[D(0), D(tas!(b"arvo")), D(0)]);
    let crud = DirectAtom::new_panic(tas!(b"crud"));
    let ovo = T(stack, &[now, wire, crud.as_noun(), goof, job_cell.tail()]);
    let trace_name = if context.nock_context.trace_info.is_some() {
        Some(work_trace_name(
            &mut context.nock_context.stack,
            wire,
            crud.as_atom(),
        ))
    } else {
        None
    };

    match soft(context, ovo, trace_name) {
        Ok(res) => {
            let cell = res.as_cell().expect("serf: work: crud +slam returned atom");
            let fec = cell.head();
            let eve = context.event_num;

            context.event_update(eve + 1, cell.tail());
            context.work_swap(ovo, fec);
        }
        Err(goof_crud) => {
            work_bail(context, &[goof_crud, goof]);
        }
    }
}

fn work_bail(context: &mut Context, goofs: &[Noun]) {
    let stack = &mut context.nock_context.stack;
    let lest = T(stack, goofs);
    let lud = T(stack, &[lest, D(0)]);
    context.work_bail(lud);
}

fn work_trace_name(stack: &mut NockStack, wire: Noun, vent: Atom) -> String {
    let wpc = path_to_cord(stack, wire);
    let wpc_len = met3_usize(wpc);
    let wpc_bytes = &wpc.as_bytes()[0..wpc_len];
    let wpc_str = match std::str::from_utf8(wpc_bytes) {
        Ok(valid) => valid,
        Err(error) => {
            let (valid, _) = wpc_bytes.split_at(error.valid_up_to());
            unsafe { std::str::from_utf8_unchecked(valid) }
        }
    };

    let vc_len = met3_usize(vent);
    let vc_bytes = &vent.as_bytes()[0..vc_len];
    let vc_str = match std::str::from_utf8(vc_bytes) {
        Ok(valid) => valid,
        Err(error) => {
            let (valid, _) = vc_bytes.split_at(error.valid_up_to());
            unsafe { std::str::from_utf8_unchecked(valid) }
        }
    };

    format!("work [{} {}]", wpc_str, vc_str)
}

fn slot(noun: Noun, axis: u64) -> io::Result<Noun> {
    noun.slot(axis)
        .map_err(|_e| io::Error::new(io::ErrorKind::InvalidInput, "Bad axis"))
}

fn clear_interrupt() {
    (*TERMINATOR).store(false, Ordering::Relaxed);
}
