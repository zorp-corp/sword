use crate::hamt::Hamt;
use crate::interpreter;
use crate::interpreter::{inc, interpret, Tone};
use crate::jets::nock::util::mook;
use crate::jets::text::util::lent;
use crate::mem::NockStack;
use crate::mug::mug_u32;
use crate::newt::Newt;
use crate::noun::{Cell, Noun, Slots, D, T};
use crate::snapshot::double_jam::DoubleJam;
use crate::snapshot::Snapshot;
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

struct Context {
    epoch: u64,
    event_num: u64,
    snapshot: DoubleJam,
    arvo: Noun,
    mug: u32,
    stack: NockStack,
    newt: Newt,
    cache: Hamt<Noun>,
    //  XX: persistent memo cache
}

impl Context {
    pub fn new(snap_path: &PathBuf) -> Self {
        // TODO: switch to Pma when ready
        // let snap = &mut snapshot::pma::Pma::new(snap_path);
        let mut snapshot = DoubleJam::new(snap_path);
        let mut stack = NockStack::new(256 << 10 << 10, 0);
        let newt = Newt::new();
        let cache = Hamt::<Noun>::new();

        let (epoch, event_num, arvo) = snapshot.load(&mut stack).unwrap_or((0, 0, D(0)));
        let mug = mug_u32(&mut stack, arvo);

        Context {
            epoch,
            event_num,
            snapshot,
            arvo,
            mug,
            stack,
            newt,
            cache,
        }
    }

    //
    // Getters
    //

    pub fn epoch(&self) -> u64 {
        self.epoch
    }

    pub fn event_num(&self) -> u64 {
        self.event_num
    }

    pub fn arvo(&self) -> Noun {
        self.arvo
    }

    pub fn stack_as_mut(&mut self) -> &mut NockStack {
        &mut self.stack
    }

    pub fn for_interpreter(&mut self) -> interpreter::Context {
        self.cache = Hamt::<Noun>::new();

        interpreter::Context {
            stack: &mut self.stack,
            newt: Some(&mut self.newt),
            cache: &mut self.cache,
        }
    }

    //
    // Setters
    //

    pub fn event_update(&mut self, new_event_num: u64, new_arvo: Noun) {
        //  XX: assert event numbers are continuous
        self.arvo = new_arvo;
        self.event_num = new_event_num;
        self.snapshot.save(&mut self.stack, &mut self.arvo);
        self.mug = mug_u32(&mut self.stack, self.arvo);
    }

    //
    // Snapshot functions
    //

    pub fn sync(&mut self) {
        self.snapshot
            .sync(&mut self.stack, self.epoch, self.event_num);
    }

    //
    // Newt functions
    //

    pub fn next(&mut self) -> Option<Noun> {
        self.newt.next(&mut self.stack)
    }

    pub fn ripe(&mut self) {
        self.newt
            .ripe(&mut self.stack, self.event_num, self.mug as u64);
    }

    pub fn live(&mut self) {
        self.newt.live(&mut self.stack);
    }

    pub fn peek_done(&mut self, dat: Noun) {
        self.newt.peek_done(&mut self.stack, dat);
    }

    pub fn play_done(&mut self) {
        self.newt.play_done(&mut self.stack, self.mug as u64);
    }

    pub fn play_bail(&mut self, dud: Noun) {
        self.newt
            .play_bail(&mut self.stack, self.event_num, self.mug as u64, dud);
    }

    pub fn work_done(&mut self, fec: Noun) {
        self.newt
            .work_done(&mut self.stack, self.event_num, self.mug as u64, fec);
    }

    pub fn work_swap(&mut self, job: Noun, fec: Noun) {
        self.newt
            .work_swap(&mut self.stack, self.event_num, self.mug as u64, job, fec);
    }

    pub fn work_bail(&mut self, lud: Noun) {
        self.newt.work_bail(&mut self.stack, lud);
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
pub fn serf() -> io::Result<()> {
    // Register SIGINT signal hook to set flag first time, shutdown second time
    signal_hook::flag::register_conditional_shutdown(SIGINT, 1, Arc::clone(&TERMINATOR))?;
    signal_hook::flag::register(SIGINT, Arc::clone(&TERMINATOR))?;

    let snap_path_string = std::env::args()
        .nth(2)
        .ok_or(io::Error::new(io::ErrorKind::Other, "no pier path"))?;
    let mut snap_path = PathBuf::from(snap_path_string);
    snap_path.push(".urb");
    snap_path.push("chk");
    create_dir_all(&snap_path)?;

    let mut context = Context::new(&snap_path);
    context.ripe();

    // Can't use for loop because it borrows newt
    while let Some(writ) = context.next() {
        //  XX: probably want to bookend this logic frame_push / frame_pop
        //      preserve jet state and persistent cache, lose everything else
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
                let sam = slot(writ, 7)?;
                let res =
                    slam(&mut context, PEEK_AXIS, sam).expect("peek error handling unimplemented");
                context.peek_done(res);
            }
            tas!(b"play") => {
                let lit = slot(writ, 7)?;
                if context.epoch() == 0 && context.event_num() == 0 {
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
    }

    Ok(())
}

fn burn(context: &mut Context, subject: Noun, formula: Noun) -> Result<Noun, Tone> {
    let burn_context = &mut context.for_interpreter();
    interpret(burn_context, subject, formula)
}

fn slam(context: &mut Context, axis: u64, ovo: Noun) -> Result<Noun, Tone> {
    let arvo = context.arvo();
    let pul = T(context.stack_as_mut(), &[D(9), D(axis), D(0), D(2)]);
    let sam = T(context.stack_as_mut(), &[D(6), D(0), D(7)]);
    let fol = T(
        context.stack_as_mut(),
        &[D(8), pul, D(9), D(2), D(10), sam, D(0), D(2)],
    );
    let sub = T(context.stack_as_mut(), &[arvo, ovo]);
    burn(context, sub, fol)
}

fn goof(context: &mut Context, trace: Noun) -> Noun {
    let tone = Cell::new(context.stack_as_mut(), D(2), trace);
    let mook_context = &mut context.for_interpreter();

    let tang = mook(mook_context, tone, false)
        .expect("serf: goof: +mook crashed on bail")
        .tail();
    //  XX: noun::Tone or noun::NockErr should use a bail enum system similar to u3m_bail motes;
    //      might be able to replace NockErr with mote and map determinism to individual motes;
    //      for, always set to %exit
    T(mook_context.stack, &[D(tas!(b"exit")), tang])
}

/** Run slam, process stack trace to tang if error */
fn soft(context: &mut Context, ovo: Noun) -> Result<Noun, Noun> {
    match slam(context, POKE_AXIS, ovo) {
        Ok(res) => Ok(res),
        Err(Tone::Error(_, trace)) => Err(goof(context, trace)),
        Err(Tone::Blocked(_)) => panic!("soft: blocked err handling unimplemented"),
    }
}

fn play_life(context: &mut Context, eve: Noun) {
    let sub = T(context.stack_as_mut(), &[D(0), D(3)]);
    let lyf = T(context.stack_as_mut(), &[D(2), sub, D(0), D(2)]);
    match burn(context, eve, lyf) {
        Ok(gat) => {
            let eved = lent(eve).expect("serf: play: boot event number failure") as u64;
            let arvo = slot(gat, 7).expect("serf: play: lifecycle didn't return initial Arvo");

            context.event_update(eved, arvo);
            context.play_done();
        }
        Err(Tone::Error(_, trace)) => {
            let goof = goof(context, trace);
            context.play_bail(goof);
        }
        Err(Tone::Blocked(_)) => {
            panic!("play: blocked err handling unimplemented")
        }
    }
}

fn play_list(context: &mut Context, mut lit: Noun) {
    let mut eve = context.event_num();
    while let Ok(cell) = lit.as_cell() {
        let ovo = cell.head();
        match soft(context, ovo) {
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
    match soft(context, job) {
        Ok(res) => {
            let cell = res.as_cell().expect("serf: work: +slam returned atom");
            let fec = cell.head();
            let eve = context.event_num();

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

    //  crud = [+(now) [%$ %arvo ~] [%crud goof ovo]]
    let job_cell = job.as_cell().expect("serf: work: job not a cell");
    let job_now = job_cell.head().as_atom().expect("serf: work: now not atom");
    let now = inc(context.stack_as_mut(), job_now).as_noun();
    let wire = T(context.stack_as_mut(), &[D(0), D(tas!(b"arvo")), D(0)]);
    let crud = T(
        context.stack_as_mut(),
        &[now, wire, D(tas!(b"crud")), goof, job_cell.tail()],
    );

    match soft(context, crud) {
        Ok(res) => {
            let cell = res.as_cell().expect("serf: work: crud +slam returned atom");
            let fec = cell.head();
            let eve = context.event_num();

            context.event_update(eve + 1, cell.tail());
            context.work_swap(crud, fec);
        }
        Err(goof_crud) => {
            work_bail(context, &[goof_crud, goof]);
        }
    }
}

fn work_bail(context: &mut Context, goofs: &[Noun]) {
    let lest = T(context.stack_as_mut(), goofs);
    let lud = T(context.stack_as_mut(), &[lest, D(0)]);
    context.work_bail(lud);
}

fn slot(noun: Noun, axis: u64) -> io::Result<Noun> {
    noun.slot(axis)
        .map_err(|_e| io::Error::new(io::ErrorKind::InvalidInput, "Bad axis"))
}

fn clear_interrupt() {
    (*TERMINATOR).store(false, Ordering::Relaxed);
}
