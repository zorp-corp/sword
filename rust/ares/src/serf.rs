use crate::hamt::Hamt;
use crate::interpreter::{inc, interpret, Error, Mote};
use crate::jets::cold::Cold;
use crate::jets::hot::{Hot, HotEntry};
use crate::jets::list::util::{lent, zing};
use crate::jets::nock::util::mook;
use crate::jets::warm::Warm;
use crate::mem::NockStack;
use crate::mug::*;
use crate::newt::Newt;
use crate::noun::{Atom, Cell, DirectAtom, Noun, Slots, D, T};
use crate::persist::pma_meta_set;
use crate::persist::{pma_meta_get, pma_open, pma_sync, Persist};
use crate::trace::*;
use crate::{flog, interpreter};
use ares_macros::tas;
use signal_hook;
use signal_hook::consts::SIGINT;
use std::fs::create_dir_all;
use std::io;
use std::mem::size_of;
use std::path::PathBuf;
use std::result::Result;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;

crate::gdb!();

const FLAG_TRACE: u32 = 1 << 8;

#[repr(usize)]
enum BTMetaField {
    SnapshotVersion = 0,
    Snapshot = 1,
}
struct Snapshot(pub *mut SnapshotMem);

impl Persist for Snapshot {
    unsafe fn space_needed(&mut self, stack: &mut NockStack) -> usize {
        let mut arvo = (*(self.0)).arvo;
        let mut cold = (*(self.0)).cold;
        let arvo_space_needed = arvo.space_needed(stack);
        let cold_space_needed = cold.space_needed(stack);
        (((size_of::<SnapshotMem>() + 7) >> 3) << 3) + arvo_space_needed + cold_space_needed
    }

    unsafe fn copy_to_buffer(&mut self, stack: &mut NockStack, buffer: &mut *mut u8) {
        let snapshot_buffer = *buffer as *mut SnapshotMem;
        std::ptr::copy_nonoverlapping(self.0, snapshot_buffer, 1);
        *self = Snapshot(snapshot_buffer);
        *buffer = snapshot_buffer.add(1) as *mut u8;

        let mut arvo = (*snapshot_buffer).arvo;
        arvo.copy_to_buffer(stack, buffer);
        (*snapshot_buffer).arvo = arvo;

        let mut cold = (*snapshot_buffer).cold;
        cold.copy_to_buffer(stack, buffer);
        (*snapshot_buffer).cold = cold;
    }

    unsafe fn handle_to_u64(&self) -> u64 {
        self.0 as u64
    }

    unsafe fn handle_from_u64(meta_handle: u64) -> Self {
        Snapshot(meta_handle as *mut SnapshotMem)
    }
}

#[repr(C)]
#[repr(packed)]
struct SnapshotMem {
    pub epoch: u64,
    pub event_num: u64,
    pub arvo: Noun,
    pub cold: Cold,
}

const PMA_CURRENT_SNAPSHOT_VERSION: u64 = 1;

struct Context {
    epoch: u64,
    event_num: u64,
    arvo: Noun,
    mug: u32,
    nock_context: interpreter::Context,
}

impl Context {
    pub fn load(
        snap_path: PathBuf,
        trace_info: Option<TraceInfo>,
        constant_hot_state: &[HotEntry],
    ) -> Context {
        pma_open(snap_path).expect("serf: pma open failed");

        let snapshot_version = pma_meta_get(BTMetaField::SnapshotVersion as usize);

        let snapshot = match snapshot_version {
            0 => None,
            1 => Some(unsafe {
                Snapshot::handle_from_u64(pma_meta_get(BTMetaField::Snapshot as usize))
            }),
            _ => panic!("Unsupported snapshot version"),
        };

        Context::new(trace_info, snapshot, constant_hot_state)
    }

    pub unsafe fn save(&mut self) {
        let handle = {
            let mut snapshot = Snapshot({
                let snapshot_mem_ptr: *mut SnapshotMem = self.nock_context.stack.struct_alloc(1);

                // Save into PMA (does not sync)
                (*snapshot_mem_ptr).epoch = self.epoch;
                (*snapshot_mem_ptr).event_num = self.event_num;
                (*snapshot_mem_ptr).arvo = self.arvo;
                (*snapshot_mem_ptr).cold = self.nock_context.cold;
                snapshot_mem_ptr
            });

            let handle = snapshot.save_to_pma(&mut self.nock_context.stack);

            self.epoch = (*snapshot.0).epoch;
            self.arvo = (*snapshot.0).arvo;
            self.event_num = (*snapshot.0).event_num;
            self.nock_context.cold = (*snapshot.0).cold;

            handle
        };
        pma_meta_set(
            BTMetaField::SnapshotVersion as usize,
            PMA_CURRENT_SNAPSHOT_VERSION,
        );
        pma_meta_set(BTMetaField::Snapshot as usize, handle);
    }

    fn new(
        trace_info: Option<TraceInfo>,
        snapshot: Option<Snapshot>,
        constant_hot_state: &[HotEntry],
    ) -> Self {
        let mut stack = NockStack::new(1048 << 10 << 10, 0);
        let newt = Newt::new();
        let cache = Hamt::<Noun>::new(&mut stack);

        let (epoch, event_num, arvo, mut cold) = unsafe {
            match snapshot {
                Some(snapshot) => (
                    (*(snapshot.0)).epoch,
                    (*(snapshot.0)).event_num,
                    (*(snapshot.0)).arvo,
                    (*(snapshot.0)).cold,
                ),
                None => (0, 0, D(0), Cold::new(&mut stack)),
            }
        };

        let hot = Hot::init(&mut stack, constant_hot_state);
        let warm = Warm::init(&mut stack, &mut cold, &hot);
        let mug = mug_u32(&mut stack, arvo);

        let nock_context = interpreter::Context {
            stack,
            newt,
            cold,
            warm,
            hot,
            cache,
            scry_stack: D(0),
            trace_info,
        };

        Context {
            epoch,
            event_num,
            arvo,
            mug,
            nock_context,
        }
    }

    //
    // Setters
    //

    ///
    /// ## Safety
    ///
    /// calls save(), which invalidates all nouns not in the context
    /// until [preserve_event_update_leftovers] is called to resolve forwarding pointers.
    pub unsafe fn event_update(&mut self, new_event_num: u64, new_arvo: Noun) {
        //  XX: assert event numbers are continuous
        self.arvo = new_arvo;
        self.event_num = new_event_num;
        self.save();

        self.nock_context.cache = Hamt::new(&mut self.nock_context.stack);
        self.nock_context.scry_stack = D(0);

        // XX save to PMA
        self.mug = mug_u32(&mut self.nock_context.stack, self.arvo);
    }

    ///
    /// ## Safety
    ///
    /// Preserves nouns and jet states in context and then calls [flip_top_frame].
    /// Other stack-allocated objects needing preservation should be preserved between
    /// [event_update] and invocation of this function
    pub unsafe fn preserve_event_update_leftovers(&mut self) {
        let stack = &mut self.nock_context.stack;
        stack.preserve(&mut self.nock_context.warm);
        stack.preserve(&mut self.nock_context.hot);
        stack.flip_top_frame(0);
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
            //  XX: chicken/egg problem with flog bc it requires context
            //      before we've initialized it, and context needs trace_info
            // eprintln!("\rError initializing trace file: {:?}", e);
            trace_info = None;
        }
    }

    let mut context = Context::load(snap_path, trace_info, constant_hot_state);
    context.ripe();

    // Can't use for loop because it borrows newt
    while let Some(writ) = context.next() {
        // Reset the local cache and scry handler stack
        context.nock_context.cache = Hamt::<Noun>::new(&mut context.nock_context.stack);
        context.nock_context.scry_stack = D(0);

        let tag = slot(writ, 2)?.as_direct().unwrap();
        match tag.data() {
            tas!(b"live") => {
                let inner = slot(writ, 6)?.as_direct().unwrap();
                match inner.data() {
                    tas!(b"cram") => {
                        flog!(&mut context.nock_context, "\r %cram: not implemented");
                    }
                    tas!(b"exit") => {
                        flog!(&mut context.nock_context, "\r %exit");
                        std::process::exit(0);
                    }
                    tas!(b"save") => {
                        // XX what is eve for?
                        pma_sync();
                    }
                    tas!(b"meld") => {
                        flog!(&mut context.nock_context, "\r %meld: not implemented");
                    }
                    tas!(b"pack") => {
                        flog!(&mut context.nock_context, "\r %pack: not implemented");
                    }
                    _ => {
                        flog!(&mut context.nock_context, "unknown live");
                    }
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
        write_serf_trace_safe(&mut context.nock_context, trace_name, start);

        slam_res.expect("peek error handling unimplemented")
    } else {
        slam(context, PEEK_AXIS, ovo).expect("peek error handling unimplemented")
    }
}

fn goof(context: &mut Context, mote: Mote, traces: Noun) -> Noun {
    let trace = zing(&mut context.nock_context.stack, traces).expect("serf: goof: zing failed");
    let tone = Cell::new(&mut context.nock_context.stack, D(2), trace);
    let tang = mook(&mut context.nock_context, tone, false)
        .expect("serf: goof: +mook crashed on bail")
        .tail();
    T(&mut context.nock_context.stack, &[D(mote as u64), tang])
}

/** Run slam; process stack trace to tang if error.
 *  Generate tracing events, if JSON tracing enabled.
 */
fn soft(context: &mut Context, ovo: Noun, trace_name: Option<String>) -> Result<Noun, Noun> {
    let slam_res = if context.nock_context.trace_info.is_some() {
        let start = Instant::now();
        let slam_res = slam(context, POKE_AXIS, ovo);
        write_serf_trace_safe(
            &mut context.nock_context,
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
            Error::Deterministic(mote, traces) | Error::NonDeterministic(mote, traces) => {
                Err(goof(context, mote, traces))
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
        write_serf_trace_safe(&mut context.nock_context, trace_name, start);

        boot_res
    } else {
        interpret(&mut context.nock_context, eve, lyf)
    };

    match res {
        Ok(gat) => {
            let eved = lent(eve).expect("serf: play: boot event number failure") as u64;
            let arvo = slot(gat, 7).expect("serf: play: lifecycle didn't return initial Arvo");

            unsafe {
                context.event_update(eved, arvo);
                context.preserve_event_update_leftovers();
            }
            context.play_done();
        }
        Err(error) => match error {
            Error::Deterministic(mote, traces) | Error::NonDeterministic(mote, traces) => {
                let goof = goof(context, mote, traces);
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
        lit = cell.tail();
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

                unsafe {
                    context.event_update(eve, arvo);
                    context.nock_context.stack.preserve(&mut lit);
                    context.preserve_event_update_leftovers();
                }
            }
            Err(goof) => {
                return context.play_bail(goof);
            }
        }
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
            let mut fec = cell.head();
            let eve = context.event_num;

            unsafe {
                context.event_update(eve + 1, cell.tail());
                context.nock_context.stack.preserve(&mut fec);
                context.preserve_event_update_leftovers();
            }
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
    context.nock_context.cache = Hamt::<Noun>::new(stack);
    //  crud ovo = [+(now) [%$ %arvo ~] [%crud goof ovo]]
    let job_cell = job.as_cell().expect("serf: work: job not a cell");
    let job_now = job_cell.head().as_atom().expect("serf: work: now not atom");
    let now = inc(stack, job_now).as_noun();
    let wire = T(stack, &[D(0), D(tas!(b"arvo")), D(0)]);
    let crud = DirectAtom::new_panic(tas!(b"crud"));
    let mut ovo = T(stack, &[now, wire, crud.as_noun(), goof, job_cell.tail()]);
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
            let mut fec = cell.head();
            let eve = context.event_num;

            unsafe {
                context.event_update(eve + 1, cell.tail());
                context.nock_context.stack.preserve(&mut ovo);
                context.nock_context.stack.preserve(&mut fec);
                context.preserve_event_update_leftovers();
            }
            context.work_swap(ovo, fec);
        }
        Err(goof_crud) => {
            flog!(&mut context.nock_context, "\rserf: bail");
            let stack = &mut context.nock_context.stack;
            let lud = T(stack, &[goof_crud, goof, D(0)]);
            context.work_bail(lud);
        }
    }
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
