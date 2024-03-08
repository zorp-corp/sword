// tracing is off
use ares_macros::tas;
use either::Either::{Left, Right};
use std::mem::size_of;
use std::time::Instant;

use crate::hamt::Hamt;
use crate::jets::JetErr;
use crate::interpreter::{inc, Context, Error};
use crate::jets::cold::Cold;
use crate::jets::util::slot;
use crate::jets::warm::Warm;
use crate::mem::{NockStack, Preserve};
use crate::noun::{Noun, D, NO, T, YES};
use crate::trace::TraceStack;
use crate::codegen::types::PileMem;
use assert_no_alloc::permit_alloc;
use crate::persist::Persist;
use types::{ActualError, Frame, Pile, JetEntry, Timer, TimerMem};
use crate::flog;

use self::util::{comp, do_call, do_goto, do_return, do_tail_call, part_peek, peek, poke, do_rack};

#[derive(Copy, Clone)]
pub struct Hill(crate::hamt::Hamt<Pile>);

impl Hill {
    pub fn new(stack: &mut NockStack) -> Hill {
        Hill(crate::hamt::Hamt::new(stack))
    }
}

impl Preserve for Hill {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        self.0.preserve(stack);
    }

    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        self.0.assert_in_stack(stack);
    }
}

impl Persist for Hill {
    unsafe fn space_needed(&mut self, stack: &mut NockStack) -> usize {
        self.0.space_needed(stack)
    }

    unsafe fn copy_to_buffer(&mut self, stack: &mut NockStack, buffer: &mut *mut u8) {
        self.0.copy_to_buffer(stack, buffer)
    }

    unsafe fn handle_to_u64(&self) -> u64 {
        self.0.handle_to_u64()
    }

    unsafe fn handle_from_u64(meta_handle: u64) -> Self {
        Hill(Hamt::<Pile>::handle_from_u64(meta_handle))
    }
}

/// Push onto the mean stack.
fn cg_mean_push(context: &mut Context, noun: Noun) {
    unsafe {
        let frame = frame_ptr(context);
        (*frame).mean = T(&mut context.stack, &[noun, (*frame).mean]);
    }
}

/// Pop off of the mean stack.
fn cg_mean_pop(context: &mut Context) {
    unsafe {
        let frame = frame_ptr(context);
        (*frame).mean = (*frame).mean
            .as_cell()
            .expect("serf: unexpected end of mean stack\r")
            .tail();
    }
}

/// Push onto the slow stack.
fn cg_slow_push(context: &mut Context, noun: Noun) {
    unsafe {
        let frame = frame_ptr(context);
        (*frame).slow = T(&mut context.stack, &[noun, (*frame).mean]);
    }
}

/// Pop off of the slow stack.
fn cg_slow_pop(context: &mut Context) {
    unsafe {
        let frame = frame_ptr(context);
        (*frame).mean = (*frame).mean
            .as_cell()
            .expect("serf: unexpected end of mean stack\r")
            .tail();
    }
}

// XX typedef for register
/// First peeks or pokes the codegen core (line) to get codegen for the
/// subject and formula, then parses the successful results into a
/// (bell, hill) tuple.
fn cg_pull_peek(context: &mut Context, subject: Noun, formula: Noun) -> Noun {
    // +peek or +poke dance
    if context.cg_context.line.is_none() {
        panic!("line not set");
    }
    let pek = peek(context, subject, formula);
    let bell = if unsafe { pek.raw_equals(D(0)) } {
        let comp = comp(context, subject, formula);
        let (_new, _old) = poke(context, comp);
        let good_peek = peek(context, subject, formula);
        let (bell, hill) = part_peek(&mut context.stack, good_peek);
        context.cg_context.hill = hill;
        bell
    } else {
        let (bell, hill) = part_peek(&mut context.stack, pek);
        context.cg_context.hill = hill;
        bell
    };

    bell
}

/// Uses the `(bell, hill)` tuple returned from `cg_pull_peek()` to lookup the first
/// `pile` in the `hill` map.
/// XX percolate the changes to this through the code
fn cg_pull_pile(context: &mut Context, subject: Noun, formula: Noun) -> Pile {
    let mut bell = cg_pull_peek(context, subject, formula);
    context
        .cg_context
        .hill
        .0
        .lookup(&mut context.stack, &mut bell)
        .expect("pile not found")
}

const FRAME_WORD_SIZE: usize = (size_of::<Frame>() + 7) >> 3; // Round to u64 words

/**
 * Push a new interpreter frame
 *
 * If tail is false, this will simply set up a new frame.
 *
 * If tail is true, this will set up a new frame in the current stack position, but in addition
 * - resize the frame to hold the old poisons, the old registers, the new poisons, and the new
 *   registers
 * - move the old poisons and registers over
 */
unsafe fn new_frame(context: &mut Context, pile: Pile, tail: bool) {
    let sans = unsafe { (*(pile.0)).sans };
    let poison_size = (sans + 63) >> 6;
    if tail {
        unsafe {
            let old_frame_ptr = context.stack.get_frame_lowest() as *mut Frame;
            let old_sans = (*((*old_frame_ptr).pile.0)).sans;
            let old_poison_size = (*old_frame_ptr).pois_sz;
            context.stack.debug_assert_sane();
            context.stack.frame_replace(
                FRAME_WORD_SIZE + poison_size + sans + old_poison_size + old_sans + 1,
            );
            context.stack.debug_assert_sane();
            let frame = frame_ptr(context);
            let frame_u64 = frame as *mut u64;
            // save old poison size and old poison and registers for new call setup
            *(frame_u64.add(FRAME_WORD_SIZE + poison_size + sans) as *mut usize) = old_poison_size;
            context.stack.debug_assert_sane();
            std::ptr::copy(
                frame_u64.add(FRAME_WORD_SIZE),
                frame_u64.add(FRAME_WORD_SIZE + poison_size + sans + 1),
                old_poison_size + old_sans,
            );
            (*frame).pile = pile;
            context.stack.debug_assert_sane();
        }
    } else {
        unsafe {
            context.stack.debug_assert_sane();
            context
                .stack
                .frame_push(FRAME_WORD_SIZE + poison_size + sans);
            context.stack.debug_assert_sane();
            let frame = frame_ptr(context);

            *frame = Frame {
                mean: D(0),
                traz: std::ptr::null::<TraceStack>() as *mut *const TraceStack,
                time: Timer(std::ptr::null::<TimerMem>()),
                slow: D(0),

                pile: pile,

                dest: 0,
                cont: D(0),
                pois_sz: poison_size,
            };
            context.stack.debug_assert_sane();
        }
    }
}

#[inline]
unsafe fn frame_ptr(context: &mut Context) -> *mut Frame {
    context.stack.get_frame_lowest() as *mut Frame
}

#[inline]
unsafe fn pile_mem_ptr(context: &mut Context) -> *mut PileMem {
    let frame = frame_ptr(context);
    (*frame).pile.0
}

struct ContextSnapshot {
    cold: Cold,
    warm: Warm,
    cache: Hamt<Noun>,
    line: Option<Noun>,
    hill: Hill,
    virtual_frame: *const u64,
}

pub fn cg_interpret(context: &mut Context, subject: Noun, formula: Noun) -> Result<Noun, Error> {
    // XX save cold, warm, cache, line, hill and current_frame to restore in case of error ?
    //    define a struct with these things, initialize it here
    //    then restore it in the error handler
    //    "context snapshot"
    let context_snapshot = ContextSnapshot {
        cold: context.cold,
        warm: context.warm,
        cache: context.cache,
        line: context.cg_context.line,
        hill: context.cg_context.hill,
        virtual_frame: context.stack.get_frame_pointer(),
    };
    let nock = cg_interpret_inner(context, context_snapshot.virtual_frame, subject, formula);
    match nock {
        Ok(noun) => Ok(noun),
        Err(ae) => Err(exit(context, context_snapshot, ae)),
    }
}

/// Fetches or creates codegen code for the subject and formula, then
/// naively interprets it.
fn cg_interpret_inner(
    context: &mut Context,
    virtual_frame: *const u64,
    subject: Noun,
    formula: Noun,
) -> Result<Noun, ActualError> {
    context.stack.debug_assert_sane();
    let pile = cg_pull_pile(context, subject, formula);

    // do_rack(context, subject, formula);

    context.stack.debug_assert_sane();
    {
        unsafe {assert!((*pile.0).will.lookup(&mut context.stack, &mut (*pile.0).wish).is_some()); }
        context.stack.debug_assert_sane();
        unsafe { new_frame(context, pile, false) };
        context.stack.debug_assert_sane();
    }

    // Load the initial subject to the sire register.
    unsafe {
        let sire = (*pile_mem_ptr(context)).sire;
        register_set(frame_ptr(context), sire, subject);
        context.stack.debug_assert_sane();
        unsafe {assert!((*pile.0).will.lookup(&mut context.stack, &mut (*pile.0).wish).is_some()); }
    }

    // Get the blob, body, and bend nouns from our pile.
    let (mut body, mut bend): (Noun, Noun) =
    {
        unsafe {assert!((*pile.0).will.lookup(&mut context.stack, &mut (*pile.0).wish).is_some()); }
        let will = unsafe { (*pile_mem_ptr(context)).will };
        let wish = &mut unsafe { (*pile_mem_ptr(context)).wish };
        let blob = will.lookup(&mut context.stack, wish).unwrap();
        context.stack.debug_assert_sane();
        ( slot(blob, 6).expect("codegen nock error"),
          slot(blob, 7).expect("codegen nock error"),
        )
    };

    loop {
        context.stack.debug_assert_sane();
        if !unsafe { body.raw_equals(D(0)) } {
            let pole = slot(body, 2).unwrap();
            body = slot(body, 3).unwrap();
            let pole_h = slot(pole, 2).unwrap().as_direct().unwrap().data().to_le_bytes();
            match slot(pole, 2).unwrap().as_direct().unwrap().data() {
                tas!(b"imm") => unsafe {
                    let local = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let value = slot(pole, 6).unwrap();
                    register_set(frame_ptr(context), local, value);
                }
                tas!(b"mov") => unsafe {
                    let src = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let dst = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let value = register_get(frame_ptr(context), src);
                    register_set(frame_ptr(context), dst, value);
                }
                tas!(b"inc") => unsafe {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(frame_ptr(context), s);
                    if let Ok(atom) = s_value.as_atom() {
                        let value = inc(&mut context.stack, atom);
                        register_set(frame_ptr(context), d, value.as_noun());
                    } else {
                        break Err(ActualError(Error::Deterministic(D(0))));
                    }
                }
                tas!(b"con") => unsafe {
                    let h = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let t = slot(pole, 14).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 15).unwrap().as_direct().unwrap().data() as usize;
                    let h_value = register_get(frame_ptr(context), h);
                    let t_value = register_get(frame_ptr(context), t);
                    let value = T(&mut context.stack, &[h_value, t_value]);
                    register_set(frame_ptr(context), d, value);
                }
                tas!(b"cop") => unsafe {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(frame_ptr(context), s);
                    if s_value.is_atom() {
                        poison_set(frame_ptr(context), s);
                    }
                }
                tas!(b"lop") => unsafe {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(frame_ptr(context), s);
                    if !( s_value.raw_equals(YES) || s_value.raw_equals(NO) ) {
                        poison_set(frame_ptr(context), s);
                    }
                }
                tas!(b"coc") => unsafe {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(frame_ptr(context), s);
                    if s_value.is_atom() {
                        break Err(ActualError(Error::Deterministic(D(0))));
                    }
                }
                tas!(b"hed") => unsafe {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(frame_ptr(context), s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            poison_set(frame_ptr(context), s);
                        }
                        Right(cell) => {
                            register_set(frame_ptr(context), d, cell.head());
                        }
                    };
                }
                tas!(b"tal") => unsafe {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(frame_ptr(context), s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            poison_set(frame_ptr(context), s);
                        }
                        Right(cell) => {
                            register_set(frame_ptr(context), d, cell.tail());
                        }
                    };
                }
                tas!(b"hci") => unsafe {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(frame_ptr(context), s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            break Err(ActualError(Error::Deterministic(D(0))));
                        }
                        Right(cell) => {
                            register_set(frame_ptr(context), d, cell.head());
                        }
                    };
                }
                tas!(b"tci") => unsafe {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(frame_ptr(context), s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            break Err(ActualError(Error::Deterministic(D(0))));
                        }
                        Right(cell) => {
                            register_set(frame_ptr(context), d, cell.tail());
                        }
                    };
                }
                tas!(b"men") => unsafe {
                    let l = slot(pole, 6).unwrap();
                    let s = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(frame_ptr(context), s);
                    let mean = T(&mut context.stack, &[l, s_value]);
                    cg_mean_push(context, mean);
                }
                tas!(b"man") => {
                    cg_mean_pop(context);
                }
                tas!(b"slo") => unsafe {
                    let s = slot(pole, 3).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(frame_ptr(context), s);
                    cg_slow_push(context, s_value);
                }
                tas!(b"sld") => {
                    cg_slow_pop(context);
                }
                tas!(b"hit") => unsafe {
                    let s = slot(pole, 3).unwrap().as_direct().unwrap().data() as usize;
                    let _s_value = register_get(frame_ptr(context), s);
                    // XX increment a profiling hit counter labeled with the noun in s
                    todo!("hit")
                }
                tas!(b"slg") => unsafe {
                    let s = slot(pole, 3).unwrap().as_direct().unwrap().data() as usize;
                    let clue = register_get(frame_ptr(context), s);
                    if let Ok(slog_cell) = clue.as_cell() {
                        if let Ok(pri_direct) = slog_cell.head().as_direct() {
                            let tank = slog_cell.tail();
                            context
                                .newt
                                .slog(&mut context.stack, pri_direct.data(), tank);
                        };
                    };
                }
                tas!(b"mew") => unsafe {
                    let k = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let u = slot(pole, 14).unwrap().as_direct().unwrap().data() as usize;
                    let f = slot(pole, 30).unwrap().as_direct().unwrap().data() as usize;
                    let r = slot(pole, 31).unwrap().as_direct().unwrap().data() as usize;

                    let _k_value = register_get(frame_ptr(context), k);
                    let u_value = register_get(frame_ptr(context), u);
                    let f_value = register_get(frame_ptr(context), f);
                    let r_value = register_get(frame_ptr(context), r);
                    // let mut key = T(&mut context.stack, &[k_value, u_value, f_value]);
                    // XX use k (clue) later, with persistent caching implementation
                    let mut key = T(&mut context.stack, &[u_value, f_value]);

                    context.cache = context.cache.insert(&mut context.stack, &mut key, r_value);
                }
                tas!(b"tim") => unsafe {
                    let timer_mem = context.stack.struct_alloc::<TimerMem>(1);
                    (*timer_mem).prev = (*frame_ptr(context)).time;
                    (*timer_mem).start = Instant::now();
                    (*frame_ptr(context)).time = Timer(timer_mem);
                }
                tas!(b"tom") => unsafe {
                    let timer_mem = (*frame_ptr(context)).time.0;
                    if timer_mem.is_null() {
                        panic!("Tried to pop an empty timer stack");
                    }
                    let time = (*timer_mem).start.elapsed();
                    (*frame_ptr(context)).time = (*timer_mem).prev;
                    
                    flog!(context, "bout {:.9}", time.as_secs_f64());
                }
                tas!(b"mem") => {
                    // XX print memory usage
                    todo!("mem")
                }
                tas!(b"pol") => unsafe {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    if poison_get(frame_ptr(context), s) {
                        poison_set(frame_ptr(context), d);
                    }
                }
                tas!(b"poi") => unsafe {
                    let d = slot(pole, 3).unwrap().as_direct().unwrap().data() as usize;
                    poison_set(frame_ptr(context), d);
                }
                tas!(b"ipb") => unsafe {
                    let mut s = slot(pole, 3).unwrap();
                    if let true = loop {
                        if s.raw_equals(D(0)) {
                            break false;
                        } else {
                            let i =
                                s.as_cell().unwrap().head().as_direct().unwrap().data() as usize;
                            if poison_get(frame_ptr(context), i) {
                                break true;
                            } else {
                                s = s.as_cell().unwrap().tail();
                            }
                        }
                    } {
                        break Err(ActualError(Error::Deterministic(D(0))));
                    }
                }
                _ => {
                    panic!("invalid pole instruction")
                }
            }
        } else {
            let bend_h = slot(bend, 2).unwrap().as_direct().unwrap().data().to_le_bytes();
            match slot(bend, 2).unwrap().as_direct().unwrap().data() {
                tas!(b"clq") => unsafe {
                    let s = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(frame_ptr(context), s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            let mut o = slot(bend, 15).unwrap();
                            do_goto(
                                context,
                                &mut body,
                                &mut bend,
                                &mut o,
                            );
                        }
                        Right(_cell) => {
                            let mut z = slot(bend, 14).unwrap();
                            do_goto(
                                context,
                                &mut body,
                                &mut bend,
                                &mut z,
                            );
                        }
                    };
                }
                tas!(b"eqq") => unsafe {
                    let l = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let r = slot(bend, 14).unwrap().as_direct().unwrap().data() as usize;
                    let l_value = register_get(frame_ptr(context), l);
                    let r_value = register_get(frame_ptr(context), r);
                    if l_value.raw_equals(r_value) {
                        let mut z = slot(bend, 30).unwrap();
                        do_goto(
                            context,
                            &mut body,
                            &mut bend,
                            &mut z,
                        );
                    } else {
                        let mut o = slot(bend, 31).unwrap();
                        do_goto(
                            context,
                            &mut body,
                            &mut bend,
                            &mut o,
                        );
                    }
                }
                tas!(b"brn") => unsafe {
                    let s = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(frame_ptr(context), s);
                    if s_value.raw_equals(D(0)) {
                        let mut z = slot(bend, 14).unwrap();
                        do_goto(
                            context,
                            &mut body,
                            &mut bend,
                            &mut z,
                        );
                    } else if s_value.raw_equals(D(1)) {
                        let mut o = slot(bend, 15).unwrap();
                        do_goto(
                            context,
                            &mut body,
                            &mut bend,
                            &mut o,
                        );
                    } else {
                        break Err(ActualError(Error::Deterministic(D(0))));
                    }
                }
                tas!(b"hop") => {
                    let mut t = slot(bend, 3).unwrap();
                    do_goto(
                        context,
                        &mut body,
                        &mut bend,
                        &mut t,
                    );
                }
                tas!(b"lnk") => unsafe {
                    let u = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let f = slot(bend, 14).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(bend, 30).unwrap().as_direct().unwrap().data() as usize;
                    let t = slot(bend, 31).unwrap();

                    let subject = register_get(frame_ptr(context), u);
                    let formula = register_get(frame_ptr(context), f);

                    {
                        let frame = frame_ptr(context);
                        (*frame).dest = d;
                        (*frame).cont = t;
                    }

                    {
                        let pile = cg_pull_pile(context, subject, formula);
                        new_frame(context, pile, false);
                    }
                    
                    // debugging: print out IR
                    // do_rack(context, subject, formula);

                    {
                        let sire = { (*pile_mem_ptr(context)).sire };
                        register_set(frame_ptr(context), sire, subject);
                    }

                    {
                        let wish = &mut (*pile_mem_ptr(context)).wish;
                        do_goto(
                            context,
                            &mut body,
                            &mut bend,
                            wish,
                        );
                    }
                }
                tas!(b"cal") => {
                    // call the arm a with subject in registers v, poisons in b,
                    // result in d, and then goto t
                    let a = slot(bend, 6).unwrap();
                    let b = slot(bend, 14).unwrap();
                    let v = slot(bend, 30).unwrap();
                    let d = slot(bend, 62).unwrap().as_direct().unwrap().data() as usize;
                    let t = slot(bend, 63).unwrap();

                    do_call(
                        context,
                        &mut body,
                        &mut bend,
                        a,
                        b,
                        v,
                        d,
                        t,
                    );
                }
                tas!(b"caf") => unsafe {
                    let a = slot(bend, 6).unwrap();
                    let b = slot(bend, 14).unwrap();
                    let v = slot(bend, 30).unwrap();
                    let d = slot(bend, 62).unwrap().as_direct().unwrap().data() as usize;
                    let mut t = slot(bend, 126).unwrap();
                    let u = slot(bend, 254).unwrap().as_direct().unwrap().data() as usize;
                    let mut n = slot(bend, 255).unwrap();

                    if let Some(JetEntry(jet)) = context.cg_context.hot_hamt.lookup(&mut context.stack, &mut n) {
                        let subject = register_get(frame_ptr(context), u);
                        let jet_result = jet(context, subject);
                        match jet_result {
                            Ok(result) => {
                                register_set(frame_ptr(context), d, result);
                                do_goto(
                                    context,
                                    &mut body,
                                    &mut bend,
                                    &mut t,
                                );
                            }
                            Err(JetErr::Fail(err)) => {
                                break Err(ActualError(err));
                            }
                            Err(JetErr::Punt) => {
                                // XX run as a normal %cal here
                                do_call(
                                    context,
                                    &mut body,
                                    &mut bend,
                                    a,
                                    b,
                                    v,
                                    d,
                                    t,
                                );
                            }
                        }
                    } else {
                        do_call(
                            context,
                            &mut body,
                            &mut bend,
                            a,
                            b,
                            v,
                            d,
                            t,
                        );
                    }
                    // XX we need to build a warm state that is just a HAMT-map of the hot state
                }
                tas!(b"lnt") => unsafe {
                    // evaluate f against u in tail position
                    let u = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let f = slot(bend, 7).unwrap().as_direct().unwrap().data() as usize;

                    let subject = register_get(frame_ptr(context), u);
                    let formula = register_get(frame_ptr(context), f);

                    {
                        let pile = cg_pull_pile(context, subject, formula);
                        new_frame(context, pile, true);
                    }

                    do_rack(context, subject, formula);

                    {
                        let sire = (*pile_mem_ptr(context)).sire;
                        register_set(frame_ptr(context), sire, subject);
                    }

                    {
                        let wish = &mut (*pile_mem_ptr(context)).wish;
                        do_goto(
                            context,
                            &mut body,
                            &mut bend,
                            wish,
                        );
                    }
                }
                tas!(b"jmp") => {
                    let a = slot(bend, 6).unwrap();
                    let b = slot(bend, 14).unwrap();
                    let v = slot(bend, 15).unwrap();

                    do_tail_call(context, &mut body, &mut bend, a, b, v);
                }
                tas!(b"jmf") => unsafe {
                    let a = slot(bend, 6).unwrap();
                    let b = slot(bend, 14).unwrap();
                    let v = slot(bend, 30).unwrap();
                    let u = slot(bend, 62).unwrap().as_direct().unwrap().data() as usize;
                    let mut n = slot(bend, 63).unwrap();

                    if let Some(JetEntry(jet)) = context.cg_context.hot_hamt.lookup(&mut context.stack, &mut n) {
                        let subject = register_get(frame_ptr(context), u);
                        let jet_result = jet(context, subject);
                        match jet_result {
                            Ok(result) => {
                                do_return(
                                    context,
                                    virtual_frame,
                                    &mut body,
                                    &mut bend,
                                    result,
                                )
                                .unwrap();
                            }
                            Err(JetErr::Fail(err)) => {
                                break Err(ActualError(err));
                            }
                            Err(JetErr::Punt) => {
                                do_tail_call(
                                    context,
                                    &mut body,
                                    &mut bend,
                                    a,
                                    b,
                                    v,
                                );
                            }
                        }
                    } else {
                        do_tail_call(context, &mut body, &mut bend, a, b, v);
                    }
                }
                tas!(b"spy") => unsafe {
                    let e = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let p = slot(bend, 14).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(bend, 30).unwrap().as_direct().unwrap().data() as usize;
                    let mut t = slot(bend, 31).unwrap();

                    if let Some(cell) = context.scry_stack.cell() {
                        let scry_ref = register_get(frame_ptr(context), e);
                        let scry_path = register_get(frame_ptr(context), p);
                        let scry_stack = context.scry_stack;
                        let scry_handler = cell.head();
                        let scry_gate;
                        if let Ok(cell) = scry_handler.as_cell() {
                            scry_gate = cell;
                        } else {
                            break Err(ActualError(Error::ScryCrashed(D(0))));
                        }
                        let payload = T(&mut context.stack, &[scry_ref, scry_path]);
                        let slam = T(&mut context.stack, &[D(9), D(2), D(0), D(1)]);
                        let scry_core = T(
                            &mut context.stack,
                            &[
                                scry_gate.head(),
                                payload,
                                match scry_gate.tail().as_cell() {
                                    Ok(cell) => cell.tail(),
                                    Err(_) => break Err(ActualError(Error::ScryCrashed(D(0)))),
                                },
                            ],
                        );

                        context.scry_stack = cell.tail();

                        match cg_interpret(context, scry_core, slam) {
                            Ok(noun) => match noun.as_either_atom_cell() {
                                Left(atom) => {
                                    if atom.as_noun().raw_equals(D(0)) {
                                        break Err(ActualError(Error::ScryBlocked(scry_path)));
                                    } else {
                                        break Err(ActualError(Error::ScryCrashed(D(0))));
                                    }
                                }
                                Right(cell) => match cell.tail().as_either_atom_cell() {
                                    Left(_) => {
                                        let stack = &mut context.stack;
                                        let hunk =
                                            T(stack, &[D(tas!(b"hunk")), scry_ref, scry_path]);
                                        cg_mean_push(context, hunk);
                                        break Err(ActualError(Error::ScryCrashed(D(0))));
                                    }
                                    Right(cell) => {
                                        register_set(frame_ptr(context), d, cell.tail());
                                        context.scry_stack = scry_stack;
                                        do_goto(
                                            context,
                                            &mut body,
                                            &mut bend,
                                            &mut t,
                                        );
                                    }
                                },
                            },
                            Err(error) => match error {
                                Error::Deterministic(trace) | Error::ScryCrashed(trace) => {
                                    break Err(ActualError(Error::ScryCrashed(trace)));
                                }
                                Error::NonDeterministic(_) => {
                                    break Err(ActualError(error));
                                }
                                Error::ScryBlocked(_) => {
                                    break Err(ActualError(Error::NonDeterministic(D(0))));
                                }
                            },
                        }
                    } else {
                        break Err(ActualError(Error::Deterministic(D(0))));
                    }
                }
                tas!(b"mer") => unsafe {
                    let k = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let u = slot(bend, 14).unwrap().as_direct().unwrap().data() as usize;
                    let f = slot(bend, 30).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(bend, 62).unwrap().as_direct().unwrap().data() as usize;
                    let mut i = slot(bend, 126).unwrap();
                    let mut m = slot(bend, 127).unwrap();

                    // let mut key = T(&mut context.stack, &[k_value, u_value, f_value]);
                    // XX use k (clue) later, with persistent caching implementation
                    let _k_value = register_get(frame_ptr(context), k);
                    let u_value = register_get(frame_ptr(context), u);
                    let f_value = register_get(frame_ptr(context), f);
                    let mut key = T(&mut context.stack, &[u_value, f_value]);

                    if let Some(value) = context.cache.lookup(&mut context.stack, &mut key) {
                        register_set(frame_ptr(context), d, value);
                        do_goto(
                            context,
                            &mut body,
                            &mut bend,
                            &mut i,
                        );
                    } else {
                        do_goto(
                            context,
                            &mut body,
                            &mut bend,
                            &mut m,
                        );
                    }
                }
                tas!(b"don") => unsafe {
                    let s = slot(bend, 3).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(frame_ptr(context), s);
                    if let Some(ret_value) = do_return(
                        context,
                        virtual_frame,
                        &mut body,
                        &mut bend,
                        s_value,
                    ) {
                        break Ok(ret_value);
                    }
                }
                tas!(b"bom") => {
                    break Err(ActualError(Error::Deterministic(D(0))));
                }
                _ => {
                    panic!("invalid bend instruction");
                }
            }
        }
    }
}

fn exit(context: &mut Context, context_snapshot: ContextSnapshot, error: ActualError) -> Error {
    context.cold = context_snapshot.cold;
    context.warm = context_snapshot.warm;
    context.cache = context_snapshot.cache;
    context.cg_context.line = context_snapshot.line;
    context.cg_context.hill = context_snapshot.hill;

    let current_frame = unsafe { context.stack.get_frame_lowest() as *const Frame };
    let mut preserve = match error.0 {
        Error::ScryBlocked(path) => path,
        Error::Deterministic(t) | Error::NonDeterministic(t) | Error::ScryCrashed(t) => {
            let h = unsafe { (*current_frame).mean };
            T(&mut context.stack, &[h, t])
        }
    };

    while context.stack.get_frame_pointer() != context_snapshot.virtual_frame {
        unsafe {
            context.stack.preserve(&mut preserve);
            context.stack.frame_pop();
        }
    }

    match error.0 {
        Error::Deterministic(_) => Error::Deterministic(preserve),
        Error::NonDeterministic(_) => Error::NonDeterministic(preserve),
        Error::ScryCrashed(_) => Error::ScryCrashed(preserve),
        Error::ScryBlocked(_) => error.0,
    }
}

fn register_set(frame: *mut Frame, local: usize, value: Noun) {
    unsafe {
        assert!(local < (*(*frame).pile.0).sans);
        let pois_sz = (*frame).pois_sz;
        let reg_ptr = (frame as *mut Noun).add(FRAME_WORD_SIZE + pois_sz + local);
        *reg_ptr = value;
    }
}

fn register_get(frame: *const Frame, local: usize) -> Noun {
    unsafe {
        assert!(local < (*(*frame).pile.0).sans);
        let pois_sz = (*frame).pois_sz;
        let reg_ptr = (frame as *mut Noun).add(FRAME_WORD_SIZE + pois_sz + local);
        *reg_ptr
    }
}

fn poison_set(frame: *mut Frame, local: usize) {
    let index = local / 64;
    let offset = local % 64;
    assert!(index < unsafe { (*frame).pois_sz });
    let bitmap = unsafe { (frame as *mut u64).add(FRAME_WORD_SIZE + index) };
    unsafe { *bitmap |= 1 << offset };
}

fn poison_get(frame: *const Frame, local: usize) -> bool {
    let index = local / 64;
    let offset = local % 64;
    assert!(index < unsafe { (*frame).pois_sz });
    let bitmap = unsafe { *(frame as *const u64).add(FRAME_WORD_SIZE + index) };
    bitmap & (1 << offset) != 0
}

pub mod types {
    use std::ptr::copy_nonoverlapping;
    use std::mem::size_of;
    use std::time::Instant;

    use crate::{
        hamt::Hamt,
        interpreter::Error,
        jets::util::slot,
        jets::Jet,
        jets::hot::Hot,
        mem::{NockStack, Preserve},
        noun::{Noun, T},
        trace::TraceStack,
        persist::Persist,
    };

    use super::Hill;
    use super::util::part_will;

    pub struct CGContext {
        /// Linearizer core
        pub line: Option<Noun>,
        /// Code table
        pub hill: Hill,
        /// Jet table, as a HAMT for codegen
        pub hot_hamt: Hamt<JetEntry>,
    }

    impl CGContext {
        pub fn new(stack: &mut NockStack, hot: Hot) -> CGContext {
            unsafe {
                stack.frame_push(0);
                let mut hill = Hill::new(stack);
                let mut hot_hamt = Hamt::new(stack);
                for lhe in hot.as_slice() {
                    let n = &mut T(stack, &[lhe.path, lhe.axis.as_noun()]);
                    hot_hamt = hot_hamt.insert(stack, n, JetEntry(lhe.jet));
                }
                stack.preserve(&mut hill);
                stack.preserve(&mut hot_hamt);
                stack.frame_pop();
                CGContext {
                    line: None,
                    hill,
                    hot_hamt,
                }
            }
        }
    }

    impl Preserve for CGContext {
        unsafe fn preserve(&mut self, stack: &mut NockStack) {
            self.line.as_mut().map(|line_mut| {
                stack.preserve(line_mut);
            });
            stack.preserve(&mut self.hill);
            stack.preserve(&mut self.hot_hamt);
        }

        unsafe fn assert_in_stack(&self, stack: &NockStack) {
            self.line.as_ref().map(|line_ref| {
                line_ref.assert_in_stack(stack);
            });
            self.hill.assert_in_stack(stack);
            self.hot_hamt.assert_in_stack(stack);
        }
    }

    #[derive(Copy,Clone)]
    pub struct JetEntry(pub Jet);

    impl Preserve for JetEntry {
        unsafe fn preserve(&mut self, _stack: &mut NockStack) {
        }

        unsafe fn assert_in_stack(&self, _stack: &NockStack) {
        }
    }

    pub struct Frame {
        pub mean: Noun,
        pub traz: *mut *const TraceStack,
        pub time: Timer,
        pub slow: Noun,
        pub pile: Pile,
        pub dest: usize,
        pub cont: Noun,
        pub pois_sz: usize, // length of poison vector
                            // poison: Vec<u64>,     // variable size
                            // registers: Vec<Noun>, // variable size
    }

    #[derive(Copy, Clone)]
    pub struct Timer(pub *const TimerMem);

    #[derive(Copy, Clone)]
    pub struct TimerMem {
        pub start: Instant,
        pub prev: Timer
    }

    #[derive(Copy, Clone)]
    //#[repr(packed)]
    //#[repr(C)]
    pub struct PileMem {
        pub long: Noun,
        pub bait: Noun,
        pub walt: Noun,
        pub wish: Noun,
        pub sire: usize,
        pub will: Hamt<Noun>,
        pub sans: usize,
    }

    const_assert_eq!(0, size_of::<PileMem>() % size_of::<u64>());

    #[derive(Copy, Clone)]
    pub struct Pile(pub *mut PileMem);
    impl Preserve for Pile {
        unsafe fn preserve(&mut self, stack: &mut NockStack) {
            if stack.is_in_frame(self.0) {
                (*self.0).long.preserve(stack);
                (*self.0).bait.preserve(stack);
                (*self.0).walt.preserve(stack);
                (*self.0).wish.preserve(stack);
                (*self.0).will.preserve(stack);
                let dest_mem: *mut PileMem = stack.struct_alloc_in_previous_frame(1);
                copy_nonoverlapping(self.0, dest_mem, 1);
                self.0 = dest_mem;
            }
        }

        unsafe fn assert_in_stack(&self, stack: &NockStack) {
            stack.assert_struct_is_in(self.0, 1);
            (*(self.0)).long.assert_in_stack(stack);
            (*(self.0)).bait.assert_in_stack(stack);
            (*(self.0)).walt.assert_in_stack(stack);
            (*(self.0)).wish.assert_in_stack(stack);
            (*(self.0)).will.assert_in_stack(stack);
        }
    }

    impl Persist for Pile {
        unsafe fn space_needed(&mut self, stack: &mut NockStack) -> usize {
            let long_space_needed = (*(self.0)).long.space_needed(stack);
            let bait_space_needed = (*(self.0)).bait.space_needed(stack);
            let walt_space_needed = (*(self.0)).walt.space_needed(stack);
            let wish_space_needed = (*(self.0)).wish.space_needed(stack);
            let will_space_needed = (*(self.0)).will.space_needed(stack);
            size_of::<PileMem>() // see assert of size_of above
              + long_space_needed
              + bait_space_needed
              + walt_space_needed
              + wish_space_needed
              + will_space_needed
        }

        unsafe fn copy_to_buffer(&mut self, stack: &mut NockStack, buffer: &mut *mut u8) {
            let pile_buffer: *mut PileMem = *buffer as *mut PileMem;
            std::ptr::copy_nonoverlapping(self.0, pile_buffer, 1);
            *self = Pile(pile_buffer);
            *buffer = pile_buffer.add(1) as *mut u8;

            (*pile_buffer).long.copy_to_buffer(stack, buffer);
            (*pile_buffer).bait.copy_to_buffer(stack, buffer);
            (*pile_buffer).walt.copy_to_buffer(stack, buffer);
            (*pile_buffer).wish.copy_to_buffer(stack, buffer);
            (*pile_buffer).will.copy_to_buffer(stack, buffer);
        }

        unsafe fn handle_to_u64(&self) -> u64 {
            self.0 as u64
        }

        unsafe fn handle_from_u64(meta_handle: u64) -> Self {
            Pile(meta_handle as *mut PileMem)
        }
    }

    impl Pile {
        pub fn from_noun(stack: &mut NockStack, p: Noun) -> Pile {
            unsafe {
                let mem: *mut PileMem = stack.struct_alloc(1);
                *mem = PileMem {
                    long: slot(p, 2).unwrap(),
                    bait: slot(p, 14).unwrap(),
                    walt: slot(p, 30).unwrap(),
                    wish: slot(p, 62).unwrap(),
                    sire: slot(p, 126).unwrap().as_direct().unwrap().data() as usize,
                    will: part_will(stack, slot(p, 254).unwrap()),
                    sans: slot(p, 255).unwrap().as_direct().unwrap().data() as usize,
                };
                assert!((*mem).will.lookup(stack, &mut (*mem).wish).is_some());
                Pile(mem)
            }
        }
    }

    // Actual nock crashes in cg_interpret_inner must return this error,
    // to allow us to distinguish between those and errors running codegen nock.
    //
    // This newtype wrapper is stripped in cg_interpret, removing the possibility of confounding
    // the meta-levels
    pub struct ActualError(pub Error);
}
mod util {
    use ares_macros::tas;

    use crate::{
        hamt::Hamt,
        interpreter::{Context, Error},
        jets::util::{kick, slam, slot},
        mem::NockStack,
        noun::{Noun, D, T},
    };

    use super::poison_get;
    use super::poison_set;
    use super::register_get;
    use super::register_set;
    use super::frame_ptr;
    use super::pile_mem_ptr;
    use super::Pile;
    use super::{new_frame, Hill};
    use super::FRAME_WORD_SIZE;
    
    use assert_no_alloc::permit_alloc;

    /// +peek slot in line core is 4
    const PEEK_AXIS: u64 = 4;

    /// +poke slot in line core is 46
    const POKE_AXIS: u64 = 46;

    /// +rake slot in line core is 95
    const RACK_AXIS: u64 = 10;

    pub type NounResult = Result<Noun, Error>;

    /// returns (unit [bell hill])
    pub fn peek(context: &mut Context, subject: Noun, formula: Noun) -> Noun {
        let line = context.cg_context.line.unwrap();
        let pek = kick(context, line, D(PEEK_AXIS)).unwrap();
        let sam = T(&mut context.stack, &[subject, formula]);
        slam(context, pek, sam).unwrap()
    }

    /// returns [(set bell) (set bell) _line]
    pub fn poke(context: &mut Context, gist: Noun) -> (Noun, Noun) {
        let line = context.cg_context.line.unwrap();
        let pok = kick(context, line, D(POKE_AXIS)).unwrap();
        let res = slam(context, pok, gist).unwrap();
        let new = slot(res, 2).unwrap();
        let old = slot(res, 6).unwrap();
        assert!(unsafe{old.raw_equals(D(0))});
        context.cg_context.line = Some(slot(res, 7).unwrap());
        (new, old)
    }

    pub fn tap(stack: &mut NockStack, map: Noun) -> NounResult {
        tap_in(stack, map, D(0))
    }

    fn tap_in(stack: &mut NockStack, a: Noun, mut b: Noun) -> NounResult {
        unsafe {
            stack.frame_push(0);
            *(stack.push::<Noun>()) = a;
            loop {
                if stack.stack_is_empty() {
                    break;
                }
                let tree = *(stack.top::<Noun>());
                stack.pop::<Noun>();
                if tree.raw_equals(D(0)) {
                    continue;
                }
                // XX needs to pop frame even if it fails
                let node = slot(tree, 2)?;
                let lr = slot(tree, 3)?;
                let l = slot(lr, 2)?;
                let r = slot(lr, 3)?;
                b = T(stack, &[node, b]);
                *(stack.push::<Noun>()) = r;
                *(stack.push::<Noun>()) = l;
            }
            stack.preserve(&mut b);
            stack.frame_pop();
            Ok(b)
        }
    }

    pub fn part_hill(stack: &mut NockStack, hill: Noun) -> Hill {
        let mut kvs = tap(stack, hill).unwrap();
        let mut hill = Hill::new(stack);
        while !unsafe { kvs.raw_equals(D(0)) } {
            let c = kvs.as_cell().unwrap();
            let kv = c.head();
            let mut bell = slot(kv, 2).unwrap();
            let pile = Pile::from_noun(stack, slot(kv, 3).unwrap());
            hill.0 = hill.0.insert(stack, &mut bell, pile);
            kvs = c.tail();
        }
        hill
    }

    pub fn part_will(stack: &mut NockStack, will: Noun) -> Hamt<Noun> {
        let mut kvs = tap(stack, will).unwrap();
        let mut hamt = Hamt::new(stack);
        while !unsafe { kvs.raw_equals(D(0)) } {
            let c = kvs.as_cell().unwrap();
            let kv = c.head();
            let mut bile = slot(kv, 2).unwrap();
            let blob = slot(kv, 3).unwrap();
            hamt = hamt.insert(stack, &mut bile, blob);
            kvs = c.tail();
        }
        hamt
    }

    pub fn part_peek(stack: &mut NockStack, peek: Noun) -> (Noun, Hill) {
        let bell = slot(peek, 6).unwrap();
        let hall = part_hill(stack, slot(peek, 7).unwrap());
        (bell, hall)
    }

    pub fn comp(context: &mut Context, s: Noun, f: Noun) -> Noun {
        T(&mut context.stack, &[D(tas!(b"comp")), D(0), s, f])
    }

    pub fn do_call(
        context: &mut Context,
        body: &mut Noun,
        bend: &mut Noun,
        mut a: Noun,
        mut b: Noun,
        mut v: Noun,
        d: usize,
        t: Noun,
    ) {
        let parent_frame = unsafe { frame_ptr(context) };
        unsafe {
            (*parent_frame).dest = d;
            (*parent_frame).cont = t;
        }

        let pile = context.cg_context.hill.0.lookup(&mut context.stack, &mut a).unwrap();

        unsafe {
            new_frame(context, pile, false);
        }

        let mut bait = unsafe { (*pile_mem_ptr(context)).bait };
        let mut walt = unsafe { (*pile_mem_ptr(context)).walt };

        let frame = unsafe { frame_ptr(context) };

        loop {
            unsafe {
                if b.raw_equals(D(0)) {
                    if !bait.raw_equals(D(0)) {
                        panic!("codegen non-deterministic");
                    }
                    break;
                }

                let b_i = slot(b, 2).unwrap().as_direct().unwrap().data() as usize;
                b = slot(b, 3).unwrap();
                let bait_i = slot(bait, 2).unwrap().as_direct().unwrap().data() as usize;
                bait = slot(bait, 3).unwrap();

                if poison_get(parent_frame, b_i) {
                    poison_set(frame, bait_i);
                }
            }
        }

        loop {
            unsafe {
                if v.raw_equals(D(0)) {
                    if !walt.raw_equals(D(0)) {
                        panic!("codegen non-deterministic");
                    }
                    break;
                }

                let v_i = slot(v, 2).unwrap().as_direct().unwrap().data() as usize;
                v = slot(v, 3).unwrap();
                let walt_i = slot(walt, 2).unwrap().as_direct().unwrap().data() as usize;
                walt = slot(walt, 3).unwrap();

                register_set(frame, walt_i, register_get(parent_frame, v_i));
            }
        }

        {
            let will = unsafe { (*pile_mem_ptr(context)).will };
            let long = &mut unsafe { (*pile_mem_ptr(context)).long };
            let blob = will
                .lookup(&mut context.stack, long)
                .unwrap();
            *body = slot(blob, 6).unwrap();
            *bend = slot(blob, 7).unwrap();
        }
    }

    pub fn do_tail_call(
        context: &mut Context,
        body: &mut Noun,
        bend: &mut Noun,
        mut a: Noun,
        mut b: Noun,
        mut v: Noun,
    ) {
        unsafe {
            let pile = context.cg_context.hill.0.lookup(&mut context.stack, &mut a).unwrap();
            new_frame(context, pile, true); // set up tail call frame

            let frame = frame_ptr(context);

            let sans = (*pile.0).sans;
            let poison_size = (*frame).pois_sz;

            let mut bait = (*pile.0).bait;
            let mut walt = (*pile.0).walt;

            let old_poison_sz = *(frame as *const usize).add(FRAME_WORD_SIZE + sans + poison_size);
            let old_poison_ptr = (frame as *const u64).add(FRAME_WORD_SIZE + sans + poison_size + 1);

            loop {
                if b.raw_equals(D(0)) {
                    if !bait.raw_equals(D(0)) {
                        panic!("codegen non-deterministic");
                    };
                    break;
                }

                let b_i = slot(b, 2).unwrap().as_direct().unwrap().data();
                b = slot(b, 3).unwrap();
                let bait_i = slot(bait, 2).unwrap().as_direct().unwrap().data();
                bait = slot(bait, 3).unwrap();

                let b_i_offset = b_i / 64;
                let b_i_bit = b_i % 64;
                assert!((b_i_offset as usize) < old_poison_sz);

                if *(old_poison_ptr.add(b_i_offset as usize)) & (1 << b_i_bit) != 0 {
                    poison_set(frame_ptr(context), bait_i as usize);
                }
            }

            let old_reg_ptr = old_poison_ptr.add(old_poison_sz) as *const Noun;

            loop {
                if v.raw_equals(D(0)) {
                    if !walt.raw_equals(D(0)) {
                        panic!("codegen non-deterministic");
                    };
                    break;
                };

                let v_i = slot(v, 2).unwrap().as_direct().unwrap().data();
                v = slot(v, 3).unwrap();
                let walt_i = slot(walt, 2).unwrap().as_direct().unwrap().data();
                walt = slot(walt, 3).unwrap();
                // XX we should also store the old reg size and assert this doesn't read
                // past it
                register_set(
                    frame_ptr(context),
                    walt_i as usize,
                    *(old_reg_ptr.add(v_i as usize)),
                );
            }

            let will = (*pile_mem_ptr(context)).will;
            let blob = will
                .lookup(&mut context.stack, &mut (*(pile.0)).long)
                .unwrap();
            *body = slot(blob, 6).unwrap();
            *bend = slot(blob, 7).unwrap();
        }
    }

    pub fn do_goto(
        context: &mut Context,
        body: &mut Noun,
        bend: &mut Noun,
        bile: &mut Noun,
    ) {
        let blob = unsafe {
            (*pile_mem_ptr(context))
                .will
                .lookup(&mut context.stack, bile)
                .ok_or(Error::NonDeterministic(D(0)))
                .unwrap()
        };
        *body = slot(blob, 6).unwrap();
        *bend = slot(blob, 7).unwrap();
    }

    pub fn do_return(
        context: &mut Context,
        base_frame: *const u64,
        body: &mut Noun,
        bend: &mut Noun,
        mut ret_value: Noun,
    ) -> Option<Noun> {
        unsafe {
            // XX debug assertions
            //
            context.preserve();
            context.stack.preserve(&mut ret_value);
            context.stack.frame_pop();

            let frame = frame_ptr(context);

            if context.stack.get_frame_pointer() == base_frame {
                return Some(ret_value);
            }
            register_set(frame, (*frame).dest, ret_value);

            let will = (*pile_mem_ptr(context)).will;
            let blob = will
                .lookup(&mut context.stack, &mut (*frame).cont)
                .unwrap();
            *body = slot(blob, 6).unwrap();
            *bend = slot(blob, 7).unwrap();
        }
        None
    }

    /** slog the IR for an arm */
    pub fn do_rack(
        context: &mut Context,
        sub: Noun,
        form: Noun,
    ) {
        let line = context.cg_context.line.unwrap();
        let gat = kick(context, line, D(RACK_AXIS)).unwrap();
        let sf_cel = T(&mut context.stack, &[sub, form]);
        let _res = slam(context, gat, sf_cel);
        ()
    }
}
