// tracing is off
use ares_macros::tas;
use either::Either::{Left, Right};
use std::mem::size_of;

use crate::hamt::Hamt;
use crate::jets::JetErr;
use crate::interpreter::{inc, Context, Error};
use crate::jets::cold::Cold;
use crate::jets::util::slot;
use crate::jets::warm::Warm;
use crate::mem::{NockStack, Preserve};
use crate::noun::{Noun, D, NO, T, YES};
use crate::trace::TraceStack;
use types::{ActualError, Frame, Pile, JetEntry};

use self::util::{comp, do_call, do_goto, do_return, do_tail_call, part_peek, peek, poke};

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

/// Push onto the mean stack.
fn cg_mean_push(stack: &mut NockStack, frame: *mut Frame, noun: Noun) {
    unsafe {
        (*frame).mean = T(stack, &[noun, (*frame).mean]);
    }
}

/// Pop off of the mean stack.
fn cg_mean_pop(frame: *mut Frame) {
    unsafe {
        (*frame).mean = (*frame).mean
            .as_cell()
            .expect("serf: unexpected end of mean stack\r")
            .tail();
    }
}

/// Push onto the slow stack.
fn cg_slow_push(stack: &mut NockStack, frame: *mut Frame, noun: Noun) {
    unsafe {
        (*frame).slow = T(stack, &[noun, (*frame).mean]);
    }
}

/// Pop off of the slow stack.
fn cg_slow_pop(frame: *mut Frame) {
    unsafe {
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
        let line = poke(context, comp);
        context.cg_context.line = Some(line);
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
 * Push a new interpreter frame and return the associated virtual frame
 *
 * If tail is false, this will simply set up a new frame.
 *
 * If tail is true, this will set up a new frame in the current stack position, but in addition
 * - resize the frame to hold the old poisons, the old registers, the new poisons, and the new
 *   registers
 * - move the old poisons and registers over
 */
unsafe fn new_frame(context: &mut Context, frame_ref: &mut *mut Frame, pile: Pile, tail: bool) {
    let sans = unsafe { (*(pile.0)).sans };
    let poison_size = (sans + 63) >> 6;
    if tail {
        unsafe {
            let old_frame_ptr = context.stack.get_frame_lowest() as *mut Frame;
            let old_sans = (*((*old_frame_ptr).pile.0)).sans;
            let old_poison_size = (*old_frame_ptr).pois_sz;
            context.stack.frame_replace(
                FRAME_WORD_SIZE + poison_size + sans + old_poison_size + old_sans + 1,
            );
            let frame_ptr = context.stack.get_frame_lowest();
            // save old poison size and old poison and registers for new call setup
            *(frame_ptr.add(FRAME_WORD_SIZE + poison_size + sans) as *mut usize) = old_poison_size;
            std::ptr::copy(
                frame_ptr.add(FRAME_WORD_SIZE),
                frame_ptr.add(FRAME_WORD_SIZE + poison_size + sans + 1),
                old_poison_size + old_sans,
            );
        }
    } else {
        context
            .stack
            .frame_push(FRAME_WORD_SIZE + poison_size + sans);
    };
    unsafe {
        *frame_ref = context.stack.get_frame_lowest() as *mut Frame;
    }

    unsafe {
        **frame_ref = Frame {
            mean: D(0),
            traz: std::ptr::null::<TraceStack>() as *mut *const TraceStack,
            slow: D(0),

            pile: pile,

            dest: 0,
            cont: D(0),
            pois_sz: poison_size,
        }
    }
}

unsafe fn pop_frame(context: &mut Context, frame_ref: &mut *mut Frame) {
    context.stack.frame_pop();
    *frame_ref = context.stack.get_frame_lowest() as *mut Frame;
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
    // Setup stack for codegen interpretation.
    // Stack frame layout: [mean trace slow pile dest cont poison registers]
    // XX update local_noun_pointer() calls with correct constants
    let mut current_frame: *mut Frame = std::ptr::null_mut();
    {
        let pile = cg_pull_pile(context, subject, formula);
        unsafe { new_frame(context, &mut current_frame, pile, false) };
    }

    // Load the initial subject to the sire register.
    {
        let sire = unsafe { (*(*current_frame).pile.0).sire };
        register_set(current_frame, sire, subject);
    }

    // Get the blob, body, and bend nouns from our pile.
    let mut body: Noun;
    let mut bend: Noun;
    {
        let will = unsafe { (*(*current_frame).pile.0).will };
        let blob = will
            .lookup(&mut context.stack, &mut unsafe {
                (*(*current_frame).pile.0).wish
            })
            .unwrap();
        body = slot(blob, 6).expect("codegen nock error");
        bend = slot(blob, 7).expect("codegen nock error");
    }

    loop {
        if !unsafe { body.raw_equals(D(0)) } {
            let pole = slot(body, 2).unwrap();
            body = slot(body, 3).unwrap();
            match slot(pole, 2).unwrap().as_direct().unwrap().data() {
                tas!(b"imm") => {
                    let local = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let value = slot(pole, 6).unwrap();
                    register_set(current_frame, local, value);
                }
                tas!(b"mov") => {
                    let src = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let dst = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let value = register_get(current_frame, src);
                    register_set(current_frame, dst, value);
                }
                tas!(b"inc") => {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(current_frame, s);
                    if let Ok(atom) = s_value.as_atom() {
                        let value = inc(&mut context.stack, atom);
                        register_set(current_frame, d, value.as_noun());
                    } else {
                        break Err(ActualError(Error::Deterministic(D(0))));
                    }
                }
                tas!(b"con") => {
                    let h = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let t = slot(pole, 14).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 15).unwrap().as_direct().unwrap().data() as usize;
                    let h_value = register_get(current_frame, h);
                    let t_value = register_get(current_frame, t);
                    let value = T(&mut context.stack, &[h_value, t_value]);
                    register_set(current_frame, d, value);
                }
                tas!(b"cop") => {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(current_frame, s);
                    if s_value.is_atom() {
                        poison_set(current_frame, s);
                    }
                }
                tas!(b"lop") => {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(current_frame, s);
                    if !unsafe { s_value.raw_equals(YES) || s_value.raw_equals(NO) } {
                        poison_set(current_frame, s);
                    }
                }
                tas!(b"coc") => {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(current_frame, s);
                    if s_value.is_atom() {
                        break Err(ActualError(Error::Deterministic(D(0))));
                    }
                }
                tas!(b"hed") => {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(current_frame, s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            poison_set(current_frame, s);
                        }
                        Right(cell) => {
                            register_set(current_frame, d, cell.head());
                        }
                    };
                }
                tas!(b"tal") => {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(current_frame, s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            poison_set(current_frame, s);
                        }
                        Right(cell) => {
                            register_set(current_frame, d, cell.tail());
                        }
                    };
                }
                tas!(b"hci") => {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(current_frame, s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            break Err(ActualError(Error::Deterministic(D(0))));
                        }
                        Right(cell) => {
                            register_set(current_frame, d, cell.head());
                        }
                    };
                }
                tas!(b"tci") => {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(current_frame, s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            break Err(ActualError(Error::Deterministic(D(0))));
                        }
                        Right(cell) => {
                            register_set(current_frame, d, cell.tail());
                        }
                    };
                }
                tas!(b"men") => {
                    let l = slot(pole, 6).unwrap();
                    let s = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(current_frame, s);
                    let mean = T(&mut context.stack, &[l, s_value]);
                    cg_mean_push(&mut context.stack, current_frame, mean);
                }
                tas!(b"man") => {
                    cg_mean_pop(current_frame);
                }
                tas!(b"slo") => {
                    let s = slot(pole, 3).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(current_frame, s);
                    cg_slow_push(&mut context.stack, current_frame, s_value);
                }
                tas!(b"sld") => {
                    cg_slow_pop(current_frame);
                }
                tas!(b"hit") => {
                    let s = slot(pole, 3).unwrap().as_direct().unwrap().data() as usize;
                    let _s_value = register_get(current_frame, s);
                    // XX increment a profiling hit counter labeled with the noun in s
                    todo!("hit")
                }
                tas!(b"slg") => {
                    let s = slot(pole, 3).unwrap().as_direct().unwrap().data() as usize;
                    let clue = register_get(current_frame, s);
                    if let Ok(slog_cell) = clue.as_cell() {
                        if let Ok(pri_direct) = slog_cell.head().as_direct() {
                            let tank = slog_cell.tail();
                            context
                                .newt
                                .slog(&mut context.stack, pri_direct.data(), tank);
                        };
                    };
                }
                tas!(b"mew") => {
                    let k = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let u = slot(pole, 14).unwrap().as_direct().unwrap().data() as usize;
                    let f = slot(pole, 30).unwrap().as_direct().unwrap().data() as usize;
                    let r = slot(pole, 31).unwrap().as_direct().unwrap().data() as usize;

                    let _k_value = register_get(current_frame, k);
                    let u_value = register_get(current_frame, u);
                    let f_value = register_get(current_frame, f);
                    let r_value = register_get(current_frame, r);
                    // let mut key = T(&mut context.stack, &[k_value, u_value, f_value]);
                    // XX use k (clue) later, with persistent caching implementation
                    let mut key = T(&mut context.stack, &[u_value, f_value]);

                    context.cache = context.cache.insert(&mut context.stack, &mut key, r_value);
                }
                tas!(b"tim") => {
                    // XX push a timer onto the stack and start it
                    todo!("tim")
                }
                tas!(b"tom") => {
                    // XX pop a timer from the stack, stop it, and print elapsed
                    todo!("tom")
                }
                tas!(b"mem") => {
                    // XX print memory usage
                    todo!("mem")
                }
                tas!(b"pol") => {
                    let s = slot(pole, 6).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(pole, 7).unwrap().as_direct().unwrap().data() as usize;
                    if poison_get(current_frame, s) {
                        poison_set(current_frame, d);
                    }
                }
                tas!(b"poi") => {
                    let d = slot(pole, 3).unwrap().as_direct().unwrap().data() as usize;
                    poison_set(current_frame, d);
                }
                tas!(b"ipb") => {
                    let mut s = slot(pole, 3).unwrap();
                    if let true = loop {
                        if unsafe { s.raw_equals(D(0)) } {
                            break false;
                        } else {
                            let i =
                                s.as_cell().unwrap().head().as_direct().unwrap().data() as usize;
                            if poison_get(current_frame, i) {
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
            match slot(bend, 2).unwrap().as_direct().unwrap().data() {
                tas!(b"clq") => {
                    let s = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(current_frame, s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            let mut o = slot(bend, 15).unwrap();
                            do_goto(
                                &mut context.stack,
                                current_frame,
                                &mut body,
                                &mut bend,
                                &mut o,
                            );
                        }
                        Right(_cell) => {
                            let mut z = slot(bend, 14).unwrap();
                            do_goto(
                                &mut context.stack,
                                current_frame,
                                &mut body,
                                &mut bend,
                                &mut z,
                            );
                        }
                    };
                }
                tas!(b"eqq") => {
                    let l = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let r = slot(bend, 14).unwrap().as_direct().unwrap().data() as usize;
                    let l_value = register_get(current_frame, l);
                    let r_value = register_get(current_frame, r);
                    if unsafe { l_value.raw_equals(r_value) } {
                        let mut z = slot(bend, 30).unwrap();
                        do_goto(
                            &mut context.stack,
                            current_frame,
                            &mut body,
                            &mut bend,
                            &mut z,
                        );
                    } else {
                        let mut o = slot(bend, 31).unwrap();
                        do_goto(
                            &mut context.stack,
                            current_frame,
                            &mut body,
                            &mut bend,
                            &mut o,
                        );
                    }
                }
                tas!(b"brn") => {
                    let s = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(current_frame, s);
                    if unsafe { s_value.raw_equals(D(0)) } {
                        let mut z = slot(bend, 14).unwrap();
                        do_goto(
                            &mut context.stack,
                            current_frame,
                            &mut body,
                            &mut bend,
                            &mut z,
                        );
                    } else if unsafe { s_value.raw_equals(D(1)) } {
                        let mut o = slot(bend, 15).unwrap();
                        do_goto(
                            &mut context.stack,
                            current_frame,
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
                        &mut context.stack,
                        current_frame,
                        &mut body,
                        &mut bend,
                        &mut t,
                    );
                }
                tas!(b"lnk") => {
                    let u = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let f = slot(bend, 14).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(bend, 30).unwrap().as_direct().unwrap().data() as usize;
                    let t = slot(bend, 31).unwrap();

                    let subject = register_get(current_frame, u);
                    let formula = register_get(current_frame, f);

                    unsafe {
                        (*current_frame).dest = d;
                        (*current_frame).cont = t;
                    }

                    {
                        let pile = cg_pull_pile(context, subject, formula);
                        unsafe {
                            new_frame(context, &mut current_frame, pile, false);
                        }
                    }

                    {
                        let sire = unsafe { (*(*current_frame).pile.0).sire };
                        register_set(current_frame, sire, subject);
                    }

                    {
                        do_goto(
                            &mut context.stack,
                            current_frame,
                            &mut body,
                            &mut bend,
                            &mut unsafe { (*(*current_frame).pile.0).wish },
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
                        &mut current_frame,
                        &mut body,
                        &mut bend,
                        a,
                        b,
                        v,
                        d,
                        t,
                    );
                }
                tas!(b"caf") => {
                    let a = slot(bend, 6).unwrap();
                    let b = slot(bend, 14).unwrap();
                    let v = slot(bend, 30).unwrap();
                    let d = slot(bend, 62).unwrap().as_direct().unwrap().data() as usize;
                    let mut t = slot(bend, 126).unwrap();
                    let u = slot(bend, 254).unwrap().as_direct().unwrap().data() as usize;
                    let mut n = slot(bend, 255).unwrap();

                    if let Some(JetEntry(jet)) = context.cg_context.hot_hamt.lookup(&mut context.stack, &mut n) {
                        let subject = register_get(current_frame, u);
                        let jet_result = jet(context, subject);
                        match jet_result {
                            Ok(result) => {
                                register_set(current_frame, d, result);
                                do_goto(
                                    &mut context.stack,
                                    current_frame,
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
                                    &mut current_frame,
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
                            &mut current_frame,
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
                tas!(b"lnt") => {
                    // evaluate f against u in tail position
                    let u = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let f = slot(bend, 7).unwrap().as_direct().unwrap().data() as usize;

                    let subject = register_get(current_frame, u);
                    let formula = register_get(current_frame, f);

                    {
                        let pile = cg_pull_pile(context, subject, formula);
                        unsafe {
                            new_frame(context, &mut current_frame, pile, true);
                        }
                    }

                    {
                        let sire = unsafe { (*(*current_frame).pile.0).sire };
                        register_set(current_frame, sire, subject);
                    }

                    {
                        do_goto(
                            &mut context.stack,
                            current_frame,
                            &mut body,
                            &mut bend,
                            &mut unsafe { (*(*current_frame).pile.0).wish },
                        );
                    }
                }
                tas!(b"jmp") => {
                    let a = slot(bend, 6).unwrap();
                    let b = slot(bend, 14).unwrap();
                    let v = slot(bend, 15).unwrap();

                    do_tail_call(context, &mut current_frame, &mut body, &mut bend, a, b, v);
                }
                tas!(b"jmf") => {
                    let a = slot(bend, 6).unwrap();
                    let b = slot(bend, 14).unwrap();
                    let v = slot(bend, 30).unwrap();
                    let u = slot(bend, 62).unwrap().as_direct().unwrap().data() as usize;
                    let mut n = slot(bend, 63).unwrap();

                    if let Some(JetEntry(jet)) = context.cg_context.hot_hamt.lookup(&mut context.stack, &mut n) {
                        let subject = register_get(current_frame, u);
                        let jet_result = jet(context, subject);
                        match jet_result {
                            Ok(result) => {
                                do_return(
                                    context,
                                    &mut current_frame,
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
                                    &mut current_frame,
                                    &mut body,
                                    &mut bend,
                                    a,
                                    b,
                                    v,
                                );
                            }
                        }
                    } else {
                        do_tail_call(context, &mut current_frame, &mut body, &mut bend, a, b, v);
                    }
                }
                tas!(b"spy") => {
                    let e = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let p = slot(bend, 14).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(bend, 30).unwrap().as_direct().unwrap().data() as usize;
                    let mut t = slot(bend, 31).unwrap();

                    if let Some(cell) = context.scry_stack.cell() {
                        let scry_ref = register_get(current_frame, e);
                        let scry_path = register_get(current_frame, p);
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
                                    if unsafe { atom.as_noun().raw_equals(D(0)) } {
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
                                        cg_mean_push(stack, current_frame, hunk);
                                        break Err(ActualError(Error::ScryCrashed(D(0))));
                                    }
                                    Right(cell) => {
                                        register_set(current_frame, d, cell.tail());
                                        context.scry_stack = scry_stack;
                                        do_goto(
                                            &mut context.stack,
                                            current_frame,
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
                tas!(b"mer") => {
                    let k = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let u = slot(bend, 14).unwrap().as_direct().unwrap().data() as usize;
                    let f = slot(bend, 30).unwrap().as_direct().unwrap().data() as usize;
                    let d = slot(bend, 62).unwrap().as_direct().unwrap().data() as usize;
                    let mut i = slot(bend, 126).unwrap();
                    let mut m = slot(bend, 127).unwrap();

                    // let mut key = T(&mut context.stack, &[k_value, u_value, f_value]);
                    // XX use k (clue) later, with persistent caching implementation
                    let _k_value = register_get(current_frame, k);
                    let u_value = register_get(current_frame, u);
                    let f_value = register_get(current_frame, f);
                    let mut key = T(&mut context.stack, &[u_value, f_value]);

                    if let Some(value) = context.cache.lookup(&mut context.stack, &mut key) {
                        register_set(current_frame, d, value);
                        do_goto(
                            &mut context.stack,
                            current_frame,
                            &mut body,
                            &mut bend,
                            &mut i,
                        );
                    } else {
                        do_goto(
                            &mut context.stack,
                            current_frame,
                            &mut body,
                            &mut bend,
                            &mut m,
                        );
                    }
                }
                tas!(b"don") => {
                    let s = slot(bend, 6).unwrap().as_direct().unwrap().data() as usize;
                    let s_value = register_get(current_frame, s);
                    if let Some(ret_value) = do_return(
                        context,
                        &mut current_frame,
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
        let pois_sz = (*(frame)).pois_sz;
        let reg_ptr = (frame as *mut Noun).add(size_of::<Frame>() + pois_sz + local);
        *reg_ptr = value;
    }
}

fn register_get(frame: *const Frame, local: usize) -> Noun {
    unsafe {
        let pois_sz = (*(frame)).pois_sz;
        let reg_ptr = (frame as *mut Noun).add(size_of::<Frame>() + pois_sz + local);
        *reg_ptr
    }
}

fn poison_set(frame: *mut Frame, local: usize) {
    let index = local / 64;
    let offset = local % 64;
    let bitmap = unsafe { (frame as *mut u64).add(size_of::<Frame>() + index) };
    unsafe { *bitmap |= 1 << offset };
}

fn poison_get(frame: *const Frame, local: usize) -> bool {
    let index = local / 64;
    let offset = local % 64;
    let bitmap = unsafe { *(frame as *const u64).add(size_of::<Frame>() + index) };
    bitmap & (1 << offset) != 0
}

pub mod types {
    use std::ptr::copy_nonoverlapping;

    use crate::{
        hamt::Hamt,
        interpreter::Error,
        jets::util::slot,
        jets::Jet,
        jets::hot::Hot,
        mem::{NockStack, Preserve},
        noun::{Noun, T},
        trace::TraceStack,
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
        pub slow: Noun,
        pub pile: Pile,
        pub dest: usize,
        pub cont: Noun,
        pub pois_sz: usize, // length of poison vector
                            // poison: Vec<u64>,     // variable size
                            // registers: Vec<Noun>, // variable size
    }

    #[derive(Copy, Clone)]
    pub struct PileMem {
        pub long: Noun,
        pub bait: Noun,
        pub walt: Noun,
        pub wish: Noun,
        pub sire: usize,
        pub will: Hamt<Noun>,
        pub sans: usize,
    }

    #[derive(Copy, Clone)]
    pub struct Pile(pub *mut PileMem);
    impl Preserve for Pile {
        unsafe fn preserve(&mut self, stack: &mut NockStack) {
            if stack.is_in_frame(self.0) {
                let mut pile_mem = *(self.0);
                pile_mem.long.preserve(stack);
                pile_mem.bait.preserve(stack);
                pile_mem.walt.preserve(stack);
                pile_mem.wish.preserve(stack);
                pile_mem.will.preserve(stack);
                let dest_mem: *mut PileMem = stack.struct_alloc_in_previous_frame(1);
                copy_nonoverlapping(self.0, dest_mem, 1);
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
                    will: part_will(stack, slot(p, 62).unwrap()),
                    sans: slot(p, 254).unwrap().as_direct().unwrap().data() as usize,
                };
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
    use super::pop_frame;
    use super::register_get;
    use super::register_set;
    use super::Frame;
    use super::Pile;
    use super::{new_frame, Hill};

    /// +peek slot in line core is 4
    const PEEK_AXIS: u64 = 4;

    /// +poke slot in line core is 46
    const POKE_AXIS: u64 = 46;

    /// +rake slot in line core is 95
    const RAKE_AXIS: u64 = 95;

    pub type NounResult = Result<Noun, Error>;

    pub fn peek(context: &mut Context, subject: Noun, formula: Noun) -> Noun {
        let line = context.cg_context.line.unwrap();
        let pek = kick(context, line, D(PEEK_AXIS)).unwrap();
        let sam = T(&mut context.stack, &[subject, formula]);
        slam(context, pek, sam).unwrap()
    }

    pub fn poke(context: &mut Context, gist: Noun) -> Noun {
        let line = context.cg_context.line.unwrap();
        let pok = kick(context, line, D(POKE_AXIS)).unwrap();
        slam(context, pok, gist).unwrap()
    }

    pub fn tap(stack: &mut NockStack, map: Noun) -> NounResult {
        tap_in(stack, slot(map, 30)?, D(0))
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
        frame_ref: &mut *mut Frame,
        body: &mut Noun,
        bend: &mut Noun,
        mut a: Noun,
        mut b: Noun,
        mut v: Noun,
        d: usize,
        t: Noun,
    ) {
        unsafe {
            (**frame_ref).dest = d;
            (**frame_ref).cont = t;
        }

        let parent_frame = *frame_ref;

        let pile = context.cg_context.hill.0.lookup(&mut context.stack, &mut a).unwrap();

        unsafe {
            new_frame(context, frame_ref, pile, false);
        }

        let mut bait = unsafe { (*(pile.0)).bait };
        let mut walt = unsafe { (*(pile.0)).walt };

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
                    poison_set(*frame_ref, bait_i);
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

                register_set(*frame_ref, walt_i, register_get(parent_frame, v_i));
            }
        }

        {
            let will = unsafe { (*(pile.0)).will };
            let blob = will
                .lookup(&mut context.stack, &mut unsafe { (*(pile.0)).long })
                .unwrap();
            *body = slot(blob, 6).unwrap();
            *bend = slot(blob, 7).unwrap();
        }
    }

    pub fn do_tail_call(
        context: &mut Context,
        frame_ref: &mut *mut Frame,
        body: &mut Noun,
        bend: &mut Noun,
        mut a: Noun,
        mut b: Noun,
        mut v: Noun,
    ) {
        unsafe {
            let pile = context.cg_context.hill.0.lookup(&mut context.stack, &mut a).unwrap();
            new_frame(context, frame_ref, pile, true); // set up tail call frame

            let sans = (*(pile.0)).sans;
            let poison_size = (**frame_ref).pois_sz;

            let mut bait = (*(pile.0)).bait;
            let mut walt = (*(pile.0)).walt;

            let old_poison_sz = *(*frame_ref as *const usize).add(sans + poison_size);
            let old_poison_ptr = (*frame_ref as *const u64).add(sans + poison_size + 1);

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
                    poison_set(*frame_ref, bait_i as usize);
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
                let walt_i = slot(v, 2).unwrap().as_direct().unwrap().data();
                walt = slot(walt, 3).unwrap();
                // XX we should also store the old reg size and assert this doesn't read
                // past it
                register_set(
                    *frame_ref,
                    walt_i as usize,
                    *(old_reg_ptr.add(v_i as usize)),
                );
            }

            let will = (*(pile.0)).will;
            let blob = will
                .lookup(&mut context.stack, &mut (*(pile.0)).long)
                .unwrap();
            *body = slot(blob, 6).unwrap();
            *bend = slot(blob, 7).unwrap();
        }
    }

    pub fn do_goto(
        stack: &mut NockStack,
        current_frame: *mut Frame,
        body: &mut Noun,
        bend: &mut Noun,
        bile: &mut Noun,
    ) {
        let blob = unsafe {
            (*(*current_frame).pile.0)
                .will
                .lookup(stack, bile)
                .ok_or(Error::NonDeterministic(D(0)))
                .unwrap()
        };
        *body = slot(blob, 6).unwrap();
        *bend = slot(blob, 7).unwrap();
    }

    pub fn do_return(
        context: &mut Context,
        frame_ref: &mut *mut Frame,
        base_frame: *const u64,
        body: &mut Noun,
        bend: &mut Noun,
        mut ret_value: Noun,
    ) -> Option<Noun> {
        unsafe {
            // XX debug assertions

            context.preserve();
            context.stack.preserve(&mut ret_value);
            pop_frame(context, frame_ref);

            if context.stack.get_frame_pointer() == base_frame {
                return Some(ret_value);
            }

            register_set(*frame_ref, (**frame_ref).dest, ret_value);

            let will = (*(**frame_ref).pile.0).will;
            let blob = will
                .lookup(&mut context.stack, &mut (**frame_ref).cont)
                .unwrap();
            *body = slot(blob, 6).unwrap();
            *bend = slot(blob, 7).unwrap();
        }
        None
    }
}
