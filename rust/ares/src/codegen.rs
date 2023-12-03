// XX codegen errors are nondeterministic: we probably need a version of slot that throws a
// non-deterministic error, any other error caused by codegen being wrong should also be
// nondeterministic, or just a panic.
//
// XX ECA TODO: figure out a robust way to make codegen errors nondeterministic
// XX ECA TODO: turn hot state directly into a HAMT so we can grab jets from it easily in %jmf
// XX ECA TODO: preprocess blobs in part_will, rewrite cal to caf and jet to jmf if no jets and
// tracing is off
use ares_macros::tas;
use either::Either::{Left, Right};
use std::mem::size_of;
use std::ptr::copy_nonoverlapping;
use std::result::Result;

use crate::hamt::Hamt;
use crate::jets::JetErr;
// XX no, we have a completely different stack layout from the tree-walking interpreter. We can't
// borrow helper functions from it
use crate::interpreter::{mean_pop, mean_push, slow_pop, slow_push, Context, Error};
use crate::jets::util::slot;
use crate::mem::{NockStack, Preserve};
use crate::noun::{Noun, D, T};
use crate::trace::TraceStack;

// XX typedef for register
#[derive(Copy, Clone)]
pub struct PileMem {
    long: Noun,
    bait: Noun,
    walt: Noun,
    wish: Noun,
    sire: usize,
    will: Hamt<Noun>,
    sans: usize,
}

#[derive(Copy, Clone)]
pub struct Pile(*mut PileMem);
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
    fn from_noun(stack: &mut NockStack, p: Noun) -> Result<Pile, Error> {
        unsafe {
            let mem: *mut PileMem = stack.struct_alloc(1);
            *mem = PileMem {
                long: slot(p, 2)?,
                bait: slot(p, 14)?,
                walt: slot(p, 30)?,
                wish: slot(p, 62)?,
                sire: slot(p, 126)?.as_direct()?.data() as usize,
                will: util::part_will(stack, slot(p, 62)?)?,
                sans: slot(p, 254)?.as_direct()?.data() as usize,
            };
            Ok(Pile(mem))
        }
    }
}

/// First peeks or pokes the codegen core (line) to get codegen for the
/// subject and formula, then parses the successful results into a
/// (bell, hill) tuple.
fn cg_pull_peek(context: &mut Context, subject: Noun, formula: Noun) -> Result<Noun, Error> {
    // +peek or +poke dance
    context.line.ok_or(Error::Deterministic(D(0)))?;
    let pek = util::peek(context, subject, formula)?;
    let bell = if unsafe { pek.raw_equals(D(0)) } {
        let comp = util::comp(context, subject, formula);
        let line = util::poke(context, comp).expect("poke failed");
        context.line = Some(line);
        let good_peek = util::peek(context, subject, formula)?;
        let (bell, hill) = util::part_peek(&mut context.stack, good_peek)?;
        context.hill = hill;
        bell
    } else {
        let (bell, hill) = util::part_peek(&mut context.stack, pek)?;
        context.hill = hill;
        bell
    };

    Ok(bell)
}

/// Uses the `(bell, hill)` tuple returned from `cg_pull_peek()` to lookup the first
/// `pile` in the `hill` map.
/// XX percolate the changes to this through the code
fn cg_pull_pile(context: &mut Context, subject: Noun, formula: Noun) -> Result<Pile, Error> {
    let mut bell = cg_pull_peek(context, subject, formula).unwrap();
    let pile = context
        .hill
        .lookup(&mut context.stack, &mut bell)
        .ok_or(Error::Deterministic(D(0)))?;
    Ok(pile)
}

struct Frame {
    mean: Noun,
    traz: *mut *const TraceStack,
    slow: Noun,
    pile: Pile,
    dest: usize,
    cont: Noun,
    pois_sz: usize, // length of poison vector
                    // poison: Vec<u64>,     // variable size
                    // registers: Vec<Noun>, // variable size
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

/// Fetches or creates codegen code for the subject and formula, then
/// naively interprets it.
pub fn cg_interpret(context: &mut Context, subject: Noun, formula: Noun) -> Result<Noun, Error> {
    let base_frame = context.stack.get_frame_pointer();
    // Setup stack for codegen interpretation.
    // Stack frame layout: [mean trace slow pile dest cont poison registers]
    // XX update local_noun_pointer() calls with correct constants
    let mut virtual_frame: *mut Frame = std::ptr::null_mut();
    {
        let pile = cg_pull_pile(context, subject, formula)?;
        unsafe { new_frame(context, &mut virtual_frame, pile, false) };
    }

    // Load the initial subject to the sire register.
    {
        let sire = unsafe { (*(*virtual_frame).pile.0).sire };
        register_set(virtual_frame, sire, subject);
    }

    // Get the blob, body, and bend nouns from our pile.
    let mut body: Noun;
    let mut bend: Noun;
    {
        let will = unsafe { (*(*virtual_frame).pile.0).will };
        let blob = will
            .lookup(&mut context.stack, &mut unsafe {
                (*(*virtual_frame).pile.0).wish
            })
            .ok_or(Error::Deterministic(D(0)))?;
        body = slot(blob, 2)?;
        bend = slot(blob, 3)?;
    }

    loop {
        if !unsafe { body.raw_equals(D(0)) } {
            let pole = slot(body, 2)?;
            body = slot(body, 3)?;
            match slot(pole, 2)?.as_direct()?.data() {
                tas!(b"imm") => {
                    let local = slot(pole, 7)?.as_direct()?.data() as usize;
                    let value = slot(pole, 6)?;
                    register_set(virtual_frame, local, value);
                }
                tas!(b"mov") => {
                    let src = slot(pole, 6)?.as_direct()?.data() as usize;
                    let dst = slot(pole, 7)?.as_direct()?.data() as usize;
                    let value = register_get(virtual_frame, src);
                    register_set(virtual_frame, dst, value);
                }
                tas!(b"inc") => {}
                tas!(b"con") => {
                    let h = slot(pole, 6)?.as_direct()?.data() as usize;
                    let t = slot(pole, 14)?.as_direct()?.data() as usize;
                    let d = slot(pole, 15)?.as_direct()?.data() as usize;
                    let h_value = register_get(virtual_frame, h);
                    let t_value = register_get(virtual_frame, t);
                    let value = T(&mut context.stack, &[h_value, t_value]);
                    register_set(virtual_frame, d, value);
                }
                tas!(b"cop") => {}
                tas!(b"lop") => {}
                tas!(b"coc") => {}
                tas!(b"hed") => {
                    let s = slot(pole, 6)?.as_direct()?.data() as usize;
                    let d = slot(pole, 7)?.as_direct()?.data() as usize;
                    let s_value = register_get(virtual_frame, s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            poison_set(virtual_frame, s);
                        }
                        Right(cell) => {
                            register_set(virtual_frame, d, cell.head());
                        }
                    };
                }
                tas!(b"tal") => {
                    let s = slot(pole, 6)?.as_direct()?.data() as usize;
                    let d = slot(pole, 7)?.as_direct()?.data() as usize;
                    let s_value = register_get(virtual_frame, s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            poison_set(virtual_frame, s);
                        }
                        Right(cell) => {
                            register_set(virtual_frame, d, cell.tail());
                        }
                    };
                }
                tas!(b"hci") => {
                    let s = slot(pole, 6)?.as_direct()?.data() as usize;
                    let d = slot(pole, 7)?.as_direct()?.data() as usize;
                    let s_value = register_get(virtual_frame, s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            // XX crash
                        }
                        Right(cell) => {
                            register_set(virtual_frame, d, cell.head());
                        }
                    };
                }
                tas!(b"tci") => {
                    let s = slot(pole, 6)?.as_direct()?.data() as usize;
                    let d = slot(pole, 7)?.as_direct()?.data() as usize;
                    let s_value = register_get(virtual_frame, s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            // XX crash
                        }
                        Right(cell) => {
                            register_set(virtual_frame, d, cell.tail());
                        }
                    };
                }
                tas!(b"men") => {
                    // XX surely we need the static part as well
                    let s = slot(pole, 7)?.as_direct()?.data() as usize;
                    let s_value = register_get(virtual_frame, s);
                    mean_push(&mut context.stack, s_value);
                }
                tas!(b"man") => {
                    mean_pop(&mut context.stack);
                }
                tas!(b"slo") => {
                    let s = slot(pole, 3)?.as_direct()?.data() as usize;
                    let s_value = register_get(virtual_frame, s);
                    slow_push(&mut context.stack, s_value);
                }
                tas!(b"sld") => {
                    slow_pop(&mut context.stack);
                }
                tas!(b"hit") => {
                    let s = slot(pole, 3)?.as_direct()?.data() as usize;
                    let _s_value = register_get(virtual_frame, s);
                    // XX increment a profiling hit counter labeled with the noun in s
                }
                tas!(b"slg") => {
                    let s = slot(pole, 3)?.as_direct()?.data() as usize;
                    let clue = register_get(virtual_frame, s);
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
                    let k = slot(pole, 6)?.as_direct()?.data() as usize;
                    let u = slot(pole, 14)?.as_direct()?.data() as usize;
                    let f = slot(pole, 30)?.as_direct()?.data() as usize;
                    let r = slot(pole, 31)?.as_direct()?.data() as usize;

                    let k_value = register_get(virtual_frame, k);
                    let u_value = register_get(virtual_frame, u);
                    let f_value = register_get(virtual_frame, f);
                    let r_value = register_get(virtual_frame, r);
                    let mut key = T(&mut context.stack, &[k_value, u_value, f_value]);

                    context.cache = context.cache.insert(&mut context.stack, &mut key, r_value);
                }
                tas!(b"tim") => {
                    // XX push a timer onto the stack and start it
                }
                tas!(b"tom") => {
                    // XX pop a timer from the stack, stop it, and print elapsed
                }
                tas!(b"mem") => {
                    // XX print memory usage
                }
                tas!(b"pol") => {
                    let s = slot(pole, 6)?.as_direct()?.data() as usize;
                    let d = slot(pole, 7)?.as_direct()?.data() as usize;
                    if poison_get(virtual_frame, s) {
                        poison_set(virtual_frame, d);
                    }
                }
                tas!(b"poi") => {
                    let d = slot(pole, 3)?.as_direct()?.data() as usize;
                    poison_set(virtual_frame, d);
                }
                tas!(b"ipb") => {
                    let mut s = slot(pole, 3)?;
                    loop {
                        if unsafe { s.raw_equals(D(0)) } {
                            break;
                        } else {
                            let i = s.as_cell()?.head().as_direct()?.data() as usize;
                            if poison_get(virtual_frame, i) {
                                // XX crash
                            } else {
                                s = s.as_cell()?.tail();
                            }
                        }
                    }
                }
                _ => {
                    panic!("invalid pole instruction")
                }
            }
        } else {
            match slot(bend, 2)?.as_direct()?.data() {
                tas!(b"clq") => {
                    let s = slot(bend, 6)?.as_direct()?.data() as usize;
                    let s_value = register_get(virtual_frame, s);
                    match s_value.as_either_atom_cell() {
                        Left(_atom) => {
                            let mut o = slot(bend, 15)?;
                            let will = unsafe { (*(*virtual_frame).pile.0).will };
                            let blob = will
                                .lookup(&mut context.stack, &mut o)
                                .ok_or(Error::Deterministic(D(0)))?;
                            body = slot(blob, 2)?;
                            bend = slot(blob, 3)?;
                            continue;
                        }
                        Right(_cell) => {
                            let mut z = slot(bend, 14)?;
                            let will = unsafe { (*(*virtual_frame).pile.0).will };
                            let blob = will
                                .lookup(&mut context.stack, &mut z)
                                .ok_or(Error::Deterministic(D(0)))?;
                            body = slot(blob, 2)?;
                            bend = slot(blob, 3)?;
                            continue;
                        }
                    };
                }
                tas!(b"eqq") => {
                    let l = slot(bend, 6)?.as_direct()?.data() as usize;
                    let r = slot(bend, 14)?.as_direct()?.data() as usize;
                    let l_value = register_get(virtual_frame, l);
                    let r_value = register_get(virtual_frame, r);
                    if unsafe { l_value.raw_equals(r_value) } {
                        let mut z = slot(bend, 30)?;
                        let will = unsafe { (*(*virtual_frame).pile.0).will };
                        let blob = will
                            .lookup(&mut context.stack, &mut z)
                            .ok_or(Error::Deterministic(D(0)))?;
                        body = slot(blob, 2)?;
                        bend = slot(blob, 3)?;
                        continue;
                    } else {
                        let mut o = slot(bend, 31)?;
                        let will = unsafe { (*(*virtual_frame).pile.0).will };
                        let blob = will
                            .lookup(&mut context.stack, &mut o)
                            .ok_or(Error::Deterministic(D(0)))?;
                        body = slot(blob, 2)?;
                        bend = slot(blob, 3)?;
                        continue;
                    }
                }
                tas!(b"brn") => {
                    let s = slot(bend, 6)?.as_direct()?.data() as usize;
                    let s_value = register_get(virtual_frame, s);
                    if unsafe { s_value.raw_equals(D(0)) } {
                        let mut z = slot(bend, 14)?;
                        let will = unsafe { (*(*virtual_frame).pile.0).will };
                        let blob = will
                            .lookup(&mut context.stack, &mut z)
                            .ok_or(Error::Deterministic(D(0)))?;
                        body = slot(blob, 2)?;
                        bend = slot(blob, 3)?;
                        continue;
                    } else if unsafe { s_value.raw_equals(D(1)) } {
                        let mut o = slot(bend, 15)?;
                        let will = unsafe { (*(*virtual_frame).pile.0).will };
                        let blob = will
                            .lookup(&mut context.stack, &mut o)
                            .ok_or(Error::Deterministic(D(0)))?;
                        body = slot(blob, 2)?;
                        bend = slot(blob, 3)?;
                        continue;
                    } else {
                        // XX crash
                    }
                }
                tas!(b"hop") => {
                    let mut t = slot(bend, 3)?;
                    let will = unsafe { (*(*virtual_frame).pile.0).will };
                    let blob = will
                        .lookup(&mut context.stack, &mut t)
                        .ok_or(Error::Deterministic(D(0)))?;
                    body = slot(blob, 2)?;
                    bend = slot(blob, 3)?;
                    continue;
                }
                // tas!(b"hip") => {
                //     // XX set comefrom label to c and goto t
                // },
                tas!(b"lnk") => {
                    // evaluate f against u and put the result in d, then goto t
                    let u = slot(bend, 6)?.as_direct()?.data() as usize;
                    let f = slot(bend, 14)?.as_direct()?.data() as usize;
                    let d = slot(bend, 30)?.as_direct()?.data() as usize;
                    let t = slot(bend, 31)?;

                    let subject = register_get(virtual_frame, u);
                    let formula = register_get(virtual_frame, f);

                    unsafe {
                        (*virtual_frame).dest = d;
                        (*virtual_frame).cont = t;
                    }

                    {
                        let pile = cg_pull_pile(context, subject, formula)?;
                        unsafe {
                            new_frame(context, &mut virtual_frame, pile, false);
                        }
                    }

                    {
                        let sire = unsafe { (*(*virtual_frame).pile.0).sire };
                        register_set(virtual_frame, sire, subject);
                    }

                    {
                        let will = unsafe { (*(*virtual_frame).pile.0).will };
                        let blob = will
                            .lookup(&mut context.stack, &mut unsafe {
                                (*(*virtual_frame).pile.0).wish
                            })
                            .ok_or(Error::Deterministic(D(0)))?;
                        body = slot(blob, 2)?;
                        bend = slot(blob, 3)?;
                    }
                    continue;
                }
                tas!(b"cal") => {
                    // call the arm a with subject in registers v, poisons in b,
                    // result in d, and then goto t
                    let mut a = slot(bend, 6)?;
                    let mut b = slot(bend, 14)?;
                    let mut v = slot(bend, 30)?;
                    let d = slot(bend, 62)?.as_direct()?.data() as usize;
                    let t = slot(bend, 63)?;

                    unsafe {
                        (*virtual_frame).dest = d;
                        (*virtual_frame).cont = t;
                    }

                    let parent_frame = virtual_frame;

                    let pile = context
                        .hill
                        .lookup(&mut context.stack, &mut a)
                        .ok_or(Error::NonDeterministic(D(0)))?;

                    unsafe {
                        new_frame(context, &mut virtual_frame, pile, false);
                    }

                    let mut bait = unsafe { (*(pile.0)).bait };
                    let mut walt = unsafe { (*(pile.0)).walt };

                    loop {
                        unsafe {
                            if b.raw_equals(D(0)) {
                                if !bait.raw_equals(D(0)) {
                                    Err(Error::NonDeterministic(D(0)))?;
                                }
                                break;
                            }

                            let b_i = slot(b, 2)?.as_direct()?.data() as usize;
                            b = slot(b, 3)?;
                            let bait_i = slot(bait, 2)?.as_direct()?.data() as usize;
                            bait = slot(bait, 3)?;

                            if poison_get(parent_frame, b_i) {
                                poison_set(virtual_frame, bait_i);
                            }
                        }
                    }

                    loop {
                        unsafe {
                            if v.raw_equals(D(0)) {
                                if !walt.raw_equals(D(0)) {
                                    Err(Error::NonDeterministic(D(0)))?;
                                }
                                break;
                            }

                            let v_i = slot(v, 2)?.as_direct()?.data() as usize;
                            v = slot(v, 3)?;
                            let walt_i = slot(walt, 2)?.as_direct()?.data() as usize;
                            walt = slot(walt, 3)?;

                            register_set(virtual_frame, walt_i, register_get(parent_frame, v_i));
                        }
                    }

                    {
                        let will = unsafe { (*(pile.0)).will };
                        let blob = will
                            .lookup(&mut context.stack, &mut unsafe { (*(pile.0)).long })
                            .ok_or(Error::NonDeterministic(D(0)))?;
                        body = slot(blob, 2)?;
                        bend = slot(blob, 3)?;
                    }
                }
                tas!(b"caf") => {
                    let mut a = slot(bend, 6)?;
                    let mut b = slot(bend, 14)?;
                    let mut v = slot(bend, 30)?;
                    let d = slot(bend, 62)?.as_direct()?.data() as usize;
                    let mut t = slot(bend, 126)?;
                    let u = slot(bend, 254)?.as_direct()?.data() as usize;
                    let n = slot(bend, 255)?;
                    let mut path = slot(n, 2)?;
                    let mut axis = slot(n, 3)?.as_atom()?;

                    if let Some(jet) = context.hot.lookup(&mut context.stack, &mut path, axis) {
                        let subject = register_get(virtual_frame, u);
                        let jet_result = jet(context, subject);
                        match jet_result {
                            Ok(result) => {
                                register_set(virtual_frame, d, result);
                                let blob = unsafe { (*(*virtual_frame).pile.0).will.lookup(&mut context.stack, &mut t).ok_or(Error::NonDeterministic(D(0)))?};
                                body = slot(blob, 2)?;
                                bend = slot(blob, 3)?;
                            },
                            Err(JetErr::Fail(err)) => {
                                Err(err)?;
                            },
                            Err(JetErr::Punt) => {
                                // XX run as a normal %cal here
                                todo!()
                            }
                        }
                    }
                    // like call but with fast label
                    //
                    // XX we need to build a warm state that is just a HAMT-map of the hot state
                }
                tas!(b"lnt") => {
                    // evaluate f against u in tail position
                    let u = slot(bend, 6)?.as_direct()?.data() as usize;
                    let f = slot(bend, 7)?.as_direct()?.data() as usize;

                    let subject = register_get(virtual_frame, u);
                    let formula = register_get(virtual_frame, f);

                    {
                        let pile = cg_pull_pile(context, subject, formula)?;
                        unsafe {
                            new_frame(context, &mut virtual_frame, pile, true);
                        }
                    }

                    {
                        let sire = unsafe { (*(*virtual_frame).pile.0).sire };
                        register_set(virtual_frame, sire, subject);
                    }

                    {
                        let will = unsafe { (*(*virtual_frame).pile.0).will };
                        let blob = will
                            .lookup(&mut context.stack, &mut unsafe {
                                (*(*virtual_frame).pile.0).wish
                            })
                            .ok_or(Error::Deterministic(D(0)))?;
                        body = slot(blob, 2)?;
                        bend = slot(blob, 3)?;
                    }
                }
                tas!(b"jmp") => {
                    let mut a = slot(bend, 6)?;
                    let mut b = slot(bend, 14)?;
                    let mut v = slot(bend, 15)?;
                    unsafe {
                        let pile = context
                            .hill
                            .lookup(&mut context.stack, &mut a)
                            .ok_or(Error::NonDeterministic(D(0)))?;
                        new_frame(context, &mut virtual_frame, pile, true); // set up tail call frame

                        let sans = (*(pile.0)).sans;
                        let poison_size = (*virtual_frame).pois_sz;

                        let mut bait = (*(pile.0)).bait;
                        let mut walt = (*(pile.0)).walt;

                        let old_poison_sz =
                            *(virtual_frame as *const usize).add(sans + poison_size);
                        let old_poison_ptr =
                            (virtual_frame as *const u64).add(sans + poison_size + 1);

                        loop {
                            if b.raw_equals(D(0)) {
                                if !bait.raw_equals(D(0)) {
                                    Err(Error::NonDeterministic(D(0)))?;
                                };
                                break;
                            }

                            let b_i = slot(b, 2)?.as_direct()?.data();
                            b = slot(b, 3)?;
                            let bait_i = slot(bait, 2)?.as_direct()?.data();
                            bait = slot(bait, 3)?;

                            let b_i_offset = b_i / 64;
                            let b_i_bit = b_i % 64;
                            assert!((b_i_offset as usize) < old_poison_sz);

                            if *(old_poison_ptr.add(b_i_offset as usize)) & (1 << b_i_bit) != 0 {
                                poison_set(virtual_frame, bait_i as usize);
                            }
                        }

                        let old_reg_ptr = old_poison_ptr.add(old_poison_sz) as *const Noun;

                        loop {
                            if v.raw_equals(D(0)) {
                                if !walt.raw_equals(D(0)) {
                                    Err(Error::NonDeterministic(D(0)))?;
                                };
                                break;
                            };

                            let v_i = slot(v, 2)?.as_direct()?.data();
                            v = slot(v, 3)?;
                            let walt_i = slot(v, 2)?.as_direct()?.data();
                            walt = slot(walt, 3)?;
                            // XX we should also store the old reg size and assert this doesn't read
                            // past it
                            register_set(
                                virtual_frame,
                                walt_i as usize,
                                *(old_reg_ptr.add(v_i as usize)),
                            );
                        }

                        let will = (*(pile.0)).will;
                        let blob = will
                            .lookup(&mut context.stack, &mut (*(pile.0)).long)
                            .ok_or(Error::Deterministic(D(0)))?;
                        body = slot(blob, 2)?;
                        bend = slot(blob, 3)?;
                    }

                    // call the arm a with subject in registers u, poisons in b, in
                    // tail position
                }
                tas!(b"jmf") => {
                    // like jmp but with fast label
                    //
                    // XX see remark on caf
                }
                tas!(b"spy") => {
                    // scry with ref in e and path in p, put result in d, goto t
                }
                tas!(b"mer") => {
                    // check if triple [k u f] is in cache, put result in d if so
                    // and goto i, else goto m
                }
                tas!(b"don") => {
                    let s = slot(bend, 6)?.as_direct()?.data() as usize;
                    let mut s_value = register_get(virtual_frame, s);

                    unsafe {
                        // XX debug assertions

                        context.preserve();
                        context.stack.preserve(&mut s_value);
                        pop_frame(context, &mut virtual_frame);

                        if context.stack.get_frame_pointer() == base_frame {
                            break Ok(s_value);
                        }

                        register_set(virtual_frame, (*virtual_frame).dest, s_value);

                        let will = (*(*virtual_frame).pile.0).will;
                        let blob = will
                            .lookup(&mut context.stack, &mut (*virtual_frame).cont)
                            .ok_or(Error::Deterministic(D(0)))?;
                        body = slot(blob, 2)?;
                        bend = slot(blob, 3)?;
                    }
                    continue;
                }
                tas!(b"bom") => {
                    // crash
                }
                _ => {
                    panic!("invalid bend instruction");
                }
            }
        }
    }
}

// XX implement a generic local_set that all the more specific
// getters and setters call
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

pub mod util {
    use ares_macros::tas;

    use crate::{
        hamt::Hamt,
        interpreter::{Context, Error},
        jets::util::{kick, slam, slot},
        mem::NockStack,
        noun::{Noun, D, T},
    };

    use super::Pile;

    pub type NounResult = Result<Noun, Error>;

    pub fn peek(context: &mut Context, subject: Noun, formula: Noun) -> NounResult {
        // +peek slot in line core is 4
        let line = context.line.ok_or(Error::Deterministic(D(0)))?;
        let pek = kick(context, line, D(4))?;
        let sam = T(&mut context.stack, &[subject, formula]);
        slam(context, pek, sam).map_err(|_| Error::Deterministic(D(0)))
    }

    pub fn poke(context: &mut Context, gist: Noun) -> NounResult {
        // +poke slot in line core is 86
        let line = context.line.ok_or(Error::Deterministic(D(0)))?;
        let pok = kick(context, line, D(86))?;
        let sam = T(&mut context.stack, &[gist]);
        slam(context, pok, sam).map_err(|_| Error::Deterministic(D(0)))
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

    pub fn part_hill(stack: &mut NockStack, hill: Noun) -> Result<Hamt<Pile>, Error> {
        let mut kvs = tap(stack, hill)?;
        let mut hamt = Hamt::new();
        while !unsafe { kvs.raw_equals(D(0)) } {
            let c = kvs.as_cell()?;
            let kv = c.head();
            let mut bell = slot(kv, 2)?;
            let pile = Pile::from_noun(stack, slot(kv, 3)?)?;
            hamt = hamt.insert(stack, &mut bell, pile);
            kvs = c.tail();
        }
        Ok(hamt)
    }

    pub fn part_will(stack: &mut NockStack, will: Noun) -> Result<Hamt<Noun>, Error> {
        let mut kvs = tap(stack, will)?;
        let mut hamt = Hamt::new();
        while !unsafe { kvs.raw_equals(D(0)) } {
            let c = kvs.as_cell()?;
            let kv = c.head();
            let mut bile = slot(kv, 2)?;
            let blob = slot(kv, 3)?;
            hamt = hamt.insert(stack, &mut bile, blob);
            kvs = c.tail();
        }
        Ok(hamt)
    }

    pub fn part_peek(stack: &mut NockStack, peek: Noun) -> Result<(Noun, Hamt<Pile>), Error> {
        let bell = slot(peek, 6)?;
        let hall = part_hill(stack, slot(peek, 7)?)?;
        Ok((bell, hall))
    }

    pub fn comp(context: &mut Context, s: Noun, f: Noun) -> Noun {
        T(&mut context.stack, &[D(tas!(b"comp")), D(0), s, f])
    }
}
