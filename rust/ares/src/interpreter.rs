use crate::hamt::Hamt;
use crate::jets;
use crate::jets::nock::util::mook;
use crate::jets::JetErr;
use crate::mem::unifying_equality;
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun;
use crate::noun::{tape, Atom, Cell, IndirectAtom, Noun, Slots, D, T};
use crate::serf::TERMINATOR;
use ares_macros::tas;
use assert_no_alloc::assert_no_alloc;
use bitvec::prelude::{BitSlice, Lsb0};
use either::Either::*;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use crate::assert_acyclic;
use assert_no_alloc::permit_alloc;

crate::gdb!();

#[derive(Copy, Clone)]
#[repr(u8)]
enum TodoCons {
    ComputeHead,
    ComputeTail,
    Cons,
}

#[derive(Copy, Clone)]
struct NockCons {
    todo: TodoCons,
    head: Noun,
    tail: Noun,
}

#[derive(Copy, Clone)]
struct Nock0 {
    axis: Atom,
}

#[derive(Copy, Clone)]
struct Nock1 {
    noun: Noun,
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum Todo2 {
    ComputeSubject,
    ComputeFormula,
    ComputeResult,
    RestoreSubject,
}

#[derive(Copy, Clone)]
struct Nock2 {
    todo: Todo2,
    subject: Noun,
    formula: Noun,
    tail: bool,
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum Todo3 {
    ComputeChild,
    ComputeType,
}

#[derive(Copy, Clone)]
struct Nock3 {
    todo: Todo3,
    child: Noun,
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum Todo4 {
    ComputeChild,
    Increment,
}

#[derive(Copy, Clone)]
struct Nock4 {
    todo: Todo4,
    child: Noun,
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum Todo5 {
    ComputeLeftChild,
    ComputeRightChild,
    TestEquals,
}

#[derive(Copy, Clone)]
struct Nock5 {
    todo: Todo5,
    left: Noun,
    right: Noun,
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum Todo6 {
    ComputeTest,
    ComputeBranch,
}

#[derive(Copy, Clone)]
struct Nock6 {
    todo: Todo6,
    test: Noun,
    zero: Noun,
    once: Noun,
    tail: bool,
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum Todo7 {
    ComputeSubject,
    ComputeResult,
    RestoreSubject,
}

#[derive(Copy, Clone)]
struct Nock7 {
    todo: Todo7,
    subject: Noun,
    formula: Noun,
    tail: bool,
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum Todo8 {
    ComputeSubject,
    ComputeResult,
    RestoreSubject,
}

#[derive(Copy, Clone)]
struct Nock8 {
    todo: Todo8,
    pin: Noun,
    formula: Noun,
    tail: bool,
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum Todo9 {
    ComputeCore,
    ComputeResult,
    RestoreSubject,
}

#[derive(Copy, Clone)]
struct Nock9 {
    todo: Todo9,
    axis: Atom,
    core: Noun,
    tail: bool,
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum Todo10 {
    ComputeTree,
    ComputePatch,
    Edit,
}

#[derive(Copy, Clone)]
struct Nock10 {
    todo: Todo10,
    axis: Atom,
    tree: Noun,
    patch: Noun,
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum Todo11D {
    ComputeHint,
    ComputeResult,
    Done,
}

#[derive(Copy, Clone)]
struct Nock11D {
    todo: Todo11D,
    tag: Atom,
    hint: Noun,
    body: Noun,
    tail: bool,
}

#[derive(Copy, Clone)]
enum Todo11S {
    ComputeResult,
    Done,
}

#[derive(Copy, Clone)]
struct Nock11S {
    todo: Todo11S,
    tag: Atom,
    body: Noun,
    tail: bool,
}

#[derive(Copy, Clone)]
enum NockWork {
    Done,
    Ret,
    WorkCons(NockCons),
    Work0(Nock0),
    Work1(Nock1),
    Work2(Nock2),
    Work3(Nock3),
    Work4(Nock4),
    Work5(Nock5),
    Work6(Nock6),
    Work7(Nock7),
    Work8(Nock8),
    Work9(Nock9),
    Work10(Nock10),
    Work11D(Nock11D),
    Work11S(Nock11S),
}

#[derive(Debug)]
pub enum NockErr {
    Deterministic,
    NonDeterministic,
}

#[derive(Debug)]
pub enum Tone {
    Blocked(Noun),
    Error(NockErr, Noun),
}

impl From<NockErr> for () {
    fn from(_: NockErr) -> Self {}
}

impl From<noun::Error> for NockErr {
    fn from(_: noun::Error) -> Self {
        NockErr::Deterministic
    }
}

impl From<JetErr> for NockErr {
    fn from(e: JetErr) -> Self {
        match e {
            JetErr::Deterministic => NockErr::Deterministic,
            JetErr::NonDeterministic => NockErr::NonDeterministic,
            JetErr::Punt => panic!("unhandled JetErr::Punt"),
        }
    }
}

/** Interpret nock */
pub fn interpret(
    stack: &mut NockStack,
    newt: &mut Option<&mut Newt>, // For printing slogs; if None, print to stdout
    mut subject: Noun,
    formula: Noun,
) -> Result<Noun, Tone> {
    let terminator = Arc::clone(&TERMINATOR);
    let mut res: Noun = D(0);
    let mut cache = Hamt::<Noun>::new();
    let virtual_frame = stack.get_frame_pointer();

    // Setup stack for Nock computation
    unsafe {
        // eprintln!("\rserf: interpreter start pre-push:");
        // eprintln!("\rserf: NockStack frame pointer = {:p}", stack.frame_pointer);
        // eprintln!("\rserf: NockStack stack pointer = {:p}", stack.stack_pointer);
        // eprintln!("\rserf: NockStack alloc pointer = {:p}", stack.alloc_pointer);
        stack.frame_push(1);
        // eprintln!("\rserf: interpreter start post-push:");
        // eprintln!("\rserf: NockStack frame pointer = {:p}", stack.frame_pointer);
        // eprintln!("\rserf: NockStack stack pointer = {:p}", stack.stack_pointer);
        // eprintln!("\rserf: NockStack alloc pointer = {:p}", stack.alloc_pointer);
        // Bottom of mean stack
        *(stack.local_noun_pointer(0)) = D(0);
        *stack.push() = NockWork::Done;
    };

    let mut last_work = NockWork::Done;

    // DO NOT REMOVE THIS ASSERTION
    //
    // If you need to allocate for debugging, wrap the debugging code in
    //
    // ```
    // permit_alloc(|| {
    //   your.code.goes.here()
    // })
    // ```
    //
    // (See https://docs.rs/assert_no_alloc/latest/assert_no_alloc/#advanced-use)
    let nock = assert_no_alloc(|| unsafe {
        push_formula(stack, formula, true)?;

        loop {
            let work: NockWork = *stack.top();
            last_work = work;
            match work {
                NockWork::Done => {
                    permit_alloc(|| { assert_acyclic!(subject); });
                    stack.preserve(&mut cache);
                    permit_alloc(|| { assert_acyclic!(subject); });
                    stack.assert_no_junior_pointers(res);
                    stack.preserve(&mut res);
                    stack.assert_no_junior_pointers(res);
                    permit_alloc(|| { assert_acyclic!(subject); });
                    // eprintln!("\rserf: interpreter done pre-pop:");
                    // eprintln!("\rserf: NockStack frame pointer = {:p}", stack.frame_pointer);
                    // eprintln!("\rserf: NockStack stack pointer = {:p}", stack.stack_pointer);
                    // eprintln!("\rserf: NockStack alloc pointer = {:p}", stack.alloc_pointer);
                    stack.frame_pop();
                    // eprintln!("\rserf: interpreter done post-pop:");
                    // eprintln!("\rserf: NockStack frame pointer = {:p}", stack.frame_pointer);
                    // eprintln!("\rserf: NockStack stack pointer = {:p}", stack.stack_pointer);
                    // eprintln!("\rserf: NockStack alloc pointer = {:p}", stack.alloc_pointer);
                    break Ok(res);
                }
                NockWork::Ret => {
                    permit_alloc(|| { assert_acyclic!(subject); });
                    stack.preserve(&mut cache);
                    permit_alloc(|| { assert_acyclic!(subject); });
                    stack.assert_no_junior_pointers(res);
                    stack.preserve(&mut res);
                    stack.assert_no_junior_pointers(res);
                    permit_alloc(|| { assert_acyclic!(subject); });
                    // eprintln!("\rserf: interpreter ret pre-pop:");
                    // eprintln!("\rserf: NockStack frame pointer = {:p}", stack.frame_pointer);
                    // eprintln!("\rserf: NockStack stack pointer = {:p}", stack.stack_pointer);
                    // eprintln!("\rserf: NockStack alloc pointer = {:p}", stack.alloc_pointer);
                    stack.frame_pop();
                    // eprintln!("\rserf: interpreter ret post-pop:");
                    // eprintln!("\rserf: NockStack frame pointer = {:p}", stack.frame_pointer);
                    // eprintln!("\rserf: NockStack stack pointer = {:p}", stack.stack_pointer);
                    // eprintln!("\rserf: NockStack alloc pointer = {:p}", stack.alloc_pointer);
                }
                NockWork::WorkCons(mut cons) => match cons.todo {
                    TodoCons::ComputeHead => {
                        cons.todo = TodoCons::ComputeTail;
                        *stack.top() = NockWork::WorkCons(cons);
                        push_formula(stack, cons.head, false)?;
                    }
                    TodoCons::ComputeTail => {
                        cons.todo = TodoCons::Cons;
                        cons.head = res;
                        *stack.top() = NockWork::WorkCons(cons);
                        push_formula(stack, cons.tail, false)?;
                    }
                    TodoCons::Cons => {
                        res = T(stack, &[cons.head, res]);
                        stack.pop::<NockWork>();
                    }
                },
                NockWork::Work0(zero) => {
                    if let Ok(noun) = subject.slot_atom(zero.axis) {
                        res = noun;
                        stack.pop::<NockWork>();
                    } else {
                        // Axis invalid for input Noun
                        break Err(NockErr::Deterministic);
                    }
                }
                NockWork::Work1(once) => {
                    res = once.noun;
                    stack.pop::<NockWork>();
                }
                NockWork::Work2(mut vale) => {
                    if (*terminator).load(Ordering::Relaxed) {
                        break Err(NockErr::NonDeterministic);
                    }

                    match vale.todo {
                        Todo2::ComputeSubject => {
                            vale.todo = Todo2::ComputeFormula;
                            *stack.top() = NockWork::Work2(vale);
                            push_formula(stack, vale.subject, false)?;
                        }
                        Todo2::ComputeFormula => {
                            vale.todo = Todo2::ComputeResult;
                            vale.subject = res;
                            *stack.top() = NockWork::Work2(vale);
                            push_formula(stack, vale.formula, false)?;
                        }
                        Todo2::ComputeResult => {
                            if vale.tail {
                                stack.pop::<NockWork>();
                                subject = vale.subject;
                                push_formula(stack, res, true)?;
                            } else {
                                vale.todo = Todo2::RestoreSubject;
                                std::mem::swap(&mut vale.subject, &mut subject);
                                *stack.top() = NockWork::Work2(vale);
                                // eprintln!("\rserf: interpreter 2 pre-push:");
                                // eprintln!("\rserf: NockStack frame pointer = {:p}", stack.frame_pointer);
                                // eprintln!("\rserf: NockStack stack pointer = {:p}", stack.stack_pointer);
                                // eprintln!("\rserf: NockStack alloc pointer = {:p}", stack.alloc_pointer);
                                mean_frame_push(stack, 0);
                                *stack.push() = NockWork::Ret;
                                // eprintln!("\rserf: interpreter 2 post-push:");
                                // eprintln!("\rserf: NockStack frame pointer = {:p}", stack.frame_pointer);
                                // eprintln!("\rserf: NockStack stack pointer = {:p}", stack.stack_pointer);
                                // eprintln!("\rserf: NockStack alloc pointer = {:p}", stack.alloc_pointer);
                                push_formula(stack, res, true)?;
                            }
                        }
                        Todo2::RestoreSubject => {
                            subject = vale.subject;
                            stack.pop::<NockWork>();
                        }
                    }
                }
                NockWork::Work3(mut thee) => match thee.todo {
                    Todo3::ComputeChild => {
                        thee.todo = Todo3::ComputeType;
                        *stack.top() = NockWork::Work3(thee);
                        push_formula(stack, thee.child, false)?;
                    }
                    Todo3::ComputeType => {
                        res = if res.is_cell() { D(0) } else { D(1) };
                        stack.pop::<NockWork>();
                    }
                },
                NockWork::Work4(mut four) => match four.todo {
                    Todo4::ComputeChild => {
                        four.todo = Todo4::Increment;
                        *stack.top() = NockWork::Work4(four);
                        push_formula(stack, four.child, false)?;
                    }
                    Todo4::Increment => {
                        if let Ok(atom) = res.as_atom() {
                            res = inc(stack, atom).as_noun();
                            stack.pop::<NockWork>();
                        } else {
                            // Cannot increment (Nock 4) a cell
                            break Err(NockErr::Deterministic);
                        }
                    }
                },
                NockWork::Work5(mut five) => match five.todo {
                    Todo5::ComputeLeftChild => {
                        five.todo = Todo5::ComputeRightChild;
                        *stack.top() = NockWork::Work5(five);
                        push_formula(stack, five.left, false)?;
                    }
                    Todo5::ComputeRightChild => {
                        five.todo = Todo5::TestEquals;
                        five.left = res;
                        *stack.top() = NockWork::Work5(five);
                        push_formula(stack, five.right, false)?;
                    }
                    Todo5::TestEquals => {
                        let saved_value_ptr = &mut five.left;
                        res = if unifying_equality(stack, &mut res, saved_value_ptr) {
                            D(0)
                        } else {
                            D(1)
                        };
                        stack.pop::<NockWork>();
                    }
                },
                NockWork::Work6(mut cond) => match cond.todo {
                    Todo6::ComputeTest => {
                        cond.todo = Todo6::ComputeBranch;
                        *stack.top() = NockWork::Work6(cond);
                        push_formula(stack, cond.test, false)?;
                    }
                    Todo6::ComputeBranch => {
                        stack.pop::<NockWork>();
                        if let Left(direct) = res.as_either_direct_allocated() {
                            if direct.data() == 0 {
                                push_formula(stack, cond.zero, cond.tail)?;
                            } else if direct.data() == 1 {
                                push_formula(stack, cond.once, cond.tail)?;
                            } else {
                                // Test branch of Nock 6 must return 0 or 1
                                break Err(NockErr::Deterministic);
                            }
                        } else {
                            // Test branch of Nock 6 must return a direct atom
                            break Err(NockErr::Deterministic);
                        }
                    }
                },
                NockWork::Work7(mut pose) => match pose.todo {
                    Todo7::ComputeSubject => {
                        pose.todo = Todo7::ComputeResult;
                        *stack.top() = NockWork::Work7(pose);
                        push_formula(stack, pose.subject, false)?;
                    }
                    Todo7::ComputeResult => {
                        if pose.tail {
                            stack.pop::<NockWork>();
                            stack.assert_no_junior_pointers(res);
                            subject = res;
                            push_formula(stack, pose.formula, true)?;
                        } else {
                            pose.todo = Todo7::RestoreSubject;
                            pose.subject = subject;
                            *stack.top() = NockWork::Work7(pose);
                            stack.assert_no_junior_pointers(res);
                            subject = res;
                            push_formula(stack, pose.formula, false)?;
                        }
                    }
                    Todo7::RestoreSubject => {
                        subject = pose.subject;
                        stack.pop::<NockWork>();
                    }
                },
                NockWork::Work8(mut pins) => match pins.todo {
                    Todo8::ComputeSubject => {
                        pins.todo = Todo8::ComputeResult;
                        *stack.top() = NockWork::Work8(pins);
                        push_formula(stack, pins.pin, false)?;
                    }
                    Todo8::ComputeResult => {
                        if pins.tail {
                            stack.assert_no_junior_pointers(res);
                            subject = T(stack, &[res, subject]);
                            stack.pop::<NockWork>();
                            push_formula(stack, pins.formula, true)?;
                        } else {
                            pins.todo = Todo8::RestoreSubject;
                            pins.pin = subject;
                            *stack.top() = NockWork::Work8(pins);
                            stack.assert_no_junior_pointers(res);
                            subject = T(stack, &[res, subject]);
                            push_formula(stack, pins.formula, false)?;
                        }
                    }
                    Todo8::RestoreSubject => {
                        subject = pins.pin;
                        stack.pop::<NockWork>();
                    }
                },
                NockWork::Work9(mut kale) => {
                    if (*terminator).load(Ordering::Relaxed) {
                        break Err(NockErr::NonDeterministic);
                    }

                    match kale.todo {
                        Todo9::ComputeCore => {
                            kale.todo = Todo9::ComputeResult;
                            *stack.top() = NockWork::Work9(kale);
                            push_formula(stack, kale.core, false)?;
                        }
                        Todo9::ComputeResult => {
                            if let Ok(formula) = res.slot_atom(kale.axis) {
                                if kale.tail {
                                    stack.pop::<NockWork>();
                                    subject = res;
                                    push_formula(stack, formula, true)?;
                                } else {
                                    kale.todo = Todo9::RestoreSubject;
                                    kale.core = subject;
                                    *stack.top() = NockWork::Work9(kale);
                                    stack.assert_no_junior_pointers(res);
                                    subject = res;
                                    // eprintln!("\rserf: interpreter 9 pre-push:");
                                    // eprintln!("\rserf: NockStack frame pointer = {:p}", stack.frame_pointer);
                                    // eprintln!("\rserf: NockStack stack pointer = {:p}", stack.stack_pointer);
                                    // eprintln!("\rserf: NockStack alloc pointer = {:p}", stack.alloc_pointer);
                                    mean_frame_push(stack, 0);
                                    *stack.push() = NockWork::Ret;
                                    // eprintln!("\rserf: interpreter 9 post-push:");
                                    // eprintln!("\rserf: NockStack frame pointer = {:p}", stack.frame_pointer);
                                    // eprintln!("\rserf: NockStack stack pointer = {:p}", stack.stack_pointer);
                                    // eprintln!("\rserf: NockStack alloc pointer = {:p}", stack.alloc_pointer);
                                    push_formula(stack, formula, true)?;
                                }
                            } else {
                                // Axis into core must be atom
                                break Err(NockErr::Deterministic);
                            }
                        }
                        Todo9::RestoreSubject => {
                            subject = kale.core;
                            stack.assert_no_junior_pointers(subject);
                            subject.assert_no_forwarding_pointers();
                            stack.pop::<NockWork>();
                        }
                    }
                }
                NockWork::Work10(mut diet) => {
                    match diet.todo {
                        Todo10::ComputeTree => {
                            diet.todo = Todo10::ComputePatch; // should we compute patch then tree?
                            *stack.top() = NockWork::Work10(diet);
                            push_formula(stack, diet.tree, false)?;
                        }
                        Todo10::ComputePatch => {
                            diet.todo = Todo10::Edit;
                            diet.tree = res;
                            *stack.top() = NockWork::Work10(diet);
                            push_formula(stack, diet.patch, false)?;
                        }
                        Todo10::Edit => {
                            res = edit(stack, diet.axis.as_bitslice(), res, diet.tree);
                            stack.assert_no_junior_pointers(res);
                            stack.pop::<NockWork>();
                        }
                    }
                }
                NockWork::Work11D(mut dint) => match dint.todo {
                    Todo11D::ComputeHint => {
                        match match_hint_pre_hint(
                            stack, newt, &cache, subject, dint.tag, dint.hint, dint.body,
                        ) {
                            Ok(Some(found)) => {
                                res = found;
                                stack.assert_no_junior_pointers(res);
                                stack.pop::<NockWork>();
                            }
                            Ok(None) => {
                                dint.todo = Todo11D::ComputeResult;
                                *stack.top() = NockWork::Work11D(dint);
                                push_formula(stack, dint.hint, false)?;
                            }
                            Err(err) => {
                                break Err(err);
                            }
                        }
                    }
                    Todo11D::ComputeResult => {
                        match match_hint_pre_nock(
                            stack,
                            newt,
                            subject,
                            dint.tag,
                            Some(dint.hint),
                            dint.body,
                            Some(res),
                        ) {
                            Ok(Some(found)) => {
                                res = found;
                                stack.assert_no_junior_pointers(res);
                                stack.pop::<NockWork>();
                            }
                            Ok(None) => {
                                dint.todo = Todo11D::Done;
                                if dint.tail {
                                    stack.pop::<NockWork>();
                                } else {
                                    *stack.top() = NockWork::Work11D(dint);
                                }
                                push_formula(stack, dint.body, dint.tail)?;
                            }
                            Err(err) => {
                                break Err(err);
                            }
                        }
                    }
                    Todo11D::Done => {
                        if let Some(found) = match_hint_post_nock(
                            stack,
                            &mut cache,
                            subject,
                            dint.tag,
                            Some(dint.hint),
                            dint.body,
                            res,
                        ) {
                            res = found;
                            stack.assert_no_junior_pointers(res);
                        }
                        stack.pop::<NockWork>();
                    }
                },
                NockWork::Work11S(mut sint) => match sint.todo {
                    Todo11S::ComputeResult => {
                        match match_hint_pre_nock(
                            stack, newt, subject, sint.tag, None, sint.body, None,
                        ) {
                            Ok(Some(found)) => {
                                res = found;
                                stack.pop::<NockWork>();
                            }
                            Ok(None) => {
                                sint.todo = Todo11S::Done;
                                if sint.tail {
                                    stack.pop::<NockWork>();
                                } else {
                                    *stack.top() = NockWork::Work11S(sint);
                                }
                                push_formula(stack, sint.body, sint.tail)?;
                            }
                            Err(err) => {
                                break Err(err);
                            }
                        }
                    }
                    Todo11S::Done => {
                        if let Some(found) = match_hint_post_nock(
                            stack, &mut cache, subject, sint.tag, None, sint.body, res,
                        ) {
                            res = found;
                        }
                        stack.pop::<NockWork>();
                    }
                },
            };
        }
    });

    match nock {
        Ok(res) => Ok(res),
        Err(err) => Err(exit_early(stack, &mut cache, virtual_frame, err)),
    }
}

fn push_formula(stack: &mut NockStack, formula: Noun, tail: bool) -> Result<(), NockErr> {
    unsafe {
        if let Ok(formula_cell) = formula.as_cell() {
            // Formula
            match formula_cell.head().as_either_atom_cell() {
                Right(_cell) => {
                    *stack.push() = NockWork::WorkCons(NockCons {
                        todo: TodoCons::ComputeHead,
                        head: formula_cell.head(),
                        tail: formula_cell.tail(),
                    });
                }
                Left(atom) => {
                    if let Ok(direct) = atom.as_direct() {
                        match direct.data() {
                            0 => {
                                if let Ok(axis_atom) = formula_cell.tail().as_atom() {
                                    *stack.push() = NockWork::Work0(Nock0 { axis: axis_atom });
                                } else {
                                    // Axis for Nock 0 must be an atom
                                    return Err(NockErr::Deterministic);
                                }
                            }
                            1 => {
                                *stack.push() = NockWork::Work1(Nock1 {
                                    noun: formula_cell.tail(),
                                });
                            }
                            2 => {
                                if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                    *stack.push() = NockWork::Work2(Nock2 {
                                        todo: Todo2::ComputeSubject,
                                        subject: arg_cell.head(),
                                        formula: arg_cell.tail(),
                                        tail,
                                    });
                                } else {
                                    // Argument to Nock 2 must be cell
                                    return Err(NockErr::Deterministic);
                                };
                            }
                            3 => {
                                *stack.push() = NockWork::Work3(Nock3 {
                                    todo: Todo3::ComputeChild,
                                    child: formula_cell.tail(),
                                });
                            }
                            4 => {
                                *stack.push() = NockWork::Work4(Nock4 {
                                    todo: Todo4::ComputeChild,
                                    child: formula_cell.tail(),
                                });
                            }
                            5 => {
                                if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                    *stack.push() = NockWork::Work5(Nock5 {
                                        todo: Todo5::ComputeLeftChild,
                                        left: arg_cell.head(),
                                        right: arg_cell.tail(),
                                    });
                                } else {
                                    // Argument to Nock 5 must be cell
                                    return Err(NockErr::Deterministic);
                                };
                            }
                            6 => {
                                if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                    if let Ok(branch_cell) = arg_cell.tail().as_cell() {
                                        *stack.push() = NockWork::Work6(Nock6 {
                                            todo: Todo6::ComputeTest,
                                            test: arg_cell.head(),
                                            zero: branch_cell.head(),
                                            once: branch_cell.tail(),
                                            tail,
                                        });
                                    } else {
                                        // Argument tail to Nock 6 must be cell
                                        return Err(NockErr::Deterministic);
                                    };
                                } else {
                                    // Argument to Nock 6 must be cell
                                    return Err(NockErr::Deterministic);
                                }
                            }
                            7 => {
                                if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                    *stack.push() = NockWork::Work7(Nock7 {
                                        todo: Todo7::ComputeSubject,
                                        subject: arg_cell.head(),
                                        formula: arg_cell.tail(),
                                        tail,
                                    });
                                } else {
                                    // Argument to Nock 7 must be cell
                                    return Err(NockErr::Deterministic);
                                };
                            }
                            8 => {
                                if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                    *stack.push() = NockWork::Work8(Nock8 {
                                        todo: Todo8::ComputeSubject,
                                        pin: arg_cell.head(),
                                        formula: arg_cell.tail(),
                                        tail,
                                    });
                                } else {
                                    // Argument to Nock 8 must be cell
                                    return Err(NockErr::Deterministic);
                                };
                            }
                            9 => {
                                if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                    if let Ok(axis_atom) = arg_cell.head().as_atom() {
                                        *stack.push() = NockWork::Work9(Nock9 {
                                            todo: Todo9::ComputeCore,
                                            axis: axis_atom,
                                            core: arg_cell.tail(),
                                            tail,
                                        });
                                    } else {
                                        // Axis for Nock 9 must be an atom
                                        return Err(NockErr::Deterministic);
                                    }
                                } else {
                                    // Argument to Nock 9 must be cell
                                    return Err(NockErr::Deterministic);
                                };
                            }
                            10 => {
                                if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                    if let Ok(patch_cell) = arg_cell.head().as_cell() {
                                        if let Ok(axis_atom) = patch_cell.head().as_atom() {
                                            *stack.push() = NockWork::Work10(Nock10 {
                                                todo: Todo10::ComputeTree,
                                                axis: axis_atom,
                                                tree: arg_cell.tail(),
                                                patch: patch_cell.tail(),
                                            });
                                        } else {
                                            // Axis for Nock 10 must be an atom
                                            return Err(NockErr::Deterministic);
                                        }
                                    } else {
                                        // Heah of argument to Nock 10 must be a cell
                                        return Err(NockErr::Deterministic);
                                    };
                                } else {
                                    // Argument to Nock 10 must be a cell
                                    return Err(NockErr::Deterministic);
                                };
                            }
                            11 => {
                                if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                    match arg_cell.head().as_either_atom_cell() {
                                        Left(tag_atom) => {
                                            *stack.push() = NockWork::Work11S(Nock11S {
                                                todo: Todo11S::ComputeResult,
                                                tag: tag_atom,
                                                body: arg_cell.tail(),
                                                tail: tail && is_hint_tail(tag_atom),
                                            });
                                        }
                                        Right(hint_cell) => {
                                            if let Ok(tag_atom) = hint_cell.head().as_atom() {
                                                *stack.push() = NockWork::Work11D(Nock11D {
                                                    todo: Todo11D::ComputeHint,
                                                    tag: tag_atom,
                                                    hint: hint_cell.tail(),
                                                    body: arg_cell.tail(),
                                                    tail: tail && is_hint_tail(tag_atom),
                                                });
                                            } else {
                                                // Hint tag must be an atom
                                                return Err(NockErr::Deterministic);
                                            }
                                        }
                                    };
                                } else {
                                    // Argument for Nock 11 must be cell
                                    return Err(NockErr::Deterministic);
                                };
                            }
                            _ => {
                                // Invalid formula opcode
                                return Err(NockErr::Deterministic);
                            }
                        }
                    } else {
                        // Formula opcode must be direct atom
                        return Err(NockErr::Deterministic);
                    }
                }
            }
        } else {
            // Bad formula: atoms are not formulas
            return Err(NockErr::Deterministic);
        }
    }
    Ok(())
}

pub fn exit_early(
    stack: &mut NockStack,
    cache: &mut Hamt<Noun>,
    virtual_frame: *const u64,
    error: NockErr,
) -> Tone {
    unsafe {
        let mut trace = *(stack.local_noun_pointer(0));
        // eprintln!("\rserf: interrupted:");
        // eprintln!("\rserf: NockStack frame pointer = {:p}", stack.frame_pointer);
        // eprintln!("\rserf: NockStack stack pointer = {:p}", stack.stack_pointer);
        // eprintln!("\rserf: NockStack alloc pointer = {:p}", stack.alloc_pointer);
        while stack.get_frame_pointer() != virtual_frame {
            stack.preserve(&mut trace);
            stack.preserve(cache);
            stack.frame_pop();
            // eprintln!("\rserf: popped:");
            // eprintln!("\rserf: NockStack frame pointer = {:p}", stack.frame_pointer);
            // eprintln!("\rserf: NockStack stack pointer = {:p}", stack.stack_pointer);
            // eprintln!("\rserf: NockStack alloc pointer = {:p}", stack.alloc_pointer);
        }
        Tone::Error(error, trace)
    }
}

/** Push frame onto NockStack while preserving the mean stack.
 */
fn mean_frame_push(stack: &mut NockStack, slots: usize) {
    unsafe {
        let trace = *(stack.local_noun_pointer(0));
        stack.frame_push(slots + 1);
        *(stack.local_noun_pointer(0)) = trace;
    }
}

/** Push onto the mean stack.
 */
fn mean_push(stack: &mut NockStack, noun: Noun) {
    unsafe {
        let cur_trace = *(stack.local_noun_pointer(0));
        let new_trace = T(stack, &[noun, cur_trace]);
        *(stack.local_noun_pointer(0)) = new_trace;
    }
}

/** Pop off of the mean stack.
 */
fn mean_pop(stack: &mut NockStack) {
    unsafe {
        *(stack.local_noun_pointer(0)) = (*(stack.local_noun_pointer(0)))
            .as_cell()
            .expect("serf: unexpected end of mean stack\r")
            .tail();
    }
}

fn edit(
    stack: &mut NockStack,
    edit_axis: &BitSlice<u64, Lsb0>,
    patch: Noun,
    mut tree: Noun,
) -> Noun {
    let mut res = patch;
    let mut dest: *mut Noun = &mut res;
    let mut cursor = edit_axis
        .last_one()
        .expect("0 is not allowed as an edit axis");
    loop {
        if cursor == 0 {
            unsafe {
                *dest = patch;
            }
            break;
        };
        if let Ok(tree_cell) = tree.as_cell() {
            cursor -= 1;
            if edit_axis[cursor] {
                unsafe {
                    let (cell, cellmem) = Cell::new_raw_mut(stack);
                    *dest = cell.as_noun();
                    (*cellmem).head = tree_cell.head();
                    dest = &mut ((*cellmem).tail);
                }
                tree = tree_cell.tail();
            } else {
                unsafe {
                    let (cell, cellmem) = Cell::new_raw_mut(stack);
                    *dest = cell.as_noun();
                    (*cellmem).tail = tree_cell.tail();
                    dest = &mut ((*cellmem).head);
                }
                tree = tree_cell.head();
            }
        } else {
            panic!("Invalid axis for edit");
        };
    }
    res
}

pub fn inc(stack: &mut NockStack, atom: Atom) -> Atom {
    match atom.as_either() {
        Left(direct) => Atom::new(stack, direct.data() + 1),
        Right(indirect) => {
            let indirect_slice = indirect.as_bitslice();
            match indirect_slice.first_zero() {
                None => {
                    // all ones, make an indirect one word bigger
                    let (new_indirect, new_slice) =
                        unsafe { IndirectAtom::new_raw_mut_bitslice(stack, indirect.size() + 1) };
                    new_slice.set(indirect_slice.len(), true);
                    new_indirect.as_atom()
                }
                Some(first_zero) => {
                    let (new_indirect, new_slice) =
                        unsafe { IndirectAtom::new_raw_mut_bitslice(stack, indirect.size()) };
                    new_slice.set(first_zero, true);
                    new_slice[first_zero + 1..]
                        .copy_from_bitslice(&indirect_slice[first_zero + 1..]);
                    new_indirect.as_atom()
                }
            }
        }
    }
}

fn is_hint_tail(tag: Atom) -> bool {
    //  XX: handle IndirectAtom tags
    match tag.direct() {
        #[allow(clippy::match_like_matches_macro)]
        Some(dtag) => match dtag.data() {
            tas!(b"fast") => false,
            tas!(b"memo") => false,
            _ => true,
        },
        None => true,
    }
}

/** Match dynamic hints before the hint formula is evaluated */
fn match_hint_pre_hint(
    stack: &mut NockStack,
    newt: &mut Option<&mut Newt>,
    cache: &Hamt<Noun>,
    subject: Noun,
    tag: Atom,
    hint: Noun,
    body: Noun,
) -> Result<Option<Noun>, NockErr> {
    //  XX: handle IndirectAtom tags
    match tag.as_direct()?.data() {
        // %sham hints are scaffolding until we have a real jet dashboard
        tas!(b"sham") => {
            let jet_formula = hint.as_cell()?;
            // XX: what is the head here?
            let jet_name = jet_formula.tail();

            if let Some(jet) = jets::get_jet(jet_name) {
                match jet(stack, newt, subject) {
                    Ok(mut jet_res) => {
                        //  XX: simplify this by moving jet test mode into the 11 code in interpret, or into its own function?
                        // if in test mode, check that the jet returns the same result as the raw nock
                        if jets::get_jet_test_mode(jet_name) {
                            //  XX: we throw away trace, which might matter for non-deterministic errors
                            //      maybe mook and slog it?
                            match interpret(stack, newt, subject, body) {
                                Ok(mut nock_res) => {
                                    if unsafe {
                                        !unifying_equality(stack, &mut nock_res, &mut jet_res)
                                    } {
                                        //  XX: need string interpolation without allocation, then delete eprintln
                                        // let tape = tape(stack, "jet mismatch in {}, raw: {}, jetted: {}", jet_name, nock_res, jet_res);
                                        eprintln!(
                                            "\rjet {} failed, raw: {:?}, jetted: {}",
                                            jet_name, nock_res, jet_res
                                        );
                                        let tape = tape(stack, "jet mismatch");
                                        let mean = T(stack, &[D(tas!(b"mean")), tape]);
                                        mean_push(stack, mean);
                                        Err(NockErr::Deterministic)
                                    } else {
                                        Ok(Some(nock_res))
                                    }
                                }
                                Err(Tone::Error(err, _)) => {
                                    //  XX: need string interpolation without allocation, then delete eprintln
                                    // let tape = tape(stack, "jet mismatch in {}, raw: {}, jetted: {}", jet_name, err, jet_res);
                                    eprintln!(
                                        "\rjet {} failed, raw: {:?}, jetted: {}",
                                        jet_name, err, jet_res
                                    );
                                    let tape = tape(stack, "jet mismatch");
                                    let mean = T(stack, &[D(tas!(b"mean")), tape]);
                                    mean_push(stack, mean);
                                    Err(err)
                                }
                                Err(Tone::Blocked(_)) => {
                                    panic!("jet test mode: no scry handling")
                                }
                            }
                        } else {
                            Ok(Some(jet_res))
                        }
                    }
                    Err(JetErr::Punt) => Ok(None),
                    Err(err) => {
                        //  XX: need string interpolation without allocation
                        // let tape = tape(stack, "{} jet error in {}", err, jet_name);
                        let tape = tape(stack, "jet error");
                        let mean = T(stack, &[D(tas!(b"mean")), tape]);
                        mean_push(stack, mean);
                        Err(err.into())
                    }
                }
            } else {
                Ok(None)
            }
        }
        tas!(b"memo") => {
            let mut key = Cell::new(stack, subject, body).as_noun();
            Ok(cache.lookup(stack, &mut key))
        }
        _ => Ok(None),
    }
}

/** Match static and dynamic hints before the nock formula is evaluated */
fn match_hint_pre_nock(
    stack: &mut NockStack,
    newt: &mut Option<&mut Newt>,
    _subject: Noun,
    tag: Atom,
    _hint: Option<Noun>,
    _body: Noun,
    res: Option<Noun>,
) -> Result<Option<Noun>, NockErr> {
    //  XX: assert Some(res) <=> Some(hint)

    //  XX: handle IndirectAtom tags
    match tag.as_direct()?.data() {
        tas!(b"slog") => {
            let slog_cell = res.ok_or(NockErr::Deterministic)?.as_cell()?;
            let pri = slog_cell.head().as_direct()?.data();
            let tank = slog_cell.tail();
            if let Some(not) = newt {
                not.slog(stack, pri, tank);
            } else {
                eprintln!("raw slog: {} {}", pri, tank);
            }
            Ok(None)
        }
        tas!(b"hand") | tas!(b"hunk") | tas!(b"lose") | tas!(b"mean") | tas!(b"spot") => {
            let terminator = Arc::clone(&TERMINATOR);
            if (*terminator).load(Ordering::Relaxed) {
                return Err(NockErr::NonDeterministic);
            }

            let noun = T(stack, &[tag.as_noun(), res.ok_or(NockErr::Deterministic)?]);
            mean_push(stack, noun);
            Ok(None)
        }
        //
        //      u3_serf_writ -> u3_serf_work -> _serf_work -> _serf_poke -> u3m_soft -> u3dc -> u3v_do -> u3v_wish -> +wish in Arvo
        //                                                                               |
        //                                                                               V
        //                                                                              mook
        //
        //  No +wish in toy Arvo; missing +slap and a ton of parsing functions needed by +ream
        //
        //      u3t_slog        = print on thing directly
        //      u3t_slog_trace  = print stack trace             = - convert tone to toon
        //                                                        - presume toon is [%2 tang]
        //                                                        - print each tank in tang one at at time using u3t_slog
        //      u3t_slog_hela   = print entire stack trace      = - weld stacks from all roads together
        //                                                        - call u3t_slog_trace on combined stack
        //      u3t_slog_nara   = print home road stack trace   = call u3t_slog_trace on home road stack
        //
        tas!(b"hela") => {
            // XX: should this be virtualized?
            //     pretty sure we should be bailing on error
            //     might need to switch return type to Result<Option<Noun>, NockErr>
            let stak = unsafe { *(stack.local_noun_pointer(0)) };
            let tone = Cell::new(stack, D(2), stak);

            match mook(stack, newt, tone, true) {
                Ok(toon) => {
                    if unsafe { !toon.head().raw_equals(D(2)) } {
                        let tape = tape(stack, "%hela failed: toon not %2");
                        let mean = T(stack, &[D(tas!(b"mean")), tape]);
                        mean_push(stack, mean);
                        return Err(NockErr::Deterministic);
                    }

                    let mut list = toon.tail();
                    loop {
                        if unsafe { list.raw_equals(D(0)) } {
                            break;
                        }

                        let cell = list.as_cell().unwrap();
                        if let Some(not) = newt {
                            // XX: %hela priority is configurable, but I'm not sure how
                            not.slog(stack, 0, cell.head());
                        } else {
                            eprintln!("raw slog: {} {}", 0, cell.head());
                        }

                        list = cell.tail();
                    }

                    Ok(None)
                }
                Err(err) => {
                    let tape = tape(stack, "%hela failed: mook error");
                    let mean = T(stack, &[D(tas!(b"mean")), tape]);
                    mean_push(stack, mean);
                    Err(err.into())
                }
            }
        }
        _ => Ok(None),
    }
}

/** Match static and dynamic hints after the nock formula is evaluated */
fn match_hint_post_nock(
    stack: &mut NockStack,
    cache: &mut Hamt<Noun>,
    subject: Noun,
    tag: Atom,
    _hint: Option<Noun>,
    body: Noun,
    res: Noun,
) -> Option<Noun> {
    //  XX: handle IndirectAtom tags
    match tag.direct()?.data() {
        tas!(b"memo") => {
            let mut key = Cell::new(stack, subject, body).as_noun();
            *cache = cache.insert(stack, &mut key, res);
        }
        tas!(b"hand") | tas!(b"hunk") | tas!(b"lose") | tas!(b"mean") | tas!(b"spot") => {
            mean_pop(stack);
        }
        _ => {}
    }

    None
}
