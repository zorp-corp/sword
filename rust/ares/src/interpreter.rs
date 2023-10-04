use crate::hamt::Hamt;
use crate::jets;
use crate::jets::cold::Cold;
use crate::jets::hot::Hot;
use crate::jets::nock::util::mook;
use crate::jets::warm::Warm;
use crate::mem::unifying_equality;
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Atom, Cell, IndirectAtom, Noun, Slots, D, T};
use ares_macros::tas;
use assert_no_alloc::assert_no_alloc;
use bitvec::prelude::{BitSlice, Lsb0};
use either::Either::*;

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
    Blocked(Noun),
    Error(Noun),
}

impl From<NockErr> for () {
    fn from(_: NockErr) -> Self {}
}

/** Interpret nock */
pub fn interpret(
    stack: &mut NockStack,
    newt: &mut Option<&mut Newt>, // For printing slogs; if None, print to stdout
    cold: &mut Cold,
    warm: &mut Warm,
    hot: Hot,
    mut subject: Noun,
    formula: Noun,
) -> Result<Noun, NockErr> {
    let mut res: Noun = D(0);
    let mut cache = Hamt::<Noun>::new();
    let virtual_frame = stack.get_frame_pointer();

    // Setup stack for Nock computation
    unsafe {
        stack.frame_push(1);
        // Bottom of mean stack
        *(stack.local_noun_pointer(0)) = D(0);
        *stack.push() = NockWork::Done;
    };

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
    let tone = assert_no_alloc(|| unsafe {
        push_formula(stack, formula, true)?;

        loop {
            let work: NockWork = *stack.top();
            match work {
                NockWork::Done => {
                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.preserve(cold);
                    stack.preserve(warm);
                    stack.frame_pop();
                    break Ok(res);
                }
                NockWork::Ret => {
                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.preserve(cold);
                    stack.preserve(warm);
                    stack.frame_pop();
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
                        break Err(NockErr::Error(D(1)));
                    }
                }
                NockWork::Work1(once) => {
                    res = once.noun;
                    stack.pop::<NockWork>();
                }
                NockWork::Work2(mut vale) => match vale.todo {
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
                        if let Some(jet) = warm.find_jet(stack, &mut vale.subject, &mut res) {
                            // a jet match
                            if let Ok(jet_res) = jet(stack, newt, vale.subject) {
                                // XX TODO: nondeterministic errors
                                res = jet_res;
                                stack.pop::<NockWork>();
                                continue;
                            }
                        };
                        if vale.tail {
                            stack.pop::<NockWork>();
                            subject = vale.subject;
                            push_formula(stack, res, true)?;
                        } else {
                            vale.todo = Todo2::RestoreSubject;
                            std::mem::swap(&mut vale.subject, &mut subject);
                            *stack.top() = NockWork::Work2(vale);
                            mean_frame_push(stack, 0);
                            *stack.push() = NockWork::Ret;
                            push_formula(stack, res, true)?;
                        }
                    }
                    Todo2::RestoreSubject => {
                        subject = vale.subject;
                        stack.pop::<NockWork>();
                    }
                },
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
                            break Err(NockErr::Error(D(2)));
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
                                break Err(NockErr::Error(D(3)));
                            }
                        } else {
                            // Test branch of Nock 6 must return a direct atom
                            break Err(NockErr::Error(D(4)));
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
                            subject = res;
                            push_formula(stack, pose.formula, true)?;
                        } else {
                            pose.todo = Todo7::RestoreSubject;
                            pose.subject = subject;
                            *stack.top() = NockWork::Work7(pose);
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
                            subject = T(stack, &[res, subject]);
                            stack.pop::<NockWork>();
                            push_formula(stack, pins.formula, true)?;
                        } else {
                            pins.todo = Todo8::RestoreSubject;
                            pins.pin = subject;
                            *stack.top() = NockWork::Work8(pins);
                            subject = T(stack, &[res, subject]);
                            push_formula(stack, pins.formula, false)?;
                        }
                    }
                    Todo8::RestoreSubject => {
                        subject = pins.pin;
                        stack.pop::<NockWork>();
                    }
                },
                NockWork::Work9(mut kale) => match kale.todo {
                    Todo9::ComputeCore => {
                        kale.todo = Todo9::ComputeResult;
                        *stack.top() = NockWork::Work9(kale);
                        push_formula(stack, kale.core, false)?;
                    }
                    Todo9::ComputeResult => {
                        if let Ok(mut formula) = res.slot_atom(kale.axis) {
                            if let Some(jet) = warm.find_jet(stack, &mut res, &mut formula) {
                                // a jet match
                                if let Ok(jet_res) = jet(stack, newt, res) {
                                    // XX TODO: nondeterministic errors
                                    res = jet_res;
                                    stack.pop::<NockWork>();
                                    continue;
                                };
                            };
                            if kale.tail {
                                stack.pop::<NockWork>();
                                subject = res;
                                push_formula(stack, formula, true)?;
                            } else {
                                kale.todo = Todo9::RestoreSubject;
                                kale.core = subject;
                                *stack.top() = NockWork::Work9(kale);
                                subject = res;
                                mean_frame_push(stack, 0);
                                *stack.push() = NockWork::Ret;
                                push_formula(stack, formula, true)?;
                            }
                        } else {
                            // Axis into core must be atom
                            break Err(NockErr::Error(D(5)));
                        }
                    }
                    Todo9::RestoreSubject => {
                        subject = kale.core;
                        stack.pop::<NockWork>();
                    }
                },
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
                            stack.pop::<NockWork>();
                        }
                    }
                }
                NockWork::Work11D(mut dint) => match dint.todo {
                    Todo11D::ComputeHint => {
                        if let Some(found) = match_hint_pre_hint(
                            stack, newt, cold, warm, hot, &cache, subject, dint.tag, dint.hint,
                            dint.body,
                        ) {
                            res = found;
                            stack.pop::<NockWork>();
                        } else {
                            dint.todo = Todo11D::ComputeResult;
                            *stack.top() = NockWork::Work11D(dint);
                            push_formula(stack, dint.hint, false)?;
                        }
                    }
                    Todo11D::ComputeResult => {
                        dint.todo = Todo11D::Done;
                        if let Some(found) = match_hint_pre_nock(
                            stack,
                            newt,
                            subject,
                            dint.tag,
                            Some(dint.hint),
                            dint.body,
                            Some(res),
                        ) {
                            res = found;
                            stack.pop::<NockWork>();
                        } else {
                            dint.todo = Todo11D::Done;
                            if dint.tail {
                                stack.pop::<NockWork>();
                            } else {
                                *stack.top() = NockWork::Work11D(dint);
                            }
                            push_formula(stack, dint.body, dint.tail)?;
                        }
                    }
                    Todo11D::Done => {
                        if let Some(found) = match_hint_post_nock(
                            stack,
                            &mut cache,
                            cold,
                            warm,
                            hot,
                            subject,
                            dint.tag,
                            Some(dint.hint),
                            dint.body,
                            res,
                        ) {
                            res = found;
                        }
                        stack.pop::<NockWork>();
                    }
                },
                NockWork::Work11S(mut sint) => match sint.todo {
                    Todo11S::ComputeResult => {
                        sint.todo = Todo11S::Done;
                        if let Some(found) = match_hint_pre_nock(
                            stack, newt, subject, sint.tag, None, sint.body, None,
                        ) {
                            res = found;
                            stack.pop::<NockWork>();
                        } else {
                            sint.todo = Todo11S::Done;
                            if sint.tail {
                                stack.pop::<NockWork>();
                            } else {
                                *stack.top() = NockWork::Work11S(sint);
                            }
                            push_formula(stack, sint.body, sint.tail)?;
                        }
                    }
                    Todo11S::Done => {
                        if let Some(found) = match_hint_post_nock(
                            stack, &mut cache, cold, warm, hot, subject, sint.tag, None, sint.body,
                            res,
                        ) {
                            res = found;
                        }
                        stack.pop::<NockWork>();
                    }
                },
            };
        }
    });

    match tone {
        Ok(res) => Ok(res),
        Err(_err) => Err(exit_early(stack, virtual_frame, &mut cache)),
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
                                    return Err(NockErr::Error(D(1)));
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
                                    return Err(NockErr::Error(D(21)));
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
                                    return Err(NockErr::Error(D(51)));
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
                                        return Err(NockErr::Error(D(62)));
                                    };
                                } else {
                                    // Argument to Nock 6 must be cell
                                    return Err(NockErr::Error(D(61)));
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
                                    return Err(NockErr::Error(D(71)));
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
                                    return Err(NockErr::Error(D(81)));
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
                                        return Err(NockErr::Error(D(92)));
                                    }
                                } else {
                                    // Argument to Nock 9 must be cell
                                    return Err(NockErr::Error(D(91)));
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
                                            return Err(NockErr::Error(D(103)));
                                        }
                                    } else {
                                        // Heah of argument to Nock 10 must be a cell
                                        return Err(NockErr::Error(D(102)));
                                    };
                                } else {
                                    // Argument to Nock 10 must be a cell
                                    return Err(NockErr::Error(D(101)));
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
                                                return Err(NockErr::Error(D(112)));
                                            }
                                        }
                                    };
                                } else {
                                    // Argument for Nock 11 must be cell
                                    return Err(NockErr::Error(D(111)));
                                };
                            }
                            _ => {
                                // Invalid formula opcode
                                return Err(NockErr::Error(D(0)));
                            }
                        }
                    } else {
                        // Formula opcode must be direct atom
                        return Err(NockErr::Error(D(0)));
                    }
                }
            }
        } else {
            // Bad formula: atoms are not formulas
            return Err(NockErr::Error(D(0)));
        }
    }
    Ok(())
}

fn exit_early(stack: &mut NockStack, virtual_frame: *const u64, cache: &mut Hamt<Noun>) -> NockErr {
    unsafe {
        let mut trace = *(stack.local_noun_pointer(0));
        while stack.get_frame_pointer() != virtual_frame {
            stack.preserve(&mut trace);
            stack.preserve(cache);
            stack.frame_pop();
        }
        NockErr::Error(trace)
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
#[allow(clippy::too_many_arguments)]
fn match_hint_pre_hint(
    stack: &mut NockStack,
    newt: &mut Option<&mut Newt>,
    cold: &mut Cold,
    warm: &mut Warm,
    hot: Hot,
    cache: &Hamt<Noun>,
    subject: Noun,
    tag: Atom,
    hint: Noun,
    body: Noun,
) -> Option<Noun> {
    //  XX: handle IndirectAtom tags
    match tag.direct()?.data() {
        // %sham hints are scaffolding until we have a real jet dashboard
        tas!(b"sham") => {
            if cfg!(feature = "sham_hints") {
                let jet_formula = hint.cell()?;
                // XX: what is the head here?
                let jet_name = jet_formula.tail();

                let jet = jets::get_jet(jet_name)?;
                if let Ok(mut jet_res) = jet(stack, newt, subject) {
                    // if in test mode, check that the jet returns the same result as the raw nock
                    if jets::get_jet_test_mode(jet_name) {
                        // Throw away trace because we'll regenerate it later, and this is in test mode
                        // so it's okay if it runs twice
                        interpret(stack, newt, cold, warm, hot, subject, body)
                            .ok()
                            .map(|mut nock_res| {
                                if unsafe { !unifying_equality(stack, &mut nock_res, &mut jet_res) }
                                {
                                    eprintln!(
                                        "\rJet {} failed, raw: {}, jetted: {}",
                                        jet_name, nock_res, jet_res
                                    );
                                    None
                                } else {
                                    Some(jet_res)
                                }
                            })
                            .unwrap()
                    } else {
                        Some(jet_res)
                    }
                } else {
                    // Print jet errors and punt to Nock
                    eprintln!("\rJet {} failed: ", jet_name);
                    None
                }
            } else {
                None
            }
        }
        tas!(b"memo") => {
            let mut key = Cell::new(stack, subject, body).as_noun();
            cache.lookup(stack, &mut key)
        }
        _ => None,
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
) -> Option<Noun> {
    //  XX: assert Some(res) <=> Some(hint)

    //  XX: handle IndirectAtom tags
    match tag.direct()?.data() {
        tas!(b"slog") => {
            let slog_cell = res?.cell()?;
            let pri = slog_cell.head().direct()?.data();
            let tank = slog_cell.tail();
            if let Some(not) = newt {
                not.slog(stack, pri, tank);
            } else {
                eprintln!("raw slog: {} {}", pri, tank);
            }
        }
        tas!(b"hand") | tas!(b"hunk") | tas!(b"lose") | tas!(b"mean") | tas!(b"spot") => {
            let noun = T(stack, &[tag.as_noun(), res?]);
            mean_push(stack, noun);
        }
        tas!(b"hela") => {
            // XX: should this be virtualized?
            //     pretty sure we should be bailing on error
            //     might need to switch return type to Result<Option<Noun>, NockErr>
            let stak = unsafe { *(stack.local_noun_pointer(0)) };
            let tone = Cell::new(stack, D(2), stak);

            if let Ok(toon) = mook(stack, newt, tone, true) {
                if unsafe { !toon.head().raw_equals(D(2)) } {
                    // Print jet error and punt to Nock
                    eprintln!("\r%hela failed: toon not %2");
                    return None;
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
            } else {
                // Print jet errors and punt to Nock
                eprintln!("\r%hela failed: mook error");
                return None;
            }
        }
        _ => {}
    }

    None
}

/** Match static and dynamic hints after the nock formula is evaluated */
#[allow(clippy::too_many_arguments)]
fn match_hint_post_nock(
    stack: &mut NockStack,
    cache: &mut Hamt<Noun>,
    cold: &mut Cold,
    warm: &mut Warm,
    hot: Hot,
    subject: Noun,
    tag: Atom,
    hint: Option<Noun>,
    body: Noun,
    res: Noun,
) -> Option<Noun> {
    //  XX: handle IndirectAtom tags
    match tag.direct()?.data() {
        tas!(b"memo") => {
            let mut key = Cell::new(stack, subject, body).as_noun();
            *cache = cache.insert(stack, &mut key, res);
        }
        tas!(b"fast") => {
            if let Some(hint) = hint {
                let clue = hint.slot(7).ok()?;
                let chum = hint.slot(2).ok()?;
                let parent_formula_op = clue.slot(12).ok()?.as_atom().ok()?.as_direct().ok()?;
                let parent_formula_ax = clue.slot(13).ok()?.as_atom().ok()?;
                if parent_formula_op.data() == 1 {
                    if parent_formula_ax.as_direct().ok()?.data() == 0 {
                        let changed = cold.register(stack, res, parent_formula_ax, chum).ok()?;
                        if changed {
                            *warm = Warm::init(stack, cold, hot);
                        }
                    } else {
                        return None;
                    }
                } else {
                    let changed = cold.register(stack, res, parent_formula_ax, chum).ok()?;
                    if changed {
                        *warm = Warm::init(stack, cold, hot);
                    }
                }
            }
        }
        tas!(b"hand") | tas!(b"hunk") | tas!(b"lose") | tas!(b"mean") | tas!(b"spot") => {
            mean_pop(stack);
        }
        _ => {}
    }

    None
}
