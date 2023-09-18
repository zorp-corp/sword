use crate::hamt::Hamt;
use crate::jets;
use crate::jets::{Cold, Hot, Warm};
use crate::mem::unifying_equality;
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Atom, Cell, DirectAtom, IndirectAtom, Noun, D, T};
use ares_macros::tas;
use assert_no_alloc::{assert_no_alloc, permit_alloc};
use bitvec::prelude::{BitSlice, Lsb0};
use either::Either::*;
use colored::*;

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

use std::fmt;

impl fmt::Display for NockWork {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NockWork::Done => write!(f, "Done"),
            NockWork::Ret => write!(f, "Done"),
            NockWork::WorkCons(_) => write!(f, "Done"),
            NockWork::Work0(_) => write!(f, "Work0"),
            NockWork::Work1(_) => write!(f, "Work1"),
            NockWork::Work2(_) => write!(f, "Work2"),
            NockWork::Work3(_) => write!(f, "Work3"),
            NockWork::Work4(_) => write!(f, "Work4"),
            NockWork::Work5(_) => write!(f, "Work5"),
            NockWork::Work6(_) => write!(f, "Work6"),
            NockWork::Work7(_) => write!(f, "Work7"),
            NockWork::Work8(_) => write!(f, "Work8"),
            NockWork::Work9(_) => write!(f, "Work9"),
            NockWork::Work10(_) => write!(f, "Work10"),
            NockWork::Work11D(_) => write!(f, "Work11D"),
            NockWork::Work11S(_) => write!(f, "Work11S"),
        }
    }
}

/** Interpret nock */
pub fn interpret(
    stack: &mut NockStack,
    newt: &mut Option<&mut Newt>, // For printing slogs; if None, print to stdout
    mut subject: Noun,
    formula: Noun,
) -> Noun {
    let mut res = unsafe { DirectAtom::new_unchecked(0).as_atom().as_noun() };
    stack.frame_push(0);
    let mut cold = Cold::new();
    let mut hot = Hot::new(stack);
    let mut warm = Warm::new();
    let mut cache = Hamt::<Noun>::new();
    unsafe {
        *stack.push() = NockWork::Done;
    };
    push_formula(stack, formula, true);
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
//    assert_no_alloc(||
    unsafe {
        loop {
            let work: NockWork = *stack.top();
            match work {
                NockWork::Done => {
                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.preserve(&mut cold);
                    stack.preserve(&mut warm);
                    stack.frame_pop();
                    break;
                }
                NockWork::Ret => {
                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.preserve(&mut cold);
                    stack.preserve(&mut warm);
                    stack.frame_pop();
                }
                NockWork::WorkCons(mut cons) => match cons.todo {
                    TodoCons::ComputeHead => {
                        cons.todo = TodoCons::ComputeTail;
                        *stack.top() = NockWork::WorkCons(cons);
                        push_formula(stack, cons.head, false);
                    }
                    TodoCons::ComputeTail => {
                        cons.todo = TodoCons::Cons;
                        cons.head = res;
                        *stack.top() = NockWork::WorkCons(cons);
                        push_formula(stack, cons.tail, false);
                    }
                    TodoCons::Cons => {
                        res = T(stack, &[cons.head, res]);
                        stack.pop::<NockWork>();
                    }
                },
                NockWork::Work0(zero) => {
                    res = slot(subject, zero.axis.as_bitslice());
                    stack.pop::<NockWork>();
                }
                NockWork::Work1(once) => {
                    res = once.noun;
                    stack.pop::<NockWork>();
                }
                NockWork::Work2(mut vale) => match vale.todo {
                    Todo2::ComputeSubject => {
                        vale.todo = Todo2::ComputeFormula;
                        *stack.top() = NockWork::Work2(vale);
                        push_formula(stack, vale.subject, false);
                    }
                    Todo2::ComputeFormula => {
                        vale.todo = Todo2::ComputeResult;
                        vale.subject = res;
                        *stack.top() = NockWork::Work2(vale);
                        push_formula(stack, vale.formula, false);
                    }
                    Todo2::ComputeResult => {
                        if let Some(jet) = warm.get_jet(stack, &mut vale.formula, subject) {
                            println!("{}", "warm match2!".red());
                            //TODO double check this, havent run into it
                            if let Ok(jet_res) = jet(stack, subject) {
                                stack.pop::<NockWork>();
                                res = jet_res;
                                break;
//                                subject = vale.subject;
//                                push_formula(stack, jet_res, true); //TODO true?
                            }
                        }
                        if vale.tail {
                            stack.pop::<NockWork>();
                            subject = vale.subject;
                            push_formula(stack, res, true);
                        } else {
                            vale.todo = Todo2::RestoreSubject;
                            std::mem::swap(&mut vale.subject, &mut subject);
                            *stack.top() = NockWork::Work2(vale);
                            stack.frame_push(0);
                            *stack.push() = NockWork::Ret;
                            push_formula(stack, res, true);
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
                        push_formula(stack, thee.child, false);
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
                        push_formula(stack, four.child, false);
                    }
                    Todo4::Increment => {
                        if let Ok(atom) = res.as_atom() {
                            res = inc(stack, atom).as_noun();
                            stack.pop::<NockWork>();
                        } else {
                            panic!("Cannot increment (Nock 4) a cell");
                        }
                    }
                },
                NockWork::Work5(mut five) => match five.todo {
                    Todo5::ComputeLeftChild => {
                        five.todo = Todo5::ComputeRightChild;
                        *stack.top() = NockWork::Work5(five);
                        push_formula(stack, five.left, false);
                    }
                    Todo5::ComputeRightChild => {
                        five.todo = Todo5::TestEquals;
                        five.left = res;
                        *stack.top() = NockWork::Work5(five);
                        push_formula(stack, five.right, false);
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
                        push_formula(stack, cond.test, false);
                    }
                    Todo6::ComputeBranch => {
                        stack.pop::<NockWork>();
                        if let Left(direct) = res.as_either_direct_allocated() {
                            if direct.data() == 0 {
                                push_formula(stack, cond.zero, cond.tail);
                            } else if direct.data() == 1 {
                                push_formula(stack, cond.once, cond.tail);
                            } else {
                                panic!("Test branch of Nock 6 must return 0 or 1");
                            }
                        } else {
                            panic!("Test branch of Nock 6 must return a direct atom");
                        }
                    }
                },
                NockWork::Work7(mut pose) => match pose.todo {
                    Todo7::ComputeSubject => {
                        pose.todo = Todo7::ComputeResult;
                        *stack.top() = NockWork::Work7(pose);
                        push_formula(stack, pose.subject, false);
                    }
                    Todo7::ComputeResult => {
                        if pose.tail {
                            stack.pop::<NockWork>();
                            subject = res;
                            push_formula(stack, pose.formula, true);
                        } else {
                            pose.todo = Todo7::RestoreSubject;
                            pose.subject = subject;
                            *stack.top() = NockWork::Work7(pose);
                            subject = res;
                            push_formula(stack, pose.formula, false);
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
                        push_formula(stack, pins.pin, false);
                    }
                    Todo8::ComputeResult => {
                        if pins.tail {
                            subject = T(stack, &[res, subject]);
                            stack.pop::<NockWork>();
                            push_formula(stack, pins.formula, true);
                        } else {
                            pins.todo = Todo8::RestoreSubject;
                            pins.pin = subject;
                            *stack.top() = NockWork::Work8(pins);
                            subject = T(stack, &[res, subject]);
                            push_formula(stack, pins.formula, false);
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
                        push_formula(stack, kale.core, false);
                    }
                    Todo9::ComputeResult => {
                        let mut formula = slot(res, kale.axis.as_bitslice());
                        if let Some(jet) = warm.get_jet(stack, &mut formula, subject) {
                            println!("{}", "warm match9!".red());
                            if let Ok(jet_res) = jet(stack, res) {
                                stack.pop::<NockWork>();
                                //TODO do i even touch the subject?
                                // subject = res
                                res = jet_res;
                                break;
                            }
                        }
                        if kale.tail {
                            stack.pop::<NockWork>();
                            subject = res;
                            push_formula(stack, formula, true);
                        } else {
                            kale.todo = Todo9::RestoreSubject;
                            kale.core = subject;
                            *stack.top() = NockWork::Work9(kale);
                            subject = res;
                            stack.frame_push(0);
                            *stack.push() = NockWork::Ret;
                            push_formula(stack, formula, true);
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
                            push_formula(stack, diet.tree, false);
                        }
                        Todo10::ComputePatch => {
                            diet.todo = Todo10::Edit;
                            diet.tree = res;
                            *stack.top() = NockWork::Work10(diet);
                            push_formula(stack, diet.patch, false);
                        }
                        Todo10::Edit => {
                            res = edit(stack, diet.axis.as_bitslice(), res, diet.tree);
                            stack.pop::<NockWork>();
                        }
                    }
                }
                NockWork::Work11D(mut dint) => match dint.todo {
                    Todo11D::ComputeHint => {
                        let hint_cell = Cell::new(stack, dint.tag.as_noun(), dint.hint);
                        if let Ok(found) =
                            match_pre_hint(stack, newt, subject, hint_cell, formula, &cache)
                        {
                            res = found;
                            stack.pop::<NockWork>();
                        } else {
                            dint.todo = Todo11D::ComputeResult;
                            *stack.top() = NockWork::Work11D(dint);
                            push_formula(stack, dint.hint, false);
                        }
                    }
                    Todo11D::ComputeResult => {
                        dint.todo = Todo11D::Done;
                        let hint = Cell::new(stack, dint.tag.as_noun(), dint.hint).as_noun();
                        if let Ok(found) = match_post_hint(stack, newt, subject, hint, res) {
                            res = found;
                            stack.pop::<NockWork>();
                        } else {
                            dint.todo = Todo11D::Done;
                            *stack.top() = NockWork::Work11D(dint);
                            push_formula(stack, dint.body, false);
                        }
                    }
                    Todo11D::Done => {
                        let hint = Cell::new(stack, dint.tag.as_noun(), dint.hint).as_noun();
                        let _ = match_post_hinted(stack, subject, hint, res, &mut cold, &mut warm, &hot, &mut cache);
                        stack.pop::<NockWork>();
                    }
                },
                NockWork::Work11S(mut sint) => match sint.todo {
                    Todo11S::ComputeResult => {
                        sint.todo = Todo11S::Done;
                        if let Ok(found) =
                            match_post_hint(stack, newt, subject, sint.tag.as_noun(), res)
                        {
                            res = found;
                            stack.pop::<NockWork>();
                        } else {
                            sint.todo = Todo11S::Done;
                            *stack.top() = NockWork::Work11S(sint);
                            push_formula(stack, sint.body, false);
                        }
                    }
                    Todo11S::Done => {
                        let _ =
                            match_post_hinted(stack, subject, sint.tag.as_noun(), res, &mut cold, &mut warm, &hot, &mut cache);
                        stack.pop::<NockWork>();
                    }
                },
            };
        }
    }//);
    res
}

fn push_formula(stack: &mut NockStack, formula: Noun, tail: bool) {
    unsafe {
        //TODO check warm state for formula match?
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
                                    panic!("Axis for 0 must be atom");
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
                                    panic!("Argument for Nock 2 must be cell");
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
                                    panic!("Argument for Nock 5 must be cell");
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
                                        panic!("Argument tail for Nock 6 must be cell");
                                    };
                                } else {
                                    panic!("Argument for Nock 6 must be cell");
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
                                    panic!("Argument for Nock 7 must be cell");
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
                                    panic!("Argument for Nock 8 must be cell");
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
                                        panic!("Axis for Nock 9 must be atom");
                                    }
                                } else {
                                    panic!("Argument for Nock 9 must be cell");
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
                                            panic!("Axis for Nock 10 must be atom");
                                        }
                                    } else {
                                        panic!("Argument head for Nock 10 must be cell");
                                    };
                                } else {
                                    panic!("Argument for Nock 10 must be cell");
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
                                            });
                                        }
                                        Right(hint_cell) => {
                                            if let Ok(tag_atom) = hint_cell.head().as_atom() {
                                                *stack.push() = NockWork::Work11D(Nock11D {
                                                    todo: Todo11D::ComputeHint,
                                                    tag: tag_atom,
                                                    hint: hint_cell.tail(),
                                                    body: arg_cell.tail(),
                                                });
                                            } else {
                                                panic!("Head of hint cell must be atom");
                                            }
                                        }
                                    };
                                } else {
                                    panic!("Argument for Nock 11 must be cell");
                                };
                            }
                            _ => {
                                panic!("Invalid opcode");
                            }
                        }
                    } else {
                        panic!("Invalid opcode");
                    }
                }
            }
        } else {
            panic!("Bad formula: atoms are not formulas: {}", formula);
        }
    }
}

/** Note: axis must fit in a direct atom */
pub fn raw_slot(noun: Noun, axis: u64) -> Noun {
    slot(noun, DirectAtom::new(axis).unwrap().as_bitslice())
}

pub fn slot(mut noun: Noun, axis: &BitSlice<u64, Lsb0>) -> Noun {
    let mut cursor = if let Some(x) = axis.last_one() {
        x
    } else {
        panic!("0 is not allowed as an axis")
    };
    loop {
        if cursor == 0 {
            break;
        };
        cursor -= 1;
        if let Ok(cell) = noun.as_cell() {
            if axis[cursor] {
                noun = cell.tail();
            } else {
                noun = cell.head();
            }
        } else {
            panic!("Axis tried to descend through atom: {}", noun);
        };
    }
    noun
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

/** Match hints which apply before the formula is evaluated */
fn match_pre_hint(
    stack: &mut NockStack,
    newt: &mut Option<&mut Newt>,
    subject: Noun,
    cell: Cell,
    formula: Noun,
    cache: &Hamt<Noun>,
) -> Result<Noun, ()> {
    let direct = cell.head().as_direct()?;
    match direct.data() {
        // %sham hints are scaffolding until we have a real jet dashboard
        tas!(b"sham") => {
            let jet_formula = cell.tail().as_cell()?;
            let jet_name = jet_formula.tail();

            let jet = jets::get_jet(jet_name).ok_or(())?;
            if let Ok(mut jet_res) = jet(stack, subject) {
                // if in test mode, check that the jet returns the same result as the raw nock
                if jets::get_jet_test_mode(jet_name) {
                    let mut nock_res = interpret(stack, newt, subject, formula);
                    if unsafe { !unifying_equality(stack, &mut nock_res, &mut jet_res) } {
                        eprintln!(
                            "\rJet {} failed, raw: {}, jetted: {}",
                            jet_name, nock_res, jet_res
                        );
                        return Err(());
                    }
                }
                Ok(jet_res)
            } else {
                // Print jet errors and punt to Nock
                eprintln!("\rJet {} failed", jet_name);
                Err(())
            }
        },
        tas!(b"memo") => {
            let formula = unsafe { *stack.local_noun_pointer(2) };
            let mut key = Cell::new(stack, subject, formula).as_noun();
            if let Some(res) = cache.lookup(stack, &mut key) {
                Ok(res)
            } else {
                Err(())
            }
        },
        _ => Err(()),
    }
}

/** Match static hints and dynamic hints after they're evaluated */
fn match_post_hint(
    stack: &mut NockStack,
    newt: &mut Option<&mut Newt>,
    _subject: Noun,
    hint: Noun,
    res: Noun,
) -> Result<Noun, ()> {
    let direct = hint.as_cell()?.head().as_direct()?;
    match direct.data() {
        tas!(b"slog") => {
            let slog_cell = res.as_cell()?;
            let pri = slog_cell.head().as_direct()?.data();
            let tank = slog_cell.tail();
            if let Some(not) = newt {
                not.slog(stack, pri, tank);
            } else {
                println!("slog: {} {}", pri, tank);
            }
            Err(())
        },
        _ => Err(()),
    }
}

fn match_post_hinted(
    stack: &mut NockStack,
    subject: Noun,
    hint: Noun,
    res: Noun,
    cold: &mut Cold,
    warm: &mut Warm,
    hot: &Hot,
    cache: &mut Hamt<Noun>,
) -> Result<(), ()> {
    let direct = hint.as_cell()?.head().as_direct()?;
    match direct.data() {
        tas!(b"memo") => {
            let formula = unsafe { *stack.local_noun_pointer(2) };
            let mut key = Cell::new(stack, subject, formula).as_noun();
            *cache = cache.insert(stack, &mut key, res);
            Ok(())
        },
        tas!(b"fast") => {
            permit_alloc(|| {
                eprintln!("{}", "match_post_hinted() fast".green());
            });
            let mut formula = raw_slot(res, 2);

            let mut core = raw_slot(res, 1);
            //TODO this is only correct for a gate I think?
//            let mut formula = raw_slot(core, 2);
//            let mut real_formula = raw_slot(formula, 3);
//            println!("formula: {:?}", formula);
            // Check to see if jet is already registered in warm state
            // else, register the jet
            //TODO checking to see if a fast jet is already registered should be
            // done first
            //  ++  clue  (trel chum nock (list (pair term nock))
            let clue = hint.as_cell()?.tail().as_cell()?.tail().as_cell()?;
            let mut chum = clue.head();

            let parent_formula = clue.tail().as_cell()?.head().as_cell()?;
            //TODO every parent formula is either a nock 0 or 1. since we ultimately
            // want an atom, just take the tail of the formula?
            let parent_formula_head = parent_formula.head().as_direct()?.data();
            let parent_axis = match parent_formula_head {
                0 => parent_formula.tail().as_atom()?, // axis
                1 => Atom::new(stack, 0),              // parent of root is 0 by convention
                _ => return Err(()),                   // fund
            };
            let _hooks = clue.tail().as_cell()?.tail();

            cold.register(stack, &mut core, &mut chum, &parent_axis, &hot, warm);
            Ok(())
        },
    _ => Err(()),
    }
}
