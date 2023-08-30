use self::NockWork::*;
use crate::hamt::Hamt;
use crate::jets;
use crate::mem::unifying_equality;
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Atom, Cell, DirectAtom, IndirectAtom, Noun};
use ares_macros::tas;
use assert_no_alloc::assert_no_alloc;
use bitvec::prelude::{BitSlice, Lsb0};
use either::Either::*;
use num_traits::cast::{FromPrimitive, ToPrimitive};

crate::gdb!();

#[derive(Copy, Clone, FromPrimitive, ToPrimitive, Debug, PartialEq, Eq)]
#[repr(u64)]
enum NockWork {
    Done,
    Ret,
    RestoreSubject,
    NockCellComputeHead,
    NockCellComputeTail,
    NockCellCons,
    Nock0Axis,
    Nock1Constant,
    Nock2ComputeSubject,
    Nock2ComputeFormula,
    Nock2ComputeResult,
    Nock2ComputeSubjectTailPos,
    Nock2ComputeFormulaTailPos,
    Nock2TailCall,
    Nock3ComputeChild,
    Nock3ComputeType,
    Nock4ComputeChild,
    Nock4Increment,
    Nock5ComputeLeftChild,
    Nock5ComputeRightChild,
    Nock5TestEquals,
    Nock6ComputeTest,
    Nock6ComputeBranch,
    Nock6ComputeTestTailPos,
    Nock6ComputeBranchTailPos,
    Nock7ComputeSubject,
    Nock7ComputeResult,
    Nock7ComputeSubjectTailPos,
    Nock7ComputeResultTailPos,
    Nock8ComputeSubject,
    Nock8ComputeResult,
    Nock8ComputeSubjectTailPos,
    Nock8ComputeResultTailPos,
    Nock9ComputeCore,
    Nock9ComputeResult,
    Nock9ComputeCoreTailPos,
    Nock9TailCall,
    Nock10ComputeTree,
    Nock10ComputePatch,
    Nock10Edit,
    Nock11ComputeHint,
    Nock11ComputeResultDynamic,
    Nock11ComputeResultStatic,
    Nock11Done,
    Nock11ComputeHintTailPos,
    Nock11ComputeResultDynamicTailPos,
    Nock11ComputeResultStaticTailPos,
}

fn work_to_noun(work: NockWork) -> Noun {
    unsafe {
        DirectAtom::new_unchecked(work.to_u64().expect("IMPOSSIBLE: work does not fit in u64"))
            .as_atom()
            .as_noun()
    }
}

fn noun_to_work(noun: Noun) -> NockWork {
    if let Left(direct) = noun.as_either_direct_allocated() {
        NockWork::from_u64(direct.data()).expect("Invalid work")
    } else {
        panic!("Work should always be a direct atom.")
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
    let mut cache = Hamt::<Noun>::new();
    unsafe {
        *(stack.push()) = work_to_noun(Done);
    }
    push_formula(stack, formula, true);
    assert_no_alloc(|| unsafe {
        loop {
            let work = noun_to_work(*stack.top());
            stack.pop::<Noun>();
            match work {
                Done => {
                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.frame_pop();
                    break;
                }
                Ret => {
                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.frame_pop();
                }
                RestoreSubject => {
                    subject = *(stack.top());
                    stack.pop::<Noun>();
                }
                NockCellComputeHead => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(NockCellComputeTail);
                    push_formula(stack, formula, false);
                }
                NockCellComputeTail => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = res;
                    *(stack.push()) = work_to_noun(NockCellCons);
                    push_formula(stack, formula, false);
                }
                NockCellCons => {
                    let head = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    res = Cell::new(stack, head, res).as_noun();
                }
                Nock0Axis => {
                    if let Ok(atom) = (*(stack.top::<Noun>())).as_atom() {
                        stack.pop::<Noun>();
                        res = slot(subject, atom.as_bitslice());
                    } else {
                        panic!("Axis must be atom");
                    };
                }
                Nock1Constant => {
                    res = *stack.top();
                    stack.pop::<Noun>();
                }
                Nock2ComputeSubject => {
                    let formula = *stack.top();
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock2ComputeFormula);
                    push_formula(stack, formula, false);
                }
                Nock2ComputeSubjectTailPos => {
                    let formula = *stack.top();
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock2ComputeFormulaTailPos);
                    push_formula(stack, formula, false);
                }
                Nock2ComputeFormula => {
                    let formula = *stack.top();
                    stack.pop::<Noun>();
                    *(stack.push()) = res;
                    *(stack.push()) = work_to_noun(Nock2ComputeResult);
                    push_formula(stack, formula, false);
                }
                Nock2ComputeFormulaTailPos => {
                    let formula = *stack.top();
                    stack.pop::<Noun>();
                    *(stack.push()) = res;
                    *(stack.push()) = work_to_noun(Nock2TailCall);
                    push_formula(stack, formula, false);
                }
                Nock2ComputeResult => {
                    let new_subject = *(stack.top());
                    stack.pop::<Noun>();
                    *(stack.push()) = subject;
                    *(stack.push()) = work_to_noun(RestoreSubject);
                    subject = new_subject;
                    stack.frame_push(0);
                    *(stack.push()) = work_to_noun(Ret);
                    push_formula(stack, res, false);
                }
                Nock2TailCall => {
                    subject = *(stack.top());
                    stack.pop::<Noun>();
                    assert!(noun_to_work(*stack.top()) == Done || noun_to_work(*stack.top()) == Ret);
                    push_formula(stack, res, true);
                }
                Nock3ComputeChild => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock3ComputeType);
                    push_formula(stack, formula, false);
                }
                Nock3ComputeType => {
                    res = if res.is_cell() {
                        DirectAtom::new_unchecked(0).as_atom().as_noun()
                    } else {
                        DirectAtom::new_unchecked(1).as_atom().as_noun()
                    };
                }
                Nock4ComputeChild => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock4Increment);
                    push_formula(stack, formula, false);
                }
                Nock4Increment => {
                    if let Ok(atom) = res.as_atom() {
                        res = inc(stack, atom).as_noun();
                    } else {
                        panic!("Cannot increment (Nock 4) a cell");
                    };
                }
                Nock5ComputeLeftChild => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock5ComputeRightChild);
                    push_formula(stack, formula, false);
                }
                Nock5ComputeRightChild => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = res;
                    *(stack.push()) = work_to_noun(Nock5TestEquals);
                    push_formula(stack, formula, false);
                }
                Nock5TestEquals => {
                    let saved_value_ptr = stack.top::<Noun>();
                    res = if unifying_equality(stack, &mut res, saved_value_ptr) {
                        DirectAtom::new_unchecked(0).as_atom().as_noun()
                    } else {
                        DirectAtom::new_unchecked(1).as_atom().as_noun()
                    };
                    stack.pop::<Noun>();
                }
                Nock6ComputeTest => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock6ComputeBranch);
                    push_formula(stack, formula, false);
                }
                Nock6ComputeTestTailPos => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock6ComputeBranchTailPos);
                    push_formula(stack, formula, false);
                }
                Nock6ComputeBranch => {
                    if let Left(direct) = res.as_either_direct_allocated() {
                        if direct.data() == 0 {
                            let formula = *(stack.top::<Noun>());
                            stack.pop::<Noun>();
                            stack.pop::<Noun>();
                            push_formula(stack, formula, false);
                        } else if direct.data() == 1 {
                            stack.pop::<Noun>();
                            let formula = *(stack.top::<Noun>());
                            stack.pop::<Noun>();
                            push_formula(stack, formula, false);
                        } else {
                            panic!("Test branch of Nock 6 must return 0 or 1");
                        };
                    } else {
                        panic!("Test branch of Nock 6 must return a direct atom");
                    }
                }
                Nock6ComputeBranchTailPos => {
                    if let Left(direct) = res.as_either_direct_allocated() {
                        if direct.data() == 0 {
                            let formula = *(stack.top::<Noun>());
                            stack.pop::<Noun>();
                            stack.pop::<Noun>();
                            push_formula(stack, formula, true);
                        } else if direct.data() == 1 {
                            stack.pop::<Noun>();
                            let formula = *(stack.top::<Noun>());
                            stack.pop::<Noun>();
                            push_formula(stack, formula, true);
                        } else {
                            panic!("Test branch of Nock 6 must return 0 or 1");
                        };
                    } else {
                        panic!("Test branch of Nock 6 must return a direct atom");
                    }
                }
                Nock7ComputeSubject => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock7ComputeResult);
                    push_formula(stack, formula, false);
                }
                Nock7ComputeSubjectTailPos => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock7ComputeResultTailPos);
                    push_formula(stack, formula, false);
                }
                Nock7ComputeResult => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = subject;
                    *(stack.push()) = work_to_noun(RestoreSubject);
                    subject = res;
                    push_formula(stack, formula, false);
                }
                Nock7ComputeResultTailPos => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    subject = res;
                    push_formula(stack, formula, true);
                }
                Nock8ComputeSubject => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock8ComputeResult);
                    push_formula(stack, formula, false);
                }
                Nock8ComputeSubjectTailPos => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock8ComputeResultTailPos);
                    push_formula(stack, formula, false);
                }
                Nock8ComputeResult => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = subject;
                    *(stack.push()) = work_to_noun(RestoreSubject);
                    subject = Cell::new(stack, res, subject).as_noun();
                    push_formula(stack, formula, false);
                }
                Nock8ComputeResultTailPos => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    subject = Cell::new(stack, res, subject).as_noun();
                    push_formula(stack, formula, true);
                }
                Nock9ComputeCore => {
                    let formula = *stack.top();
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock9ComputeResult);
                    push_formula(stack, formula, false);
                }
                Nock9ComputeCoreTailPos => {
                    let formula = *stack.top();
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock9TailCall);
                    push_formula(stack, formula, false);
                }
                Nock9ComputeResult => {
                    if let Ok(formula_axis) = (*stack.top::<Noun>()).as_atom() {
                        stack.pop::<Noun>();
                        *(stack.push()) = subject;
                        *(stack.push()) = work_to_noun(RestoreSubject);
                        subject = res;
                        push_formula(stack, slot(subject, formula_axis.as_bitslice()), false);
                    } else {
                        panic!("Axis into core must be atom");
                    }
                }
                Nock9TailCall => {
                    if let Ok(formula_axis) = (*stack.top::<Noun>()).as_atom() {
                        stack.pop::<Noun>();
                        assert!(noun_to_work(*stack.top()) == Done || noun_to_work(*stack.top()) == Ret);
                        subject = res;
                        push_formula(stack, slot(subject, formula_axis.as_bitslice()), true);
                    } else {
                        panic!("Axis into core must be atom");
                    }
                }
                Nock10ComputeTree => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push()) = work_to_noun(Nock10ComputePatch);
                    push_formula(stack, formula, false);
                }
                Nock10ComputePatch => {
                    let formula = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    let axis = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    *(stack.push::<Noun>()) = res;
                    *(stack.push::<Noun>()) = axis;
                    *(stack.push::<Noun>()) = work_to_noun(Nock10Edit);
                    push_formula(stack, formula, false);
                }
                Nock10Edit => {
                    if let Ok(edit_axis) = (*stack.top::<Noun>()).as_atom() {
                        stack.pop::<Noun>();
                        let tree = *stack.top::<Noun>();
                        stack.pop::<Noun>();
                        res = edit(stack, edit_axis.as_bitslice(), res, tree);
                    } else {
                        panic!("Axis into tree must be atom");
                    }
                }
                Nock11ComputeHint => {
                    let hint = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    let formula = *(stack.top());
                    stack.pop::<Noun>();
                    let hint_cell = Cell::new(stack, hint, formula);
                    if let Ok(found) =
                        match_pre_hint(stack, newt, subject, hint_cell, formula, &cache)
                    {
                        res = found;
                    } else {
                        *(stack.push()) = hint;
                        *(stack.push()) = work_to_noun(Nock11ComputeResultDynamic);
                        push_formula(stack, formula, false);
                    }
                },
                Nock11ComputeHintTailPos => {
                    let hint = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    let formula = *(stack.top());
                    stack.pop::<Noun>();
                    let hint_cell = Cell::new(stack, hint, formula);
                    if let Ok(found) =
                        match_pre_hint(stack, newt, subject, hint_cell, formula, &cache)
                    {
                        res = found;
                    } else {
                        *(stack.push()) = hint;
                        *(stack.push()) = work_to_noun(Nock11ComputeResultDynamicTailPos);
                        push_formula(stack, formula, false);
                    }
                },
                Nock11ComputeResultDynamic => {
                    let hint = *(stack.top());
                    stack.pop::<Noun>();
                    if let Ok(found) = match_post_hint(stack, newt, subject, hint, res) {
                        res = found;
                    } else {
                        let formula = *(stack.top());
                        stack.pop::<Noun>();
                        *(stack.push()) = Cell::new(stack, hint, res);
                        *(stack.push()) = work_to_noun(Nock11Done);
                        push_formula(stack, formula, false);
                    }
                }
                Nock11ComputeResultDynamicTailPos => {
                    let hint = *(stack.top());
                    stack.pop::<Noun>();
                    if let Ok(found) = match_post_hint(stack, newt, subject, hint, res) {
                        res = found;
                    } else {
                        let formula = *(stack.top());
                        stack.pop::<Noun>();
                        push_formula(stack, formula, true);
                    }
                }
                Nock11ComputeResultStatic => {
                    let hint = *(stack.top::<Noun>());
                    stack.pop::<Noun>();
                    let formula = *(stack.top());
                    stack.pop::<Noun>();
                    *(stack.push()) = hint;
                    *(stack.push()) = work_to_noun(Nock11Done);
                    push_formula(stack, formula, false);
                }
                Nock11ComputeResultStaticTailPos => {
                    stack.pop::<Noun>();
                    let formula = *(stack.top());
                    stack.pop::<Noun>();
                    push_formula(stack, formula, true);
                }
                Nock11Done => {
                    let hint = *stack.top();
                    stack.pop::<Noun>();
                    let _ = match_post_hinted(stack, subject, hint, res, &mut cache);
                    continue;
                }
            };
        }
    });
    res
}

fn push_formula(stack: &mut NockStack, formula: Noun, tail: bool) {
    if let Ok(formula_cell) = formula.as_cell() {
        // Formula
        match formula_cell.head().as_either_atom_cell() {
            Right(_cell) => {
                unsafe {
                    *(stack.push()) = formula_cell.tail();
                    *(stack.push()) = formula_cell.head();
                    *(stack.push()) = work_to_noun(NockCellComputeHead);
                }
            }
            Left(atom) => {
                if let Ok(direct) = atom.as_direct() {
                    match direct.data() {
                        0 => {
                            unsafe {
                                *(stack.push()) = formula_cell.tail();
                                *(stack.push()) = work_to_noun(Nock0Axis);
                            }
                        }
                        1 => {
                            unsafe {
                                *(stack.push()) = formula_cell.tail();
                                *(stack.push()) = work_to_noun(Nock1Constant);
                            };
                        }
                        2 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                unsafe {
                                    *(stack.push()) = arg_cell.tail();
                                    *(stack.push()) = arg_cell.head();
                                    if tail {
                                        *(stack.push()) =
                                            work_to_noun(Nock2ComputeSubjectTailPos);
                                    } else {
                                        *(stack.push()) =
                                            work_to_noun(Nock2ComputeSubject);
                                    }
                                }
                            } else {
                                panic!("Argument for Nock 2 must be cell");
                            };
                        }
                        3 => {
                            unsafe {
                                *(stack.push()) = formula_cell.tail();
                                *(stack.push()) = work_to_noun(Nock3ComputeChild);
                            }
                        }
                        4 => {
                            unsafe {
                                *(stack.push()) = formula_cell.tail();
                                *(stack.push()) = work_to_noun(Nock4ComputeChild);
                            }
                        }
                        5 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                unsafe {
                                    *(stack.push()) = arg_cell.tail();
                                    *(stack.push()) = arg_cell.head();
                                    *(stack.push()) = work_to_noun(Nock5ComputeLeftChild);
                                };
                            } else {
                                panic!("Argument for Nock 5 must be cell");
                            };
                        }
                        6 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                if let Ok(branch_cell) = arg_cell.tail().as_cell() {
                                    unsafe {
                                        *(stack.push()) = branch_cell.tail();
                                        *(stack.push()) = branch_cell.head();
                                        *(stack.push()) = arg_cell.head();
                                        if tail {
                                            *(stack.push()) = work_to_noun(Nock6ComputeTestTailPos);
                                        } else {
                                            *(stack.push()) = work_to_noun(Nock6ComputeTest);
                                        }
                                    }
                                } else {
                                    panic!("Argument tail for Nock 6 must be cell");
                                };
                            } else {
                                panic!("Argument for Nock 6 must be cell");
                            }
                        }
                        7 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                unsafe {
                                    *(stack.push()) = arg_cell.tail();
                                    *(stack.push()) = arg_cell.head();
                                    if tail {
                                        *(stack.push()) = work_to_noun(Nock7ComputeSubjectTailPos);
                                    } else {
                                        *(stack.push()) = work_to_noun(Nock7ComputeSubject);
                                    }
                                }
                            } else {
                                panic!("Argument for Nock 7 must be cell");
                            };
                        }
                        8 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                unsafe {
                                    *(stack.push()) = arg_cell.tail();
                                    *(stack.push()) = arg_cell.head();
                                    if tail {
                                        *(stack.push()) =
                                            work_to_noun(Nock8ComputeSubjectTailPos);
                                    } else {
                                        *(stack.push()) =
                                            work_to_noun(Nock8ComputeSubjectTailPos);
                                    }
                                };
                            } else {
                                panic!("Argument for Nock 8 must be cell");
                            };
                        }
                        9 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                stack.frame_push(3);
                                unsafe {
                                    *(stack.push()) = arg_cell.head();
                                    *(stack.push()) = arg_cell.tail(); // we use the tail first
                                    if tail {
                                        *(stack.push()) = work_to_noun(Nock9ComputeCoreTailPos);
                                    } else {
                                        *(stack.push()) = work_to_noun(Nock9ComputeCore);
                                    }
                                };
                            } else {
                                panic!("Argument for Nock 9 must be cell");
                            };
                        }
                        10 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                if let Ok(patch_cell) = arg_cell.head().as_cell() {
                                    unsafe {
                                        *(stack.push()) = patch_cell.tail();
                                        *(stack.push()) = patch_cell.head();
                                        *(stack.push()) = arg_cell.tail();
                                        *(stack.push()) = work_to_noun(Nock10ComputeTree);
                                    };
                                } else {
                                    panic!("Argument head for Nock 10 must be cell");
                                };
                            } else {
                                panic!("Argument for Nock 10 must be cell");
                            };
                        }
                        11 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                unsafe {
                                    *(stack.push()) = arg_cell.tail();
                                    if let Ok(hint_cell) = arg_cell.head().as_cell() {
                                        *(stack.push()) = hint_cell.head();
                                        *(stack.push()) = hint_cell.tail();
                                        *(stack.push()) = work_to_noun(
                                            if tail {
                                                Nock11ComputeHintTailPos
                                            } else {
                                                Nock11ComputeHint
                                            });
                                    } else {
                                        *(stack.push()) = arg_cell.head();
                                        *(stack.push()) = work_to_noun(
                                            if tail {
                                                Nock11ComputeResultStaticTailPos
                                            } else {
                                                Nock11ComputeResultStatic
                                            });
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
        }
        tas!(b"memo") => {
            let formula = unsafe { *stack.local_noun_pointer(2) };
            let mut key = Cell::new(stack, subject, formula).as_noun();
            if let Some(res) = cache.lookup(stack, &mut key) {
                Ok(res)
            } else {
                Err(())
            }
        }
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
        }
        _ => Err(()),
    }
}

fn match_post_hinted(
    stack: &mut NockStack,
    subject: Noun,
    hint: Noun,
    res: Noun,
    cache: &mut Hamt<Noun>,
) -> Result<(), ()> {
    let direct = hint.as_cell()?.head().as_direct()?;
    match direct.data() {
        tas!(b"memo") => {
            let formula = unsafe { *stack.local_noun_pointer(2) };
            let mut key = Cell::new(stack, subject, formula).as_noun();
            *cache = cache.insert(stack, &mut key, res);
            Ok(())
        }
        _ => Err(()),
    }
}
