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

#[derive(Copy, Clone, FromPrimitive, ToPrimitive, Debug)]
#[repr(u64)]
enum NockWork {
    Done,
    NockCellComputeHead,
    NockCellComputeTail,
    NockCellCons,
    Nock0Axis,
    Nock1Constant,
    Nock2ComputeSubject,
    Nock2ComputeFormula,
    Nock2ComputeResult,
    Nock2RestoreSubject,
    Nock3ComputeChild,
    Nock3ComputeType,
    Nock4ComputeChild,
    Nock4Increment,
    Nock5ComputeLeftChild,
    Nock5ComputeRightChild,
    Nock5TestEquals,
    Nock6ComputeTest,
    Nock6ComputeBranch,
    Nock6Done,
    Nock7ComputeSubject,
    Nock7ComputeResult,
    Nock7RestoreSubject,
    Nock8ComputeSubject,
    Nock8ComputeResult,
    Nock8RestoreSubject,
    Nock9ComputeCore,
    Nock9ComputeResult,
    Nock9RestoreSubject,
    Nock10ComputeTree,
    Nock10ComputePatch,
    Nock10Edit,
    Nock11ComputeHint,
    Nock11ComputeResult,
    Nock11Done,
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
    stack.frame_push(1);
    let mut cache = Hamt::<Noun>::new();
    unsafe {
        *(stack.local_noun_pointer(0)) = work_to_noun(Done);
    }
    push_formula(stack, formula);
    assert_no_alloc(|| unsafe {
        loop {
            match noun_to_work(*(stack.local_noun_pointer(0))) {
                Done => {
                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.frame_pop();
                    break;
                }
                NockCellComputeHead => {
                    *stack.local_noun_pointer(0) = work_to_noun(NockCellComputeTail);
                    let formula = *stack.local_noun_pointer(1);
                    push_formula(stack, formula);
                }
                NockCellComputeTail => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(NockCellCons);
                    *(stack.local_noun_pointer(1)) = res;
                    let formula = *stack.local_noun_pointer(2);
                    push_formula(stack, formula);
                }
                NockCellCons => {
                    let head = *stack.local_noun_pointer(1);
                    res = Cell::new(stack, head, res).as_noun();

                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.frame_pop();
                }
                Nock0Axis => {
                    if let Ok(atom) = (*(stack.local_noun_pointer(1))).as_atom() {
                        res = slot(subject, atom.as_bitslice());

                        stack.preserve(&mut cache);
                        stack.preserve(&mut res);
                        stack.frame_pop();
                    } else {
                        panic!("Axis must be atom");
                    };
                }
                Nock1Constant => {
                    res = *(stack.local_noun_pointer(1));

                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.frame_pop();
                }
                Nock2ComputeSubject => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock2ComputeFormula);
                    let formula = *stack.local_noun_pointer(1);
                    push_formula(stack, formula);
                }
                Nock2ComputeFormula => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock2ComputeResult);
                    *(stack.local_noun_pointer(1)) = res;
                    let formula = *stack.local_noun_pointer(2);
                    push_formula(stack, formula);
                }
                Nock2ComputeResult => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock2RestoreSubject);
                    *(stack.local_noun_pointer(2)) = subject;
                    subject = *(stack.local_noun_pointer(1));
                    push_formula(stack, res);
                }
                Nock2RestoreSubject => {
                    subject = *(stack.local_noun_pointer(2));

                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.frame_pop();
                }
                Nock3ComputeChild => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock3ComputeType);
                    let formula = *stack.local_noun_pointer(1);
                    push_formula(stack, formula);
                }
                Nock3ComputeType => {
                    res = if res.is_cell() {
                        DirectAtom::new_unchecked(0).as_atom().as_noun()
                    } else {
                        DirectAtom::new_unchecked(1).as_atom().as_noun()
                    };

                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.frame_pop();
                }
                Nock4ComputeChild => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock4Increment);
                    let formula = *stack.local_noun_pointer(1);
                    push_formula(stack, formula);
                }
                Nock4Increment => {
                    if let Ok(atom) = res.as_atom() {
                        res = inc(stack, atom).as_noun();

                        stack.preserve(&mut cache);
                        stack.preserve(&mut res);
                        stack.frame_pop();
                    } else {
                        panic!("Cannot increment (Nock 4) a cell");
                    };
                }
                Nock5ComputeLeftChild => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock5ComputeRightChild);
                    let formula = *stack.local_noun_pointer(1);
                    push_formula(stack, formula);
                }
                Nock5ComputeRightChild => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock5TestEquals);
                    *(stack.local_noun_pointer(1)) = res;
                    let formula = *stack.local_noun_pointer(2);
                    push_formula(stack, formula);
                }
                Nock5TestEquals => {
                    let saved_value_ptr = stack.local_noun_pointer(1);
                    res = if unifying_equality(stack, &mut res, saved_value_ptr) {
                        DirectAtom::new_unchecked(0).as_atom().as_noun()
                    } else {
                        DirectAtom::new_unchecked(1).as_atom().as_noun()
                    };

                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.frame_pop();
                }
                Nock6ComputeTest => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock6ComputeBranch);
                    let formula = *stack.local_noun_pointer(1);
                    push_formula(stack, formula);
                }
                Nock6ComputeBranch => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock6Done);
                    if let Left(direct) = res.as_either_direct_allocated() {
                        if direct.data() == 0 {
                            let formula = *stack.local_noun_pointer(2);
                            push_formula(stack, formula);
                        } else if direct.data() == 1 {
                            let formula = *stack.local_noun_pointer(3);
                            push_formula(stack, formula);
                        } else {
                            panic!("Test branch of Nock 6 must return 0 or 1");
                        };
                    } else {
                        panic!("Test branch of Nock 6 must return a direct atom");
                    }
                }
                Nock6Done => {
                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.frame_pop();
                }
                Nock7ComputeSubject => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock7ComputeResult);
                    let formula = *stack.local_noun_pointer(1);
                    push_formula(stack, formula);
                }
                Nock7ComputeResult => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock7RestoreSubject);
                    *(stack.local_noun_pointer(1)) = subject;
                    subject = res;
                    let formula = *stack.local_noun_pointer(2);
                    push_formula(stack, formula);
                }
                Nock7RestoreSubject => {
                    subject = *(stack.local_noun_pointer(1));

                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.frame_pop();
                }
                Nock8ComputeSubject => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock8ComputeResult);
                    let formula = *stack.local_noun_pointer(1);
                    push_formula(stack, formula);
                }
                Nock8ComputeResult => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock8RestoreSubject);
                    *(stack.local_noun_pointer(1)) = subject;
                    subject = Cell::new(stack, res, subject).as_noun();
                    let formula = *stack.local_noun_pointer(2);
                    push_formula(stack, formula);
                }
                Nock8RestoreSubject => {
                    subject = *(stack.local_noun_pointer(1));

                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.frame_pop();
                }
                Nock9ComputeCore => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock9ComputeResult);
                    let formula = *stack.local_noun_pointer(2);
                    push_formula(stack, formula);
                }
                Nock9ComputeResult => {
                    if let Ok(formula_axis) = (*(stack.local_noun_pointer(1))).as_atom() {
                        *(stack.local_noun_pointer(0)) = work_to_noun(Nock9RestoreSubject);
                        *(stack.local_noun_pointer(2)) = subject;
                        subject = res;
                        push_formula(stack, slot(subject, formula_axis.as_bitslice()));
                    } else {
                        panic!("Axis into core must be atom");
                    }
                }
                Nock9RestoreSubject => {
                    subject = *(stack.local_noun_pointer(2));

                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.frame_pop();
                }
                Nock10ComputeTree => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock10ComputePatch);
                    let formula = *stack.local_noun_pointer(3);
                    push_formula(stack, formula);
                }
                Nock10ComputePatch => {
                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock10Edit);
                    *(stack.local_noun_pointer(3)) = res;
                    let formula = *stack.local_noun_pointer(2);
                    push_formula(stack, formula);
                }
                Nock10Edit => {
                    if let Ok(edit_axis) = (*stack.local_noun_pointer(1)).as_atom() {
                        let tree = *stack.local_noun_pointer(3);
                        res = edit(stack, edit_axis.as_bitslice(), res, tree);

                        stack.preserve(&mut cache);
                        stack.preserve(&mut res);
                        stack.frame_pop();
                    } else {
                        panic!("Axis into tree must be atom");
                    }
                }
                Nock11ComputeHint => {
                    let hint = (*stack.local_noun_pointer(1))
                        .as_cell()
                        .expect("IMPOSSIBLE: tried to compute a dynamic hint but hint is an atom");
                    let formula = *stack.local_noun_pointer(2);

                    if let Some(found) =
                        match_hint_pre_hint(stack, newt, subject, hint, formula, &cache)
                    {
                        res = found;
                        stack.preserve(&mut cache);
                        stack.preserve(&mut res);
                        stack.frame_pop();
                    } else {
                        *(stack.local_noun_pointer(0)) = work_to_noun(Nock11ComputeResult);
                        push_formula(stack, hint.tail());
                    }
                }
                Nock11ComputeResult => {
                    let hint = *stack.local_noun_pointer(1);

                    if let Some(found) = match_hint_pre_nock(stack, newt, subject, hint, res) {
                        res = found;

                        stack.preserve(&mut cache);
                        stack.preserve(&mut res);
                        stack.frame_pop();
                    } else {
                        *(stack.local_noun_pointer(0)) = work_to_noun(Nock11Done);
                        let formula = *stack.local_noun_pointer(2);
                        push_formula(stack, formula);
                    }
                }
                Nock11Done => {
                    let hint = *stack.local_noun_pointer(1);

                    if let Some(found) = match_hint_post_nock(stack, subject, hint, res, &mut cache)
                    {
                        res = found;
                    }

                    stack.preserve(&mut cache);
                    stack.preserve(&mut res);
                    stack.frame_pop();
                }
            };
        }
    });
    res
}

fn push_formula(stack: &mut NockStack, formula: Noun) {
    if let Ok(formula_cell) = formula.as_cell() {
        // Formula
        match formula_cell.head().as_either_atom_cell() {
            Right(_cell) => {
                stack.frame_push(3);
                unsafe {
                    *(stack.local_noun_pointer(0)) = work_to_noun(NockCellComputeHead);
                    *(stack.local_noun_pointer(1)) = formula_cell.head();
                    *(stack.local_noun_pointer(2)) = formula_cell.tail();
                }
            }
            Left(atom) => {
                if let Ok(direct) = atom.as_direct() {
                    match direct.data() {
                        0 => {
                            stack.frame_push(2);
                            unsafe {
                                *(stack.local_noun_pointer(0)) = work_to_noun(Nock0Axis);
                                *(stack.local_noun_pointer(1)) = formula_cell.tail();
                            };
                        }
                        1 => {
                            stack.frame_push(2);
                            unsafe {
                                *(stack.local_noun_pointer(0)) = work_to_noun(Nock1Constant);
                                *(stack.local_noun_pointer(1)) = formula_cell.tail();
                            };
                        }
                        2 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                stack.frame_push(3);
                                unsafe {
                                    *(stack.local_noun_pointer(0)) =
                                        work_to_noun(Nock2ComputeSubject);
                                    *(stack.local_noun_pointer(1)) = arg_cell.head();
                                    *(stack.local_noun_pointer(2)) = arg_cell.tail();
                                };
                            } else {
                                panic!("Argument for Nock 2 must be cell");
                            };
                        }
                        3 => {
                            stack.frame_push(2);
                            unsafe {
                                *(stack.local_noun_pointer(0)) = work_to_noun(Nock3ComputeChild);
                                *(stack.local_noun_pointer(1)) = formula_cell.tail();
                            };
                        }
                        4 => {
                            stack.frame_push(2);
                            unsafe {
                                *(stack.local_noun_pointer(0)) = work_to_noun(Nock4ComputeChild);
                                *(stack.local_noun_pointer(1)) = formula_cell.tail();
                            };
                        }
                        5 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                stack.frame_push(3);
                                unsafe {
                                    *(stack.local_noun_pointer(0)) =
                                        work_to_noun(Nock5ComputeLeftChild);
                                    *(stack.local_noun_pointer(1)) = arg_cell.head();
                                    *(stack.local_noun_pointer(2)) = arg_cell.tail();
                                };
                            } else {
                                panic!("Argument for Nock 5 must be cell");
                            };
                        }
                        6 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                if let Ok(branch_cell) = arg_cell.tail().as_cell() {
                                    stack.frame_push(4);
                                    unsafe {
                                        *(stack.local_noun_pointer(0)) =
                                            work_to_noun(Nock6ComputeTest);
                                        *(stack.local_noun_pointer(1)) = arg_cell.head();
                                        *(stack.local_noun_pointer(2)) = branch_cell.head();
                                        *(stack.local_noun_pointer(3)) = branch_cell.tail();
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
                                stack.frame_push(3);
                                unsafe {
                                    *(stack.local_noun_pointer(0)) =
                                        work_to_noun(Nock7ComputeSubject);
                                    *(stack.local_noun_pointer(1)) = arg_cell.head();
                                    *(stack.local_noun_pointer(2)) = arg_cell.tail();
                                }
                            } else {
                                panic!("Argument for Nock 7 must be cell");
                            };
                        }
                        8 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                stack.frame_push(3);
                                unsafe {
                                    *(stack.local_noun_pointer(0)) =
                                        work_to_noun(Nock8ComputeSubject);
                                    *(stack.local_noun_pointer(1)) = arg_cell.head();
                                    *(stack.local_noun_pointer(2)) = arg_cell.tail();
                                };
                            } else {
                                panic!("Argument for Nock 8 must be cell");
                            };
                        }
                        9 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                stack.frame_push(3);
                                unsafe {
                                    *(stack.local_noun_pointer(0)) = work_to_noun(Nock9ComputeCore);
                                    *(stack.local_noun_pointer(1)) = arg_cell.head();
                                    *(stack.local_noun_pointer(2)) = arg_cell.tail();
                                };
                            } else {
                                panic!("Argument for Nock 9 must be cell");
                            };
                        }
                        10 => {
                            if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                if let Ok(patch_cell) = arg_cell.head().as_cell() {
                                    stack.frame_push(4);
                                    unsafe {
                                        *(stack.local_noun_pointer(0)) =
                                            work_to_noun(Nock10ComputeTree);
                                        *(stack.local_noun_pointer(1)) = patch_cell.head();
                                        *(stack.local_noun_pointer(2)) = patch_cell.tail();
                                        *(stack.local_noun_pointer(3)) = arg_cell.tail();
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
                                stack.frame_push(3);
                                unsafe {
                                    *(stack.local_noun_pointer(0)) =
                                        work_to_noun(if arg_cell.head().is_cell() {
                                            Nock11ComputeHint
                                        } else {
                                            Nock11ComputeResult
                                        });
                                    *(stack.local_noun_pointer(1)) = arg_cell.head();
                                    *(stack.local_noun_pointer(2)) = arg_cell.tail();
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

/** Match dynamic hints before the hint formula is evaluated */
fn match_hint_pre_hint(
    stack: &mut NockStack,
    newt: &mut Option<&mut Newt>,
    subject: Noun,
    hint: Cell,
    formula: Noun,
    cache: &Hamt<Noun>,
) -> Option<Noun> {
    let tag = hint.head().direct()?;

    match tag.data() {
        // %sham hints are scaffolding until we have a real jet dashboard
        tas!(b"sham") => {
            let jet_formula = hint.tail().cell()?;
            let jet_name = jet_formula.tail();

            let jet = jets::get_jet(jet_name)?;
            if let Ok(mut jet_res) = jet(stack, subject) {
                // if in test mode, check that the jet returns the same result as the raw nock
                if jets::get_jet_test_mode(jet_name) {
                    let mut nock_res = interpret(stack, newt, subject, formula);
                    if unsafe { !unifying_equality(stack, &mut nock_res, &mut jet_res) } {
                        eprintln!(
                            "\rJet {} failed, raw: {}, jetted: {}",
                            jet_name, nock_res, jet_res
                        );
                        return None;
                    }
                }
                Some(jet_res)
            } else {
                // Print jet errors and punt to Nock
                eprintln!("\rJet {} failed: ", jet_name);
                None
            }
        }
        tas!(b"memo") => {
            let formula = unsafe { *stack.local_noun_pointer(2) };
            let mut key = Cell::new(stack, subject, formula).as_noun();
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
    hint: Noun,
    res: Noun,
) -> Option<Noun> {
    let tag = hint
        .as_either_atom_cell()
        .either(|a| a.direct(), |c| c.head().direct())?;

    match tag.data() {
        tas!(b"slog") => {
            let slog_cell = res.cell()?;
            let pri = slog_cell.head().direct()?.data();
            let tank = slog_cell.tail();
            if let Some(not) = newt {
                not.slog(stack, pri, tank);
            } else {
                println!("raw slog: {} {}", pri, tank);
            }

            None
        }
        _ => None,
    }
}

/** Match static and dynamic hints after the nock formula is evaluated */
fn match_hint_post_nock(
    stack: &mut NockStack,
    subject: Noun,
    hint: Noun,
    res: Noun,
    cache: &mut Hamt<Noun>,
) -> Option<Noun> {
    let tag = hint
        .as_either_atom_cell()
        .either(|a| a.direct(), |c| c.head().direct())?;

    match tag.data() {
        tas!(b"memo") => {
            let formula = unsafe { *stack.local_noun_pointer(2) };
            let mut key = Cell::new(stack, subject, formula).as_noun();
            *cache = cache.insert(stack, &mut key, res);
            None
        }
        _ => None,
    }
}
