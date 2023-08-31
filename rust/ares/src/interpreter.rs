use self::NockWork::*;
use crate::hamt::Hamt;
use crate::interpreter::Todo10::*;
use crate::interpreter::Todo11D::*;
use crate::interpreter::Todo11S::*;
use crate::interpreter::Todo2::*;
use crate::interpreter::Todo3::*;
use crate::interpreter::Todo4::*;
use crate::interpreter::Todo5::*;
use crate::interpreter::Todo6::*;
use crate::interpreter::Todo7::*;
use crate::interpreter::Todo8::*;
use crate::interpreter::Todo9::*;
use crate::interpreter::TodoCons::*;
use crate::jets;
use crate::mem::unifying_equality;
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Atom, Cell, DirectAtom, IndirectAtom, Noun, D, T};
use ares_macros::tas;
use assert_no_alloc::assert_no_alloc;
use bitvec::prelude::{BitSlice, Lsb0};
use either::Either::*;

crate::gdb!();

#[derive(Copy, Clone)]
#[repr(u8)]
enum TodoCons {
    ConsComputeHead,
    ConsComputeTail,
    ConsCons,
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
    Nock2ComputeSubject,
    Nock2ComputeFormula,
    Nock2ComputeResult,
    Nock2RestoreSubject,
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
    Nock3ComputeChild,
    Nock3ComputeType,
}

#[derive(Copy, Clone)]
struct Nock3 {
    todo: Todo3,
    child: Noun,
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum Todo4 {
    Nock4ComputeChild,
    Nock4Increment,
}

#[derive(Copy, Clone)]
struct Nock4 {
    todo: Todo4,
    child: Noun,
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum Todo5 {
    Nock5ComputeLeftChild,
    Nock5ComputeRightChild,
    Nock5TestEquals,
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
    Nock6ComputeTest,
    Nock6ComputeBranch,
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
    Nock7ComputeSubject,
    Nock7ComputeResult,
    Nock7RestoreSubject,
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
    Nock8ComputeSubject,
    Nock8ComputeResult,
    Nock8RestoreSubject,
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
    Nock9ComputeCore,
    Nock9ComputeResult,
    Nock9RestoreSubject,
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
    Nock10ComputeTree,
    Nock10ComputePatch,
    Nock10Edit,
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
    Nock11DComputeHint,
    Nock11DComputeResult,
    Nock11DDone,
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
    Nock11SComputeResult,
    Nock11SDone,
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
        *stack.push() = Done;
    };
    push_formula(stack, formula, true);
    assert_no_alloc(|| unsafe {
        loop {
            let work: NockWork = *stack.top();
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
                WorkCons(mut cons) => match cons.todo {
                    ConsComputeHead => {
                        cons.todo = ConsComputeTail;
                        *stack.top() = WorkCons(cons);
                        push_formula(stack, cons.head, false);
                    }
                    ConsComputeTail => {
                        cons.todo = ConsCons;
                        cons.head = res;
                        *stack.top() = WorkCons(cons);
                        push_formula(stack, cons.tail, false);
                    }
                    ConsCons => {
                        res = T(stack, &[cons.head, res]);
                        stack.pop::<NockWork>();
                    }
                },
                Work0(zero) => {
                    res = slot(subject, zero.axis.as_bitslice());
                    stack.pop::<NockWork>();
                }
                Work1(once) => {
                    res = once.noun;
                    stack.pop::<NockWork>();
                }
                Work2(mut vale) => match vale.todo {
                    Nock2ComputeSubject => {
                        vale.todo = Nock2ComputeFormula;
                        *stack.top() = Work2(vale);
                        push_formula(stack, vale.subject, false);
                    }
                    Nock2ComputeFormula => {
                        vale.todo = Nock2ComputeResult;
                        vale.subject = res;
                        *stack.top() = Work2(vale);
                        push_formula(stack, vale.formula, false);
                    }
                    Nock2ComputeResult => {
                        if vale.tail {
                            stack.pop::<NockWork>();
                            push_formula(stack, res, true);
                        } else {
                            vale.todo = Nock2RestoreSubject;
                            std::mem::swap(&mut vale.subject, &mut subject);
                            *stack.top() = Work2(vale);
                            stack.frame_push(0);
                            *stack.push() = Ret;
                            push_formula(stack, res, false);
                        }
                    }
                    Nock2RestoreSubject => {
                        subject = vale.subject;
                        stack.pop::<NockWork>();
                    }
                },
                Work3(mut thee) => match thee.todo {
                    Nock3ComputeChild => {
                        thee.todo = Nock3ComputeType;
                        *stack.top() = Work3(thee);
                        push_formula(stack, thee.child, false);
                    }
                    Nock3ComputeType => {
                        res = if res.is_cell() { D(0) } else { D(1) };
                        stack.pop::<NockWork>();
                    }
                },
                Work4(mut four) => match four.todo {
                    Nock4ComputeChild => {
                        four.todo = Nock4Increment;
                        *stack.top() = Work4(four);
                        push_formula(stack, four.child, false);
                    }
                    Nock4Increment => {
                        if let Ok(atom) = res.as_atom() {
                            res = inc(stack, atom).as_noun();
                        } else {
                            panic!("Cannot increment (Nock 4) a cell");
                        }
                    }
                },
                Work5(mut five) => match five.todo {
                    Nock5ComputeLeftChild => {
                        five.todo = Nock5ComputeRightChild;
                        *stack.top() = Work5(five);
                        push_formula(stack, five.left, false);
                    }
                    Nock5ComputeRightChild => {
                        five.todo = Nock5TestEquals;
                        five.left = res;
                        *stack.top() = Work5(five);
                        push_formula(stack, five.right, false);
                    }
                    Nock5TestEquals => {
                        let saved_value_ptr = &mut five.left as *mut Noun;
                        res = if unifying_equality(stack, &mut res, saved_value_ptr) {
                            D(0)
                        } else {
                            D(1)
                        };
                        stack.pop::<NockWork>();
                    }
                },
                Work6(mut cond) => match cond.todo {
                    Nock6ComputeTest => {
                        cond.todo = Nock6ComputeBranch;
                        *stack.top() = Work6(cond);
                        push_formula(stack, cond.test, false);
                    }
                    Nock6ComputeBranch => {
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
                Work7(mut pose) => match pose.todo {
                    Nock7ComputeSubject => {
                        pose.todo = Nock7ComputeResult;
                        *stack.top() = Work7(pose);
                        push_formula(stack, pose.subject, false);
                    }
                    Nock7ComputeResult => {
                        if pose.tail {
                            stack.pop::<NockWork>();
                            subject = res;
                            push_formula(stack, pose.formula, true);
                        } else {
                            pose.todo = Nock7RestoreSubject;
                            pose.subject = subject;
                            *stack.top() = Work7(pose);
                            subject = res;
                            push_formula(stack, pose.formula, false);
                        }
                    }
                    Nock7RestoreSubject => {
                        subject = pose.subject;
                        stack.pop::<NockWork>();
                    }
                },
                Work8(mut pins) => match pins.todo {
                    Nock8ComputeSubject => {
                        pins.todo = Nock8ComputeResult;
                        *stack.top() = Work8(pins);
                        push_formula(stack, pins.pin, false);
                    }
                    Nock8ComputeResult => {
                        if pins.tail {
                            subject = T(stack, &[res, subject]);
                            stack.pop::<NockWork>();
                            push_formula(stack, pins.formula, true);
                        } else {
                            pins.todo = Nock8RestoreSubject;
                            pins.pin = subject;
                            *stack.top() = Work8(pins);
                            push_formula(stack, pins.formula, false);
                        }
                    }
                    Nock8RestoreSubject => {
                        subject = pins.pin;
                        stack.pop::<NockWork>();
                    }
                },
                Work9(mut kale) => match kale.todo {
                    Nock9ComputeCore => {
                        kale.todo = Nock9ComputeResult;
                        *stack.top() = Work9(kale);
                        push_formula(stack, kale.core, false);
                    }
                    Nock9ComputeResult => {
                        let formula = slot(res, kale.axis.as_bitslice());
                        if kale.tail {
                            stack.pop::<NockWork>();
                            subject = res;
                            push_formula(stack, formula, true);
                        } else {
                            kale.todo = Nock9RestoreSubject;
                            kale.core = subject;
                            *stack.top() = Work9(kale);
                            subject = res;
                            stack.frame_push(0);
                            *stack.push() = Ret;
                            push_formula(stack, formula, false);
                        }
                    }
                    Nock9RestoreSubject => {
                        subject = kale.core;
                        stack.pop::<NockWork>();
                    }
                },
                Work10(mut diet) => {
                    match diet.todo {
                        Nock10ComputeTree => {
                            diet.todo = Nock10ComputePatch; // should we compute patch then tree?
                            *stack.top() = Work10(diet);
                            push_formula(stack, diet.tree, false);
                        }
                        Nock10ComputePatch => {
                            diet.todo = Nock10Edit;
                            diet.tree = res;
                            *stack.top() = Work10(diet);
                            push_formula(stack, diet.patch, false);
                        }
                        Nock10Edit => {
                            res = edit(stack, diet.axis.as_bitslice(), res, diet.tree);
                            stack.pop::<NockWork>();
                        }
                    }
                }
                Work11D(mut dint) => match dint.todo {
                    Nock11DComputeHint => {
                        let hint_cell = Cell::new(stack, dint.tag.as_noun(), dint.hint);
                        if let Ok(found) =
                            match_pre_hint(stack, newt, subject, hint_cell, formula, &cache)
                        {
                            res = found;
                            stack.pop::<NockWork>();
                        } else {
                            dint.todo = Nock11DComputeResult;
                            *stack.top() = Work11D(dint);
                            push_formula(stack, dint.hint, false);
                        }
                    }
                    Nock11DComputeResult => {
                        dint.todo = Nock11DDone;
                        let hint = Cell::new(stack, dint.tag.as_noun(), dint.hint).as_noun();
                        if let Ok(found) = match_post_hint(stack, newt, subject, hint, res) {
                            res = found;
                            stack.pop::<NockWork>();
                        } else {
                            dint.todo = Nock11DDone;
                            *stack.top() = Work11D(dint);
                            push_formula(stack, dint.body, false);
                        }
                    }
                    Nock11DDone => {
                        let hint = Cell::new(stack, dint.tag.as_noun(), dint.hint).as_noun();
                        let _ = match_post_hinted(stack, subject, hint, res, &mut cache);
                        stack.pop::<NockWork>();
                    }
                },
                Work11S(mut sint) => match sint.todo {
                    Nock11SComputeResult => {
                        sint.todo = Nock11SDone;
                        if let Ok(found) =
                            match_post_hint(stack, newt, subject, sint.tag.as_noun(), res)
                        {
                            res = found;
                            stack.pop::<NockWork>();
                        } else {
                            sint.todo = Nock11SDone;
                            *stack.top() = Work11S(sint);
                            push_formula(stack, sint.body, false);
                        }
                    }
                    Nock11SDone => {
                        let _ =
                            match_post_hinted(stack, subject, sint.tag.as_noun(), res, &mut cache);
                        stack.pop::<NockWork>();
                    }
                },
            };
        }
    });
    res
}

fn push_formula(stack: &mut NockStack, formula: Noun, tail: bool) {
    unsafe {
        if let Ok(formula_cell) = formula.as_cell() {
            // Formula
            match formula_cell.head().as_either_atom_cell() {
                Right(_cell) => {
                    *stack.push() = WorkCons(NockCons {
                        todo: ConsComputeHead,
                        head: formula_cell.head(),
                        tail: formula_cell.tail(),
                    });
                }
                Left(atom) => {
                    if let Ok(direct) = atom.as_direct() {
                        match direct.data() {
                            0 => {
                                if let Ok(axis_atom) = formula_cell.tail().as_atom() {
                                    *stack.push() = Work0(Nock0 { axis: axis_atom });
                                } else {
                                    panic!("Axis for 0 must be atom");
                                }
                            }
                            1 => {
                                *stack.push() = Work1(Nock1 {
                                    noun: formula_cell.tail(),
                                });
                            }
                            2 => {
                                if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                    *stack.push() = Work2(Nock2 {
                                        todo: Nock2ComputeSubject,
                                        subject: arg_cell.head(),
                                        formula: arg_cell.tail(),
                                        tail,
                                    });
                                } else {
                                    panic!("Argument for Nock 2 must be cell");
                                };
                            }
                            3 => {
                                *stack.push() = Work3(Nock3 {
                                    todo: Nock3ComputeChild,
                                    child: formula_cell.tail(),
                                });
                            }
                            4 => {
                                *stack.push() = Work4(Nock4 {
                                    todo: Nock4ComputeChild,
                                    child: formula_cell.tail(),
                                });
                            }
                            5 => {
                                if let Ok(arg_cell) = formula_cell.tail().as_cell() {
                                    *stack.push() = Work5(Nock5 {
                                        todo: Nock5ComputeLeftChild,
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
                                        *stack.push() = Work6(Nock6 {
                                            todo: Nock6ComputeTest,
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
                                    *stack.push() = Work7(Nock7 {
                                        todo: Nock7ComputeSubject,
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
                                    *stack.push() = Work8(Nock8 {
                                        todo: Nock8ComputeSubject,
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
                                        *stack.push() = Work9(Nock9 {
                                            todo: Nock9ComputeCore,
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
                                            *stack.push() = Work10(Nock10 {
                                                todo: Nock10ComputeTree,
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
                                            *stack.push() = Work11S(Nock11S {
                                                todo: Nock11SComputeResult,
                                                tag: tag_atom,
                                                body: arg_cell.tail(),
                                            });
                                        }
                                        Right(hint_cell) => {
                                            if let Ok(tag_atom) = hint_cell.head().as_atom() {
                                                *stack.push() = Work11D(Nock11D {
                                                    todo: Nock11DComputeHint,
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
