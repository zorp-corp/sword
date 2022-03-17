use noun::atom;
use noun::Noun;
use std::rc::Rc;

use std::ptr::copy_nonoverlapping;

use crate::memory;

/*
enum Work {
    Head(Rc<Noun>, Rc<Noun>),
    Tail(Rc<Noun>),
    Cell,
}

fn atom_onto_stack(stack: &mut memory::NockStack, atom: &atom::Atom) -> u64 {
    if atom.v().len() == 0 {
        0
    } else if atom.v().len() == 1 && atom.v()[0] <= memory::DIRECT_MAX {
        atom.v()[0]
    } else {
        let indirect_dest: *mut u64 = memory::alloc(stack, atom.v().len() + 1);
        unsafe {
            *indirect_dest = atom.v().len() as u64;
            copy_nonoverlapping(atom.v().as_ptr(), indirect_dest.add(1), atom.v().len());
        }
        ((indirect_dest as u64) >> 3) | memory::INDIRECT
    }
}

pub fn noun_onto_stack(stack: &mut memory::NockStack, noun: &Noun) -> u64 {
    let mut work: Vec<Work> = Vec::new();
    let mut results: Vec<u64> = Vec::new();
    match noun {
        Noun::Atom(atom) => {
            results.push(atom_onto_stack(stack, atom));
        }
        Noun::Cell(cell) => work.push(Work::Head(cell.h(), cell.t())),
    }
    loop {
        match work.pop() {
            None => {
                break;
            }
            Some(work_type) => match work_type {
                Work::Head(head_noun, tail_noun) => {
                    work.push(Work::Tail(tail_noun));
                    match &*head_noun {
                        Noun::Atom(atom) => {
                            results.push(atom_onto_stack(stack, &atom));
                        }
                        Noun::Cell(cell) => work.push(Work::Head(cell.h(), cell.t())),
                    }
                }
                Work::Tail(tail_noun) => {
                    work.push(Work::Cell);
                    match &*tail_noun {
                        Noun::Atom(atom) => {
                            results.push(atom_onto_stack(stack, &atom));
                        }
                        Noun::Cell(cell) => work.push(Work::Head(cell.h(), cell.t())),
                    }
                }
                Work::Cell => match results.pop() {
                    None => {
                        panic!("Shouldn't happen: no results when making a cell");
                    }
                    Some(tail_noun) => match results.pop() {
                        None => {
                            panic!("Shouldn't happen: no results when making a cell");
                        }
                        Some(head_noun) => {
                            results.push(memory::cell(stack, head_noun, tail_noun));
                        }
                    },
                },
            },
        }
    }
    match results.pop() {
        None => {
            panic!("shouldn't happen: no result to return")
        }
        Some(result) => result,
    }
}
*/
