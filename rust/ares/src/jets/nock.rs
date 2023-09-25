/** Virtualization jets
 */
use crate::interpreter::{interpret, NockErr, Tone};
use crate::jets;
use crate::jets::util::slot;
use crate::jets::JetErr;
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Noun, D, T};

crate::gdb!();

//  XX: interpret should accept optional scry function and potentially produce blocked
pub fn jet_mink(
    stack: &mut NockStack,
    newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let arg = slot(subject, 6)?;
    // mink sample = [nock scry_namespace]
    //             = [[subject formula] scry_namespace]
    let v_subject = slot(arg, 4)?;
    let v_formula = slot(arg, 5)?;
    let _scry = slot(arg, 3)?;

    //  XX: no partial traces; all of our traces go down to the "home road"
    match interpret(stack, newt, v_subject, v_formula) {
        Ok(res) => Ok(T(stack, &[D(0), res])),
        Err(err) => match err {
            Tone::Blocked(block) => Ok(T(stack, &[D(1), block])),
            Tone::Error(err, trace) => match err {
                NockErr::Deterministic => Ok(T(stack, &[D(2), trace])),
                NockErr::NonDeterministic => Err(JetErr::NonDeterministic),
            },
        },
    }
}

pub mod util {
    use crate::jets;
    use crate::jets::form::util::scow;
    use crate::jets::util::rip;
    use crate::jets::{jet_mink, JetErr};
    use crate::mem::NockStack;
    use crate::newt::Newt;
    use crate::noun::{tape, Cell, Noun, D, T};
    use ares_macros::tas;
    use std::result;

    const LEAF: Noun = D(tas!(b"leaf"));
    const ROSE: Noun = D(tas!(b"rose"));

    /** Consume $tone, produce $toon
     */
    //  XX: should write a jet_mook wrapper for this function
    pub fn mook(
        stack: &mut NockStack,
        newt: &mut Option<&mut Newt>,
        tone: Cell,
        flop: bool,
    ) -> result::Result<Cell, JetErr> {
        let tag = tone.head().as_direct()?;
        let original_list = tone.tail();

        match tag.data() {
            x if x < 2 => return Ok(tone),
            x if x > 2 => return Err(JetErr::Deterministic),
            _ => {}
        };

        if unsafe { original_list.raw_equals(D(0)) } {
            return Ok(tone);
        } else if original_list.atom().is_some() {
            return Err(JetErr::Deterministic);
        }

        // XX: trim traces longer than 1024 frames

        unsafe {
            let mut res = D(0);
            let mut list = original_list;
            // Unused if flopping
            let (mut new_cell, mut new_memory) = Cell::new_raw_mut(stack);
            let mut memory = new_memory;

            // loop guaranteed to run at least once
            loop {
                if list.raw_equals(D(0)) {
                    break;
                } else if !flop && res.raw_equals(D(0)) {
                    res = new_cell.as_noun();
                } else if !flop {
                    (new_cell, new_memory) = Cell::new_raw_mut(stack);
                    (*memory).tail = new_cell.as_noun();
                    memory = new_memory
                }

                let cell = list.as_cell()?;
                let trace = cell.head().as_cell()?;
                let tag = trace.head().as_direct()?;
                let dat = trace.tail();

                let tank: Noun = match tag.data() {
                    tas!(b"mean") => {
                        if let Ok(atom) = dat.as_atom() {
                            let tape = rip(stack, 3, 1, atom)?;
                            T(stack, &[LEAF, tape])
                        } else {
                            let virt = T(stack, &[dat, dat.as_cell()?.head()]);
                            let load = T(stack, &[virt, D(0)]);
                            let subj = T(stack, &[D(0), load, D(0)]);
                            let tone = jet_mink(stack, newt, subj)?.as_cell()?;
                            if !tone.head().raw_equals(D(0)) {
                                let tape = tape(stack, "####");
                                T(stack, &[LEAF, tape])
                            } else {
                                // XX: need to check that this is actually a tank
                                //     return leaf+"mean.mook" if not
                                tone.tail()
                            }
                        }
                    }
                    tas!(b"spot") => {
                        let spot = dat.as_cell()?;
                        let pint = spot.tail().as_cell()?;
                        let pstr = pint.head().as_cell()?;
                        let pend = pint.tail().as_cell()?;

                        let colo = T(stack, &[D(b':' as u64), D(0)]);
                        let trel = T(stack, &[colo, D(0), D(0)]);

                        let smyt = smyt(stack, spot.head())?;

                        let aura = D(tas!(b"ud")).as_direct()?;
                        let str_lin = scow(stack, aura, pstr.head().as_atom()?)?;
                        let str_col = scow(stack, aura, pstr.tail().as_atom()?)?;
                        let end_lin = scow(stack, aura, pend.head().as_atom()?)?;
                        let end_col = scow(stack, aura, pend.tail().as_atom()?)?;

                        let mut list = end_col.as_cell()?;
                        loop {
                            if list.tail().atom().is_some() {
                                break;
                            }
                            list = list.tail().as_cell()?;
                        }
                        // "{end_col}]>"
                        let p4 = T(stack, &[D(b']' as u64), D(b'>' as u64), D(0)]);
                        (*list.tail_as_mut()) = p4;

                        list = end_lin.as_cell()?;
                        loop {
                            if list.tail().atom().is_some() {
                                break;
                            }
                            list = list.tail().as_cell()?;
                        }
                        // "{end_lin} {end_col}]>"
                        let p3 = T(stack, &[D(b' ' as u64), end_col]);
                        (*list.tail_as_mut()) = p3;

                        list = str_col.as_cell()?;
                        loop {
                            if list.tail().atom().is_some() {
                                break;
                            }
                            list = list.tail().as_cell()?;
                        }
                        // "{str_col}].[{end_lin} {end_col}]>"
                        let p2 = T(
                            stack,
                            &[D(b']' as u64), D(b'.' as u64), D(b'[' as u64), end_lin],
                        );
                        (*list.tail_as_mut()) = p2;

                        list = str_lin.as_cell()?;
                        loop {
                            if list.tail().atom().is_some() {
                                break;
                            }
                            list = list.tail().as_cell()?;
                        }
                        // "{str_lin} {str_col}].[{end_lin} {end_col}]>"
                        let p1 = T(stack, &[D(b' ' as u64), str_col]);
                        (*list.tail_as_mut()) = p1;

                        // "<[{str_lin} {str_col}].[{end_lin} {end_col}]>"
                        let tape = T(stack, &[D(b'<' as u64), D(b'[' as u64), str_lin]);
                        let finn = T(stack, &[LEAF, tape]);

                        T(stack, &[ROSE, trel, smyt, finn, D(0)])
                    }
                    _ => {
                        let tape = rip(stack, 3, 1, tag.as_atom())?;
                        T(
                            stack,
                            &[
                                D(tas!(b"m")),
                                D(tas!(b"o")),
                                D(tas!(b"o")),
                                D(tas!(b"k")),
                                D(tas!(b".")),
                                tape,
                            ],
                        )
                    } // XX: TODO
                      //  %hand
                      //  %hunk
                      //  %lose
                };

                if flop {
                    res = T(stack, &[tank, res]);
                } else {
                    (*memory).head = tank;
                }
                list = cell.tail();
            }

            if !flop {
                (*memory).tail = D(0);
            }

            let toon = Cell::new(stack, D(2), res);
            Ok(toon)
        }
    }

    pub fn smyt(stack: &mut NockStack, path: Noun) -> jets::Result {
        let lash = D(tas!(b"/"));
        let zero = D(0);
        let sep = T(stack, &[lash, zero]);

        let trel = T(stack, &[sep, sep, zero]);
        let tank = smyt_help(stack, path)?;

        Ok(T(stack, &[ROSE, trel, tank]))
    }

    fn smyt_help(stack: &mut NockStack, path: Noun) -> jets::Result {
        //  XX: switch to using Cell:new_raw_mut
        if unsafe { path.raw_equals(D(0)) } {
            return Ok(D(0));
        }

        let cell = path.as_cell()?;
        let tail = smyt_help(stack, cell.tail())?;
        let trip = rip(stack, 3, 1, cell.head().as_atom()?)?;
        let head = T(stack, &[LEAF, trip]);

        Ok(T(stack, &[head, tail]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::{assert_jet, init_stack};
    use crate::serf::TERMINATOR;
    use std::sync::Arc;

    #[test]
    fn init() {
        // This needs to be done because TERMINATOR is lazy allocated, and if you don't
        // do it before you call the unit tests it'll get allocated on the Rust heap
        // inside an assert_no_alloc block.
        //
        // Also Rust has no primitive for pre-test setup / post-test teardown, so we
        // do it in a test that we rely on being called before any other in this file,
        // since we're already using single-threaded test mode to avoid race conditions
        // (because Rust doesn't support test order dependencies either).
        let _ = Arc::clone(&TERMINATOR);
    }

    #[test]
    fn test_mink_success() {
        let sack = &mut init_stack();
        let subj = D(0);
        let form = T(sack, &[D(1), D(53)]);
        let nock = T(sack, &[subj, form]);
        let scry = D(0);
        let samp = T(sack, &[nock, scry]);
        let rest = T(sack, &[D(0), D(53)]);
        assert_jet(sack, jet_mink, samp, rest);
    }

    #[test]
    fn test_mink_zapzap() {
        let sack = &mut init_stack();
        let subj = D(0);
        let form = T(sack, &[D(0), D(0)]);
        let nock = T(sack, &[subj, form]);
        let scry = D(0);
        let samp = T(sack, &[nock, scry]);
        let rest = T(sack, &[D(2), D(0)]);
        assert_jet(sack, jet_mink, samp, rest);
    }

    #[test]
    fn test_mink_trace() {
        let sack = &mut init_stack();
        let subj = D(0);
        let scry = D(0);

        //  !=  !:  ?~  0  !!  53
        //  [ 11
        //    [1.953.460.339 1 [1.953.719.668 0] [1 9] [1 22]]
        //    [ 8
        //      [11 [1.953.460.339 1 [1.953.719.668 0] [1 13] [1 14]] [1 0]]
        //      [ 6
        //        [5 [1 0] [0 2]]
        //        [7 [0 3] [11 [1.953.460.339 1 [1.953.719.668 0] [1 16] [1 18]] [0 0]]]
        //        [7 [0 3] [11 [1.953.460.339 1 [1.953.719.668 0] [1 20] [1 22]] [1 53]]]
        //      ]
        //    ]
        //  ]
        //
        //  1.953.719.668 = 'test'
        //  1.953.460.339 = 'spot'
        //
        //  All of this below is because of "two-phase borrow checks"
        //      https://stackoverflow.com/questions/60686259/mutable-borrow-in-function-argument

        let hint_spot = D(1953460339);
        let hint_path = T(sack, &[D(1953719668), D(0)]);
        let hint_dyn = D(1);
        let hint_row = D(1);

        let make_hint = |sack: &mut NockStack, col_start: u64, col_end: u64| {
            let start = T(sack, &[hint_row, D(col_start)]);
            let end = T(sack, &[hint_row, D(col_end)]);

            T(sack, &[hint_spot, hint_dyn, hint_path, start, end])
        };

        let sss3s1 = T(sack, &[D(0), D(3)]);
        let sss3s2s1 = make_hint(sack, 20, 22);
        let sss3s2s2 = T(sack, &[D(1), D(53)]);
        let sss3s2 = T(sack, &[D(11), sss3s2s1, sss3s2s2]);
        let sss3 = T(sack, &[D(7), sss3s1, sss3s2]);

        let sss2s1 = sss3s1;
        let sss2s2s1 = make_hint(sack, 16, 18);
        let sss2s2s2 = T(sack, &[D(0), D(0)]);
        let sss2s2 = T(sack, &[D(11), sss2s2s1, sss2s2s2]);
        let sss2 = T(sack, &[D(7), sss2s1, sss2s2]);

        let sss1s1 = T(sack, &[D(1), D(0)]);
        let sss1s2 = T(sack, &[D(0), D(2)]);
        let sss1 = T(sack, &[D(5), sss1s1, sss1s2]);

        let ss2 = T(sack, &[D(6), sss1, sss2, sss3]);

        let ss1s1 = make_hint(sack, 13, 14);
        let ss1s2 = sss1s1;
        let ss1 = T(sack, &[D(11), ss1s1, ss1s2]);

        let s2 = T(sack, &[D(8), ss1, ss2]);
        let s1 = make_hint(sack, 9, 22);
        let form = T(sack, &[D(11), s1, s2]);

        let nock = T(sack, &[subj, form]);
        let samp = T(sack, &[nock, scry]);

        //  trace
        //  [%2 trace=~[[~.spot [[1.953.719.668 0] [1 16] 1 18]] [~.spot [[1.953.719.668 0] [1 9] 1 22]]]]
        let ttt2t1 = T(sack, &[D(1), D(9)]);
        let ttt2t2 = T(sack, &[D(1), D(22)]);
        let ttt2 = T(sack, &[hint_path, ttt2t1, ttt2t2]);

        let ttt1t1 = T(sack, &[D(1), D(16)]);
        let ttt1t2 = T(sack, &[D(1), D(18)]);
        let ttt1 = T(sack, &[hint_path, ttt1t1, ttt1t2]);

        let tt2 = T(sack, &[hint_spot, ttt2]);
        let tt1 = T(sack, &[hint_spot, ttt1]);

        let t1 = T(sack, &[tt1, tt2, D(0)]);

        let rest = T(sack, &[D(2), t1]);

        assert_jet(sack, jet_mink, samp, rest);
    }
}
