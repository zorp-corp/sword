/** Virtualization jets
 */
use crate::hamt::Hamt;
use crate::interpreter::{interpret, Context, Error};
use crate::jets::util::slot;
use crate::jets::{JetErr, Result};
use crate::noun::{Cell, Noun, D, NO, T, YES};

crate::gdb!();

// Deterministic scry crashes should have the following behaviour:
//
//  root                <-- bail, %crud
//      mink            <-- return Deterministic
//  scry                <-- return ScryCrashed
//
//  root                <-- bail, %crud
//      mink            <-- return Deterministic
//          mink        <-- return ScryCrashed
//      scry            <-- return ScryCrashed
//  scry                <-- return ScryCrashed
//
//  root
//      mink            <-- return Tone
//          mink        <-- return Deterministic
//              mink    <-- return ScryCrashed
//          scry        <-- return ScryCrashed
//      scry            <-- return ScryCrashed
//  scry
//
pub fn jet_mink(context: &mut Context, subject: Noun) -> Result {
    let arg = slot(subject, 6)?;
    // mink sample = [nock scry_namespace]
    //             = [[subject formula] scry_namespace]
    let v_subject = slot(arg, 4)?;
    let v_formula = slot(arg, 5)?;
    let scry_handler = slot(arg, 3)?;

    let old_cache = context.cache;
    let old_scry_stack = context.scry_stack;

    context.cache = Hamt::<Noun>::new();
    context.scry_stack = T(&mut context.stack, &[scry_handler, old_scry_stack]);

    match util::mink(context, v_subject, v_formula) {
        Ok(noun) => {
            context.cache = old_cache;
            context.scry_stack = old_scry_stack;
            Ok(noun)
        }
        Err(error) => match error {
            Error::NonDeterministic(_) => Err(JetErr::Fail(error)),
            Error::ScryCrashed(trace) => {
                // When we enter a +mink call, we record the state of the scry handler stack at the
                // time (i.e. the Noun representing (list scry)). Each scry will pop the head off of
                // this scry handler stack and calls interpret(), using the rest of the scry handler
                // stack in the event that it scries again recursively. When a scry succeeds, it
                // replaces the scry handler that it used by pushing it back onto the top of the
                // scry handler stack. However, it never does so when it fails. Therefore, we can
                // tell which particular virtualization instance failed by comparing the scry
                // handler stack at the time of failure (i.e. the scry handler stack in the context
                // after a failed scry) with the scry handler stack at the time of the virtualization
                // call. Thus, whenever a virtualized interpret() call fails with a
                // Error::ScryCrashed, jet_mink() compares the two scry handler stack Nouns> If they
                // are identical, jet_mink() bails with Error::Deterministic. Otherwise, it forwards
                // the Error::ScryCrashed to the senior virtualization call.
                if unsafe { context.scry_stack.raw_equals(old_scry_stack) } {
                    Err(JetErr::Fail(Error::Deterministic(trace)))
                } else {
                    Err(JetErr::Fail(error))
                }
            }
            Error::Deterministic(_) | Error::ScryBlocked(_) => {
                panic!("scry: mink: unhandled errors in helper")
            }
        },
    }
}

pub fn jet_mole(context: &mut Context, subject: Noun) -> Result {
    jet_mure(context, subject)
}

pub fn jet_mule(context: &mut Context, subject: Noun) -> Result {
    jet_mute(context, subject)
}

pub fn jet_mure(context: &mut Context, subject: Noun) -> Result {
    let tap = slot(subject, 6)?;
    let fol = util::slam_gate_fol(&mut context.stack);
    let scry = util::pass_thru_scry(&mut context.stack);

    context.scry_stack = T(&mut context.stack, &[scry, context.scry_stack]);
    match interpret(context, tap, fol) {
        Ok(res) => {
            context.scry_stack = context.scry_stack.as_cell()?.tail();
            Ok(T(&mut context.stack, &[D(0), res]))
        }
        Err(error) => match error {
            // Since we are using the pass-through scry handler, we know for a fact that a scry
            // crash must have come from a senior virtualization context.
            Error::NonDeterministic(_) | Error::ScryCrashed(_) => Err(JetErr::Fail(error)),
            Error::Deterministic(_) | Error::ScryBlocked(_) => Ok(D(0)),
        },
    }
}

pub fn jet_mute(context: &mut Context, subject: Noun) -> Result {
    let tap = slot(subject, 6)?;
    let fol = util::slam_gate_fol(&mut context.stack);
    let scry = util::pass_thru_scry(&mut context.stack);

    context.scry_stack = T(&mut context.stack, &[scry, context.scry_stack]);
    match interpret(context, tap, fol) {
        Ok(res) => {
            context.scry_stack = context.scry_stack.as_cell()?.tail();
            Ok(T(&mut context.stack, &[YES, res]))
        }
        Err(error) => match error {
            // Since we are using the pass-through scry handler, we know for a fact that a scry
            // crash must have come from a senior virtualization context.
            Error::NonDeterministic(_) | Error::ScryCrashed(_) => Err(JetErr::Fail(error)),
            Error::ScryBlocked(path) => {
                //  XX: Need to check that result is actually of type path
                //      return [[%leaf "mute.hunk"] ~] if not
                let bon = util::smyt(&mut context.stack, path)?;
                Ok(T(&mut context.stack, &[NO, bon, D(0)]))
            }
            Error::Deterministic(trace) => {
                let ton = Cell::new(&mut context.stack, D(2), trace);
                let tun = util::mook(context, ton, false)?;
                Ok(T(&mut context.stack, &[NO, tun.tail()]))
            }
        },
    }
}

pub mod util {
    use crate::interpreter::{interpret, Context, Error};
    use crate::jets;
    use crate::jets::bits::util::rip;
    use crate::jets::form::util::scow;
    use crate::jets::JetErr;
    use crate::mem::NockStack;
    use crate::noun::{tape, Cell, Noun, D, T};
    use ares_macros::tas;
    use either::{Left, Right};
    use std::result;

    pub const LEAF: Noun = D(tas!(b"leaf"));
    pub const ROSE: Noun = D(tas!(b"rose"));

    /// The classic "slam gate" formula.
    pub fn slam_gate_fol(stack: &mut NockStack) -> Noun {
        T(stack, &[D(9), D(2), D(0), D(1)])
    }

    /// The classic "pass-through" scry handler.
    pub fn pass_thru_scry(stack: &mut NockStack) -> Noun {
        //  > .*  0  !=  =>  ~  |=(a=^ ``.*(a [%12 [%0 2] %0 3]))
        // [[[1 0] [1 0] 2 [0 6] 1 12 [0 2] 0 3] [0 0] 0]
        let sig = T(stack, &[D(1), D(0)]);
        let sam = T(stack, &[D(0), D(6)]);
        let hed = T(stack, &[D(0), D(2)]);
        let tel = T(stack, &[D(0), D(3)]);
        let zap = T(stack, &[D(0), D(0)]);

        let cry = T(stack, &[D(12), hed, tel]);
        let fol = T(stack, &[D(1), cry]);
        let res = T(stack, &[D(2), sam, fol]);
        let uno = T(stack, &[sig, res]);
        let dos = T(stack, &[sig, uno]);

        let gat = T(stack, &[zap, D(0)]);

        T(stack, &[dos, gat])
    }

    pub fn mink(context: &mut Context, subject: Noun, formula: Noun) -> Result<Noun, Error> {
        match interpret(context, subject, formula) {
            Ok(res) => Ok(T(&mut context.stack, &[D(0), res])),
            Err(err) => match err {
                Error::ScryBlocked(path) => Ok(T(&mut context.stack, &[D(1), path])),
                Error::Deterministic(trace) => Ok(T(&mut context.stack, &[D(2), trace])),
                Error::NonDeterministic(_) | Error::ScryCrashed(_) => Err(err),
            },
        }
    }

    /** Consume $tone, produce $toon
     */
    //  XX: should write a jet_mook wrapper for this function
    pub fn mook(context: &mut Context, tone: Cell, flop: bool) -> result::Result<Cell, JetErr> {
        let tag = tone.head().as_direct()?;
        let original_list = tone.tail();

        match tag.data() {
            x if x < 2 => return Ok(tone),
            x if x > 2 => return Err(JetErr::Fail(Error::Deterministic(D(0)))),
            _ => {}
        }

        if unsafe { original_list.raw_equals(D(0)) } {
            return Ok(tone);
        } else if original_list.atom().is_some() {
            return Err(JetErr::Fail(Error::Deterministic(D(0))));
        }

        // XX: trim traces longer than 1024 frames

        unsafe {
            let mut res = D(0);
            let mut list = original_list;
            // Unused if flopping
            let (mut new_cell, mut new_memory) = Cell::new_raw_mut(&mut context.stack);
            let mut memory = new_memory;

            // loop guaranteed to run at least once
            loop {
                if list.raw_equals(D(0)) {
                    break;
                } else if !flop && res.raw_equals(D(0)) {
                    res = new_cell.as_noun();
                } else if !flop {
                    (new_cell, new_memory) = Cell::new_raw_mut(&mut context.stack);
                    (*memory).tail = new_cell.as_noun();
                    memory = new_memory
                }

                let cell = list.as_cell()?;
                let trace = cell.head().as_cell()?;
                let tag = trace.head().as_direct()?;
                eprintln!("mook trace tag: {}", tag);
                eprintln!("mook-list-head cold state null? {}", context.cold.is_null());
                let dat = trace.tail();
                //eprintln!("mook dat: {}", dat);

                let tank: Noun = match tag.data() {
                    tas!(b"hunk") => match dat.as_either_atom_cell() {
                        Left(_) => {
                            let stack = &mut context.stack;
                            let tape = tape(stack, "mook.hunk");
                            T(stack, &[LEAF, tape])
                        }
                        Right(cell) => {
                            //  XX: need to check that this is actually a path
                            //      return leaf+"mook.hunk" if not
                            let path = cell.tail();
                            smyt(&mut context.stack, path)?
                        }
                    },
                    tas!(b"mean") => match dat.as_either_atom_cell() {
                        Left(atom) => {
                            let stack = &mut context.stack;
                            let tape = rip(stack, 3, 1, atom)?;
                            T(stack, &[LEAF, tape])
                        }
                        Right(cell) => {
                            'tank: {
                                eprintln!("mook-call-mink: cold state null before? {}", context.cold.is_null());
                                let res = mink(context, dat, cell.head());
                                eprintln!("mook-call-mink: cold state null after? {}", context.cold.is_null());
                                if let Ok(tone) = res {
                                    if let Some(cell) = tone.cell() {
                                        if cell.head().raw_equals(D(0)) {
                                            //  XX: need to check that this is
                                            //      actually a path;
                                            //      return leaf+"mook.mean" if not
                                            break 'tank cell.tail();
                                        }
                                    }
                                }

                                let stack = &mut context.stack;
                                let tape = tape(stack, "####");
                                T(stack, &[LEAF, tape])
                            }
                        }
                    },
                    tas!(b"spot") => {
                        eprintln!("spot cold state null? {}", context.cold.is_null());
                        let stack = &mut context.stack;

                        eprintln!("spot cold state null 2? {}", context.cold.is_null());

                        let spot = dat.as_cell()?;
                        eprintln!("spot cold state null 3? {}", context.cold.is_null());
                        let pint = spot.tail().as_cell()?;
                        eprintln!("spot cold state null 4? {}", context.cold.is_null());
                        let pstr = pint.head().as_cell()?;
                        eprintln!("spot cold state null 5? {}", context.cold.is_null());
                        let pend = pint.tail().as_cell()?;
                        eprintln!("spot cold state null 6? {}", context.cold.is_null());

                        let colo = T(stack, &[D(b':' as u64), D(0)]);
                        eprintln!("spot cold state null 7? {}", context.cold.is_null());
                        let trel = T(stack, &[colo, D(0), D(0)]);
                        eprintln!("spot cold state null 8? {}", context.cold.is_null());

                        let smyt = smyt(stack, spot.head())?;
                        eprintln!("spot cold state null 9? {}", context.cold.is_null());

                        let aura = D(tas!(b"ud")).as_direct()?;
                        eprintln!("spot cold state null 10? {}", context.cold.is_null());
                        let str_lin = scow(stack, aura, pstr.head().as_atom()?)?;
                        eprintln!("spot cold state null 11? {}", context.cold.is_null());
                        let str_col = scow(stack, aura, pstr.tail().as_atom()?)?;
                        eprintln!("spot cold state null 12? {}", context.cold.is_null());
                        let end_lin = scow(stack, aura, pend.head().as_atom()?)?;
                        eprintln!("spot cold state null 13? {}", context.cold.is_null());
                        let end_col = scow(stack, aura, pend.tail().as_atom()?)?;
                        eprintln!("spot cold state null 14? {}", context.cold.is_null());

                        let mut list = end_col.as_cell()?;
                        eprintln!("spot cold state null 15? {}", context.cold.is_null());
                        loop {
                            if list.tail().atom().is_some() {
                                break;
                            }
                            list = list.tail().as_cell()?;
                        }
                        eprintln!("spot cold state null 16? {}", context.cold.is_null());
                        // "{end_col}]>"
                        let p4 = T(stack, &[D(b']' as u64), D(b'>' as u64), D(0)]);
                        eprintln!("spot cold state null 16? {}", context.cold.is_null());
                        (*list.tail_as_mut()) = p4;

                        eprintln!("spot cold state null 17? {}", context.cold.is_null());

                        list = end_lin.as_cell()?;

                        eprintln!("spot cold state null 18? {}", context.cold.is_null());
                        loop {
                            if list.tail().atom().is_some() {
                                break;
                            }
                            list = list.tail().as_cell()?;
                        }
                        eprintln!("spot cold state null 19? {}", context.cold.is_null());
                        // "{end_lin} {end_col}]>"
                        let p3 = T(stack, &[D(b' ' as u64), end_col]);
                        eprintln!("spot cold state null? {} 20", context.cold.is_null());
                        (*list.tail_as_mut()) = p3;
                        eprintln!("spot cold state null? {} 21", context.cold.is_null());

                        list = str_col.as_cell()?;
                        eprintln!("spot cold state null? {} 22", context.cold.is_null());
                        loop {
                            if list.tail().atom().is_some() {
                                break;
                            }
                            list = list.tail().as_cell()?;
                        }
                        eprintln!("spot cold state null 23? {}", context.cold.is_null());
                        // "{str_col}].[{end_lin} {end_col}]>"
                        let p2 = T(
                            stack,
                            &[D(b']' as u64), D(b'.' as u64), D(b'[' as u64), end_lin],
                        );
                        eprintln!("spot cold state null 24? {}", context.cold.is_null());
                        (*list.tail_as_mut()) = p2;
                        eprintln!("spot cold state null 25? {}", context.cold.is_null());

                        list = str_lin.as_cell()?;
                        eprintln!("spot cold state null 26? {}", context.cold.is_null());
                        loop {
                            if list.tail().atom().is_some() {
                                break;
                            }
                            list = list.tail().as_cell()?;
                        }
                        eprintln!("spot cold state null 27? {}", context.cold.is_null());
                        // "{str_lin} {str_col}].[{end_lin} {end_col}]>"
                        let p1 = T(stack, &[D(b' ' as u64), str_col]);
                        eprintln!("spot cold state null 28? {}", context.cold.is_null());
                        (*list.tail_as_mut()) = p1;
                        eprintln!("spot cold state null 30? {}", context.cold.is_null());

                        // "<[{str_lin} {str_col}].[{end_lin} {end_col}]>"
                        let tape = T(stack, &[D(b'<' as u64), D(b'[' as u64), str_lin]);
                        eprintln!("spot cold state null 31? {}", context.cold.is_null());
                        let finn = T(stack, &[LEAF, tape]);
                        eprintln!("spot cold state null 32? {}", context.cold.is_null());

                        T(stack, &[ROSE, trel, smyt, finn, D(0)])
                    }
                    _ => {
                        let stack = &mut context.stack;
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
                      //  %lose
                };

                if flop {
                    res = T(&mut context.stack, &[tank, res]);
                } else {
                    (*memory).head = tank;
                }
                eprintln!("spot cold state null 33? {}", context.cold.is_null());
                list = cell.tail();
            }

            if !flop {
                (*memory).tail = D(0);
            }

            let toon = Cell::new(&mut context.stack, D(2), res);
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
    use crate::jets::util::test::{assert_jet, init_context};
    use crate::mem::NockStack;
    use crate::noun::{D, T};
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
        let context = &mut init_context();
        let stack = &mut context.stack;

        let subj = D(0);
        let form = T(stack, &[D(1), D(53)]);
        let nock = T(stack, &[subj, form]);
        let scry = D(0);
        let samp = T(stack, &[nock, scry]);
        let rest = T(stack, &[D(0), D(53)]);
        assert_jet(context, jet_mink, samp, rest);
    }

    #[test]
    fn test_mink_zapzap() {
        let context = &mut init_context();
        let stack = &mut context.stack;

        let subj = D(0);
        let form = T(stack, &[D(0), D(0)]);
        let nock = T(stack, &[subj, form]);
        let scry = D(0);
        let samp = T(stack, &[nock, scry]);
        let rest = T(stack, &[D(2), D(0), D(0)]);
        assert_jet(context, jet_mink, samp, rest);
    }

    #[test]
    fn test_mink_trace() {
        let context = &mut init_context();
        let stack = &mut context.stack;
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
        let hint_path = T(stack, &[D(1953719668), D(0)]);
        let hint_dyn = D(1);
        let hint_row = D(1);

        let make_hint = |stack: &mut NockStack, col_start: u64, col_end: u64| {
            let start = T(stack, &[hint_row, D(col_start)]);
            let end = T(stack, &[hint_row, D(col_end)]);

            T(stack, &[hint_spot, hint_dyn, hint_path, start, end])
        };

        let sss3s1 = T(stack, &[D(0), D(3)]);
        let sss3s2s1 = make_hint(stack, 20, 22);
        let sss3s2s2 = T(stack, &[D(1), D(53)]);
        let sss3s2 = T(stack, &[D(11), sss3s2s1, sss3s2s2]);
        let sss3 = T(stack, &[D(7), sss3s1, sss3s2]);

        let sss2s1 = sss3s1;
        let sss2s2s1 = make_hint(stack, 16, 18);
        let sss2s2s2 = T(stack, &[D(0), D(0)]);
        let sss2s2 = T(stack, &[D(11), sss2s2s1, sss2s2s2]);
        let sss2 = T(stack, &[D(7), sss2s1, sss2s2]);

        let sss1s1 = T(stack, &[D(1), D(0)]);
        let sss1s2 = T(stack, &[D(0), D(2)]);
        let sss1 = T(stack, &[D(5), sss1s1, sss1s2]);

        let ss2 = T(stack, &[D(6), sss1, sss2, sss3]);

        let ss1s1 = make_hint(stack, 13, 14);
        let ss1s2 = sss1s1;
        let ss1 = T(stack, &[D(11), ss1s1, ss1s2]);

        let s2 = T(stack, &[D(8), ss1, ss2]);
        let s1 = make_hint(stack, 9, 22);
        let form = T(stack, &[D(11), s1, s2]);

        let nock = T(stack, &[subj, form]);
        let samp = T(stack, &[nock, scry]);

        //  trace
        //  [%2 trace=~[[~.spot [[1.953.719.668 0] [1 16] 1 18]] [~.spot [[1.953.719.668 0] [1 9] 1 22]]]]
        let ttt2t1 = T(stack, &[D(1), D(9)]);
        let ttt2t2 = T(stack, &[D(1), D(22)]);
        let ttt2 = T(stack, &[hint_path, ttt2t1, ttt2t2]);

        let ttt1t1 = T(stack, &[D(1), D(16)]);
        let ttt1t2 = T(stack, &[D(1), D(18)]);
        let ttt1 = T(stack, &[hint_path, ttt1t1, ttt1t2]);

        let tt2 = T(stack, &[hint_spot, ttt2]);
        let tt1 = T(stack, &[hint_spot, ttt1]);

        let t1 = T(stack, &[tt1, tt2, D(0)]);

        let rest = T(stack, &[D(2), t1, D(0)]);

        assert_jet(context, jet_mink, samp, rest);
    }
}
