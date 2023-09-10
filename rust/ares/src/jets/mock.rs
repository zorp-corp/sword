/** Virtualization jets
 */
use crate::interpreter::{interpret, NockErr};
use crate::jets;
use crate::jets::util::slot;
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Noun, D, T};

crate::gdb!();

// XX: interpret should accept optional scry function and potentially produce blocked
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
    
    match interpret(stack, newt, v_subject, v_formula) {
        Ok(res) => Ok(T(stack, &[D(0), res])),
        Err(err) => match err {
            NockErr::Blocked(block) => Ok(T(stack, &[D(1), block])),
            NockErr::Error(error) => Ok(T(stack, &[D(2), error])),
        },
    }
}

mod util {
    // +$  tone  $%  [%0 product=*]
    //               [%1 block=*]
    //               [%2 trace=(list [@ta *])]
    //           ==
    // +$  toon  $%  [%0 p=*]
    //               [%1 p=*]
    //               [%2 p=(list tank)]
    //           ==
    // ++  mook
    //   |=  ton=tone
    //   ^-  toon
    //   ?.  ?=([%2 *] ton)
    //     ton
    //   |^  [%2 (turn skip rend)]
    //   ::
    //   ++  skip
    //     ^+  trace.ton
    //     =/  yel  (lent trace.ton)                ::  get length of the trace
    //     ?.  (gth yel 1.024)  trace.ton           ::  return early if the length of the trace is less than 1024
    //     %+  weld
    //       (scag 512 trace.ton)
    //     ^+  trace.ton
    //     :_  (slag (sub yel 512) trace.ton)
    //     :-  %lose
    //     (crip "[skipped {(scow %ud (sub yel 1.024))} frames]")
    //   ::
    //   ::  +rend: raw stack frame to tank
    //   ::
    //   ::    $%  [%hunk ref=* path]            ::  failed scry ([~ ~])
    //   ::        [%lose cord]                  ::  skipped frames
    //   ::        [%hand *]                     ::  mug any
    //   ::        [%mean $@(cord (trap tank))]  ::  ~_ et al
    //   ::        [%spot spot]                  ::  source location
    //   ::    ==
    //   ::
    //   ++  rend
    //     |=  [tag=@ta dat=*]
    //     ^-  tank
    //     ?+    tag
    //     ::
    //       leaf+"mook.{(rip 3 tag)}"
    //     ::
    //         %hunk
    //       ?@  dat  leaf+"mook.hunk"
    //       =/  sof=(unit path)  ((soft path) +.dat)
    //       ?~  sof  leaf+"mook.hunk"
    //       (smyt u.sof)
    //     ::
    //         %lose
    //       ?^  dat  leaf+"mook.lose"
    //       leaf+(rip 3 dat)
    //     ::
    //         %hand
    //       leaf+(scow %p (mug dat))
    //     ::
    //         %mean
    //       ?@  dat  leaf+(rip 3 dat)
    //       =/  mac  (mack dat -.dat)
    //       ?~  mac  leaf+"####"
    //       =/  sof  ((soft tank) u.mac)
    //       ?~  sof  leaf+"mook.mean"
    //       u.sof
    //     ::
    //         %spot
    //       =/  sof=(unit spot)  ((soft spot) dat)
    //       ?~  sof  leaf+"mook.spot"
    //       :+  %rose  [":" ~ ~]
    //       :~  (smyt p.u.sof)
    //           =*  l   p.q.u.sof
    //           =*  r   q.q.u.sof
    //           =/  ud  |=(a=@u (scow %ud a))
    //           leaf+"<[{(ud p.l)} {(ud q.l)}].[{(ud p.r)} {(ud q.r)}]>"
    //       ==
    //     ==
    //   --

    /** Consume $tone, produce $toon
     */
    pub fn mook(
        stack: NockStack,
        newt: &mut Option<&mut Newt>,
        tone: Noun,
        flop: bool
    ) -> Result {
        let temp = tone.as_cell()?;
        let tag = temp.head().as_direct()?;
        let original_list = temp.tail();

        if tag.data() < 2 {
            return Ok(tone);
        } else if tag.data() > 2 {
            return Err(JetErr::Deterministic)
        }

        // XX: trim traces longer than 1024 frames
        let leaf = D(tas!(b"leaf"));
        
        if flop {
            let mut list = original_list;
            let mut res = D(0);
            loop {
                if unsafe { list.raw_equals(D(0)) } {
                    break;
                }

                let cell = list.as_cell()?;
                let trace = cell.head().as_cell()?;
                let tag = trace.head().as_direct()?;
                let dat = trace.tail();
                
                // XX: implement
                let tank: Noun = match tag.data() {
                    // *[a [9 2 [0 1]]]
                    // *[*[a [0 1]] [2 [0 1] [0 2]]]
                    // *[\[1 a] [2 [0 1] [0 2]]]
                    // *[a [2 [0 1] [0 2]]]
                    // *[*[a [0 1]] *[a [0 2]]]
                    // *[\[1 a] \[2 a]]
                    // *[a \[2 a]]
                    tas!(b"spot") => {
                        let subj = T(stack, &[D(0), dat, D(0)]);
                        let spot = jet_mink(stack, newt, subj)?;
                        if spot.as_cell()?.head().as_direct().data() != 0 {
                            // XX test this in Vere
                            // if the formula in dat crashes, we produce leaf+"mook.spot"
                            let msg = tape(stack, "mook.spot");
                            T(stack, &[leaf, msg])
                        } else {

                        }
                    }
                    // XX: TODO
                    //  %hand
                    //  %hunk
                    //  %lose
                    //  %mean
                    //  default

                };
                
                res = T(stack, &[tank, res]);
                list = cell.tail();
            }
            
            Ok(Res)
        } else {
            // XX: need non-tail recursive helper to build +mook without +flop, because no helper in noun.rs to allocate Cell and set tail later (like u3i_defcons in Vere)
            Ok(D(0))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jets::util::test::{assert_jet, init_stack};

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
