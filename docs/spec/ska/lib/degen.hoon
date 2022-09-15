:: TODO: generate labels and jumps everywhere and stick them in the code
:: table immediately, then linearize
/-  *gene
/-  *sock
/+  ska
=|  prog=tinn
=|  buff=linn
=|  bust=(list linn)
|%
++  this  .
++  inst
  |=  =dinn
  ^-  _this
  this(buff [dinn buff])
++  gene
  |=  bloc=labl
  ^-  [boot _this]
  =/  puff  (~(get by prog) bloc)
  ?.  ?=(~ puff)
    [says.u.puff this]
  =.  prog  (~(put by prog) bloc [~ [%risk %gues ~]]) :: prevent recursive functions from infinite looping the compiler
  =.  bust  [buff bust]
  =.  buff  ~
  =+  faxe=1
  =+  fate=for.bloc
  =+  ject=sub.bloc
  =|  heir=cost
  =|  vale=dast
  =<
  =^  moot  this
    |-
    ^-  [boot _this]
    ?.  (hast vale)
      ~|  'vale must not be 5 or a subaxis of 5'  !!
    ?+  fate  bomb
        [[* *] *]
      ?:  ?=  [%bab *]  heir
        bomb
      =/  tier  [%dab (bear 3)]
      =^  shed  this  $(faxe (peg faxe 2), fate -.fate, heir tier, vale 4) :: we can't clobber subject
      =.  this  (inst [%sft ~])
      =^  stal  this  $(faxe (peg faxe 3), fate +.fate, heir [%dab here], vale (peg vale 3))
      =.  this  (inst [%mov 10 (peg vale 2)])
      =.  this  (inst [%mov 11 5]) :: pop without overwriting 4
      =.  this  tale
      :_  this
      (cobb:ska shed stal)
        ::
        [%0 @]
      ?:  =(0 +.fate)
        bomb
      =.  this  (inst [%mov (peg 3 +.fate) vale])
      =.  this  bale
      :_  this
      (pull:ska +.fate ject)
        ::
        [%1 *]
      =.  this  (inst [%imm +.fate vale])
      ?:  ?=  [%bab *]  heir
        ?:  =(0 +.fate)
          =.  this  (inst [%hop troo.heir])
          =.  this  tale
          :_  this
          [%safe %know +.fate]
        ?:  =(1 +.fate)
          =.  this  (inst [%hop fals.heir])
          =.  this  tale
          :_  this
          [%safe %know +.fate]
        bomb
      =.  this  tale
      :_  this
      [%safe %know +.fate]
        ::
        [%2 * *]
      =/  shis  this
      =/  tier  [%dab (bear 7)]
      =.  this  hide
      =^  norm  this  $(faxe (peg faxe 6), fate +<.fate, heir tier, vale 4)
      ?:  ?=  [%boom ~]  norm
        bomb
      =?  this  ?=([%safe %know *] norm)  =.(this shis hide)
      =^  news  this  $(faxe (peg faxe 7), fate +>.fate, heir [%dab here], vale 3)
      ?:  ?=  [%boom ~]  news
        bomb
      =/  sewn
        ?-  news
          [%safe *]  sure.news
          [%risk *]  hope.news
        ==
      ?:  ?=([$?(%safe %risk) %know *] norm)
        =/  sabl
          ?-  norm
            [%safe *]  [sewn know.sure.norm]
            [%risk *]  [sewn know.hope.norm]
          ==
        =^  toot  this  (gene sabl)
        ?:  ?=([%ret ~] heir)
          ?.  =(vale 4)
            ~|  'Value destination for tail call should always be 4'  !!
          =.  this  (inst [%jmp sabl])
          :_  this
          ?:  ?&  ?=([%safe *] news)  ?=([%safe *] norm)  ==
            toot
          (dare:ska toot)
        =.  this  (inst [%cal sabl])
        =.  this  show
        =.  this  (inst [%mov 4 vale])
        =.  this  bran
        :_  this
        ?:  ?&  ?=([%safe *] news)  ?=([%safe *] norm)  ==
          toot
        (dare:ska toot)
      ?:  ?=([%ret ~] heir)
        ?.  =(vale 4)
          ~|  'Value destination for tail call should always be 4'  !!
        =.  this  (inst [%lnt ~])
        :_  this
        [%risk %gues ~]
      =.  this  (inst [%lnk ~])
      =.  this  show
      =.  this  (inst [%mov 4 vale])
      =.  this  bran
      :_  this
      [%risk %gues ~]
        ::
        [%3 *]
      :: TODO: statically jump to a branch if we know atom or cell
      =^  spec  this  $(faxe (peg 3 faxe), fate +.fate, heir [%dab here], vale 4)
      =.  this  (inst [%clq will wont])
      =.  this  does
      =.  this  dont
      :_  this
      (ques:ska spec)
        ::
        [%4 *]
      =^  mota  this  $(faxe (peg 3 faxe), fate +.fate, heir [%dab here])  :: leave vale the same
      =.  this  (inst [%inc vale])
      =.  this  bale
      :_  this
      (pile:ska mota) 
        ::
        [%5 * *]
      ::  TODO: statically jump to a branch if we know equal or
      ::  disequal
      =/  tier  (bear 7)
      =^  left  this  $(faxe (peg 6 faxe), fate +<.fate, heir [%dab tier], vale 4)
      =.  this  (inst [%sft ~])
      =^  rite  this  $(faxe (peg 7 faxe), fate +>.fate, heir [%dab here], vale 9)
      =.  this  (inst [%mov 10 8])
      =.  this  (inst [%mov 11 5]) :: pop without overwriting 4 XX this is probably what we want ust to do anyway
      =.  this  (inst [%eqq will wont])
      =.  this  does
      =.  this  dont
      :_  this
      (bopp:ska left rite)
        ::
        [%6 * * *]
      =/  troo  (bear 14)
      =/  fals  (bear 15)
      =/  shis  this
      =^  cond  this  $(faxe (peg 6 faxe), fate +<.fate, heir [%bab troo fals], vale 4)
      ?:  ?=  [%safe %know *]  cond
        ?:  =(0 know.sure.cond)
          =.  this  shis
          $(faxe (peg 14 faxe), fate +>-.fate) :: pass on heir and vale
        ?:  =(1 know.sure.cond)
          =.  this  shis
          $(faxe (peg 15 faxe), fate +>+.fate) :: pass on heir and vale
        bomb
      ?:  ?=  [%risk %know *]  cond
        ?:  =(0 know.hope.cond)
          =.  this  (inst [%her troo])
          =^  trus  this  $(faxe (peg 14 faxe), fate +>-.fate) :: pass on heir and vale
          :_  this
          (dare:ska trus)
        ?:  =(1 know.hope.cond)
          =.  this  (inst [%her fals])
          =^  lies  this  $(faxe (peg 15 faxe), fate +>+.fate)
          :_  this
          (dare:ska lies)
        bomb
      ?:  ?=  [%safe %bets *]  cond
        bomb
      ?:  ?=  [%risk %bets *]  cond
        bomb
      =.  this  (inst [%her troo])
      =^  trus  this  $(faxe (peg 14 faxe), fate +>-.fate) :: pass on heir and vale
      =.  this  (inst [%her fals])
      =^  lies  this  $(faxe (peg 15 faxe), fate +>+.fate) :: pass on heir and vale
      :_  this
      ?:  ?=  [%safe %flip ~]  cond
        (gnaw:ska trus lies)
      (dare:ska (gnaw:ska trus lies))
        ::
        [%7 * *]
      =.  this  hide
      =^  news  this  $(faxe (peg 6 faxe), fate +<.fate, heir [%dab (bear 7)], vale 3) :: put result in subject
      ?:  ?=  [%boom ~]  news
        bomb
      =/  sewn
        ?-  news
          [%safe *]  sure.news
          [%risk *]  hope.news
        ==
      ?:  ?=  [%ret ~]  heir
        =^  soot  this  $(faxe (peg 7 faxe), fate +>.fate, ject sewn)
        :_  this
        ?:  ?=  [%safe *]  news
          soot
        (dare:ska soot)
      ::  this is what we must do for now to make sure the subject is
      ::  cleaned up in a test expression.
      ::  TODO: some way to inject the cleanup code at the start of
      ::  the branches so we can still branch directly
      =^  soot  this  $(faxe (peg 7 faxe), fate +>.fate, heir [%dab here], vale 4, ject sewn)
      =.  this  show
      =.  this  (inst [%mov 4 vale])
      =.  this  bran
      :_  this
      ?:  ?=  [%safe *]  news
        soot
      (dare:ska soot)
        ::
        [%8 * *]
      =^  news  this  $(faxe (peg 6 faxe), fate +<.fate, heir [%dab (bear 7)], vale 8) :: store in head of result
      ?:  ?=  [%boom ~]  news
        bomb
      =/  sewn
        ?-  news
          [%safe *]  sure.news
          [%risk *]  hope.news
        ==
      =/  newp  (knit:ska sewn ject)
      =.  this  (inst [%mov 3 9]) :: copy subject to tail of result
      =.  this  (inst [%mov 4 3]) :: copy cell back to subject
      ?:  ?=  [%ret ~]  heir
        =^  hoot  this  $(faxe (peg 7 faxe), fate +>.fate, ject newp)
        :_  this
        ?:  ?=  [%safe *]  news
          hoot
        (dare:ska hoot)
      =^  hoot  this  $(faxe (peg 7 faxe), fate +>.fate, ject newp, heir [%dab here], vale 4)
      =.  this  (inst [%mov 7 3]) :: put the subject back
      =.  this  (inst [%mov 4 vale]) :: put the result in the right place
      :_  this
      ?:  ?=  [%safe *]  news
        hoot
      (dare:ska hoot)
      ::
      [%9 @ *]
    =.  this  hide
    =^  bore  this  $(faxe (peg 7 faxe), fate +>.fate, heir [%dab (bear 6)], vale 3)
    ?:  ?=  [%boom ~]  bore
      bomb
    =/  sore
      ?-  bore
        [%safe *]  sure.bore
        [%risk *]  hope.bore
      ==
    =/  norm  (pull:ska +<.fate sore)
    ?:  ?=  [%boom ~]  norm
      bomb
    =?  this  ?!(?=([%safe %know *] norm))  (inst [%mov (peg 3 +<.fate) 4]) :: look up axis
    ?:  ?=([$?(%safe %risk) %know *] norm)
      =/  sabl
        ?-  norm
          [%safe %know *]  [sore know.sure.norm]
          [%risk %know *]  [sore know.hope.norm]
        ==
      =^  noot  this  (gene sabl)
      ?:  ?=  [%ret ~]  heir
        =.  this  (inst [%jmp sabl])
        :_  this
        ?:  ?&(?=([%safe *] bore) ?=([%safe *] norm))
          noot
        (dare:ska noot)
      =.  this  (inst [%cal sabl])
      =.  this  show
      =.  this  (inst [%mov 4 vale])
      =.  this  bran
      :_  this
      ?:  ?&(?=([%safe *] bore) ?=([%safe *] norm))
        noot
      (dare:ska noot)
    ?:  ?=  [%ret ~]  heir
      =.  this  (inst [%lnt ~])
      :_  this
      [%risk %gues ~]
    =.  this  (inst [%lnk ~])
    =.  this  show
    =.  this  (inst [%mov 4 vale])
    =.  this  bran
    :_  this
    [%risk %gues ~]
        ::
        [%10 [@ *] *]
      =^  soot  this  $(faxe (peg 13 faxe), fate +<+.fate, heir [%dab (bear 7)], vale 4)
      =.  this  (inst [%sft ~])
      =^  toot  this  $(faxe (peg 7 faxe), fate +>.fate, heir [%dab here]) :: write tree to destination
      =.  this  (inst [%ust ~])
      =.  this  (inst [%mov 4 (peg vale +<-.fate)]) :: write patch to axis under destination
      :_  this
      (welt:ska +<-.fate soot toot)
        ::
        [%11 @ *]
      $(faxe (peg 7 faxe), fate +>.fate)
        ::
        [%11 [@ *] *]
      =^  hoot  this  $(faxe (peg 13 faxe), fate +<+.fate, heir [%dab (bear 7)], vale 4)
      ?:  ?=  [%boom ~]  hoot
        bomb
      =^  root  this  $(faxe (peg 7 faxe), fate +>.fate)
      :_  this
      ?:  ?=  [%safe *]  hoot
        root
      (dare:ska root)
        ::
        [%12 * *]
      =^  root  this  $(faxe (peg 6 faxe), fate +<.fate, heir [%dab (bear 7)], vale 8)
      ?:  ?=  [%boom ~]  root
        bomb
      =^  soot  this  $(faxe (peg 7 faxe), fate +>.fate, heir [%dab here], vale 9)
      ?:  ?=  [%boom ~]  soot
        bomb
      =.  this  (inst [%spy ~])
      =.  this  (inst [%mov 4 vale])
      :_  this
      [%risk %gues ~]
    ==
  :_  this(prog (~(put by prog) bloc [(flop buff) moot]), buff -.bust, bust +.bust)
  moot
  |%
  ++  bomb
    ^-  [boot _this]
    =.  this  (inst [%bom ~])
    :_  this
    [%boom ~]
  ++  bear
    |=  weir=@
    ^-  dabl
    [sub.bloc for.bloc (peg faxe weir) 0]
  ++  here
    ^-  dabl
    [sub.bloc for.bloc faxe 1]
  ++  will
    ^-  dabl
    [sub.bloc for.bloc faxe 2]
  ++  wont
    ^-  dabl
    [sub.bloc for.bloc faxe 3]
  :: hide away a subject for later (clobbers 4)
  ++  hide
    =.  this  (inst [%mov 3 4])
    (inst [%sft ~]) :: subject is now in 10
  :: put back a subject that was hidden away (does not clobber 4)
  ++  show
    =.  this  (inst [%mov 10 3]) :: put back the subject
    =.  this  (inst [%mov 4 10]) :: put the result where it will get shifted back into result space
    (inst [%ust ~])
  ++  bran
    ?:  ?=  [%bab *]  heir
      (inst [%brn troo.heir fals.heir])
    this
  ++  tale
    ?:  ?=  [%ret ~]  heir
      ?.  =(4 vale)
        ~|  'Value destination for tail call should always be 4'  !!
      (inst [%don ~])
    this
  ++  bale
    =.  this  bran
    tale
  ++  does
    =.  this  (inst [%her will])
    =.  this  (inst [%imm 0 vale])
    ?-  heir
      [%dab *]  (inst [%hop wher.heir])
      [%bab *]  (inst [%hop troo.heir])
      [%ret ~]  (inst [%don ~])
    ==
  ++  dont
    =.  this  (inst [%her wont])
    =.  this  (inst [%imm 1 vale])
    ?-  heir
      [%dab *]  (inst [%hop wher.heir])
      [%bab *]  (inst [%hop fals.heir])
      [%ret ~]  (inst [%don ~])
    ==
  --
::  assert correctness of a dast: must not be 5 or a subaxis of 5
++  hast
  |=  wast=dast
  ^-  ?
  ?.  (lth wast 5)
    ?.  =(wast 5)
      $(wast (rsh [0 1] wast))
    %.n
  %.y
--
