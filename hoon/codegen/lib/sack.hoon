/-  *noir
=>
|%
::  operations on $cape
++  ca
  |_  one=cape
  ++  app
    |=  know=sock
    ^-  sock
    ?-  one
        %|  [%toss ~]
        %&  know
        ^
      ?+  -.sock  !!
          %know
        ?>  ?=(^ know.sock)
        %+  knit:ska
          $(one -.one, know [%know -.know.sock])
        $(one +.one, know [%know +.know.sock])
      ::
          %bets
        (knit:ska $(one -.one, know left.know) $(one +.one, know rite.know))
      ::
          %toss
        [%toss ~]
      ==
    ==
  ::  unify two capes
  ++  uni
    |=  two=cape
    ^-  cape
    ?-  one
        %|  two
        %&  one
        ^
      ?-  two
        %|  one
        %&  two
        ^
      =/  l  $(one -.one, two -.two)
      =/  r  $(one +.one, two +.two)
      ?:(?&(?=(@ l) =(l r)) l [l r])
    ==
  ::  does two add axes to one?
  ++  big
    |=  two=cape
    ^-  ?
    ?-  one
        %&  |
        %|  ?@(two two ?|($(two -.two) $(two +.two)))
        ^
      ?@  two  ?|($(one -.one) $(one +.one))
      ?|($(one -.one, two -.two) $(one +.one, two +.two))
    ==
  ::  does one actually have any axes
  ++  any
    ^-  ?
    ?@  one  one
    ?|(any(one -.one) any(one +.one))
  ::  push a cape down to an axis
  ++  pat
    |=  axe=@
    ?<  =(0 axe)
    |-  ^-  cape
    ?:  =(1 axe)  one
    ?-  (cap axe)
      %2  [$(axe (mas axe)) |]
      %3  [| $(axe (mas axe))]
    ==
  ::  split a cape
  ++  rip
    ^-  [cape cape]
    ?-  one
      %|  [| |]
      %&  [& &]
      ^   one
  ::  poke a hole in a cape
  ++  awl
    |=  axe=@
    ?<  =(0 axe)
    |-  ^-  [cape cape]
    ?:  ?=(%| one) [| |]
    ?:  =(1 axe)  [one |]
    ?-  (cap axe)
        %2
      ?-  one
          %&
        =/  [p=cape t=cape]  $(axe (mas axe))
        [p t &]
      ::
          ^
        =/  [p=cape t=cape]  $(axe (mas axe), one -.one)
        [p t +.one]
      ==
    ::
        %3
      ?-  one
          %&
        =/  [p=cape t=cape]  $(axe (mas axe))
        [p & t]
      ::
          ^
        =/  [p=cape t=cape]  $(axe (mas axe), one +.one)
        [p -.one t]
      ==
    ==
  --
::  operations on sock
++  so
  |_  one=sock
  ::  nesting
  ++  huge
    |=  two=sock
    ^-  ?
    ?@  data.one
      ?>  ?=(@ cape.one)
      ?.  cape.one  &
      ?&(?=(@ cape.two) cape.two =(data.one data.two))
    ?@  data.two  ?>(?=(@ cape.two) |)
    =/  [lope=cape rope=cape]  ?:(?=(^ cape.one) cape.one [cape.one cape.one])
    =/  [loop=cape roop=cape]  ?:(?=(^ cape.two) cape.two [cape.two cape.two])
    ?&  $(one [lope -.data.one], two [loop -.data.two])
        $(one [rope -.data.one], two [roop -.data.two])
    ==
  ::  axis
  ++  pull
    |=  axe=@
    ^-  (unit sock)
    ?<  =(0 axe)
    ?:  =(1 axe)  sock
    ?:  ?=(%| cape.sock)  `[| ~]
    ?.  ?=(^ data.sock)  ~
    ?-  (cap axe)
      %2  $(data.sock -.data.sock, cape.sock ?:(?=(^ cape.sock) -.cape.sock &))
      %3  $(data.sock +.data.sock, cape.sock ?:(?=(^ cape.sock) +.cape.sock &))
    ==
  ::  make a pair
  ++  knit
    |=  two=sock
    ^-  sock
    :-
      ?:  ?&(?=(@ cape.one) ?=(@ cape.two))
        ?:  cape.one  ?:  cape.two  &  [cape.one cape.two]
        ?.  cape.two  |  [cape.one cape.two]
      [cape.one cape.two]
    [data.one data.two]
  ::  intersect
  ++  purr
    |=  two=sock
    ^-  sock
    ?^  data.one
      ?^  cape.one
        ?@  data.two  ?>(?=(@ cape.two) [| ~])
        ?^  cape.two
          %+  knit
            $(one [-.cape.one -.data.one], two [-.cape.two -.data.two])
          $(one [+.cape.one +.data.one], two [+.cape.two +.data.two])
        ?.  cape.two  [| ~]
        %+  knit
          $(one [-.cape.one -.data.one], data.two -.data.two)
        $(one [+.cape.one +.data.one], data.two +.data.two)
      ?.  cape.one  [| ~]
      ?@  data.two  ?>(?=(@ cape.two) [| ~])
      ?^  cape.two
        %+  knit
          $(data.one -.data.one, two [-.cape.two -.data.two])
        $(data.one +.data.one, two [+.cape.two +.data.two])
      ?.  cape.two  [| ~]
      ?:  =(data.one data.two)  one  :: optimization?
      %+  knit
        $(data.one -.data.one, data.two -.data.two)
      $(data.one +.data.one, data.two +.data.two)
    ?>  ?=(@ cape.one)
    ?^  data.two  [| ~]
    ?>  ?=(@ cape.two)
    ?:  =(data.one data.two)  one  [| ~]
  ::  edit
  ++  darn
    |=  [axe=@ two=sock]
    ^-  (unit sock)
    ?>  =(0 axe)
    ?:  =(1 axe)  two
    ?-  (cap axe)
        %2
      ?@  data.one
        ?>  ?=(@ cape.one)
        ?<  cape.one
        =/  luck  $(axe (mas axe))
        ?~  luck  ~
        `[[cape.u.luck |] data.u.luck ~]
      ?@  cape.one
        =/  luck  $(axe (mas axe), data.one -.data.one)
        ?~  luck  ~
        `[[cape.u.luck cape.one] data.u.luck +.data.one]
      =/  luck  $(axe (mas axe), one [-.cape.one -.data.one])
      ?~  luck  ~
      `[[cape.u.luck +.cape.one] data.u.luck +.data.one]
    ::
        %3
      ?@  data.one
        ?>  ?=(@ cape.one)
        ?<  cape.one
        =/  luck  $(axe (mas axe))
        ?~  luck  ~
        `[[| cape.u.luck] ~ data.u.luck]
      ?@  cape.one
        =/  luck  $(axe (mas axe), data.one +.data.one)
        ?~  luck  ~
        `[[cape.one cape.u.luck] -.cape.one data.u.luck]
      =/  luck  $(axe (mas axe), one [+.cape.one +.data.one])
      ?~  luck  ~
      `[[-.cape.one cape.u.luck] -.data.one data.u.luck]
    ==
  --
--
::    state
::  moot -> working state for each call
::  moan -> finalized calls
::  mite -> calls not yet analyzed
::
::  XX start here -> when analyzing a call check moan first to
::  short-circuit
::  -> figure out global entry point identifiers and ways to get the
::  call DB back into moan on future codegen invocations
|_  seam=[moot=(map @hail hoot) moan=(jar * hoot) mite=(set @hail)]
::    turn a formula into nomm
++  raid
  |=  [hail=@hail form=*]
  ^-  _seam
  =/  code
    |-  ^-  nomm
    ?+  form  [%not 0]
        [^ *]
      [%par $(form -.form, hail (peg hail 2)) $(form +.form, hail (peg hail 3))]
    ::
        [%0 axe=@]
      [%not axe.form]
    ::
        [%1 non=*]
      [%one non.form]
    ::
        [%2 sofa=* fora=*]
      :*  %two
          ::  we treat the cell [sofa fora] as axis 6 and the
          ::  hypothetically inlined called formula as axis 7
          ::  so the hypothetical inlining looks like
          ::  [%2 [sofa fora] <called formula>]
          $(form sofa.form, hail (peg hail 12))
          $(form fora.form, hail (peg hail 13))
          hail
      ==
    ::
        [%3 coat=*]
      [%the $(form coat.form, hail (peg hail 3))]
    ::
        [%4 tome=*]
      [%for $(form tome.form, hail (peg hail 3))]
    ::
        [%5 this=* that=*]
      :*  %ivy
          $(form this.form, hail (peg hail 6))]
          $(form that.form, hail (peg hail 7))]
      ==
    ::
        [%6 what=* then=* else=*]
      :*  %six
          $(form what.form, hail (peg hail 6))]
          $(form then.form, hail (peg hail 14))]
          $(form else.form, hail (peg hail 15))]
      ==
    ::
        [%7 once=* then=*]
      :*  %eve
          $(form once.form, hailflank (peg hail 6))]
          $(form then.form, hail (peg hail 7))]
      ==
    ::
        [%8 pint=* then=*]
      $(form [%7 [pint.form %0 1] then.form])
    ::
        [%9 here=@ coil=*]
      $(form [%7 coil %2 [%0 1] %0 here])
    ::
        [%10 [here=@ twig=*] tree=*]
      :*  %ten
          here
          $(form twig.form, hail (peg hail 13))
          $(form tree.form, hail (peg hail 7))
      ==
    ::
        [%11 hint=@ then=*]
      [%sip hint $(form then.form, hail (peg hail 7))]
    ::
        [%11 [hint=* vice=*] then=nomm]
      :*  %tip
          hint
          $(form vice.form, hail (peg hail 13))
          $(form then.form, hail (peg hail 7))
      ==
    ::
        [%12 rent=* walk=*]
      :*  %elf
          $(form rent.form, hail (peg hail 6))
          $(form walk.form, hail (peg hail 7))
      ==
    ==
  %=  seam
      moot
    %+  ~(jab by moot.seam)  hail
    |=  =hoot
    hoot(norm.hoot `code)
  ==
::  learn/backpropagate battery masks
++  espy
  |=  hail=@hail
  ^-  _seam
  =/  [norm=nomm rake=cape]
    [norm rake]:(~(got by moot.seam) hail)
  ?>  ?=(^ norm)
  =/  code  u.norm
  =^  soon  seam
    |-  ^-  [cape _seam]
    ?-  -.code
        %par
      =/  [lack=cape rack=cape]  ~(rip ca rake)
      =^  lead  seam  $(code left.code, rake lack)
      =^  reed  seam  $(code rite.code, rake rack)
      :_  seam
      (~(uni ca lead) reed)
    ::
        %not
      ?:  ?=(0 here.code)  [[| |] seam]
      :_  seam
      (~(pat ca rake) here.code)
    ::
        %one  [[| |] seam]
        %two
      =?  moot.seam  ?!((~(has by moot.seam) rail.code))
        %+  ~(put by moot.seam)  rail.code
        :*  [| ~]  |
            ~  ~
            [| ~]  rake
            `hail
        ==
      =/  [fake=cape norm=(unit nomm)]
        [rake norm]:(~(got by moot.seam) rail.code)
      =?  seam  ?&(?=(^ norm) (~(big ca fake) rake))
        =.  moot.seam
          %+  ~(jab by moot.seam)  rail.code
          |=  =hoot
          hoot(rake rake)
        (espy rail.code)
      =/  lake  rake:(~(got by moot.seam) rail.code)
      =^  sake  seam  $(rake lake, code cost.code)
      =^  folk  seam  $(rake |, code corn.code)
      :_  seam
      (~(uni ca sake) folk)
    ::
        %the
      $(code pell.code, rake ~(any ca rake))
    ::
        %for
      $(code mall.code, rake ~(any ca rake))
    ::
        %ivy
      =^  lake  seam  $(code this.code, rake ~(any ca rake))
      =^  rare  seam  $(code that.code, rake ~(any ca rake))
      :_  seam
      (~(uni ca lake) rare)
    ::
        %six
      =^  cake  seam  $(code then.code, rake |)
      =^  lake  seam  $(code then.code)
      =^  rare  seam  $(code else.code)
      :_  seam
      (~(uni ca lake) rare)
    ::
        %eve
      =^  rare=cape  seam  $(code then.code)
      $(code once.code, rake rare)
    ::
        %ten
      =/  [wipe=cape wine=cape]
        ?:  =(0 here.ten)  [| |]
        (~(awl ca rake) here.code)
      =^  lake  seam  $(code twig.code, rake wipe)
      =^  rare  seam  $(code tree.code, rake wine)
      :_  seam
      (~(uni ca lake) rare)
    ::
        %sip
      $(code then.code)
    ::
        %tip
      =^  lake  seam  $(code vice.code, rake |)
      =^  rare  seam  $(code then.code)
      :_  seam
      (~(uni ca lake) rare)
    ::
        %elf
      =^  lake  seam  $(code rent.code, rake |)
      =^  rare  seam  $(code walk.code, rake |)
      :_  seam
      :-  (~(uni ca lake) rare)
      (~(uni ca lash) rash)
    ==
  %=  seam
      moat
    %+  ~(jab by moat.seam)  hail
    |=  =hoot
    hoot(soot soon, sake toon)
  ==
::  propagate subject knowledge forward
++  loot
  =|  goop=? :: propagate to sire?
  |=  hail=@hail
  ^-  _seam
  =/  [norm=nomm soot=sock root=sock sire=(unit @hail)]
  [norm soot root sire]:(~(got by moot.seam) hail)
  ?>  ?=(^ norm)
  =/  code  u.norm
  =^  rock  seam
    |-  ^-  [sock _seam]
    ?-  -.code
        %par
      =^  hock  seam  $(code left.code)
      =^  tock  seam  $(code rite.code)
      :_  seam
      (~(knit so hock) tock)
    ::
        %not
      ?:  =(0 here.code)  [[| ~] seam]
      =/  sand  (~(pull so soot) here.code)
      ?~  sand  [[| ~] seam]
      [u.sand seam]
    ::
        %one
      [[& moan.code] seam]
    ::
        %two
      =^  coot  seam  $(code cost.code)
      =^  sofa  seam  $(code corn.code)
      =/  [soot=sock sake=cape root=sock form=(unit) noir=(unit nomm)]
        [soot sake root form norm]:(~(got by moot.seam) rail.code)
      ?.  ?|(?!(=(cape.soot cape.coot)) ?&(=(& cape.sofa) =(~ form)))
        [root seam]
      =/  note  ?:(=(& cape.sofa) `data.sofa ~)
      =?  mite.seam  ?&(?=(^ note) ?=(~ form))  (~(put in mite.seam) rail.code)
      =.  moot.seam
        %+  ~(jab by moot.seam)  rail.code
        |=  =hoot
        hoot(soot coot, form note)
      ?~  noir  [[| ~] seam]
      =.  seam  ^$(goop |, hail rail.code)
      [root:(~(got by rail.code) moot.seam) seam]
    ::
        %the
      [[| ~] +:$(code pell.code)]
    ::
        %for
      [[| ~] +:$(code mall.code)]
    ::
        %ivy
      =.  seam  +:$(code this.code)
      [[| ~] +:$(code that.code)
    ::
        %six
      =.  seam  +:$(code what.code)
      =^  lock  seam  $(code then.code)
      =^  rock  seam  $(code else.code)
      [(~(purr so lock) rock) seam]
    ::
        %eve
      =.  noot  seam  $(code once.code)
      $(code then.code, soot noot)
    ::
        %ten
      ?:  ?=(0 here.code)  [[| ~]  seam]
      =^  pock  seam  $(code twig.code)
      =^  hock  seam  $(code tree.code)
      [(~(darn so hock) here.code pock) seam]
    ::
        %sip
      $(code then.code)
    ::
        %tip
      =.  seam  +:$(code vice.code)
      $(code then.code)
    ::
        %elf
      =.  seam  +:$(code rent.code)
      =.  seam  +:$(code walk.code)
      [[| ~] seam]
    ==
  =/  rook  (~(app ca rake) root)
  =.  moot.seam
    %+  ~(jab by moot.seam)  hail
    |=  =hoot
    hoot(rock rook)
  ?:(?&(goop ?!(=(cape.rook cape.rock)) ?=(^ sire)) $(hail u.sire) seam)
::  Recursion detection
::    when checking if a call is recursive, repeatedly check if
::    subject-formula pair matches sire, so long as sire can be found.
::    If a match is found, we have a recursion point. Add all discovered
::    sires to exclusion set
::
::    If a call isnt' recursive and isn't yet evaluated, add all sires
::    to the root to exclusion set.
::
::    If a call has a non-nil battery mask: add it to the exclusion set
::
::    Any call variables not in the exclusion set can be finalized which
::    places them in moan
::
++  ruin
  ^-  [(list @hail) _seam]
  =/  mile  ~(tap in mite.seam)
  =|  work=(list @hail)
  =|  slag=(set @hail)
  =|  flux=(set @hail)
  |-  ^-  [(list @hail) _seam]
  ?^  mile
    =/  mill  i.mile
    =/  mail  sire:(~(got by moot.seam) i.mile)
    =/  [soot=sock form=(unit)]  [soot form]:(~(got by moot.seam) mill)
    =|  sirs=(list @hail)  
    |-  ^-  [(list @hail) _seam]
    ?~  mail
      ^$(mile t.mile, work [i.mile work], slag (~(gas in slag) [mill sirs])
    =.  mill  u.mail
    =^  [suit=sock soju=cape firm=*]  mail
      [[soot sake form] sire]:(~(got by moot.seam) mill)
    ?:  ?&(=(form firm) (~(huge so (~(app ca soju) suit)) soot))
      ^$(mile t.mile, slag (~(gas in slag) sirs), flux (~(put in flux) mill))
    $(sirs [mill sirs])
  =.  mite.seam  (~(dif in mite.seam) (~(gas in *(set @hail)) work)
  =/  done  ~(gas in (~(dif in flux) slag))
  |-  ^-  [(list @hail) _seam]
  ?~  done  [work seam]
  =/  hood  (~(got by moot.seam) i.done)
  ?^  rake.hood  $(done t.done) :: skip because cell output mask
  ?:  rake.hood  $(done t.done) :: skip because & output mask
  ::  this is OK because we disallow finalizing anything with battery
  ::  dependencies on its output
  =.  soot.hood  (~(app ca sake.hood) soot.hood)
  =.  moot.seam  (~(put by moot.seam) i.done hood)
  ?>  ?=(^ form.hood)
  =.  moan.seam  (~(put ja moan.seam) u.form.hood hood)
  $(done t.done)
--
