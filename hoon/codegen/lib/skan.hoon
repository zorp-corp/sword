/-  noir
|.
=>  $:noir
=<
:: moan contains analyzed code for the linearizer
=|  moan=(jar * hone)
:: memo contains saved analyses of arms
=|  memo=(jar * meme)
:: cole is the cold state
=|  cole=cool
::
::  compile-time verbosity control (un/comment individual faces)
::
=/  verb
  :*  ::  call-site/spot lifecycle rubric:
      ::    memo hit:         <1
      ::    melo hit:         <2
      ::    pseudo-recursive: <3
      ::    indirect:         <4
      ::    analysis:           >>
      ::      meloize:          >2
      ::      finalize:         >3
      ::
      :: call=&
      ::
      jet=&           ::  jet re/registration
      ::
      ~
  ==
::
~%  %skan  ..ride  ~
|%
++  thus  .
::
+$  lore
  $:  rail=@hail  :: generator for call sites
      hare=@hint  :: generator for hint sites
      kids=(jug @hail @hail)  :: map parent to child callsites
      hint=(jug @hail @hint) :: hint sites under a callsite
      forb=(map @hint *)  ::  hint bodies
      call=cafe  :: finished callsite information
      want=urge  :: callsite needs
      loop=(map @hail [t=@hail s=sock l=naan]) :: recursion targets
      memo=_memo  :: memoized call analysis
      moan=_moan
      cole=_cole  :: cold state
      dire=?      :: fully direct?
      area=(unit spot)  :: outermost spot within this site
      wait=(jar @hail @hail)  :: sites to finalize
      melo=(jar * meal)       ::  non-final memoization targets
      remo=(map @hail [site=@hail =sock]) :: non-final memoization hits
  ==
::
++  snak
  ~%  %snak  ..snak  ~
  =|  $:  gen=lore
      $=  dad
          $:  sirs=(jar * [site=@hail less=naan])
              lord=(set @hail)  :: enclosing scope
              tack=(list @hail)
              seat=(unit spot)  :: innermost spot in lord
              wake=(list spot)  :: trace within arm
      ==  ==
  |%
  ::  +memo: check for memoized, finalized analysis
  ::
  ::    hits are guaranteed complete
  ::    XX: add debug sanity-check that entries are never in [melo.gen]
  ::
  ++  memo
    ~/  %memo
    |=  [entr=@hail form=* less=naan]
    ^-  (unit [naan lore])
    =/  germ  (~(get ja memo.gen) form)
    |-  ^-  (unit [naan lore])
    ?~  germ  ~
    ?.  (~(huge so soot.i.germ) sock.less)
      $(germ t.germ)
    =>  !@(call.verb ((outa:blot "<1 " entr seat.dad area.i.germ) .) .)
    =/  mope  (~(rue qui prot.less) have.i.germ)
    =.  mope  (~(cut qui mope) lord.dad cape.root.i.germ)
    =/  more  [mope root.i.germ]
    ::  propagate memoized subject needs
    =/  pant  (~(due qui prot.less) want.i.germ)
    =.  want.gen
      %-  (~(uno by want.gen) pant)
      |=  [@hail a=cape b=cape]
      ~(cut ca (~(uni ca a) b))
    =.  call.gen  (~(put by call.gen) entr [less more form ~ & ~ seat.dad area.i.germ])
    `[more gen]
  ::
  ++  melo-punt
    |=  [entr=@hail form=* less=naan]
    ^-  (unit [naan lore])
    =/  gorm  (~(get ja melo.gen) form)
    |-  ^-  (unit [naan lore])
    ?~  gorm  ~
    ?.  (~(huge so soot.i.gorm) sock.less)
      $(gorm t.gorm)
    =>  !@(call.verb ((onto:blot "<2 " entr seat.dad [site seat area]:i.gorm) .) .)
    :+  ~  [~ | ~]
    ?>  ?=([* * *] tack.dad)
    gen(dire |, kids (~(del ju kids.gen) i.t.tack.dad entr))
  ::
  ::  +melo: check for in-progress analysis
  ::
  ::    NB: hits are estimates, must be validated in +seal
  ::
  ++  melo
    ~/  %melo
    |=  [entr=@hail form=* less=naan]
    ^-  (unit [naan lore])
    =/  gorm  (~(get ja melo.gen) form)
    |-  ^-  (unit [naan lore])
    ?~  gorm  ~
    ?.  (~(huge so soot.i.gorm) sock.less)
      $(gorm t.gorm)
    =>  !@(call.verb ((onto:blot "<2 " entr seat.dad [site seat area]:i.gorm) .) .)
    =/  mope  (~(rue qui prot.less) have.i.gorm)
    =.  mope  (~(cut qui mope) lord.dad cape.root.i.gorm)
    =/  more  [mope root.i.gorm]
    ::
    :+  ~  more
    %=    gen
        remo  (~(put by remo.gen) entr [site.i.gorm sock.less])
        loop
      %+  roll  loom.i.gorm
      |=  [[c=@hail t=@hail s=sock n=noon] gen=loop=_loop.gen]
      ?~  op=(~(get by loop.gen) c)
        loop.gen  :: NB: got:by has crashed here
      =/  rot  (~(rue qui prot.less) plop.n)
      %+  ~(put by loop.gen)  c
      u.op(prot.l (~(int qui rot) prot.l.u.op))
    ::
        call
      =/  lac  (~(got by call.gen) site.i.gorm)
      (~(put by call.gen) site.i.gorm lac(remos (~(put in remos.lac) entr)))
    ::
        wait
      =<  +
      %.  site.i.gorm
      wait(tack.dad +.tack.dad, wait.gen (~(del by wait.gen) entr))
    ==
  ::
  ::  +wait: register dependence on [site] for finalization
  ::
  ++  wait
    ~/  %wait
    |=  site=@hail
    =|  wire=(list (list @hail))
    |-  ^-  [@hail _wait.gen]
    ?>  ?=(^ tack.dad)
    =/  fire  (~(get ja wait.gen) i.tack.dad)
    ?.  (lien fire |=(h=@hail =(site h)))
      %=  $
        tack.dad  t.tack.dad
        wire      [fire wire]
        wait.gen  (~(del by wait.gen) i.tack.dad)
      ==
    :-  i.tack.dad
    (~(put by wait.gen) i.tack.dad (zing (flop [fire wire])))
  ::
  ::  +loop: check for recursion
  ::
  ::    hits are estimates, must be validated in +mend and +seal
  ::    XX: improve debug output (link site -> target -> finalization site)
  ::
  ++  loop
    ~/  %loop
    |=  [roil=@hail fork=naan sand=naan]
    ^-  (unit lore)
    =/  pore  (~(get ja sirs.dad) data.sock.fork)
    |-  ^-  (unit lore)
    ?~  pore  ~
    =/  tote
      =/  cope  (~(gut by want.gen) site.i.pore |)
      (~(app ca cope) sock.less.i.pore)
    ?.  (~(huge so tote) sock.sand)
      $(pore t.pore)
    :: recursive
    =.  loop.gen   (~(put by loop.gen) roil [site.i.pore sock.less.i.pore sand])
    =^  til=@hail  wait.gen  (wait site.i.pore)
    ::  XX also print [site.i.pore] and its spot
    =>  !@(call.verb ((onto:blot "<3 " roil seat.dad til ~ ~) .) .)
    `gen
  ::
  ::  +hint: update lore after analyzing through hint (currently just %fast)
  ::
  ::    XX: validate recursive hint processing, refactor
  ::
  ++  hint
    ~/  %hint
    |=  [form=h=@ mild=naan bite=naan]
    ^-  lore
    ?+    h.form  gen
        %fast
      ::  fast hint registration
      ?.  =(& cape.sock.mild)  ~&  %fast-hide-clue  gen
      =*  clue  data.sock.mild
      ?.  ?=([name=$@(@tas [@tas @]) pare=* *] clue)
        ~&  [%fast-bad-clue clue]  gen
      =/  pell
        ?@  name.clue
          name.clue
        (crip "{(trip -.name.clue)}.{(trip (scot %ud +.name.clue))}")
      |-  ^-  lore
      ?+  pare.clue  ~&  [%fast-bad-clue clue]  gen
          ::  XX may elide crashes [%11 [%1 *] *]
          [%11 * *]
        $(pare.clue +>.pare.clue)
      ::
          [%1 %0]
        :: register root
        ?.  =(& cape.sock.bite)
          ~&  %fast-hide-root  gen
        %=  gen
          core.cole  (~(put ju core.cole.gen) ~[pell] sock.bite)
          root.cole  (~(put ju root.cole.gen) data.sock.bite ~[pell])
        ==
      ::
          [%0 a=@]
        ?:  =(0 @)  ~&  [%fast-bad-clue clue]  gen
        :: register child core
        =/  batt  (~(pull so sock.bite) 2)
        ?.  =(& cape.batt)  ~&  [%fast-hide-batt pell]  gen
        ?.  ?=(^ data.batt)  gen
        =/  park  (~(pull so sock.bite) a.pare.clue)
        :: XX ???
        :: ?.  =(& cape.park)  ~&  %fast-lost-sire  gen
        =/  past=(set path)
          ?.  =(& cape.park)  ~
          (~(get ju root.cole.gen) data.park)
        =/  bork  (~(pull so park) 2)
        =?  past  &(?=(%& cape.bork) ?=(^ data.bork))
          (~(uni in past) (~(get ju batt.cole.gen) data.bork))
        =/  pale  ~(tap in past)
        |-  ^-  lore
        =*  pale-loop  $
        ?~  pale  gen
        =/  nape=path  [pell i.pale]
        =/  coal  ~(tap in (~(get ju core.cole.gen) i.pale))
        |-  ^-  lore
        ?~  coal  pale-loop(pale t.pale)
        ?.  (~(huge so i.coal) park)  $(coal t.coal)
        =/  naut
          =/  bake  (~(darn so [| ~]) 2 batt)  :: XX [[& |] batt ~]
          (~(darn so bake) a.pare.clue i.coal)
        ::
        =>  =*  dot  .
            !@  jet.verb
              =/  cod
                ?:(?!((~(has by core.cole.gen) nape)) %cold-into %cold-peat)
              ~>  %slog.[0 [%rose [": " ~ ~] cod (smyt nape) ~]]
              dot
            dot
        ::
        %=  gen
          core.cole  (~(put ju core.cole.gen) nape naut)
          batt.cole  (~(put ju batt.cole.gen) data.batt nape)
        ==
      ==
    ==
  ::
  ::  +bide: save non-final results for "meloization"
  ::
  ++  bide
    ~/  %bide
    |=  [entr=@hail form=* less=naan more=naan]
    ^+  melo.gen
    =>  !@(call.verb ((outa:blot ">2 " entr seat.dad area.gen) .) .)
    =/  want=cape  (~(gut by want.gen) entr |)
    =/  have  (~(rel qui prot.more) entr cape.sock.more)
    =/  sutt
      =/  such
        %-  ~(uni ca want)
        (~(gut by (~(due qui prot.more) cape.sock.more)) entr |)
      ~(norm so (~(app ca such) sock.less))
    =/  loom
      %-  ~(rep by loop.gen)
      |=  [[c=@hail t=@hail s=sock l=naan] loom=(list [c=@hail t=@hail s=sock =noon])]
      ^+  loom
      :: XX maybe require that t is in sirs
      =/  p=plop  (~(rel qui prot.l) entr &)
      =/  n=noon  [p sock.l]
      [[c t s n] loom]         :: XX skip if ?=(~ p) ?
    (~(add ja melo.gen) form [[sutt want sock.more have area.gen] entr seat.dad loom])
  ::
  ::  +mend: fixpoints to validate pseudo-recursive estimates
  ::
  ::    XX performance
  ::
  ++  mend
    =<  $
    ~%  %mend  ..mend  ~
    |.  ^-  [? lore]
    =/  sap  gen  :: for reset
    =|  nop=(map @hail [t=@hail s=sock l=naan])
    =|  i=@ud
    |-  ^-  [? lore]
    =*  redo-loop  $
    =.  gen  sap
    =.  loop.gen  (~(dif by loop.gen) nop)
    =|  j=@ud
    |-  ^-  [? lore]
    =*  need-loop  $
    =/  wap  want.gen
    =.  gen
      %-  ~(rep by loop.gen)
      |=  [[c=@hail t=@hail s=sock l=naan] =_gen]
      ^-  _gen
      =/  teed=cape  (~(gut by want.gen) t |)
      =?  want.gen  ?!(.=(| teed))
        %-  (~(uno by want.gen) (~(due qui prot.l) teed))
        |=([@hail a=cape b=cape] ~(cut ca (~(uni ca a) b)))
      gen
    ?.  =(wap want.gen)
      ~?  !=(0 j)  [%mend-need i=i j=j]
      need-loop(j +(j))
    =/  nap  nop
    =.  nop
      %-  ~(rep by loop.gen)
      |=  [[c=@hail t=@hail s=sock l=naan] =_nop]
      =/  teed  (~(gut by want.gen) t |)
      =.  s  (~(app ca teed) s)
      ::  XX log non pseudo-recursive
      ?.  (~(huge so s) sock.l)  (~(put by nop) c *[t=@hail s=sock l=naan])
      nop
    ?.  =(nap nop)
      ~&  [%mend-redo i=i j=j]
      redo-loop(i +(i))
    [?=(~ nop) gen]
  ::
  ::  +seal: finalize analysis (including recursive descendants)
  ::
  ::    XX: debug %ices-fail-2, prevent memoization
  ::    XX: refactor
  ::
  ++  seal
    ~/  %seal
    |=  [entr=@hail sane=? wise=(list @hail)]
    ^-  lore
    =>  ::  XX also log kid
        !@(call.verb ((outa:blot ">3 " entr seat.dad area.gen) .) .)
    ~?  ?=([* * *] wise)  wise=(tail (flop wise))
    ?>  =(entr (rear wise)) :: current callsite should be last item of finalization list
    %+  roll  wise
    |=  [site=@hail =_gen]
    =/  kid  (~(get ju kids.gen) site)
    ~?  !=(~ kid)  [site=site `kid=(set @hail)`kid]
    =+  (~(got by call.gen) site)
    ?>  ?=(^ load)
    =/  want=cape  (~(gut by want.gen) site |)
    =/  sutt
      =/  such
        %-  ~(uni ca want)
        (~(gut by (~(due qui prot.more) cape.sock.more)) site |)
      ~(norm so (~(app ca such) sock.less))
    =?  memo.gen  ?&(rect sane)
      =/  have  (~(rel qui prot.more) site cape.sock.more)
      (~(add ja memo.gen) form [sutt want sock.more have area])
    =.  melo.gen
      ?~  mel=(~(get by melo.gen) form)
        melo.gen
      =/  lit  (skip u.mel |=([^ lite=@hail *] =(site lite)))
      :: =+  [len nel]=[(lent lit) (lent u.mel)]
      :: ~?  !=(len nel)  [%del-melo nel len]
      ?:  =(~ lit)
        (~(del by melo.gen) form)
      (~(put by melo.gen) form lit)
    ::
    =/  soot
      =>  [s=sock.less w=want so=so ca=ca]
      ~+  ~(norm so (~(app ca w) s))
    =^  [ices=(map @hail [=sock form=*]) lope=(set [=sock form=*])]  gen
      %-  ~(rep in kid)
      |=  [k=@hail [ices=(map @hail [=sock form=*]) lope=(set [=sock form=*])] =_gen]
      =/  rem  (~(get by remo.gen) k)
      =/  n  ?~(rem k site.u.rem)
      =/  m  t:(~(gut by loop.gen) k [t=n s=*sock l=*naan])
      =/  w=cape  (~(gut by want.gen) m |)
      =/  c  (~(got by call.gen) m)  :: XX would crash on mend-redo?
      =/  s
        =>  [s=sock.less.c w=w so=so ca=ca]
        ~+  ~(norm so (~(app ca w) s))
      ::
      ?.  ?|  ?=(~ rem)
              (~(huge so s) (~(app ca w) sock.u.rem))
          ==
        ::  XX should this prevent memoization?
        ::
        ~&  [%ices-fail-nest k=k n=n m=m]
        [[ices lope] gen]
      ::
      ::  XX s/b debug assert (only if rect.c?)
      ::
      ?.  ?|  ?=(~ rem)
              (lien (~(get ja moan.gen) form.c) |=([ss=sock *] =(s ss)))
          ==
        ~&  [%ices-fail-link k=k n=n m=m direct=rect.c]
        [[ices lope] gen]
      ::
      =.  ices  (~(put by ices) k [s form.c])
      =?  lope  ?!(.=(m n))  (~(put in lope) [s form.c])
      [[ices lope] gen]
    ::  trim want/call/loop tables
    =.  gen
      %-  ~(rep in kid)
      |=  [k=@hail =_gen]
      %_    gen
          call
        =.  k  ?~(rem=(~(get by remo.gen) k) k site.u.rem)
        ?~  lac=(~(get by call.gen) k)  call.gen
        ?~  mos=(~(del in remos.u.lac) site)
          (~(del by call.gen) k)
        (~(put by call.gen) k u.lac(remos mos))
      ::
          want  (~(del by want.gen) k)
          loop  (~(del by loop.gen) k)
          remo  (~(del by remo.gen) k)
      ==
    =/  hiss  (~(get ju hint.gen) site)
    =^  fizz  gen
      %-  ~(rep in hiss)
      |=  [h=@hint fizz=(map @hint *) gen=_gen]
      =.  fizz  (~(put by fizz) h (~(got by forb.gen) h))
      ::  trim forb table
      =.  forb.gen  (~(del by forb.gen) h)
      [fizz gen]
    ::  trim kids and hint tables
    =.  kids.gen  (~(del by kids.gen) site)
    =.  hint.gen  (~(del by hint.gen) site)
    =/  loan  (~(get ja moan.gen) form)
    =?  moan.gen  (levy loan |=([ss=sock *] !=(soot ss)))
      (~(add ja moan.gen) form [soot u.load ices lope fizz])
    gen
  ::
  ::  +scan: statefully analyze formula against subject
  ::
  ++  scan
    ~/  %scan
    |=  queu=i=todo
    ^-  [naan lore]
    =^  entr  gen  [rail.gen gen(rail .+(rail.gen))]  :: initial callsite
    :: ~>  %bout.[0 `tank`?.(?=(^ calm.i.queu) %raw [%rose [": " ~ ~] leaf+['+' (scow %ud +.j.u.calm.i.queu)] (smyt -.j.u.calm.i.queu) ~])]
    =/  less=naan  [~ soot.i.queu]  :: subject
    =*  form       form.i.queu      :: formula
    =.  dad
      %=  dad
        sirs  (~(add ja ^+(sirs.dad ~)) form [entr less])
        lord  ~
        tack  ~[entr]
        wake  ~
        seat  ~
      ==
    ::  wrapper for callsite formulas
    |-  ^-  [naan _gen]
    =*  arm-loop  $
    =.  prot.less  (~(tag qui prot.less) [entr 1])
    =.  wait.gen  (~(add ja wait.gen) entr entr)
    ::  check if memoized
    ?^  m=(memo entr form less)  u.m
    ?^  m=(melo entr form less)  u.m
    :: ?^  m=(melo-punt entr form less)  u.m
    ::
    =^  [load=nomm more=naan]  gen
      :: structurally recur over formula
      =>  !@(call.verb ((into:blot ">> " entr seat.dad) .) .)
      |-  ^-  [[=nomm =naan] _gen]
      ?+  form  [[[%not 0] [~ | ~]] gen]
          [b=^ c=*]
        =^  [leno=nomm lire=naan]  gen  $(form b.form)
        =^  [reno=nomm rile=naan]  gen  $(form c.form)
        :_  gen
        :-  [%par leno reno]
        [(~(con qui prot.lire) prot.rile) (~(knit so sock.lire) sock.rile)]
      ::
          [%0 axe=@]
        ?:  =(0 axe.form)  [[[%not 0] [~ | ~]] gen]
        :_  gen
        :+  [%not axe.form]
          (~(ask qui prot.less) axe.form)
        (~(pull so sock.less) axe.form)
      ::
          [%1 n=*]
        [[[%one n.form] [~ & n.form]] gen]
      ::
          [%2 s=* f=*]
        =^  roil  gen  [rail.gen gen(rail .+(rail.gen))]
        =/  area  area.gen
        =:  wake.dad  ~
            seat.dad  ?~(wake.dad ~ `i.wake.dad)
          ==
        =^  [sown=nomm sand=naan]  gen  $(form s.form, area.gen ~)
        =^  [fond=nomm fork=naan]  gen  $(form f.form, area.gen ~)
        ?.  =(& cape.sock.fork)
          ::  indirect call
          =>  !@(call.verb ((outa:blot "<4 " roil seat.dad ~) .) .)
          [[[%two sown fond roil] [~ | ~]] gen(dire |, area area)]
        :: direct call
        =.  kids.gen  (~(put ju kids.gen) entr roil)
        ::  record need
        =/  pant  (~(due qui prot.fork) &) :: callsite provenance by needs
        =.  want.gen
          %-  (~(uno by want.gen) pant)
          |=  [@hail a=cape b=cape]
          ~(cut ca (~(uni ca a) b))
        ::  check for recursion
        ?^  l=(loop roil fork sand)
          [[[%two sown fond roil] [~ | ~]] u.l(area area)]
        ::  not recursive
        :: analyze through direct call
        =/  dire  dire.gen
        =^  more  gen
          %=  arm-loop
            form  data.sock.fork
            less  sand
            entr  roil
            gen   gen(dire &, area ~)
            dad   %=  dad
                    sirs  (~(add ja sirs.dad) data.sock.fork [roil sand])
                    lord  (~(put in lord.dad) entr)
                    tack  [roil tack.dad]
          ==      ==
        :-  [[%two sown fond roil] more]
        gen(dire &(dire dire.gen), area area)
      ::
          [%3 c=*]
        =^  [knob=nomm mild=naan]  gen  $(form c.form)
        :_  gen
        [[%the knob] [~ | ~]]
      ::
          [%4 c=*]
        =^  [knob=nomm mild=naan]  gen  $(form c.form)
        :_  gen
        [[%for knob] [~ | ~]]
      ::
          [%5 l=* r=*]
        =^  [leno=nomm lire=naan]  gen  $(form l.form)
        =^  [reno=nomm rile=naan]  gen  $(form r.form)
        :_  gen
        [[%ivy leno reno] [~ | ~]]
      ::
          [%6 c=* t=* f=*]
        =^  [xeno=nomm mild=naan]  gen  $(form c.form)
        =^  [zero=nomm thin=naan]  gen  $(form t.form)
        =^  [once=nomm fast=naan]  gen  $(form f.form)
        :_  gen
        :-  [%six xeno zero once]
        :-  (~(int qui prot.thin) prot.fast)
        (~(purr so sock.thin) sock.fast)
      ::
          [%7 b=* c=*]
        =^  [anon=nomm mean=naan]  gen  $(form b.form)
        =^  [then=nomm salt=naan]  gen  $(form c.form, less mean)
        :_  gen
        [[%eve anon then] salt]
      ::
          [%8 b=* c=*]
        ?@  b.form  $(form [7 [[0 0] 0 1] c.form])
        $(form [7 [b.form 0 1] c.form])
      ::
          [%9 b=@ c=*]
        $(form [7 c.form 2 [0 1] 0 b.form])
      ::
          [%10 [a=@ p=*] b=*]
        ?:  =(0 a.form)  [[[%not 0] [~ | ~]] gen]
        =^  [twig=nomm bite=naan]  gen  $(form p.form)
        =^  [tree=nomm hole=naan]  gen  $(form b.form)
        :_  gen
        :+  [%ten a.form twig tree]
          (~(put qui prot.hole) a.form prot.bite)
        (~(darn so sock.hole) a.form sock.bite)
      ::
          [%11 h=@ f=*]
        =^  [then=nomm bite=naan]  gen  $(form f.form)
        [[[%sip h.form then] bite] gen]
      ::
          [%11 [h=@ v=*] f=*]
        =^  hare  gen  [hare.gen gen(hare .+(hare.gen))]
        =^  [vice=nomm mild=naan]  gen  $(form v.form)
        ::
        ::  XX !@ on call.verb?
        ::
        =>  =/  pot=(unit spot)
              ?.(=(%spot h.form) ~ ((soft spot) data.sock.mild))
            ?~  pot  +
            %_  +
              wake.dad  [u.pot wake.dad]
              area.gen  ?~(area.gen pot area.gen)
            ==
        ::
        =^  [then=nomm bite=naan]  gen  $(form f.form)
        ::  save body formula
        =.  hint.gen  (~(put ju hint.gen) entr hare)
        =.  forb.gen  (~(put by forb.gen) hare f.form)
        :-  [[%tip h.form vice then hare] bite]
        (hint h.form mild bite)
      ::
          [%12 r=* p=*]
        =^  [rend=nomm rita=naan]  gen  $(form r.form)
        =^  [pond=nomm walk=naan]  gen  $(form p.form)
        [[[%elf rend pond] [~ | ~]] gen]
      ==
    ::
    =.  prot.more  (~(cut qui prot.more) lord.dad cape.sock.more)
    :-  more
    ::  write to call table
    =.  call.gen  (~(put by call.gen) entr [less more form `load dire.gen ~ seat.dad area.gen])
    =/  wise      (~(get ja wait.gen) entr)
    =.  wait.gen  (~(del by wait.gen) entr)
    ?:  =(~ wise)
      :: no finalizing here
      gen(melo (bide entr form less more))
    ::  fixed-point loops to propagate their needs and check that they are really loops
    =^  sane=?  gen  mend
    ::  finalize waiting callsites
    (seal entr sane wise)
  --
::
::    Analyze a subject/formula pair
++  rout
  ~/  %rout
  |=  [soot=* form=*]
  =/  colt  cole
  =/  queu=(list todo)  [[& soot] form ~]~
  =|  back=(list todo)
  :: analysis queue: first the actual subject/formula pair,
  :: then formulas from batteries of any new cold-state registrations
  |-  ^-  _thus
  =*  cold-loop  $
  =/  gnat  ((dif-ju core.cole) core.colt)
  =.  colt  cole
  =.  back
    ::  queue unanalyzed cold-state batteries
    ::  (shortest-path first gives a rough topo-sort)
    ::
    %+  roll
      %+  sort
        %+  turn  ~(tap by gnat)
        |=([p=path q=(set sock)] [(lent p) p q])
      |=([l=[len=@ *] r=[len=@ *]] (lth len.l len.r))
    |:  [[len=*@ p=*path q=*(set sock)] b=back]
    %-  ~(rep in q)
    |:  [s=*sock b=b]
    =/  batt  (~(pull so s) 2)
    ?.  =(& cape.batt)  ~&  [%cold-miss-batt p]  b
    ::  split up battery at autocons sites
    =*  f  data.batt
    =/  a=@  1
    |-  ^-  _b
    ?.  ?=([^ *] f)
      [[s f `[| p a]] b]
    =.  b  $(f -.f, a (peg a 2))
    =.  b  $(f +.f, a (peg a 3))
    [[s f `[& p a]] b]
  ?~  queu
    ?~  back  thus
    ~&  [%cold-loop (lent back)]
    cold-loop(queu (flop back), back ~)
  ::  finish analysis of an autocons from a cold-state battery
  ?:  ?&(?=(^ calm.i.queu) auto.u.calm.i.queu)
    =*  j  j.u.calm.i.queu
    =/  balk=(list [sock *])  ~(tap in (~(get ju back.cole) [-.j (peg +.j 2)]))
    =/  bark=(list [sock *])  ~(tap in (~(get ju back.cole) [-.j (peg +.j 3)]))
    ?>  ?=(^ form.i.queu)
    |-  ^-  _thus
    =*  balk-loop  $
    ?~  balk  cold-loop(queu t.queu)
    ?.  =(-.form.i.queu +.i.balk)  balk-loop(balk t.balk)
    ?.  (~(huge so -.i.balk) soot.i.queu)  balk-loop(balk t.balk)
    =/  bart  bark
    |-  ^-  _thus
    =*  bart-loop  $
    ?~  bart  balk-loop(balk t.balk)
    ?.  =(+.form.i.queu +.i.bart)  bart-loop(bart t.bart)
    ?.  (~(huge so -.i.bart) soot.i.queu)  bart-loop(bart t.bart)
    =/  soot  (~(pack so -.i.balk) -.i.bart)
    =.  call.cole  (~(put by call.cole) [soot form.i.queu] j)
    =.  back.cole  (~(put ju back.cole) j [soot form.i.queu])
    bart-loop(bart t.bart)
  :: analyze a formula
  =/  gen=lore  +:(scan:snak i.queu)
  %=    cold-loop
      queu  t.queu
      moan  moan.gen
      memo  memo.gen
      cole
    ?~  calm.i.queu  cole.gen
    =/  boot
      :_  form
      =/  want  (fall (~(get by want.gen) `@hail`0x0) |) :: first site is always 0x0
      =>  [s=soot.i.queu w=want so=so ca=ca]
      ~+  ~(norm so (~(app ca w) s))
    =*  pax  j.u.calm.i.queu
    %=  cole.gen
      call  (~(put by call.cole.gen) boot pax)
      back  (~(put ju back.cole.gen) pax boot)
    ==
  ==
--
=<
::  utilities
|%
::
::    operations on provenance
++  qui
  |_  prog=prot
  ::
  ::    provenance tree for +axis
  ++  ask
    |=  axe=@
    ?<  =(0 axe)
    =/  rev  1
    =|  don=(list (pair @ (list peon)))
    |-  ^-  prot
    =+  [n l r]=?@(prog [~ ~ ~] prog)
    ?.  =(1 axe)
      ?-  (cap axe)
        %2  $(axe (mas axe), don [[rev n] don], rev (peg rev 2), prog l)
        %3  $(axe (mas axe), don [[rev n] don], rev (peg rev 3), prog r)
      ==
    =.  n
      %+  roll  don
      |=  [[axe=@ lit=(list peon)] out=_n]
      ?:  =(~ lit)  out
      =/  rel  (hub axe rev)
      %+  roll  lit
      |=([p=peon =_out] [p(axe (peg axe.p rel)) out])
    ?:  ?&(?=(~ n) ?=(~ l) ?=(~ r))  ~
    [n l r]
  ::
  ++  put
    |=  [axe=@ poor=prot]
    ?<  =(0 axe)
    ?:  &(?=(~ prog) ?=(~ poor))  ~
    =|  tack=(list [c=?(%2 %3) p=prot])
    |-  ^-  prot
    ?.  =(1 axe)
      ?-  (cap axe)
        %2  $(axe (mas axe), prog hed, tack [[%2 tal] tack])
        %3  $(axe (mas axe), prog tal, tack [[%3 hed] tack])
      ==
    |-  ^-  prot
    ?~  tack  poor
    ?-  c.i.tack
      %2  $(poor [~ poor p.i.tack], tack t.tack)
      %3  $(poor [~ p.i.tack poor], tack t.tack)
    ==
  ++  cut
    |=  [sire=(set @hail) =cape]
    ^-  prot
    ?:  |(?=(%| cape) ?=(~ prog))  ~
    =/  n  (skim n.prog |=([s=@hail @] (~(has in sire) s)))
    =+  [p q]=?@(cape [& &] cape)
    =/  l  $(prog l.prog, cape p)
    =/  r  $(prog l.prog, cape q)
    ?:  ?&(?=(~ n) ?=(~ l) ?=(~ r))  ~
    [n l r]
  ::
  ::    provenance tree for +2
  ++  hed
    ^-  prot
    ?~  prog  ~
    =+  [n l r]=?@(l.prog [~ ~ ~] l.prog)
    :_  [l r]
    %+  roll  n.prog
    |=([p=peon out=_n] [p(axe (peg axe.p 2)) out])
  ::
  ::    provenance tree for +3
  ++  tal
    ^-  prot
    ?~  prog  ~
    =+  [n l r]=?@(r.prog [~ ~ ~] r.prog)
    :_  [l r]
    %+  roll  n.prog
    |=([p=peon out=_n] [p(axe (peg axe.p 3)) out])
  ::
  ::    provenance tree from two subtrees (cons)
  ++  con
    |=  poor=prot
    ^-  prot
    ?:  &(?=(~ prog) ?=(~ poor))  ~
    [~ prog poor]
  ::
  ::    provenance tree of intersection
  ++  int
    |=  poor=prot
    ^-  prot
    ?~  prog  poor
    ?~  poor  prog
    =/  n  ~(tap in (~(gas in (~(gas in *(set peon)) n.poor)) n.prog))
    [n $(prog l.prog, poor l.poor) $(prog r.prog, poor r.poor)]
  ::
  ::    add a label to the root
  ++  tag
    |=  =peon
    ?~  prog  [~[peon] ~ ~]
    prog(n [peon n.prog])
  ::
  ::    given a cape, distribute that cape to callsites by provenance
  ++  due
    !.
    =/  unica  |=([@hail a=cape b=cape] (~(uni ca a) b))
    |=  cave=cape
    ^-  (map @hail cape)
    ?:  |(?=(%| cave) ?=(~ prog))  ~
    =/  n
      %+  roll  n.prog
      |=  [[s=@hail a=@] m=(map @hail cape)]
      (~(put by m) s (~(pat ca cave) a))
    =+  [p q]=?@(cave [& &] cave)
    =/  l  $(prog l.prog, cave p)
    =/  r  $(prog r.prog, cave q)
    ((~(uno by ((~(uno by l) r) unica)) n) unica)
  ::
  ::    given a callsite produce a new provenance tree only retaining
  ::    provenance for that callsite's subject
  ++  rel
    |=  [site=@hail =cape]
    ^-  plop
    ?:  |(?=(%| cape) ?=(~ prog))  ~
    =/  n  (murn n.prog |=(p=peon ?:(=(site site.p) `axe.p ~)))
    =+  [p q]=?@(cape [& &] cape)
    =/  l  $(prog l.prog, cape p)
    =/  r  $(prog r.prog, cape q)
    ?:  &(?=(~ l) ?=(~ r) ?=(~ n))  ~
    [n l r]
  ::
  ::    relocate cached provenance
  ++  rue
    |=  prop=plop
    =|  t=prot
    |-  ^-  prot
    ?~  prop  t
    =.  t
      %+  roll  n.prop
      |:  [a=*@ t=t]
      (int(prog (ask a)) t)
    ?~  t
      =/  l  $(prop l.prop)
      =/  r  $(prop r.prop)
      ?:(?&(?=(~ l) ?=(~ r)) ~ [~ l r])
    =/  l  $(prop l.prop, t l.t)
    =/  r  $(prop r.prop, t r.t)
    [n.t l r]
  --
::
::    push down axes on a list of peons
++  pepe
  |=  [slav=(list peon) axe=@]
  ^-  _slav
  %+  turn  slav
  |=  =peon
  peon(axe (peg axe.peon axe))
::
++  hub
  :: ~/  %hub
  ::    axis after axis
  ::
  ::  computes the remainder of axis {b} when navigating to {a}.
  ::  (crashes if not `(pin a b)`)
  |=  [a=@ b=@]
  ?<  =(0 a)
  ?<  =(0 b)
  |-  ^-  @
  ?:  =(a 1)  b
  ?>  =((cap a) (cap b))
  $(a (mas a), b (mas b))
::
++  dif-ju
  |*  a=(jug)
  |*  b=_a
  ^+  a
  =/  c=_a  (~(dif by a) b)
  =/  i=_a  (~(int by a) b)
  ?:  =(~ i)  c
  %-  ~(rep by i)
  |=  [[p=_?>(?=(^ i) p.n.i) q=_?>(?=(^ i) q.n.i)] =_c]
  =/  r=_q  (~(get ju b) p)
  =/  s=_q  (~(dif in q) r)
  ?:  =(~ s)  c
  (~(put by c) p s)
::
++  blot
  |%
  ++  ren
    |=  pot=spot
    ^-  tank
    :+  %rose  [":" ~ ~]
    :~  (smyt p.pot)
        =*  l   p.q.pot
        =*  r   q.q.pot
        =/  ud  |=(a=@u (scow %ud a))
        leaf+"<[{(ud p.l)} {(ud q.l)}].[{(ud p.r)} {(ud q.r)}]>"
    ==
  ::
  ++  hal  (cury scot %x)
  ::
  ++  one
    |=  [tap=tape ale=@hail pot=spot]
    ^-  tank
    [%rose [": " tap ~] (hal ale) (ren pot) ~]
  ::
  ++  two
    |=  [tap=tape ale=@hail pot=spot top=spot]
    ^-  tank
    :+  %rose  [": " tap ~]
    :~  (hal ale)
        [%rose [" -> " ~ ~] (ren pot) (ren top) ~]
    ==
  ::
  ++  into
    |=  [tap=tape ale=@hail f=(unit spot)]
    %-  slog  :_  ~
    :^    %rose
        [": " tap ~]
      (hal ale)
    ?~  f  ~
    [(ren u.f) ~]
  ::
  ++  outa
    |=  [tap=tape ale=@hail f=(unit spot) t=(unit spot)]
    ?~  t
      (into tap ale f)
    %-  slog  :_  ~
    :+  %rose  [": " tap ~]
    :~  (hal ale)
        [%rose [" -> " ~ ~] ?~(f '??' (ren u.f)) (ren u.t) ~]
    ==
  ::
  ++  onto
    |=  $:  tap=tape
            ale=@hail
            s=(unit spot)
            ole=@hail
            f=(unit spot)
            t=(unit spot)
        ==
    %-  slog  :_  ~
    :^    %rose
        [": " tap ~]
      (hal ale)
    ?~  s  ~
    :+  (ren u.s)
      (hal ole)
    ?:  |(?=(~ f) ?=(~ t))  ~
    [[%rose [" -> " ~ ~] (ren u.f) (ren u.t) ~] ~]
  --
--
::  utility types
::
|%
::
::    abstract noun with provenance
+$  naan  [=prot =sock]
::
::    abstract noun with local provenance
+$  noon  [=plop =sock]
::
::    callsite information
+$  cafe  (map @hail [less=naan more=naan form=* load=(unit nomm) rect=? remos=(set @hail) seat=(unit spot) area=(unit spot)])
::
::    subject requirements for callsites
+$  urge  (map @hail cape)
::
::    individual provenance tag: callsite and subject axis
+$  peon  [site=@hail axe=@]
::
::    provenance information to go with a sock
+$  prot  (tree (list peon))
::
::    single-site provenance tree (axes only, no callsites)
+$  plop  (tree (list @))
::
::    analysis queue entry
+$  todo
  $:  soot=sock
      form=*
      calm=(unit [auto=? j=[path @]])
  ==
::
::    analysis memoization entry
+$  meme  [soot=sock want=cape root=sock have=plop area=(unit spot)]
::
::    loop-local analysis memoization entry
+$  meal  [meme site=@hail seat=(unit spot) loom=(list [c=@hail t=@hail s=sock n=noon])]
--
