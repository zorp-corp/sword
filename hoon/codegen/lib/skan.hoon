=<
:: moan contains analyzed code for the linearizer
=|  moan=(jar * hone)
:: memo contains saved analyses of arms
=|  memo=(jar * meme)
:: cole is the cold state
=|  cole=cool
|%
++  thus  .
::
::    Analyze a subject/formula pair
::  
++  rout
  |=  [soot=* form=*]
  =/  colt  cole
  =/  queu=(list todo)  [[& soot] form ~]~
  =|  back=(list todo)
  :: analysis queue: first the actual subject/formula pair,
  :: then formulas from batteries of any new cold-state registrations
  |-  ^-  _thus
  =*  cold-loop  $
  =/  gnat  ((dif-ju core.cole) core.colt)
  =.  back
    :: queue unanalyzed cold-state batteries
    %-  ~(rep by gnat)
    |:  [[p=*path q=*(set sock)] b=back]
    %-  ~(rep in q)
    |:  [s=*sock b=b]
    =/  batt  (~(pull so s) 2)
    ?~  batt  b
    ?.  =(& cape.u.batt)  ~&  [%cold-miss-batt p]  b
    ::  split up battery at autocons sites
    =*  f  data.u.batt
    =/  a=@  1
    |-  ^-  _b
    ?.  ?=([^ *] f)
      [[s f `[| p a]] b]
    =.  b  $(f -.f, a (peg a 2))
    =.  b  $(f +.f, a (peg a 3))
    [[s f `[& p a]] b]
  ?~  queu
    ?~  back  thus
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
    ?.  (~(huge so soot.i.queu) -.i.balk)  balk-loop(balk t.balk)
    =/  bart  bark
    |-  ^-  _thus
    =*  bart-loop  $
    ?~  bart  balk-loop(balk t.balk)
    ?.  =(+.form.i.queu +.i.bart)  bart-loop(bart t.bart)
    ?.  (~(huge so soot.i.queu) -.i.bart)  bart-loop(bart t.bart)
    =/  soot  (~(pack so -.i.balk) -.i.bart)
    =.  call.cole  (~(put by call.cole) [soot form.i.queu] j)
    =.  back.cole  (~(put ju back.cole) j [soot form.i.queu])
    bart-loop(bart t.bart)
  :: analyze a formula
  =|  $=  gen
      $:
          rail=@hail  :: generator for call sites
          hare=@hint  :: generator for hint sites
          kids=(jug @hail @hail)  :: map parent to child callsites
          loop=(map @hail @hail) :: recursion targets
          hint=(jug @hail @hint) :: hint sites under a callsite
          forb=(map @hint *)  ::  hint bodies
          call=cafe  :: finished callsite information
          want=urge  :: callsite needs
          memo=_memo  :: memoized call analysis
          cole=_cole
          dire=?
      ==
  =^  entr  gen  [rail.gen gen(rail .+(rail.gen))]  :: initial callsite
  =^  more  gen
    =/  less=naan  [~ soot.i.queu]  :: subject
    =*  form  form.i.queu :: formula
    =/  sirs  (~(add ja *(jar * [site=@hail less=naan])) form [entr less]) :: 
    ::  wrapper for callsite formulas
    |-  ^-  [naan _gen]
    =*  arm-loop  $
    =.  prot.less  (~(tag qui prot.less) [entr 1])
    ::  check if memoized
    =/  germ  (~(get ja memo.gen) form)
    |-  ^-  [naan _gen]
    ?^  germ
      ?.  (~(huge so sock.less) soot.i.germ)
        $(germ t.germ)
      =/  more  [(~(rue qui prot.less) have.i.germ) root.i.germ]
      =.  call.gen  (~(put by call.gen) entr [less want.i.germ more form ~])
      [more gen]
    =^  [load=nomm more=naan]  gen
      :: structurally recur over formula
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
        =/  salt  (~(pull so sock.less) axe.form)
        ?~  salt  [[[%not axe.form] [~ | ~]] gen]
        :_  gen
        :-  [%not axe.form]
        [(~(ask qui prot.less) axe.form) u.salt]
      ::
          [%1 n=*]
        [[[%one n.form] [~ & n.form]] gen]
      ::
          [%2 s=* f=*]
        =^  roil  gen  [rail.gen gen(rail .+(rail.gen))]
        =^  [sown=nomm sand=naan]  gen  $(form s.form)
        =^  [fond=nomm fork=naan]  gen  $(form f.form)
        ?:  =(& cape.sock.fork)
          :: direct call
          =.  kids.gen  (~(put ju kids.gen) entr roil)
          ::  record need
          =/  pant  (~(due qui prot.fork) &) :: callsite provenance by needs
          =.  want.gen
            %-  (~(uno by want.gen) pant)
            |=  [@hail a=cape b=cape]
            (~(uni ca a) b)
          ::  check for recursion
          =/  pore  (~(get ja sirs) data.sock.fork)
          |-  ^-  [[=nomm =naan] _gen]
          ?^  pore
            =/  cope  (~(gut by want.gen) site.i.pore |)
            =/  tote  (~(app ca cope) sock.less.i.pore)
            ?:  (~(huge so tote) sock.sand)
              :: recursive
              =.  loop.gen  (~(put by loop.gen) roil site.i.pore)
              [[[%two sown fond roil] [~ | ~]] gen]
            $(pore t.pore)
          ::  not recursive
          :: analyze through direct call
          =/  dire  dire.gen
          =.  dire.gen  &
          =^  more  gen
            %=  arm-loop  
              form  data.sock.fork
              less  sand
              entr  roil
              sirs  (~(add ja sirs) data.sock.fork [roil sand])
            ==
          =.  dire.gen  ?&(dire dire.gen)
          [[[%two sown fond roil] more] gen]
        ::  indirect call
        =.  dire.gen  |
        [[[%two sown fond roil] [~ | ~]] gen]
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
        =/  salt  (~(darn so sock.hole) a.form sock.bite)
        ?~  salt  [[[%ten a.form twig tree] [~ | ~]] gen]
        :_  gen
        :-  [%ten a.form twig tree]
        [(~(put qui prot.hole) a.form prot.bite) u.salt]
      ::
          [%11 h=@ f=*]
        =^  [then=nomm bite=naan]  gen  $(form f.form)
        [[[%sip h.form then] bite] gen]
      ::
          [%11 [h=@ v=*] f=*]
        :: XX if =(h %fast) update cold state
        =^  hare  gen  [hare.gen gen(hare .+(hare.gen))]
        =^  [vice=nomm mild=naan]  gen  $(form v.form)
        =^  [then=nomm bite=naan]  gen  $(form f.form)
        ::  save body formula
        =.  hint.gen  (~(put ju hint.gen) entr hare)
        =.  forb.gen  (~(put by forb.gen) hare f.form)
        |^
          ?:  =(%fast h.form)
            ::  fast hint registration
            ?.  =(& cape.sock.mild)  ~&  %fast-hide-clue  fail
            =*  clue  data.sock.mild
            ?.  ?=([name=$@(@tas [@tas @]) pare=$%([%0 a=@] [%1 %0]) *] clue)
              ~&  [%fast-bad-clue clue]  fail
            =/  pell
              ?@  name.clue
                name.clue
              (crip "{(trip -.name.clue)}.{(trip (scot %ud +.name.clue))}")
            ?-  -.pare.clue
                %1
              :: register root
              ?.  =(& cape.sock.bite)
                ~&  %fast-hide-root  fail
              =.  core.cole.gen  (~(put ju core.cole.gen) ~[pell] sock.bite)
              =.  root.cole.gen  (~(put ju root.cole.gen) data.sock.bite ~[pell])
              fail
            ::
                %0
              :: register child core
              =/  butt  (~(pull so sock.bite) 2)
              ?~  butt
                ~&  %fast-miss-batt  fail
              =*  batt  u.butt
              ?.  =(& cape.batt)  ~&  %fast-hide-batt  fail
              ?.  ?=(^ data.batt)  fail
              =/  perk  (~(pull so sock.bite) a.pare.clue)
              ?~  perk  ~&  %fast-lost-sire  fail
              =*  park  u.perk
              =/  past=(set path)
                ?.  =(& cape.park)  ~
                (~(get ju root.cole.gen) data.park)
              =/  bork  (~(pull so park) 2)
              =?  past  ?&(?=(^ bork) =(& cape.u.bork) ?=(^ data.u.bork))
                (~(uni in past) (~(get ju batt.cole.gen) data.u.bork))
              =/  pale  ~(tap in past)
              |-  ^-  [[=nomm =naan] _gen]
              =*  pale-loop  $
              ?~  pale  fail
              =/  nape  [pell i.pale]
              =/  coal  ~(tap in (~(get ju core.cole.gen) i.pale))
              |-  ^-  [[=nomm =naan] _gen]
              ?~  coal  pale-loop(pale t.pale)
              ?.  (~(huge so park) i.coal)  $(coal t.coal)
              =/  naut
                =/  bake  (~(darn so [| ~]) 2 batt)
                ?>  ?=(^ bake)
                =/  folk  (~(darn so u.bake) a.pare.clue i.coal)
                ?>  ?=(^ folk)
                u.folk
              =.  core.cole.gen  (~(put ju core.cole.gen) nape naut)
              =.  batt.cole.gen  (~(put ju batt.cole.gen) data.batt nape)
              fail
            ==
          fail  ::  not a fast hint, just return analysis
        ++  fail  [[[%tip h.form vice then hare] bite] gen]
        --
      ::
          [%12 r=* p=*]
        =^  [rend=nomm rita=naan]  gen  $(form r.form)
        =^  [pond=nomm walk=naan]  gen  $(form p.form)
        [[[%elf rend pond] [~ | ~]] gen]
      ==
    ::
    =/  want  (~(gut by want.gen) entr |)
    ::  write to call table
    =.  call.gen  (~(put by call.gen) entr [less want more form `load])
    ::  write to memo table
    =?  memo  dire.gen
      =/  have  (~(rel qui prot.more) entr)
      =/  such  (~(uni ca want) (~(gut by (~(due qui prot.more) cape.sock.more)) entr |))
      =/  soot  ~(norm so (~(app ca such) sock.less))
      (~(add ja memo) form [soot want sock.more have])
    [more gen]
  ::
  =?  moan  ?=(~ calm.i.queu) :: don't write cold state arms into code table
    %-  ~(rep by call.gen)
    |=  [[site=@hail less=naan want=cape more=naan form=* load=(unit nomm)] =_moan]
    ?~  load  moan
    =/  soot  
      =>  [s=sock.less w=want so=so ca=ca]
      ~+  ~(norm so (~(app ca w) s))
    =/  loan  (~(get ja moan) form)
    ?:  (lien loan |=([soot=sock norm=food] =(soot ^soot)))
      moan
    =/  norm=food
      =|  ices=(map @hail [=sock form=*])
      =|  lope=(set [=sock form=*])
      =/  next  ~(tap in (~(get ju kids.gen) site))
      |-  ^-  food  
      ?^  next
        =/  n  (~(gut by loop.gen) i.next i.next)
        =/  c  (~(got by call.gen) n)
        =/  s
          =>  [s=sock.less.c w=want.c so=so ca=ca]
          ~+  ~(norm so (~(app ca w) s))
        =.  ices  (~(put by ices) i.next [s form.c])
        =?  lope  ?!(.=(i.next n))  (~(put in lope) [s form.c])
        $(next t.next)
      =/  hiss  (~(get ju hint.gen) site)  
      =/  fizz
        %-  ~(rep in hiss)
        |=  [h=@hint fizz=(map @hint *)]
        (~(put by fizz) h (~(got by forb.gen) h))
      [u.load ices lope fizz]
    (~(add ja moan) form [soot norm])
  =.  cole  cole.gen
  =/  want  want:(~(got by call.gen) entr)
  =/  boot
    :-
      =>  [s=soot.i.queu w=want so=so ca=ca]
      ~+  ~(norm so (~(app ca w) s))
    form
  =?  call.cole  ?=(^ calm.i.queu)  (~(put by call.cole) boot j.u.calm.i.queu)
  =?  back.cole  ?=(^ calm.i.queu)  (~(put ju back.cole) j.u.calm.i.queu boot)
  =.  memo  memo.gen
  cold-loop(queu t.queu)
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
    |-  ^-  prot
    ?:  =(1 axe)  prog
    ?-  (cap axe)
      %2  $(axe (mas axe), prog hed)
      %3  $(axe (mas axe), prog tal)
    ==
  ++  put
    |=  [axe=@ poor=prot]
    ?<  =(0 axe)
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
      %2  $(poor (con(prog poor) p.i.tack), tack t.tack)
      %3  $(poor (con(prog p.i.tack) poor), tack t.tack)
    ==
  ::
  ::    provenance tree for +2
  ++  hed
    ^-  prot
    ?~  prog  ~
    =/  slav  (pepe n.prog 2)
    ?~  l.prog  [slav ~ ~]
    l.prog(n (weld slav n.l.prog))
  ::
  ::    provenance tree for +3
  ++  tal
    ^-  prot
    ?~  prog  ~
    =/  slav  (pepe n.prog 3)
    ?~  r.prog  [slav ~ ~]
    r.prog(n (weld slav n.r.prog))
  ::
  ::    provenance tree from two subtrees (cons)
  ++  con
    |=  poor=prot
    ^-  prot
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
    =/  unica  |=([@hail a=cape b=cape] (~(uni ca a) b))
    |=  cave=cape
    ^-  (map @hail cape)
    ?:  =(| cape)  ~
    ?~  prog  ~
    =/  [cale=cape care=cape]  ~(rip ca cave)
    %-  
      :: XX wrong: unify repeated callsites at node
      %~  gas  by 
      ((~(uno by $(cave cale, prog l.prog)) $(cave care, prog r.prog)) unica)
    %+  turn  n.prog
    |=  [site=@hail axe=@]
    [site (~(pat ca cave) axe)]
  ::
  ::    given a callsite produce a new provenance tree only retaining
  ::    provenance for that callsite's subject
  ++  rel
    |=  site=@hail
    ^-  plop
    ?~  prog  ~
    =/  n  (murn n.prog |=(p=peon ?:(=(site site.p) `axe.p ~)))
    =/  l  $(prog l.prog)
    =/  r  $(prog r.prog)
    ?:  ?&(?=(~ l) ?=(~ r) ?=(~ n))  ~
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
--
::  utility types
::
|%
::
::    abstract noun with provenance
+$  naan  [=prot =sock]
::
::    callsite information
+$  cafe  (map @hail [less=naan want=cape more=naan form=* load=(unit nomm)])
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
+$  meme  [soot=sock want=cape root=sock have=plop]
--
