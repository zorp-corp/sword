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
|%
++  thus  .
::
::    Analyze a subject/formula pair
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
  =.  colt  cole
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
  =|  $=  gen
      $:
          rail=@hail  :: generator for call sites
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
          wait=(jar @hail @hail)  :: sites to finalize
      ==
  =^  entr  gen  [rail.gen gen(rail .+(rail.gen))]  :: initial callsite
  =^  more  gen
    =/  less=naan  [~ soot.i.queu]  :: subject
    =*  form  form.i.queu :: formula
    =/  sirs  (~(add ja *(jar * [site=@hail less=naan])) form [entr less]) :: 
    ::  XX write into this when you refuse memoization
    ::    for each entry in loop.gen
    ::    - generate plop for noon by rel:qui of prot from naan with roil
    ::      at memoizing callsite
    ::  XX check this after memo in same manner as memo
    ::    on hit
    ::    -  reconstruct result in same manner as memo
    ::    -  generate fresh callsites for each entry in loom and put in
    ::       loop.gen, rue:qui noon with subject provenance at melo hit site
    ::    -  insert each [fresh-callsite roil] into demo
    ::    after fixpoint, intersect nop and demo and force roils
    ::       from intersection to be indirect (delete from call.gen)
    ::  XX  melo=(jar * meal) 
    ::  XX  demo=(map @hail @hail)
    =|  lord=(set @hail)  :: enclosing scope
    =/  tack=(list @hail)  ~[entr]
    ::  wrapper for callsite formulas
    |-  ^-  [naan _gen]
    =*  arm-loop  $
    =.  prot.less  (~(tag qui prot.less) [entr 1])
    ::  check if memoized
    =/  germ  (~(get ja memo.gen) form)
    |-  ^-  [naan _gen]
    ?^  germ
      ?.  (~(huge so soot.i.germ) sock.less)
        $(germ t.germ)
      =/  mope  (~(rue qui prot.less) have.i.germ)
      =.  mope  (~(cut qui mope) lord cape.root.i.germ)
      =/  more  [mope root.i.germ]
      ::  propagate memoized subject needs
      =/  pant  (~(due qui prot.less) want.i.germ)
      =.  want.gen
        %-  (~(uno by want.gen) pant)
        |=  [@hail a=cape b=cape]
        ~(cut ca (~(uni ca a) b))
      =.  call.gen  (~(put by call.gen) entr [less more form ~ &])
      [more gen]
    =^  [load=nomm more=naan]  gen
      :: structurally recur over formula
      |-  ^-  [[=nomm =naan] _gen]
      =*  nock-loop
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
            ~(cut ca (~(uni ca a) b))
          ::  check for recursion
          =/  pore  (~(get ja sirs) data.sock.fork)
          |-  ^-  [[=nomm =naan] _gen]
          ?^  pore
            =/  cope  (~(gut by want.gen) site.i.pore |)
            =/  tote  (~(app ca cope) sock.less.i.pore)
            ?:  (~(huge so tote) sock.sand)
              :: recursive
              =.  loop.gen  (~(put by loop.gen) roil [site.i.pore sock.less.i.pore sand])
              :: push 
              =|  wire=(list (list @hail))
              |-  ^-  [[nomm naan] _gen] 
              ?>  ?=(^ tack)
              =/  fire  (~(get ja wait.gen) i.tack)
              ?.  (lien fire |=(h=@hail =(site.i.pore h)))
                =/  wise  (~(get ja wait.gen) i.tack)
                =.  wait.gen  (~(del by wait.gen) i.tack)
                =.  wire  [wise wire]
                $(tack t.tack)
              =.  wire  [(~(get ja wait.gen) i.tack) wire]
              =.  wait.gen  (~(put by wait.gen) i.tack (zing (flop wire)))
              [[[%two sown fond roil] [~ | ~]] gen]
            $(pore t.pore)
          ::  not recursive
          :: analyze through direct call
          =/  dire  dire.gen
          =.  wait.gen  (~(add ja wait.gen) roil roil)
          =^  more  gen
            %=  arm-loop  
              form  data.sock.fork
              less  sand
              entr  roil
              sirs  (~(add ja sirs) data.sock.fork [roil sand])
              lord  (~(put in lord) entr)
              tack  [roil tack]
              dire.gen  &
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
            ?.  ?=([name=$@(@tas [@tas @]) pare=* *] clue)
              ~&  [%fast-bad-clue clue]  fail
            =/  pell
              ?@  name.clue
                name.clue
              (crip "{(trip -.name.clue)}.{(trip (scot %ud +.name.clue))}")
            |-  ^-  [[nomm naan] _gen]
            ?+  pare.clue  ~&  [%fast-bad-clue clue]  fail 
                [%11 * *]
              $(pare.clue +>.pare.clue)
            ::
                [%1 %0]
              :: register root
              ?.  =(& cape.sock.bite)
                ~&  %fast-hide-root  fail
              =.  core.cole.gen  (~(put ju core.cole.gen) ~[pell] sock.bite)
              =.  root.cole.gen  (~(put ju root.cole.gen) data.sock.bite ~[pell])
              fail
            ::
                [%0 a=@]
              ?:  =(0 @)  ~&  [%fast-bad-clue clue]  fail
              :: register child core
              =/  butt  (~(pull so sock.bite) 2)
              ?~  butt
                ~&  %fast-miss-batt  fail
              =*  batt  u.butt
              ?.  =(& cape.batt)  ~&  [%fast-hide-batt pell]  fail
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
              =/  nape=path  [pell i.pale]
              =/  coal  ~(tap in (~(get ju core.cole.gen) i.pale))
              |-  ^-  [[=nomm =naan] _gen]
              ?~  coal  pale-loop(pale t.pale)
              ?.  (~(huge so i.coal) park)  $(coal t.coal)
              =/  naut
                =/  bake  (~(darn so [| ~]) 2 batt)
                ?>  ?=(^ bake)
                =/  folk  (~(darn so u.bake) a.pare.clue i.coal)
                ?>  ?=(^ folk)
                u.folk
              ~>  %meme
              ~?  ?!((~(has by core.cole.gen) nape))  [%cold-into nape]
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
    ::  write to call table
    =.  prot.more  (~(cut qui prot.more) lord cape.sock.more)
    =.  call.gen  (~(put by call.gen) entr [less more form `load dire.gen])
    =/  wise  (~(get ja wait.gen) entr)
    =.  wait.gen  (~(del by wait.gen) entr)
    ?:  =(~ wise)  [more gen] :: no finalizing here
    ?>  =(entr (rear wise)) :: current callsite should be last item of finalization list
    ::  fixed-point loops to propagate their needs and check that they are really loops
    =/  sap  gen  :: for reset
    =|  nop=(map @hail [t=@hail s=sock l=naan])
    |-  ^-  [naan _gen]
    =*  redo-loop  $
    =.  gen  sap
    =.  loop.gen  (~(dif by loop.gen) nop)
    |-  ^-  [naan _gen]
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
    ?.  =(wap want.gen)  need-loop
    =/  nap  nop
    =.  nop
      %-  ~(rep by loop.gen)
      |=  [[c=@hail t=@hail s=sock l=naan] =_nop]
      =/  teed  (~(gut by want.gen) t |)
      =.  s  (~(app ca teed) s)
      ?.  (~(huge so s) sock.l)  (~(put by nop) c *[t=@hail s=sock l=naan])
      nop
    ?.  =(nap nop)  redo-loop
    ::  write to memo table
    =/  want=cape  (~(gut by want.gen) entr |)
    ::  finalize waiting callsites
    =.  gen
      ~|  wise=wise
      %+  roll  wise
      |=  [site=@hail =_gen]
      =/  kid  (~(get ju kids.gen) site)
      =+  (~(got by call.gen) site)
      ?>  ?=(^ load)
      =/  want=cape  (~(gut by want.gen) site |)
      =?  memo.gen  ?&(rect ?=(~ nop))
        =/  have  (~(rel qui prot.more) site cape.sock.more)
        =/  such  (~(uni ca want) (~(gut by (~(due qui prot.more) cape.sock.more)) site |))
        =/  soot  ~(norm so (~(app ca such) sock.less))
        (~(add ja memo.gen) form [soot want sock.more have])
      =/  soot  
        =>  [s=sock.less w=want so=so ca=ca]
        ~+  ~(norm so (~(app ca w) s))
      =^  [ices=(map @hail [=sock form=*]) lope=(set [=sock form=*])]  gen
        %-  ~(rep in kid)
        |=  [k=@hail [ices=(map @hail [=sock form=*]) lope=(set [=sock form=*])] =_gen]
        ~|  [k=k]
        =/  n  t:(~(gut by loop.gen) k [t=k s=*sock l=*naan])
        ~|  [n=n]
        ~|  [wk=~(key by call.gen)]
        ~|  [tack=tack]
        ~|  [entr=entr]
        =/  c  (~(got by call.gen) n)
        =/  w=cape  (~(gut by want.gen) n |)
        =/  s
          =>  [s=sock.less.c w=w so=so ca=ca]
          ~+  ~(norm so (~(app ca w) s))
        =.  ices  (~(put by ices) k [s form.c])
        =?  lope  ?!(.=(k n))  (~(put in lope) [s form.c])
        ::  trim want/call/loop tables
        [[ices lope] gen]
      =.  gen  
        %-  ~(rep in kid)
        |=  [k=@hail =_gen]
        ^-  _gen
        =.  want.gen  (~(del by want.gen) k)
        =.  call.gen  (~(del by call.gen) k)
        =.  loop.gen  (~(del by loop.gen) k)
        gen
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
      =?  moan.gen  (lien loan |=([soot=sock norm=food] =(soot ^soot)))
        (~(add ja moan.gen) form [soot u.load ices lope fizz])
      gen
    [more gen]
  ::
  =.  moan  moan.gen
  =.  memo  memo.gen
  =.  cole  cole.gen
  =/  want  (~(got by want.gen) entr)
  =/  boot
    :-
      =>  [s=soot.i.queu w=want so=so ca=ca]
      ~+  ~(norm so (~(app ca w) s))
    form
  =?  call.cole  ?=(^ calm.i.queu)  (~(put by call.cole) boot j.u.calm.i.queu)
  =?  back.cole  ?=(^ calm.i.queu)  (~(put ju back.cole) j.u.calm.i.queu boot)
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
  ++  cut
    |=  [sire=(set @hail) =cape]
    ^-  prot
    ?~  prog  ~
    ?:  =(| cape)  ~
    ?^  cape
      =/  n  (murn n.prog |=([s=@hail a=@] ?:((~(has in sire) s) `[s a] ~)))
      =/  l  $(prog l.prog, cape -.cape)
      =/  r  $(prog l.prog, cape +.cape)
      ?:  ?&(?=(~ n) ?=(~ l) ?=(~ r))  ~
      [n l r]
    =/  prop  `prot`prog
    |-  ^-  prot
    ?~  prop  ~
    =/  n  (murn n.prop |=([s=@hail a=@] ?:((~(has in sire) s) `[s a] ~)))
    =/  l  $(prop l.prop)
    =/  r  $(prop r.prop)
    ?:  ?&(?=(~ n) ?=(~ l) ?=(~ r))  ~
    [n l r]
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
    !.
    |-  ^-  (map @hail cape)
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
    |=  [site=@hail =cape]
    ^-  plop
    ?~  prog  ~
    ?:  =(| cape)  ~
    =/  n  (murn n.prog |=(p=peon ?:(=(site site.p) `axe.p ~)))
    =/  r  ~(rip ca cape)
    =/  l  $(prog l.prog, cape -.r)
    =/  r  $(prog r.prog, cape +.r)
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
::    abstract noun with local provenance
+$  noon  [=plop =sock]
::
::    callsite information
+$  cafe  (map @hail [less=naan more=naan form=* load=(unit nomm) rect=?])
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
::
::    loop-local analysis memoization entry
+$  meal  [soot=sock want=cape root=sock have=plop loom=(list [t=@ s=sock =noon])]
--
