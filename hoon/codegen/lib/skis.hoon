:: destination-driven subject knowledge analysis
::
!:
=|  moan=(jar * hone)
=|  cole=cool
=<
|%
++  keep
  |=  [live=(set [sock *]) seed=[sock *]]
  =/  work=(list [=sock f=*])  ~[seed]
  =|  krow=(list [=sock f=*])
  |-  ^-  _live
  ?~  work
    ?~  krow  live
    $(work (flop krow), krow ~)
  ?:  (~(has in live) i.work)  $(work t.work)
  =/  hose  (~(get ja moan) f.i.work)
  |-  ^-  _live
  ?~  hose  ~|  %keep-wiff  !!
  ?.  (~(huge so soot.i.hose) sock.i.work)  $(hose t.hose)
  =/  next  ~(val by ices.norm.i.hose)
  ^$(live (~(put in live) i.work), krow (weld next krow), work t.work)
::
::  drop labels not in the given live set
::
::  To build the live set, repeatedly invoke +keep, threading through
::  the live set, and passing labels which will still be needed. This
::  will ensure that transitive dependencies remain in +moan.
++  drop
  |=  live=(set [sock *])
  =|  noam=(jar * hone)
  =/  moal=(list [* (list hone)])  ~(tap by moan)
  |-  ^-  _thus
  ?~  moal  thus(moan noam)
  |-  ^-  _thus
  ?~  +.i.moal  ^$(moal t.moal)
  =/  bell  [soot.i.+.i.moal -.i.moal]
  ?.  (~(has in live) bell)  $(+.i.moal t.+.i.moal)
  $(+.i.moal t.+.i.moal, noam (~(add ja noam) -.i.moal i.+.i.moal))
::
:: this core
++  thus  .
::  subject knowledge analysis loop
::
::  XX TODO
::  - caching of analysis
::  - check if compatible with finalization before putting in worklist
::  - cold state
++  rout
  |=  [soot=* form=*]
  =/  colt  cole  :: cold state from previous iteration, for diffing
  =/  queu=(list todo)  [[& soot] form ~]~
  =|  back=(list todo)
  |-  ^-  _thus
  =*  cold-loop  $
  ::  check for unprocessed cold state registrations
  =/  gnat  ((dif-ju core.cole) core.colt)
  =.  back
    %-  ~(rep by gnat)
    |:  [[p=*path q=*(set sock)] b=back]
    %-  ~(rep in q)
    |:  [s=*sock b=b]
    =/  batt  (~(pull so s) 2)
    ?~  batt  b
    ?.  =(& cape.u.batt)  ~&  [%cold-miss-batt p]  b
    =*  f  data.u.batt
    =/  a=@  1
    |-  ^-  _b
    ?.  ?=([^ *] f)
      [[s f `[| p a]] b]
    =.  b  $(f -.f, a (peg a 2))
    =.  b  $(f +.f, a (peg a 3))
    [[s f `[& p a]] b]
  =.  colt  cole
  :: dequeu one analysis pair
  ?~  queu
    ?~  back  thus
    cold-loop(queu (flop back), back ~)
  ?:  ?&(?=(^ calm.i.queu) auto.u.calm.i.queu)
    ::  if the formula is an autocons from a battery, shortcut
    ::  by just combining the socks
    =/  [p=path a=@]  j.u.calm.i.queu
    =/  hell  ~(tap in (~(get ju back.cole) [p (peg a 2)]))
    =/  tell  ~(tap in (~(get ju back.cole) [p (peg a 3)]))
    ?>  ?=([^ *] form.i.queu)
    =.  cole
      %+  roll  hell
      |:  [[hs=*sock hf=**] c=cole]
      ^-  _c
      ?.  =(-.form.i.queu hf)  c
      ?.  (~(huge so soot.i.queu) hs)  c
      %+  roll  tell
      |:  [[ts=*sock tf=**] c=c]
      ^-  _c
      ?.  =(+.form.i.queu tf)  c
      ?.  (~(huge so soot.i.queu) ts)  c
      =/  s  (~(pack so hs) ts)
      ?>  (~(huge so soot.i.queu) s)
      %=  c
        call  (~(put by call.c) [s form.i.queu] j.u.calm.i.queu)
        back  (~(put ju back.c) j.u.calm.i.queu [s form.i.queu])
      ==
    cold-loop(queu t.queu)
  :: analysis state
  =|  rail=@hail  ::  generator for callsites
  =|  hare=@hint  ::  generator for hint sites
  =|  rain=@uvar  ::  generator for dataflow graph nodes
  =|  kids=(jug @hail @hail)  ::  child callsites
  =|  sirs=(map @hail (jug * @hail))  ::  ancestor callsites
  =|  loop=(map @hail @hail)  ::  recursion targets
  =^  entr  rail  [rail .+(rail)]
  =^  sv  rain  [rain .+(rain)]
  =^  fv  rain  [rain .+(rain)]
  =^  ov  rain  [rain .+(rain)]
  =/  gr  c
  =.  gr  (plea:gr fv)
  =.  gr  (prem:gr sv soot.i.queu)
  =.  gr  (prem:gr fv [& form.i.queu])
  =/  call  (~(put by *cafe) entr [~ sv fv ov ~])
  =|  fats=fate
  =/  tack=(list (each [done=? site=@hail] @hint))  [%& | entr]~
  :: depth-first traversal of call graph
  |-  ^-  _thus
  =*  rout-loop  $
  ?~  tack
    ::  no more work, write entries to moan
    =/  flak=(list @hail)  ~[entr]
    =|  flux=(list @hail)
    |-  ^-  _thus
    ?^  flak
      $(flak (weld ~(tap in (~(get ju kids) i.flak)) t.flak), flux [i.flak flux])
    =/  bolt
      |_  belt=(map @hail [=sock form=*])
      ++  this  .
      ++  get
        |=  h=@hail
        ^-  [(unit [=sock form=*]) _this]
        =/  bull  (~(get by belt) h)
        ?^  bull  [`u.bull this]
        =/  cull  (~(get by call) h)
        ?~  cull  [~ this]
        =/  fork  (peek:gr form.u.cull)
        ?.  =(& cape.fork)  [~ this]
        =/  soot  (peek:gr less.u.cull)
        =.  soot  (~(app ca (dine:gr less.u.cull)) soot)
        =/  bell  [soot data.fork]
        [`bell this(belt (~(put by belt) h bell))]
      --
    |-  ^-  _thus
    =*  food-loop  $
    ?^  flux
      =/  cull  (~(get by call) i.flux)
      ?~  cull  food-loop(flux t.flux)
      ?~  load.u.cull  food-loop(flux t.flux)
      =^  f  bolt  (get:bolt i.flux)
      ?~  f  food-loop(flux t.flux)
      =/  mile  ~(tap in (~(get ju kids) i.flux))
      =|  ices=(map @hail [=sock form=*])
      =|  loop=(set [=sock form=*])
      |-  ^-  _thus
      ?^  mile
        =/  k  (~(gut by ^loop) i.mile i.mile)
        =^  b  bolt  (get:bolt k)
        ?~  b  $(mile t.mile)
        =?  loop  ?!(=(k i.mile))  (~(put in loop) u.b)
        =.  ices  (~(put by ices) i.mile u.b)
        $(mile t.mile)
      =/  root  (peek:gr more.u.cull)
      =.  root  (~(app ca (dine:gr more.u.cull)) root)
      %=  food-loop
        flux  t.flux
        moan  (~(add ja moan) form.u.f ^-(hone [sock.u.f [u.load.u.cull ices loop] root])) 
      ==
    =?  cole  ?=(^ calm.i.queu)
      =/  bell  (need -:(get:bolt entr))
      ?<  auto.u.calm.i.queu
      %=  cole
        call  (~(put by call.cole) bell j.u.calm.i.queu)
        back  (~(put ju back.cole) j.u.calm.i.queu bell)
      ==
    cold-loop(queu t.queu)
  ?:  ?=(%| -.i.tack)
    :: parse a fast hint and register a core
    =/  comb  (~(got by fats) p.i.tack) :: fast hint information
    =/  clue  (peek:gr hive.comb)       :: hint clue: label and parent axis
    ?.  =(& cape.clue)  ~&  [%fast-hide-clue clue]  rout-loop(tack t.tack)
    ?.  ?=([name=fame pear=$%([%1 %0] [%0 a=@]) *] data.clue)
      ~&  [%fast-tear-clue data.clue]  rout-loop(tack t.tack)
    =/  nate=@tas  :: name to path element
      ?^  name.data.clue
        (crip "{(trip p.name.data.clue)}.{(trip (scot %ud q.name.data.clue))}")
      name.data.clue
    =/  cure  (peek:gr bees.comb)  :: hinted core
    ?:  ?=(%1 -.pear.data.clue)
      ?.  =(& cape.cure)  ~&  [%fast-hide-batt data.clue]  rout-loop(tack t.tack)
      =/  pax  ~[nate]
      =.  core.cole  (~(put ju core.cole) pax cure)
      =.  root.cole  (~(put ju root.cole) data.cure pax)
      rout-loop(tack t.tack)
    =/  burr  (~(pull so cure) 2)  :: battery
    ?~  burr  ~&  [%fast-tear-core data.clue]  rout-loop(tack t.tack)
    ?.  =(& cape.u.burr)  ~&  [%fast-hide-batt data.clue]  rout-loop(tack t.tack)
    ?@  data.u.burr  ~&  [%fast-atom-batt data.clue]  rout-loop(tack t.tack)
    ?:  =(0 a.pear.data.clue)  ~&  [%fast-zero-clue data.clue]  rout-loop(tack t.tack)
    =/  pure  (~(pull so cure) a.pear.data.clue) :: parent
    ?~  pure  ~&  [%fast-tear-pear data.clue]  rout-loop(tack t.tack)
    =/  bare  (~(pull so u.pure) 2) :: parent battery
    =/  glop=(set path)  ?.  =(& cape.u.pure)  ~  (~(get ju root.cole) data.u.pure)
    =/  glue=(set path)
      ?~  bare  ~
      ?.  ?&(=(& cape.u.bare) ?=(^ data.u.bare))  ~
      (~(get ju batt.cole) data.u.bare)
    =/  gook  (~(uni in glop) glue)
    ~?  =(~ gook)  [%fast-gook-miss data.clue]
    %=  rout-loop
        tack  t.tack
        cole
      %-  ~(rep in (~(uni in glop) glue))
      |:  [p=*path c=cole]
      ^-  _cole
      %-  ~(rep in (~(get ju core.cole) p))
      |:  [s=*sock c=c]
      ^-  _cole
      ?.  (~(huge so u.pure) s)  c
      =/  pax  [nate p]
      =/  cobb  (~(darn so [| ~]) 2 u.burr)
      ?<  ?=(~ cobb)
      =/  hobb  (~(darn so u.cobb) a.pear.data.clue s) :: battery hierarchy
      ?<  ?=(~ hobb)
      %=  c
        core  (~(put ju core.c) pax u.hobb) 
        batt  (~(put ju batt.c) data.u.burr pax) 
      ==
    ==
  ?:  done.p.i.tack
    :: finished with a callsite and its children
    :: XX later we'll do caching here
    rout-loop(tack t.tack)
  ::  analyze a callsite
  =/  brew  (~(got by call) site.p.i.tack)
  =/  fork  (peek:gr form.brew)
  :: if we don't know the formula now we won't know it, as
  :: data can only flow from callsites we've already analyzed
  ?.  =(& cape.fork)
    rout-loop(tack t.tack)
  =*  nock  data.fork
  ::  check if recursive
  =/  sous  (peek:gr less.brew)
  =/  mire=(jug * @hail)  (~(gut by sirs) site.p.i.tack *(jug * @hail))
  =/  pore  (~(get ju mire) nock)
  :: =/  hole  (peek:gr less.brew)
  =/  lope
      %-  ~(gas in *(set @hail))
      %+  murn  ~(tap in pore)
      |=  sire=@hail
      =/  beer  (~(got by call) sire)
      =/  foam  (peek:gr less.beer)
      =.  foam  (~(app ca (dine:gr less.beer)) foam)
      ?:((~(huge so foam) sous) `sire ~)
  ?.  =(~ lope)  :: recursive callsite(s) found
    |-  ^-  _thus  ::  find nearest callsite
    ?>  ?=(^ sire.brew)
    ?:  (~(has in lope) u.sire.brew)
      rout-loop(loop (~(put by loop) site.p.i.tack u.sire.brew), tack t.tack)
    $(brew (~(got by call) u.sire.brew))
  ::  lower entries in work to nomm
  =/  rate  [rail=rail hare=hare]
  =^  lire  rate
    |-  ^-  [nomm _rate]
    ?+  nock  [[%not 0] rate]
        [l=^ r=*]
      =^  left  rate  $(nock l.nock)
      =^  rite  rate  $(nock r.nock)
      [[%par left rite] rate]
    ::
        [%0 a=@]
      [[%not a.nock] rate]
    ::
        [%1 n=*]
      [[%one n.nock] rate]
    ::
        [%2 s=* f=*]
      =^  sn  rate  $(nock s.nock)
      =^  fn  rate  $(nock f.nock)
      [[%two sn fn rail.rate] rate(rail .+(rail.rate))]
    ::
        [%3 e=*]
      =^  en  rate  $(nock e.nock)

      [[%the en] rate]
    ::
        [%4 e=*]
      =^  en  rate  $(nock e.nock)
      [[%for en] rate]
    ::
        [%5 l=* r=*]
      =^  ln  rate  $(nock l.nock)
      =^  rn  rate  $(nock r.nock)
      [[%ivy ln rn] rate]
    ::
        [%6 c=* z=* o=*]
      =^  cn  rate  $(nock c.nock)
      =^  zn  rate  $(nock z.nock)
      =^  on  rate  $(nock o.nock)
      [[%six cn zn on] rate]
    ::
        [%7 a=* b=*]
      =^  an  rate  $(nock a.nock)
      =^  bn  rate  $(nock b.nock)
      [[%eve an bn] rate]
    ::
        [%8 a=* b=*]
      ?@  a.nock  ~&  [%eight-atom nock]  $(nock [7 [[0 0] 0 1] b.nock])
      $(nock [7 [a.nock 0 1] b.nock])
    ::
        [%9 b=@ c=*]
      $(nock [7 c.nock 2 [0 1] 0 b.nock])
    ::
        [%10 [a=@ p=*] t=*]
      =^  pn  rate  $(nock p.nock)
      =^  tn  rate  $(nock t.nock)
      [[%ten a.nock pn tn] rate]
    ::
        [%11 t=@ b=*]
      =^  bn  rate  $(nock b.nock)
      [[%sip t.nock bn] rate]
    ::
        [%11 [t=@ h=*] b=*]
      =^  hn  rate  $(nock h.nock)
      =^  bn  rate  $(nock b.nock)
      [[%tip t.nock hn bn hare.rate] rate(hare .+(hare.rate))]
    ::
        [%12 r=* p=*]
      =^  rn  rate  $(nock r.nock)
      =^  pn  rate  $(nock p.nock)
      [[%elf rn pn] rate]
    ==
  =.  rail  rail.rate
  =.  hare  hare.rate
  =.  call  (~(put by call) site.p.i.tack brew(load `lire))
  ::  set up edges for nomm
  =/  gen  [gr=gr rain=rain call=call kids=kids sirs=sirs fats=fats maid=*(list (each @hail @hint))]
  =/  [loud=(unit nomm) less=@uvar more=@uvar]
    [load less more]:(~(got by call) site.p.i.tack)
  ?>  ?=(^ loud)
  =/  load  u.loud
  =/  goal=nick  [%this more]
  =>  |%
      ++  kerf
        |=  goal=nick
        ^-  [@uvar _gen]
        ?-  -.goal
            %none
          =^  nv  gen  [rain.gen gen(rain .+(rain.gen))]
          [nv gen]
        ::
            %this
          [v.goal gen]
        ::
            %both
          =^  hv  gen  $(goal l.goal)
          =^  tv  gen  $(goal r.goal)
          =^  cv  gen  [rain.gen gen(rain .+(rain.gen))]
          =.  gr.gen  (edge:gr.gen [%hed cv hv])
          =.  gr.gen  (edge:gr.gen [%tal cv tv])
          [cv gen]
        ==
      --
  =^  coal  gen
    |^  ^-  [nick _gen]
      ?-  -.load
          %par
        =^  [gl=nick gr=nick]  gen  (park goal)
        =^  lg  gen  $(load left.load, goal gl) 
        =^  rg  gen  $(load rite.load, goal gr)
        (copy lg rg)
      ::
          %not
        [(from goal here.load) gen]
      ::
          %one
        [[%none ~] (just goal moan.load)]
      ::
          %two
        =.  maid.gen  [[%& rail.load] maid.gen]
        =^  ov  gen  (kerf goal)
        =^  sv  gen  [rain.gen gen(rain .+(rain.gen))]
        =^  fv  gen  [rain.gen gen(rain .+(rain.gen))]
        =.  call.gen  (~(put by call.gen) rail.load [`site.p.i.tack sv fv ov ~])
        =.  kids.gen  (~(put ju kids.gen) site.p.i.tack rail.load)
        =.  sirs.gen
          =/  sigs  (~(gut by sirs.gen) site.p.i.tack *(jug * @hail))
          =.  sigs  (~(put ju sigs) data.fork site.p.i.tack)
          (~(put by sirs.gen) rail.load sigs)
        =.  gr.gen  (plea:gr.gen fv)
        =^  fg  gen  $(goal [%this fv], load corn.load)
        =^  sg  gen  $(goal [%this sv], load cost.load)
        (copy sg fg)
      ::
          %the
        $(goal [%none ~], load pell.load)
      ::
          %for
        $(goal [%none ~], load mall.load)
      ::
          %ivy
        =^  rg  gen  $(goal [%none ~], load that.load)
        =^  lg  gen  $(goal [%none ~], load this.load)
        (copy lg rg)
      ::
          %six
        =^  rv  gen  (kerf goal)
        =^  zv  gen  [rain.gen gen(rain .+(rain.gen))]
        =^  ov  gen  [rain.gen gen(rain .+(rain.gen))]
        =.  gr.gen  (edge:gr.gen %int zv ov rv)
        =^  og  gen  $(goal [%this ov], load else.load)
        =^  zg  gen  $(goal [%this zv], load then.load)
        =^  ig  gen  (sect zg og)
        =^  cg  gen  $(goal [%none ~], load what.load)
        (copy cg ig)
      ::
          %eve
        =^  tg  gen  $(load then.load)
        $(load once.load, goal tg)
      ::
          %ten
        =^  [tg=nick pg=nick]  gen  (diet here.load goal)
        =^  tig  gen  $(load tree.load, goal tg)
        =^  pig  gen  $(load twig.load, goal pg)
        (copy tig pig)
      ::
          %sip
        $(load then.load)
      ::
          %tip
        ?:  ?=(%fast hint.load)
          =.  maid.gen  [[%| hare.load] maid.gen]
          =^  bv  gen  (kerf goal)
          =^  tg  gen  $(goal [%this bv], load then.load)
          =^  hv  gen  [rain.gen gen(rain .+(rain.gen))]
          =^  hg  gen  $(goal [%this hv], load vice.load)
          =.  fats.gen  (~(put by fats.gen) hare.load [site.p.i.tack hv bv])
          (copy hg tg)
        ?:  ?=(%slow hint.load)
          $(goal [%none ~], load vice.load)
        =^  tg  gen  $(load then.load)
        =^  vg  gen  $(goal [%none ~], load vice.load)
        (copy vg tg)
      ::
          %elf
        =^  pg  gen  $(goal [%none ~], load walk.load)
        =^  rg  gen  $(goal [%none ~], load rent.load)
        (copy rg pg)
      ==
    ::  split a nick into two, adding con edge
    ++  park
      |=  goal=nick
      ^-  [[nick nick] _gen]
      ?-  -.goal
          %none  [[[%none ~] %none ~] gen]
          %both  [[l.goal r.goal] gen]
          %this
        =^  lv  gen  [rain.gen gen(rain .+(rain.gen))]
        =^  rv  gen  [rain.gen gen(rain .+(rain.gen))]
        =.  gr.gen  (edge:gr.gen %con lv rv v.goal)
        [[[%this lv] %this rv] gen]
      ==
    ::  split a nick along an axis, adding con edges if necessary
    ++  diet
      |=  [a=@ g=nick]
      ^-  [[nick nick] _gen]
      ?:  =(0 a)  [[[%none ~] %none ~] gen]
      =|  tack=(list [w=?(%2 %3) g=nick])
      |-  ^-  [[nick nick] _gen]
      ?.  =(1 a)
      ?-  -.g
          %none  $(tack [[(cap a) g] tack], a (mas a))
          %both
        ?-  (cap a)
            %2
          $(tack [[%2 r.g] tack], g l.g, a (mas a))
        ::
            %3
          $(tack [[%3 l.g] tack], g r.g, a (mas a))
        ==
      ::
          %this
        =^  lv  gen  [rain.gen gen(rain .+(rain.gen))]
        =^  rv  gen  [rain.gen gen(rain .+(rain.gen))]
        =.  gr.gen  (edge:gr.gen [%con lv rv v.g])
        ?-  (cap a)
            %2
          $(tack [[%2 %this rv] tack], g [%this lv], a (mas a))
        ::
            %3
          $(tack [[%3 %this lv] tack], g [%this rv], a (mas a))
        ==
      ==
    =/  tg=nick  [%none ~]
    |-  ^-  [[nick nick] _gen]
    ?~  tack  [[tg g] gen]
    %=  $
        tack  t.tack
        tg
      ?-  w.i.tack
        %2  [%both tg g.i.tack]
        %3  [%both g.i.tack tg]
      ==
    ==
    ::  combine two nicks, adding tis edges
    ++  copy
      |=  [a=nick b=nick]
      ^-  [nick _gen]
      ?:  ?=(%none -.a)  [b gen]
      ?:  ?=(%none -.b)  [a gen]
      ?-  -.a
          %both
        ?-  -.b
            %both
          =^  l  gen  (copy l.a l.b)
          =^  r  gen  (copy r.a r.b)
          [[%both l r] gen]
        ::
            %this
          =^  lb  gen  [rain.gen gen(rain .+(rain.gen))]
          =^  rb  gen  [rain.gen gen(rain .+(rain.gen))]
          =.  gr.gen  (edge:gr.gen %con lb rb v.b)
          =^  l  gen  (copy l.a [%this lb])
          =^  r  gen  (copy r.a [%this rb])
          [[%both l r] gen]
        ==
      ::
          %this
        ?-  -.b
            %both
          =^  la  gen  [rain.gen gen(rain .+(rain.gen))]
          =^  ra  gen  [rain.gen gen(rain .+(rain.gen))]
          =.  gr.gen  (edge:gr.gen %con la ra v.a)
          =^  l  gen  (copy [%this la] l.b)
          =^  r  gen  (copy [%this ra] r.b)
          [[%both l r] gen]
        ::
            %this
          =?  gr.gen  ?!(=(v.a v.b))  (edge:gr.gen %tis v.a v.b)
          [a gen]
        ==
      ==
    ::  intersect two nicks, adding con and tis edges
    ++   sect
      |=  [z=nick o=nick]
      ^-  [nick _gen]
      ?:  ?=(%none -.z)
        =^  ov  gen  (kerf o)
        [[%this ov] gen]
      ?:  ?=(%this -.z)
        =^  ov  gen  (kerf o)
        =.  gr.gen  (edge:gr.gen %tis v.z ov)
        [z gen]
      ?:  ?=(%none -.o)
        =^  zv  gen  (kerf z)
        [[%this zv] gen]
      ?:  ?=(%this -.o)
        =^  zv  gen  (kerf z)
        =.  gr.gen  (edge:gr.gen %tis v.o zv)
        [o gen]
      ::  both are %both
      =^  lg  gen  $(z l.z, o l.o)
      =^  rg  gen  $(z r.z, o r.o)
      [[%both lg rg] gen]
    ::  push a goal down by an axis
    ++  from
      |=  [goal=nick here=@]
      ?:  =(0 here)  [%none ~]
      |-  ^-  nick
      ?:  =(1 here)  goal
      ?-  (cap here)
        %2  [%both $(here (mas here)) %none ~]
        %3  [%both [%none ~] $(here (mas here))]
      ==
    ::  split a noun out to variables as premises
    ++  just
      |=  [goal=nick n=*]
      ^-  _gen
      ?-  -.goal
          %none  gen
          %this  gen(gr (prem:gr.gen v.goal [& n]))
          %both
        ?@  n  gen
        =.  gen  $(goal l.goal, n -.n)
        $(goal r.goal, n +.n)
      ==
    --
  =^  iv  gen  (kerf coal)
  =.  gr.gen  (edge:gr.gen %tis less iv)
  =.  gr.gen  (fork:gr.gen less)
  =.  gr.gen  (rock:gr.gen more)
  =.  gr  gr.gen
  =.  rain  rain.gen
  =.  call  call.gen
  =.  kids  kids.gen
  =.  sirs  sirs.gen
  =.  fats  fats.gen
  ::  propagate knowledge and needs
  =.  gr  push:gr
  =.  gr  pull:gr
  =.  done.p.i.tack  &
  %=  rout-loop
      tack
    %+  welp
      %+  turn  maid.gen
        |=  site=(each @hail @hint)
        ^-  (each [done=? site=@hail] @hint)
        ?-  -.site
          %&  [%& | p.site]
          %|  [%| p.site]
        ==
    tack
  ==
::
::  subject knowledge / need propagation and state
::
::  XX TODO
::  - don't propagate variables if no update
::  - store pull/push queues in-core and reset after execution
++  c
  =|  fq=(list @uvar)
  =|  rq=(list @uvar)
  =|  fn=(set @uvar)
  =|  rn=(set @uvar)
  |_  =dish
  ++  this  .
  ::  get current sock at uvar
  ++  peek
    |=  v=@uvar
    ^-  sock
    =/  suck  (~(get by sign.dish) v)
    ?:  ?=(^ suck)  u.suck
    =/  nuon  (~(get by init.dish) v)
    ?~  nuon  [| ~]  
    u.nuon
  ::  get current cape at uvar
  ++  dine
    |=  v=@uvar
    ^-  cape
    ?:  (~(has in fine.dish) v)
      &
    (~(gut by wine.dish) v |)
  ::  pull need capes backward from uvars in queu
  ++  pull
    =*  queu  rq
    =|  back=(list @uvar)
    |-  ^-  _this
    =*  pull-loop  $
    ?:  =(~ queu)
      ?~  back  this(rn ~)
      $(queu (flop back), back ~)
    =/  mane  
      %~  tap  in
      %+  roll  queu
      |=  [v=@uvar m=(set @uvar)]
      (~(uni in m) (~(get ju back.dish) v))
    |-  ^-  _this
    =*  mane-loop  $
    ?~  mane  pull-loop(queu ~)
    =/  mine=cape  |
    =/  menu  (~(get ja fore.dish) i.mane)
    |-  ^-  _this
    ?~  menu
      =/  kirk  (dine i.mane)
      =/  beck  (~(int in (~(get ju back.dish) i.mane)) rn)
      =?  back  ?!(?&(=(kirk mine) =(~ beck)))  [i.mane back]
      %=  mane-loop
        mane  t.mane
        wine.dish  (~(put by wine.dish) i.mane mine)
      ==
    ?-  -.i.menu
        %con
      =/  [hc=cape tc=cape]  ~(rip ca (dine d.i.menu))
      ?:  =(i.mane h.i.menu)
        %=  $
          menu  t.menu
          mine  (~(uni ca hc) mine)
        ==
      ?>  =(i.mane t.i.menu)
      %=  $
        menu  t.menu
        mine  (~(uni ca tc) mine)
      ==
    ::
        %int
      =/  c  (dine d.i.menu)
      ?>  ?|(=(i.mane l.i.menu) =(i.mane r.i.menu))
      %=  $
        menu  t.menu
        mine  (~(uni ca c) mine)
      ==
    ::
        %hed
      =/  c  (~(pat ca (dine d.i.menu)) 2)
      ?>  =(i.mane s.i.menu)
      %=  $
        menu  t.menu
        mine  (~(uni ca c) mine)
      ==
    ::
        %tal
      =/  c  (~(pat ca (dine d.i.menu)) 3)
      ?>  =(i.mane s.i.menu)
      %=  $
        menu  t.menu
        mine  (~(uni ca c) mine)
      ==
    ::
        %tis
      =/  c  (dine d.i.menu)
      ?>  =(i.mane s.i.menu)
      %=  $
        menu  t.menu
        mine  (~(uni ca c) mine)
      ==
    ==
  ::
  ::  propagate knowledge forward starting at variables in queu
  ++  push
    =*  queu  fq
    =|  back=(list @uvar)
    |-  ^-  _this
    ?~  queu
      ?~  back  this(fn ~)
      $(queu (flop back), back ~)
    =/  menu  (~(get ja fore.dish) i.queu)
    |-  ^-  _this
    ?~  menu  ^$(queu t.queu)
    ?-  -.i.menu
        %con
      =/  hs  (peek h.i.menu)
      =/  ts  (peek t.i.menu)
      %=  $
        menu  t.menu
        back  [d.i.menu back]
        sign.dish  (~(put by sign.dish) d.i.menu (~(knit so hs) ts))
      ==
    ::
        %int
      =/  ls  (peek l.i.menu)
      =/  rs  (peek r.i.menu)
      %=  $
        menu  t.menu
        back  [d.i.menu back]
        sign.dish  (~(put by sign.dish) d.i.menu (~(purr so ls) rs))
      ==
    ::
        %hed
      =/  ss  (peek s.i.menu)
      =/  ps  (~(pull so ss) 2)
      =/  ds=sock
        ?~  ps  [| ~]  u.ps
      %=  $
          menu  t.menu
          back  [d.i.menu back]
          sign.dish  (~(put by sign.dish) d.i.menu ds)
      ==
    ::
        %tal
      =/  ss  (peek s.i.menu)
      =/  ps  (~(pull so ss) 3)
      =/  ds=sock
        ?~  ps  [| ~]  u.ps
      %=  $
          menu  t.menu
          back  [d.i.menu back]
          sign.dish  (~(put by sign.dish) d.i.menu ds)
      ==
    ::
        %tis
      =/  ss  (peek s.i.menu)
      %=  $
          menu  t.menu
          back  [d.i.menu back]
          sign.dish  (~(put by sign.dish) d.i.menu ss)
      ==
    ==
  ::  start next push from this variable
  ++  fork
    |=  v=@uvar
    this(fq [v fq])
  ::  start next pull from this variable
  ++  rock
    |=  v=@uvar
    this(rq [v rq])
  ::  set a uvar to be equal to a noun
  ++  prem
    |=  [v=@uvar n=sock]
    this(init.dish (~(put by init.dish) v n), fq [v fq])
  ::  set a uvar to always be needed
  ++  plea
    |=  f=@uvar
    this(fine.dish (~(put in fine.dish) f), rq [f rq])
  ::  add a dataflow edge between uvars
  ++  edge
    |=  =tray
    ^-  _this
    ?-  -.tray
        %con
      %=  this
          fore.dish  (~(add ja (~(add ja fore.dish) h.tray tray)) t.tray tray)
          back.dish  (~(put ju (~(put ju back.dish) d.tray h.tray)) d.tray t.tray)
          fn  (~(put in fn) d.tray)
          rn  (~(gas in rn) ~[h t]:tray)
      ==
    ::
        %int
      %=  this
          fore.dish  (~(add ja (~(add ja fore.dish) l.tray tray)) r.tray tray)
          back.dish  (~(put ju (~(put ju back.dish) d.tray l.tray)) d.tray r.tray)
          fn  (~(put in fn) d.tray)
          rn  (~(gas in rn) ~[l r]:tray)
      ==
    ::
        %hed
      %=  this
          fore.dish  (~(add ja fore.dish) s.tray tray)
          back.dish  (~(put ju back.dish) d.tray s.tray)
          fn  (~(put in fn) d.tray)
          rn  (~(put in rn) s.tray)
      ==
    ::
        %tal
      %=  this
          fore.dish  (~(add ja fore.dish) s.tray tray)
          back.dish  (~(put ju back.dish) d.tray s.tray)
          fn  (~(put in fn) d.tray)
          rn  (~(put in rn) s.tray)
      ==
    ::
        %tis
      %=  this
          fore.dish  (~(add ja fore.dish) s.tray tray)
          back.dish  (~(put ju back.dish) d.tray s.tray)
          fn  (~(put in fn) d.tray)
          rn  (~(put in rn) s.tray)
      ==
    ==
  ::  get the current dish
  ++  ware  dish
  --
--
=>
:: 
:: utilites
|%
::  XX TODO MAKE WET
++  dif-ju
  |*  a=(jug)
  |*  b=_a
  ^+  a
  =/  c=_a  (~(dif by a) b)
  =/  i  (~(int by a) b)
  ?:  =(~ i)  c
  %-  ~(rep by i)
  |=  [[p=_?>(?=(^ i) p.n.i) q=_?>(?=(^ i) q.n.i)] =_c]
  =/  r=_q  (~(get ju b) p)
  =/  s  (~(dif in q) r)
  ?:  =(~ s)  c
  (~(put by c) p (~(dif in q) r))
--
::
:: local types
|%
::
::  analysis queue entries
+$  todo
  $:  soot=sock             :: subject knowledge
      form=*                :: formula
      calm=(unit [auto=? j=[path @]])  :: key for call face of cold state
  ==
::
::  noun need
+$  nick
  $%  [%none ~]
      [%both l=nick r=nick]
      [%this v=@uvar]
  ==
::
::  edge
+$  tray
  $%  [%con h=@uvar t=@uvar d=@uvar] 
      [%int l=@uvar r=@uvar d=@uvar]
      [%hed s=@uvar d=@uvar]
      [%tal s=@uvar d=@uvar]
      [%tis s=@uvar d=@uvar]
  ==
::
::  constraint table
+$  dish
  $:  init=(map @uvar sock)  :: immediates
      fine=(set @uvar)       :: needed as formula variables
      fore=(jar @uvar tray)  :: forward-propagation constraints
      back=(jug @uvar @uvar)  :: reverse-propagation constraints
      sign=(map @uvar sock)  :: discovered socks
      wine=(map @uvar cape)  :: discovered capes
  ==
::
::  call site table
+$  cafe
  %+  map  @hail
  $:  sire=(unit @hail) :: caller
      less=@uvar        :: subject variable
      form=@uvar        :: formula variable
      more=@uvar        :: output variable
      load=(unit nomm)  :: lowered formula
  ==
::
::  fast hint site table
+$  fate
  %+  map  @hint
  $:  sire=@hail          :: containing callsite
      hive=@uvar          :: clue variable
      bees=@uvar          :: core result variable
  ==
::
::  names in fast hints
+$  fame  $@(@tas [p=@tas q=@])
--
