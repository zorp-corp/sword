:: destination-driven subject knowledge analysis
::
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
::  - finer-grained finalization
::  - check if compatible with finalization before putting in worklist
::  - cold state
++  rout
  |=  [soot=* form=*]
  =|  rail=@hail
  =|  rain=@uvar
  =|  maid=(set @hail)
  =|  kids=(jug @hail @hail)
  =|  loop=(map @hail @hail)
  =^  entr  rail  [rail .+(rail)]
  =^  sv  rain  [rain .+(rain)]
  =^  fv  rain  [rain .+(rain)]
  =^  ov  rain  [rain .+(rain)]
  =/  gr  c
  =.  gr  (plea:gr fv)
  =.  gr  (prem:gr sv soot)
  =.  gr  (prem:gr fv form)
  =/  call  (~(put by *cafe) entr [~ sv fv ov ~])
  =/  work=(list @hail)  ~[entr]
  :: outer work loop
  |-  ^-  _thus 
  =*  rout-loop  $
  ?:  =(~ work)
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
    thus
  ::  lower entries in work to nomm
  =/  toil  work
  |-  ^-  _thus  
  ?^  toil
    =/  brew  (~(got by call) i.toil)
    =/  fork  (peek:gr form.brew)
    ?>  =(& cape.fork)  :: we must know the formula or it shouldn't be in toil
    =*  nock  data.fork
    =^  lire  rail
      |-  ^-  [nomm _rail]
      ?+  nock  [[%not 0] rail]
          [l=^ r=*]
        =^  left  rail  $(nock l.nock)
        =^  rite  rail  $(nock r.nock)
        [[%par left rite] rail]
      ::
          [%0 a=@]
        [[%not a.nock] rail]
      ::
          [%1 n=*]
        [[%one n.nock] rail]
      ::
          [%2 s=* f=*]
        =^  sn  rail  $(nock s.nock)
        =^  fn  rail  $(nock f.nock)
        [[%two sn fn rail] .+(rail)]
      ::
          [%3 e=*]
        =^  en  rail  $(nock e.nock)

        [[%the en] rail]
      ::
          [%4 e=*]
        =^  en  rail  $(nock e.nock)
        [[%for en] rail]
      ::
          [%5 l=* r=*]
        =^  ln  rail  $(nock l.nock)
        =^  rn  rail  $(nock r.nock)
        [[%ivy ln rn] rail]
      ::
          [%6 c=* z=* o=*]
        =^  cn  rail  $(nock c.nock)
        =^  zn  rail  $(nock z.nock)
        =^  on  rail  $(nock o.nock)
        [[%six cn zn on] rail]
      ::
          [%7 a=* b=*]
        =^  an  rail  $(nock a.nock)
        =^  bn  rail  $(nock b.nock)
        [[%eve an bn] rail]
      ::
          [%8 a=* b=*]
        $(nock [7 [a.nock 0 1] b.nock])
      ::
          [%9 b=@ c=*]
        $(nock [7 c.nock 2 [0 1] 0 b.nock])
      ::
          [%10 [a=@ p=*] t=*]
        =^  pn  rail  $(nock p.nock)
        =^  tn  rail  $(nock t.nock)
        [[%ten a.nock pn tn] rail]
      ::
          [%11 t=@ b=*]
        =^  bn  rail  $(nock b.nock)
        [[%sip t.nock bn] rail]
      ::
          [%11 [t=@ h=*] b=*]
        =^  hn  rail  $(nock h.nock)
        =^  bn  rail  $(nock b.nock)
        [[%tip t.nock hn bn rail] .+(rail)]
      ::
          [%12 r=* p=*]
        =^  rn  rail  $(nock r.nock)
        =^  pn  rail  $(nock p.nock)
        [[%elf rn pn] rail]
      ==
    %=  $
      call  (~(put by call) i.toil brew(load `lire))
    ==
  ::  set up edges for nomm
  =/  toil  work
  |-  ^-  _thus
  =/  gen  [gr=gr rain=rain call=call novo=*(list @uvar)]
  ?^  toil
    =/  [loud=(unit nomm) less=@uvar more=@uvar]
      [load less more]:(~(got by call) i.toil)
    ?>  ?=(^ loud)
    =/  load  u.loud
    =/  goal=nick  [%this more]
    =>  |%
        ++  kerf
          |=  goal=nick
          ^-  [@uvar _gen]
          ?-  -.goal
              %none
            =^  nv  gen  [rain gen(rain .+(rain.gen))]
            [nv gen]
          ::
              %this
            [v.goal gen]
          ::
              %both
            =^  hv  gen  $(goal l.goal)
            =^  tv  gen  $(goal r.goal)
            =^  cv  gen  [rain gen(rain .+(rain.gen))]
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
          =^  ov  gen  (kerf goal)
          =^  sv  gen  [rain.gen gen(rain .+(rain.gen))]
          =^  fv  gen  [rain.gen gen(rain .+(rain.gen))]
          =.  novo.gen  [fv novo.gen]
          =.  call.gen  (~(put by call.gen) rail.load [`i.toil sv fv ov ~])
          =^  sg  gen  $(goal [%this sv], load cost.load)
          =^  fg  gen  $(goal [%this fv], load corn.load)
          (copy sg fg)
        ::
            %the
          $(goal [%none ~], load pell.load)
        ::
            %for
          $(goal [%none ~], load mall.load)
        ::
            %ivy
          =^  lg  gen  $(goal [%none ~], load this.load)
          =^  rg  gen  $(goal [%none ~], load that.load)
          (copy lg rg)
        ::
            %six
          =^  rv  gen  (kerf goal)
          =^  zv  gen  [rain.gen gen(rain .+(rain.gen))]
          =^  ov  gen  [rain.gen gen(rain .+(rain.gen))]
          =.  gr.gen  (edge:gr.gen %int zv ov rv)
          =^  cg  gen  $(goal [%none ~], load what.load)
          =^  zg  gen  $(goal [%this zv], load then.load)
          =^  og  gen  $(goal [%this ov], load else.load)
          =^  ig  gen  (sect zg og)
          (copy cg ig)
        ::
            %eve
          =^  tg  gen  $(load then.load)
          $(load once.load, goal tg)
        ::
            %ten
          ::  XX graph construction for ten
          =^  [tg=nick pg=nick]  gen  (diet here.load goal)
          =^  tig  gen  $(load tree.load, goal tg)
          =^  pig  gen  $(load tree.load, goal pg)
          (copy tig pig)
        ::
            %sip
          $(load then.load)
        ::
            %tip
          :: later we can do custom things based on which hint here
          =^  vg  gen  $(goal [%none ~], load vice.load)
          =^  tg  gen  $(load then.load)
          :: don't propagate information to hint body for %slow
          ?:  =(%slow vice.load)
            [vg gen]
          (copy vg tg)
        ::
            %elf
          =^  rg  gen  $(goal [%none ~], load rent.load)
          =^  pg  gen  $(goal [%none ~], load walk.load)
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
            %this  gen(gr (prem:gr.gen v.goal n), novo [v.goal novo.gen])
            %both
          ?@  n  gen
          =.  gen  $(goal l.goal, n -.n)
          $(goal r.goal, n +.n)
        ==
      --
    =^  iv  gen  (kerf coal)
    =.  gr.gen  (edge:gr.gen %tis less iv)
    =.  novo.gen  [less novo.gen]
    $(toil t.toil)
  =.  gr  gr.gen
  =.  rain  rain.gen
  =.  call  call.gen
  ::  propagate knowledge and needs
  =.  gr  (push:gr novo.gen)
  =.  gr  (pull:gr novo.gen)
  ::  a few things together:
  ::  - rebuild worklist with newly discovered direct calls
  ::  - detect recursive calls and exclude from worklist
  =.  maid  (roll work |:([h=*@hail m=maid] (~(del in m) h)))
  =/  toil  ~(tap in maid)
  =.  work  ~
  =|  slag=(set @hail)  ::  excluded as finalization roots
  |-  ^-  _thus
  =*  ruin-loop  $
  ?^  toil
    =+  (~(got by call) i.toil)
    =/  f  (peek:gr form)
    ?.  =(& cape.f)  $(toil t.toil) :: formula not known, not a direct call
    =/  mill  i.toil
    =|  sirs=(list @hail)  
    :: build list of caller-ancestors
    |-  ^-  _thus
    ?~  sire
      %=  ruin-loop
        toil  t.toil
        work  [i.toil work]
        slag  (~(gas in slag) [mill sirs])
      ==
    =.  kids  (~(put ju kids) u.sire mill)
    =.  mill  u.sire
    =/  [papa=(unit @hail) sv=@uvar fv=@uvar]  [sire less form]:(~(got by call) mill)
    =/  gf  (peek:gr fv)
    =/  s   (peek:gr less)
    =/  gs  (peek:gr sv)
    =/  gn  (dine:gr sv)
    ?>  =(& cape.gf)
    ::  check for recursion
    ?:  ?&  =(f gf)
            (~(huge so (~(app ca gn) gs)) s)
        ==
      :: recursion found[0..3]
      ::   T <--+
      ::   C1   |
      ::   C2   |
      ::   C3 ---
      %=  ruin-loop
        toil  t.toil
        slag  (~(gas in slag) sirs)
        loop  (~(put by loop) i.toil mill)
      ==
    $(sirs [mill sirs], sire papa)
  rout-loop
::
::  subject knowledge / need propagation and state
++  c
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
    [& u.nuon]
  ::  get current cape at uvar
  ++  dine
    |=  v=@uvar
    ^-  cape
    ?:  (~(has in fine.dish) v)
      &
    (~(gut by wine.dish) v |)
  ::  pull need capes backward from uvars in queu
  ++  pull
    |=  queu=(list @uvar)
    =|  back=(list @uvar)
    |-  ^-  _this
    ?~  queu
      ?~  back  this
      $(queu (flop back), back ~)
    =/  menu  (~(get ja back.dish) i.queu)
    |-  ^-  _this
    ?~  menu  ^$(queu t.queu)
    ?-  -.i.menu
        %con
      =/  [hc=cape tc=cape]  ~(rip ca (dine d.i.menu))
      =.  wine.dish  (~(put by wine.dish) h.i.menu (~(uni ca (~(gut by wine.dish) h.i.menu |)) hc))
      =.  wine.dish  (~(put by wine.dish) t.i.menu (~(uni ca (~(gut by wine.dish) t.i.menu |)) tc))
      %=  $
        menu  t.menu
        back  [t.i.menu h.i.menu back]
      ==
    ::
        %int
      =/  c  (dine d.i.menu)
      =.  wine.dish  (~(put by wine.dish) l.i.menu (~(uni ca (~(gut by wine.dish) l.i.menu |)) c))
      =.  wine.dish  (~(put by wine.dish) r.i.menu (~(uni ca (~(gut by wine.dish) r.i.menu |)) c))
      %=  $
        menu  t.menu
        back  [l.i.menu r.i.menu back]
      ==
    ::
        %hed
      =/  c  (~(pat ca (dine d.i.menu)) 2)
      =.  wine.dish  (~(put by wine.dish) s.i.menu (~(uni ca (~(gut by wine.dish) s.i.menu |)) c))
      %=  $
        menu  t.menu
        back  [s.i.menu back]
      ==
    ::
        %tal
      =/  c  (~(pat ca (dine d.i.menu)) 3)
      =.  wine.dish  (~(put by wine.dish) s.i.menu (~(uni ca (~(gut by wine.dish) s.i.menu |)) c))
      %=  $
        menu  t.menu
        back  [s.i.menu back]
      ==
    ::
        %tis
      =/  c  (dine d.i.menu)
      =.  wine.dish  (~(put by wine.dish) s.i.menu (~(uni ca (~(gut by wine.dish) s.i.menu |)) c))
      %=  $
        menu  t.menu
        back  [s.i.menu back]
      ==
    ==
  ::
  ::  propagate knowledge forward starting at variables in queu
  ++  push
    |=  queu=(list @uvar)
    =|  back=(list @uvar)
    |-  ^-  _this
    ?~  queu
      ?~  back  this
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
      =/  ds
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
      =/  ds
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
  ::  set a uvar to be equal to a noun
  ++  prem
    |=  [v=@uvar n=*]
    this(init.dish (~(put by init.dish) v n))
  ::  set a uvar to always be needed
  ++  plea
    |=  f=@uvar
    this(fine.dish (~(put in fine.dish) f))
  ::  add a dataflow edge between uvars
  ++  edge
    |=  =tray
    ^-  _this
    ?-  -.tray
        %con
      %=  this
          fore.dish  (~(add ja (~(add ja fore.dish) h.tray tray)) t.tray tray)
          back.dish  (~(add ja back.dish) d.tray tray)
      ==
    ::
        %int
      %=  this
          fore.dish  (~(add ja (~(add ja fore.dish) l.tray tray)) r.tray tray)
          back.dish  (~(add ja back.dish) d.tray tray)
      ==
    ::
        %hed
      %=  this
          fore.dish  (~(add ja fore.dish) s.tray tray)
          back.dish  (~(add ja back.dish) d.tray tray)
      ==
    ::
        %tal
      %=  this
          fore.dish  (~(add ja fore.dish) s.tray tray)
          back.dish  (~(add ja back.dish) d.tray tray)
      ==
    ::
        %tis
      %=  this
          fore.dish  (~(add ja fore.dish) s.tray tray)
          back.dish  (~(add ja back.dish) d.tray tray)
      ==
    ==
  ::  get the current dish
  ++  ware  dish
  --
--
::
:: local types
|%
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
  $:  init=(map @uvar *)     :: immediates
      fine=(set @uvar)       :: needed as formula variables
      fore=(jar @uvar tray)  :: forward-propagation constraints
      back=(jar @uvar tray)  :: reverse-propagation constraints
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
--
