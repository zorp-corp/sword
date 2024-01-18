:: destination-driven subject knowledge analysis
::
=|  moan=(jar * hone)
=|  cole=cool
=<
|%
::
:: this core
++  thus  .
::  subject knowledge analysis loop
++  rout
  |=  [soot=* form=*]
  =|  rail=@hail
  =|  done=(set @hail)
  =|  rain=@uvar
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
  =*  rout-loop
  ?~  work  thus
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
        =^  left  rail  $(nock l)
        =^  rite  rail  $(nock r)
        [[%par left rite] rail]
      ::
          [%0 a=@]
        [[%not a] rail]
      ::
          [%1 n=*]
        [[%one n] rail]
      ::
          [%2 s=* f=*]
        =^  sn  rail  $(nock s)
        =^  fn  rail  $(nock f)
        [[%two sn fn rail] .+(rail)]
      ::
          [%3 e=*]
        =^  en  rail  $(nock e)
        [[%the en] rail]
      ::
          [%4 e=*]
        =^  en  rail  $(nock e)
        [[%for en] rail]
      ::
          [%5 l=* r=*]
        =^  ln  rail  $(nock l)
        =^  rn  rail  $(nock r)
        [[%ivy ln rn] rail]
      ::
          [%6 c=* z=* o=*]
        =^  cn  rail  $(nock c)
        =^  zn  rail  $(nock z)
        =^  on  rail  $(nock o)
        [[%six cn zn on] rail]
      ::
          [%7 a=* b=*]
        =^  an  rail  $(nock a)
        =^  bn  rail  $(nock b)
        [[%eve an bn] rail]
      ::
          [%8 a=* b=*]
        $(nock [7 [a 0 1] b])
      ::
          [%9 a=@ c=*]
        $(nock [7 c 2 [0 1] 0 b])
      ::
          [%10 [a=@ p=*] t=*]
        =^  pn  $(nock p)
        =^  tn  $(nock t)
        [[%ten a pn tn] rail]
      ::
          [%11 t=@ b=*]
        =^  bn  rail  $(nock b)
        [[%sip t bn] rail]
      ::
          [%11 [t=@ h=*] b=*]
        =^  hn  rail  $(nock h)
        =^  bn  rail  $(nock b)
        [[%tip t hn bn rail] .+(rail)]
      ::
          [%12 r=* p=*]
        =^  rn  rail  $(nock r)
        =^  pn  rail  $(nock p)
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
    =/  [load=nomm less=@uvar more=@uvar]
      [load less more]:load:(~(got by call) i.toil)
    =/  goal=nick  [%this more]
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
          [[%none ~] (just n goal)]
        ::
            %two
          =^  ov  gen  (kerf goal)
          =/  sv  [rain.gen gen(rain .+(rain.gen)]
          =/  fv  [rain.gen gen(rain .+(rain.gen)]
          =.  novo.gen  [fv novo.gen]
          =/  cs  [rail.gen gen(rail .+(rail.gen)]
          =.  call.gen  (~(put by call.gen) cs [`i.toil sv fv ov ~]
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
          =^  zv  gen  [rain.gen gen(rain .+(rain.gen)]
          =^  ov  gen  [rain.gen gen(rain .+(rain.gen)]
          =.  gr.gen  (edge:gr.gen %int zv ov rv)
          =^  cg  gen  $(goal [%none ~], load what.load)
          =^  zg  gen  $(goal [%this zv], load then.load)
          =^  og  gen  4(goal [%this ov], load else.load)
          =^  ig  gen  (sect zg og)
          (copy cg ig)
        ::
            %eve
          =^  tg  gen  $(load then.load)
          $(load once.load, goal tg)
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
        ^-  [[goal goal] gen]
        ?-  -.goal
            %none  [[[%none ~] %none ~] gen]
            %both  [[l.goal r.goal] gen]
            %this
          =^  lv  rain.gen  [rain.gen gen(rain .+(rain.gen))]
          =^  rv  rain.gen  [rain.gen gen(rain .+(rain.gen))]
          =.  gr.gen  (edge:gr.gen %con lv rv v.goal)
          [[[%this lv] %this rv] gen]
      ::  combine two nicks, adding tis edges
      ++  copy
        |=  [a=nick b=nick]
        ^-  [nick _gen]
        ?:  ?=(%none -.a) [b gen]
        ?:  ?=(%none -.b) [a gen]
        ?-  -.a.goal
            %both
          ?-  -.b.goal
              %both
            =^  l  gen  (copy left.a left.b)
            =^  r  gen  (copy rite.a rite.b)
            [[%both l r] gen]
          ::
              %this
            =^  lb  gen  [rain.gen gen(rain .+(rain.gen))]
            =^  rb  gen  [rain.gen gen(rain .+(rain.gen))]
            =.  gr.gen  (edge:gr.gen %con lb rb v.b)
            =^  l  gen  (copy left.a [%this lb])
            =^  r  gen  (copy rite.a [%this rb])
            [[%both l r] gen]
          ==
        ::
            %this
          ?-  -.b.goal
              %both
            =^  la  gen  [rain.gen gen(rain .+(rain.gen))]
            =^  ra  gen  [rain.gen gen(rain .+(rain.gen))]
            =.  gr.gen  (edge:gr.gen %con la ra v.a)
            =^  l  gen  (copy [%this la] left.b)
            =^  r  gen  (copy [%this ra] rite.b)
            [[%both l r] gen]
          ::
              %this
            =?  gr.gen  !(=(v.a v.b))  (edge:gr.gen %tis v.a v.b)
            [a gen]
          ==
        ==
      ::  intersect two nicks, adding con and tis edges
      ++   sect
        |=  [z=nick o=nick]
        ^-  [nick _gen]
        ?:  ?=(%none -.z)
          =/  ov  (kerf o)
          [[%this ov] gen]
        ?:  ?=(%this -.z)
          =/  ov  (kerf o)
          =.  gr.gen  (edge:gr.gen %tis v.z ov)
          [z gen]
        ?:  ?=(%none -.o)
          =/  zv  (kerf z)
          [[%this zv] gen]
        ?:  ?=(%this -.o)
          =/  zv  (kerf z)
          =.  gr.gen  (edge:gr.gen %tis v.o zv)
          [o gen]
        ::  both are %both
        =^  lg  gen  $(z left.z, o left.o)
        =^  rg  gen  $(z rite.z, o rite.o)
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
          =.  $(goal left.goal, n -.n)
          $(goal rite.goal, n +.n)
        ==
      ++  kerf
        |=  goal=nick
        ^-  [@uvar _gen]
        ~|  %todo  !!
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
  ::  - finalize callsites with no newly-direct non-recursive callsites
  =/  toil  (skip (gulf 0 (dec rail)) ~(has in done))
  =.  work  ~
  =|  slag=(set @hail)  ::  excluded as finalization roots
  =.  
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
      :: recursion found
      %=  ruin-loop
        toil  t.toil
        slag  (~(gas in slag) sirs)
        loop  (~(put by loop) i.toil mill)
      ==
    $(sirs [mill sirs], sire papa)
  :: XX todo find callsites with unmet needs and put site+callers in slag (except for
  :: entr)
  :: discover finalization roots
  =|  flux=(list @hail)  ::  to be finalized
  =/  queu=(list @hail) ~[entr]  ::  deq for BFS of callsites
  =|  back=(list @hail)  :: backside of deq for BFS of callsites
  =|  ripe=[y=(set @hail) n=(set @hail)] :: cache of whether call tree is full (all needs met)
  |-  ^-  _thus
  =*  flux-loop  $
  ?^  queu
    ?:  ?&  !((~(has in done) i.queu))
            (~(has in slag) i.queu)
        ==
      ::  unanalyzed or prevented from being finalization root, go
      ::  around
      %=  $
        queu  t.queu
        back  (weld ~(tap in (~(get ju kids) i.queu)) back)
      ==
    ?.  =(i.queu entr)  :: skip unfulfilled check for entry callsite
      flux-loop(queu t.queu, flux [i.queu flux])
    =*  r  i.queu
    =^  full  ripe
      :: cached lookup of whether needs are satisfied
      |-  ^-  [?  _ripe]
      ?:  (~(has in y.ripe) r)  [& ripe]
      ?:  (~(has in n.ripe) r)  [| ripe]
      =/  rs  (peek:gr r)
      =/  rn  (dine:gr r)
      ?:  (~(big ca rn) cape.rs)  [| ripe(n (~(put in n.ripe) r))]
      =/  ks  ~(tap in (~(get ju kids) r))
      ?:  (levy (turn ks |=(k=@hail $(r k))))  [& ripe(y (~(put in y.ripe) r))]
      [| ripe(n (~(put in n.ripe) r))]
    ?.  full
      flux-loop(queu t.queu)
    flux-loop(queu t.queu, flux [i.queu flux])
  ?^  back  $(queu (flop back), back ~)
  |-  ^-  thus
  ?^  flux
    ~|  %todo-finalize  !!
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
    ?:  (~(has in fine.dish)
      &
    (~(gut by wine.dish) v |]
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
    ?-  i.menu
        %con
      =/  [hc=cape tc=cape]  ~(rip ca (dine d.i.menu))
      =.  wine.dish  (~(put by wine.dish) (~(uni ca (~(gut by wine.dish) h.i.menu |)) hc))
      =.  wine.dish  (~(put by wine.dish) (~(uni ca (~(gut by wine.dish) t.i.menu |)) tc))
      %=  $
        menu  t.menu
        back  [t.i.menu h.i.menu back]
      ==
    ::
        %int
      =/  c  (dine d.i.menu)
      =.  wine.dish  (~(put by wine.dish) (~(uni ca (~(gut by wine.dish) l.i.menu |)) c))
      =.  wine.dish  (~(put by wine.dish) (~(uni ca (~(gut by wine.dish) l.i.menu |)) c))
      %=  $
        menu  t.menu
        back  [t.i.menu h.i.menu back]
      ==
    ::
        %hed
      =/  c  (~(pat ca (dine d.i.menu)) 2)
      =.  wine.dish  (~(put by wine.dish) (~(uni ca (~(gut by wine.dish) s.i.menu |)) c))
      %=  $
        menu  t.menu
        back  [t.i.menu h.i.menu back]
      ==
    ::
        %tal
      =/  c  (~(pat ca (dine d.i.menu)) 3)
      =.  wine.dish  (~(put by wine.dish) (~(uni ca (~(gut by wine.dish) s.i.menu |)) c))
      %=  $
        menu  t.menu
        back  [t.i.menu h.i.menu back]
      ==
    ::
        %tis
      =/  c  (dine d.i.menu)
      =.  wine.dish  (~(put by wine.dish) (~(uni ca (~(gut by wine.dish) s.i.menu |)) c))
      %=  $
        menu  t.menu
        back  [t.i.menu h.i.menu back]
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
    ?-  i.menu
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
    this(init.dish (~(put by init.dish) v n)
  ::  set a uvar to always be needed
  ++  plea
    |=  f=@uvar
    this(fine.dish (~(put in fine.dish) f))
  ::  add a dataflow edge between uvars
  ++  edge
    |=  tray
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
          fore.dish  (~(add ja (~(add ja fore.dish) h.tray tray)) t.tray tray)
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
::
:: local types
|%
::
::  noun need
+$  nick
  |%  [%none ~]
      [%both l=nick r=nick]
      [%this v=@uvar]
  ==
::
::  constraint
+$  tray
  [%con h=@uvar t=@uvar d=@uvar] 
  [%int l=@uvar r=@uvar d=@uvar]
  [%hed s=@uvar d=@uvar]
  [%tal s=@uvar d=@uvar]
  [%tis s=@uvar d=@uvar]
::
::  constraint table
+$  dish
  $:  init=(map @uvar *)     :: immediates
      fine=(set @uvar)       :: needed as formula variables
      fore=(jar @uvar tray)  :: forward-propagation constraints
      back=(jar @uvar tray)  :: reverse-propagation constraints
      sign=(map @uvar sock)  :: discovered socks
      wine=(map @uvar cape)] :: discovered capes
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
