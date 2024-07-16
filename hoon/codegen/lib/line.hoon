/-  gen
|.
=>  $:gene
%zero
=*  skan  +3
=*  moan  moan.sack
=*  cole  cole.sack
=|  =fuji
=>
|%
::    get analysis result by bell
::
++  puck
  |=  b=bell
  =>  [b=b m=moan ja=ja hone=hone]
  ~+
  =/  h  (~(get ja m) form.b)
  |-  ^-  (unit hone)
  ?<  ?=(h ~)  
  ?:  =(text.b soot.i.h)  `h
  $(h t.h)
::    compute which analysis results are not linearized and return in
::    toposorted order
::
++  work
  =/  mell
    %-  ~(rep by moan)
    |=  [[f=* l=(list hone)] mell=(set bell)]
    (roll l |:([h=*hone mell=mell] (~(put in mell) [soot.h f])))
  =/  novo  (~(dif in mell) ~(key by peal.fuji))
  =/  sire=(jug bell bell)
    %-  ~(rep in novo)
    |=  [b=bell sire=(jug bell bell)]
    =/  hues  (puck b)
    ?<  ?=(~ hues)
    =/  team  (~(gas in *(set bell)) ~(val by ices.norm.u.hues))
    =.  team  (~(dif in team) loop.norm.u.hues)
    =.  team  (~(int in team) novo)
    %-  ~(rep in team)
    |:  [k=*bell s=sire]
    (~(put ju s) k b)
  =<  tack.s
  %+  roll  novo
  |=  [v=bell s=[done=(set bell) tack=(list bell)]]
  =*  dfs  $
  ^-  _s
  ?:  (~(has in done.s v)  s
  =.  done.s  (~(put in done.s)  v
  =/  e=(set bell)  (~(get ju sire) v)
  =.  s
    %-  ~(rep in e)
    |:  [n=*bell s=s]
    ^-  _s
    dfs(v n, s s)
  s(tack [v tack.s])  
+$  gen  [redo=(list [t=bell b=@uwoo]) =fuji sans=@uvre chan=@uwoo]
++  jean
  |_  =gen
  ::    core DDCG linearizer
  ::
  ++  cuts
    =/  =goal  [%done ~]
    |=  =bell
    =/  h  (puck bell)
    =*  n  nomm.norm.h
    |-  ^-  [_goal _gen]
    ?-  -.n
        %par
      ?-  -.goal
          %done
        =^  last  gen  rain
        =^  loch  gen  (emit ~ ~ %don last)
        $(goal [%next [%this last] loch])
      ::
          %pick
        (mine sass.goal zero.goal)
      ::
          %next
        =^  [bill=@uwoo left=need rite=need]  gen  (lyse goal)
        =^  tale  gen
          $(nomm rite.nomm, goal [%next rite bill])
        =^  hale  gen
          $(nomm left.nomm, goal [%next then.tale])
        (copy hale what.tale)
      ==
    ::
        %not
      ?:  =(0 here.nomm)  bomb
      ?-  -.goal
          %done
        =^  last  gen  rain
        =^  dear  gen  (emit ~ ~ %don last)
        $(goal [%next [%this last] dear])
      ::
          %pick
        =^  cove  gen  rain
        =^  cozy  gen  (emit ~ ~ %brn cove [zero once]:goal)
        $(goal [%next [%this cove] cozy])
      ::
          %next
        (from here.nomm goal)
      ==
    ::
        %one
      ?-  -.goal
          %done
        =^  last  gen  rain
        =^  rime  gen  (emit ~ [%imm moan.nomm last]~ %don last)
      ::
          %pick
        ?:  =(0 moan.nomm)
          [[%next [%none ~] zero.goal] gen]
        ?:  =(1 moan.nomm)
          [[%next [%none ~] once.goal] gen]
        (mine sass.goal zero.goal)
      ::
          %next
        =^  bill  gen  (mede then.goal moan.nomm what.goal)
        [[%next [%none ~] bill] gen]
      ==
    ::
        %two  ~|  %todo-two  !!
      ?:  ?=(pick -.goal)
        =^  flip  gen  rain
        =^  bird  gen  (emit ~ ~ %brn flip [zero once]:goal)
        $(goal [%next [%this flip] bird])
      =/  bull  (~(get by ices.norm.h) rail.n)
      ?~  bull
        :: indirect call
        ?-  -.goal
            %done
          =^  s  gen  rain
          =^  f  gen  rain
          =^  tide  gen  (emit ~ ~ %lnt s f)
          =^  corn  gen  $(nomm corn.nomm, goal [%next [%this f] tide])
          =^  cost  gen  $(nomm cost.nomm, goal [%next [%this s] then.corn])
          (copy cost what.corn)
        ::
            %next
          =^  [post=@uwoo salt=@uvre]  gen  (kerf %post goal)
          =^  s  gen  rain
          =^  f  gen  rain
          =^  dine  gen  (emit ~ ~ %lnk s f salt post)
          =^  corn  gen  $(nomm corn.nomm, goal [%next [%this f] dine])
          =^  cost  gen  $(nomm cost.nomm, goal [%next [%this s] then.corn])
          (copy cost what.corn)
        ==
      :: direct call
      ::  XX we need a placeholder for the @uwoo for a recursive call
      =/  hope  (~(get by call.cole) u.bull)
      =^  a  gen  (args u.bull)
      =/  wool
        ?.  r.a  (~(got by peal.fuji.gen) u.bull)
        0
      ?-  -.goal
          %done
        =^  [dire=@uwoo seed=need]  gen
          ?~  hope
            =^  dike  gen  (emit ~ ~ %jmp wool v.a)
            =?  redo.gen  r.a  [[u.bull dike] redo.gen]
            [[dike n.a] gen]
          =^  s  gen  rain
          =^  dial  gen  (emit ~ ~ %jmf wool v.a s u.hope)
          =?  redo.gen  r.a  [[u.bull dial] redo.gen]
          =^  next  gen  (copy [%next n.a dial] [%this s])
          [[then.nest what.nest] gen]
      ::
          %next
        =^  [post=@uwoo salt=@uvre]  gen  (kerf %post goal)
        =^  [dire=@uwoo seed=need]  gen
          ?~  hope
            =^  dine  gen  (emit ~ ~ %cal wool v.a salt post)
            =?  redo.gen  r.a  [[u.bull dine] redo.gen]
            [[dine n.a] gen]
          =^  s  gen  rain
          =^  dime  gen  (emit ~ ~ %caf wool v.a salt post s u.hope)
          =?  redo.gen  r.a  [[u.bull dime] redo.gen]
          =^  nest  gen  (copy [%next n.a dime] [%this s])
          [[then.nest what.nest] gen]
        =^  corn  gen  $(nomm corn.nomm, goal [%next [%none ~] dire])
        =^  cost  gen  $(nomm cost.nomm, goal [%next seed then.corn])
        (copy cost what.corn)
      ==
    ::
        %the
      ?-  -.goal
          %done
        =^  last  gen  rain
        =^  hasp  gen  rain
        =^  barf  gen  rain
        =^  tear  gen  (emit ~ [%imm 0 last]~ %don last)
        =^  fear  gen  (emit ~ [%imm 1 hasp]~ %don hasp)
        $(goal [%pick barf tear fear])
      ::
          %next
        ?:  ?=(%both -.what.goal)  (mine sass.what.goal then.goal)
        ::  no need for check if result unused
        ?:  ?=(%none -.what.goal)
          $(nomm pell.nom)
        =^  tare  gen  rain
        =^  tile  gen  vial
        =^  fare  gen  rain
        =^  file  gen  vial
        =^  thin  gen
          %:  emit
            %:  ~(put by *(map @uvre (map @uwoo @uvre)))
                sass.what.goal
                (~(gas by *(map @uwoo @uvre)) ~[[tile tare] [file fare]])
            ==
            ~
            %hop  then.goal
          ==
        =^  tear  gen  (come tile thin)
        =^  fear  gen  (come file thin)
        =^  celt  gen  (emit ~ [%imm 0 tare]~ %hop tear)
        =^  felt  gen  (emit ~ [%imm 1 fare]~ %hop fear)
        =^  barf  gen  rain
        $(goal [%pick barf celt felt])
      ::
          %pick
        =^  coat  gen  rain
        =^  pith  gen  (emit ~ ~ %clq coat [zero once]:goal)
        $(nomm pell.nomm, goall [%next [%this coat] pith])
      ==
    ::
        %for
      ?-  -.goal
          %done
        =^  rink  gen  rain
        =^  pink  gen  rain
        =^  tike  gen  (emit ~ [%inc pink rink]~ %don rink)
        $(nomm mall.nomm, goal [%next [%this pink] tike])
      ::
          %pick
        =^  rink  gen  rain
        =^  pink  gen  rain
        =^  pike  gen
          (emit %pike ~ [%inc pink rink]~ %brn rink [zero once]:goal)
        $(nomm mall.nomm, goal [%next [%this pink] pike])
      ::
          %next
        ?:  ?=(%both -.what.goal)  (mine sass.what.goal then.goal)
        =^  rink  gen
          ?:  ?=(%none -.what.goal)
            rain
          [sass.what.goal gen]
        =^  pink  gen  rain
        =^  bike  gen  (emit ~ [%inc pink rin]~ %hop then.goal)
        $(nomm mall.nomm, goal [%next [%this pink] bike])
      ==
    ::
        %ivy
      ?-  -.goal
          %done
        =^  last  gen  rain
        =^  hasp  gen  rain
        =^  reek  gen  (emit ~ [%imm 0 last]~ %don last)
        =^  riff  gen  (emit ~ [%imm 1 hasp]~ %don hasp)
        =^  crap  gen  rain
        $(goal [%pick crap reek riff])
      ::
          %next
        ?:  ?=(%both -.what.goal)  (mine sass.what.goal then.goal)
        ?:  ?=(%none -.what.goal)
          =^  than  gen  $(nomm that.nomm)
          =^  thin  gen  $(nomm this.nomm, then.goal then.than)
          (copy thin what.than)
        =^  tare  gen  rain
        =^  till  gen  vial
        =^  fare  gen  rain
        =^  fill  gen  vial
        =^  ward  gen
          %:  emit
            %:  ~(put by *(map @uvre (map @uwoo @uvre)))
                sass.what.goal
                (~(gas by *(map @uwoo @uvre)) ~[[till tare] [fill fare]])
            ==
            ~
            %hop
            then.goal
          ==
        =^  weir  gen  (come till ward)
        =^  mere  gen  (come fill ward)
        =^  ware  gen  (emit ~ [%imm 0 tare]~ %hop weir)
        =^  mare  gen  (emit ~ [%imm 1 tare]~ %hop mere)
        =^  crap  gen  rain
        $(goal [%pick crap ware mare])
      ::
          %pick
        =^  tire  gen  rain
        =^  tear  gen  rain
        =^  pare  gen  (emit ~ ~ %eqq tire tear [zero once]:goal)
        =^  than  gen  $(nomm that.nomm, goal [%next [%this tear] pare])
        =^  thin  gen  $(nomm this.nomm, goal [%next [%this tire] then.than])
        (copy thin what.than)
      ==
    ::
        %six
      ?:  ?=(%next -.goal)
        =^  [teal=next feel=next]  gen  (phil goal)
        =^  fest  gen  $(nomm else.nomm, goal feel)
        =^  zest  gen  $(nomm then.nomm, goal teal)
        =^  [bead=need tile=@uwoo file=@uwoo]  gen  (sect zest fest)
        =^  lead  gen  rain
        =^  cond  gen  $(nomm what.nomm, goal [%pick lead tile file])
        (copy cond bead)
      =^  fest  gen  $(nomm else.nomm)
      =^  zest  gen  $(nomm then.nomm)
      =^  [bead=need tile=@uwoo file=@uwoo]  gen  (sect zest fest)
      =^  barf  gen  rain
      =^  tool  gen  (emit ~ [%ipb ~[barf]]~ %hop tile)
      =^  cond  gen
        $(nomm what.nomm, goal [%pick barf tool file])
      (copy cond bead)
    ::
        %eve
      =^  thin  gen  $(nomm then.nomm)
      $(nomm once.nomm, goal thin)
    ::
        %ten
      ?-  -.goal
          %done
        =^  last  gen  rain
        =^  dead  gen  (emit ~ ~ %don last)
        $(goal [%next [%this last] dead])
      ::
          %pick
        ?.  =(here.nomm 1)  (mine sass.goal zero.goal)
        =^  flip  gen  rain
        =^  deep  gen  (emit ~ ~ %brn flip [zero once]:goal)
        $(goal [%next [%this flip] deep])
      ::
          %next
        =^  [twig=need tree=need then=@uwoo]  gen  (into here.nomm goal)
        =^  nest  gen  $(nomm tree.nomm, goal [%next tree then])
        =^  eggs  gen  $(nomm twig.nomm, goal [%next twig then.nest])
        (copy eggs what.nest)
      ==
    ::
        %sip
      ?+  hint.nomm  $(nomm then.nomm)
          %bout
        ?-  -.goal
            %done
          =^  last  gen  rain
          =^  dime  gen  (emit ~ ~ %don last)
          $(goal [%next [%this last] dime)
        ::
            %pick
          =^  tome  gen  (emit ~ [%tom ~]~ %hop zero.goal)
          =^  foam  gen  (emit ~ [%tom ~]~ %hop once.goal)
          =^  race  gen  $(nomm then.nomm, goal [%pick sass.goal tome foam])
          =^  tick  gen  (emit ~ [%tim ~]~ %hop then.race)
          [race(then tick) gen]
        ::
            %next
          =^  stop  gen  (emit ~ [%tom ~]~ %hop then.goal)
          =^  race  gen  $(nomm then.nomm, then.goal stop)
          =^  goes  gen  (emit %goes ~ [%tim ~]~ %hop then.race)
          [race(then goes) gen]
        ==
      ::
          %meme
        =^  raft  gen  $(nomm then.nomm)
        =^  meme  gen  (emit ~ [%mem ~]~ %hop then.raft)
        [raft(then meme) gen]
      ==
    ::
        %tip
      ?+    hint.nomm
        =^  thin  gen  $(nomm then.nomm)
        =^  fake  gen  $(nomm vice.nomm, goal [%next [%none ~] then.thin])
        (copy fake what.thin)
      ::
          ?(%hunk %hand %lose %mean %spot)
        =^  mane  gen  rain
        ?-  -.goal
            %done
          =^  real  gen  $(nomm then.nomm)
          =^  dint  gen  (emit ~ [%men hint.nomm mane]~ %hop then.real)
          =^  fake  gen  $(nomm vice.nomm, goal [%next [%this maine] dint])
          (copy fake what.real)
        ::
            %pick
          =^  tame  gen  (emit ~ [%man ~]~ %hop zero.goal)
          =^  fame  gen  (emit ~ [%man ~]~ %hop once.goal)
          =^  real  gen  $(nomm then.nomm, goal [%pick sass.goal tame fame])
          =^  dint  gen  (emit ~ [%men hint.nomm mane]~ %hop then.real)
          =^  fake  gen  $(nomm vice.nomm, goal [%next [%this mane] dint])
          (copy fake what.real)
        ::
            %next
          =^  rugs  gen  (emit ~ [%man ~]~ %hop then.goal)
          =^  real  gen  $(nomm then.nomm, then.goal rugs)
          =^  dint  gen  (emit ~ [%men hint.nomm mane]~ %hop then.real)
          =^  fake  gen  $(nomm vice.nomm, goal [%next [%this mane] dint])
          (copy fake what.real)
        ==
      :: XX TODO %live %slog %memo %bout
      ==
    ==
  ::  XX 
  ::  rain - new register
  ::  emit - add basic block
  ::  lyse - split need for cons
  ::  mine - set up deferred crash
  ::  from - push need down by axis
  ::  mede - immediates into need
  ::  kerf - split single register into goal
  ::  args - look up subject registerization
  ::  vial - new basic block label
  ::  copy - align subject needs for sequential computation
  ::  sect - align subject needs for branching computation
  ::  phil - generate phi nodes
  ::  into - split need for edit
  --
--
=+  ver=%1
|%
++  this  .
++  peek  ~|  %todo  !!
++  poke  ~|  %todo  !!
++  xray  ~|  %todo  !!
--
