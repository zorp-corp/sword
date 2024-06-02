/-  gene
|.
=>  $:gene
:: XX
::  - generate ipb tests at 0/10 sites (done)
::  - make copy pessimize %this/%both case (not %both/%this) (done)
::  - do not ipb at mean entry/exit (done)
::  - no crash-immediate instructions, they could incorrectly skip over
::    other crashes (done)
::  - we don't actually need to track sick registers (done)
!:
=*  sack  +3
=*  moan  moan.sack
=*  cole  cole.sack
=|  =hill
=|  rots=shed
=>
|%
::   work
::
::  new: set of bells in moan, not in hill (require codegen)
::  old: set of bells in hill, not in moan (should be dropped from hill)
++  peck
  =|  miel=(list bell)
  =/  foam  ~(tap by moan)
  |-  ^-  [new=(set bell) old=(set bell)]
  ?^  foam
    ?^  q.i.foam
      $(q.i.foam t.q.i.foam, miel [[soot.i.q.i.foam p.i.foam] miel])
    $(foam t.foam)
  =/  jell  ~(key by hill)
  =/  mell  (~(gas in *(set bell)) miel)
  [(~(dif in mell) jell) (~(dif in jell) mell)] :: want mif-in
::
::  new bells
++  noob
  ^-  (set bell)
  =/  new  new:peck
  new
::
::  bells to drop
++  dead
  ^-  (set bell)
  old:peck
::   
::   look up analysis
::
:: look up an arm in the moan face of the sack core
++  puck
  |=  =bell
  ^-  (unit hone)
  =/  hose  (~(get ja moan) form.bell)
  |-  ^-  (unit hone)
  ?^  hose
    ?:  =(text.bell soot.i.hose)  `i.hose
    $(hose t.hose)
  ~
::    worklist
::
::  sort all of the un-linearized arms in `moan` topologically by the
::  reverse call DAG (so terminal arms come first). This ensures that we
::  only have to "redo" recursive callsites.
++  work
  ^-  (list bell)
  =/  news  noob
  ?:  =(~ news)  ~
  =/  sire=(jug bell bell)
    %-  ~(rep in news)
    |=  [b=bell sire=(jug bell bell)]
    =/  hues  (puck b)
    ?<  ?=(~ hues)
    =/  team  (~(gas in *(set bell)) ~(val by ices.norm.u.hues))
    =.  team  (~(dif in team) loop.norm.u.hues)
    =.  team  (~(int in team) news)
    %-  ~(rep in team)
    |:  [k=*bell s=sire]
    (~(put ju s) k b)
  =<  tack.s
  %-  ~(rep in news)
  |=  [v=bell s=[done=(set bell) tack=(list bell)]]
  =*  dfs  $
  ^-  _s
  ?:  (~(has in done.s) v)  s
  =.  done.s  (~(put in done.s) v)
  =/  e=(set bell)  (~(get ju sire) v)
  =.  e  (~(dif in e) done.s)
  =.  s
    %-  ~(rep in e)
    |:  [n=*bell s=s]
    ^-  _s
    dfs(v n, s s)
  s(tack [v tack.s])
::
::    internal state
::
::  redo: arms called without knowing registerization
::  will: code table
::  sans: next SSA register
::  sick: registers which should be checked for crashing at next mean
::    boundary
+$  gen  [redo=(list bile) will=(map bile blob) sans=@uvre]
::    codegen
::
::  door containing core codegen operations
++  jean
  =/  fax  1
  =/  =goal  [%done ~]
  |_  [=bell =gen like=(map bell need)]
  ::   codegen loop
  :: 
  :: traverse nomm RLN and emit linearized code
  ++  cuts
    =+  =/  huns  (puck bell)
        ?>  ?=(^ huns)
        norm.u.huns
    |-  ^-  [next _gen]
    ?-  -.nomm
        %par
      ?-  -.goal
          %done
        =^  last  gen  rain
        =^  loch  gen  (emit %loch ~ ~ %don last)
        $(goal [%next [%this last] loch])
      ::
          %pick
        (mine sass.goal zero.goal)
      ::
          %next
        =^  [bill=bile left=need rite=need]  gen  (lyse goal)
        =^  tale  gen
          $(nomm rite.nomm, goal [%next rite bill], fax (peg fax 3))
        =^  hale  gen
          $(nomm left.nomm, goal [%next left then.tale], fax (peg fax 2))
        (copy hale what.tale)
      ==
    ::
        %not
      ?:  =(0 here.nomm)  bomb
      ?-  -.goal
          %done
        =^  last  gen  rain
        =^  dear  gen  (emit %dear ~ ~ %don last)
        $(goal [%next [%this last] dear])
      ::
          %pick
        =^  cove  gen  rain
        =^  cozy  gen  (emit %cozy ~ ~ %brn cove [zero once]:goal)
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
        =^  rime  gen  (emit %rime ~ [%imm moan.nomm last]~ %don last)
        [[%next [%none ~] rime] gen]
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
        %two
      ?:  ?=(%pick -.goal)
        =^  flip  gen  rain
        =^  bird  gen  (emit %bird ~ ~ %brn flip [zero once]:goal)
        $(goal [%next [%this flip] bird])
      =/  bull  (~(get by ices) rail.nomm)
      ?~  bull
        ?-  -.goal
            %done
          =^  s  gen  rain
          =^  f  gen  rain
          =^  tide  gen  (emit %tide ~ ~ %lnt s f)
          =^  corn  gen  $(nomm corn.nomm, fax (peg fax 7), goal [%next [%this f] tide])
          =^  cost  gen  $(nomm cost.nomm, fax (peg fax 6), goal [%next [%this s] then.corn])
          (copy cost what.corn)
        ::
            %next
          =^  [post=bile salt=@uvre]  gen  (kerf %post goal)
          =^  s  gen  rain
          =^  f  gen  rain
          =^  dine  gen  (emit %dine ~ ~ %lnk s f salt post)
          =^  corn  gen  $(nomm corn.nomm, fax (peg fax 7), goal [%next [%this f] dine])
          =^  cost  gen  $(nomm cost.nomm, fax (peg fax 6), goal [%next [%this s] then.corn])
          (copy cost what.corn)
        ==
      =/  hope  (~(get by call.cole) u.bull)
      =^  a  gen  (args u.bull)
      ?-  -.goal
          %done
        =^  [dire=bile seed=need]  gen
          ?~  hope
            =^  dike  gen  (emit %dike ~ ~ %jmp u.bull v.a)
            =?  redo.gen  r.a  [dike redo.gen]  
            [[dike n.a] gen]
          =^  s  gen  rain
          ~!  u.hope
          =^  dial  gen  (emit %dial ~ ~ %jmf u.bull v.a s u.hope)
          =?  redo.gen  r.a  [dial redo.gen]
          =^  nest  gen  (copy [%next n.a dial] [%this s])
          [[then.nest what.nest] gen]
        =^  corn  gen  $(nomm corn.nomm, fax (peg fax 7), goal [%next [%none ~] dire])
        =^  cost  gen  $(nomm cost.nomm, fax (peg fax 6), goal [%next seed then.corn])
        (copy cost what.corn)
      ::
          %next
        =^  [post=bile salt=@uvre]  gen  (kerf %post goal)
        =^  [dire=bile seed=need]  gen
          ?~  hope
            =^  dine  gen  (emit %dine ~ ~ %cal u.bull v.a salt post)
            =?  redo.gen  r.a  [dine redo.gen]
            [[dine n.a] gen]
          =^  s  gen  rain
          =^  dime  gen  (emit %dime ~ ~ %caf u.bull v.a salt post s u.hope)
          =?  redo.gen  r.a  [dime redo.gen]
          =^  nest  gen  (copy [%next n.a dime] [%this s])
          [[then.nest what.nest] gen]
        =^  corn  gen  $(nomm corn.nomm, fax (peg fax 7), goal [%next [%none ~] dire])
        =^  cost  gen  $(nomm cost.nomm, fax (peg fax 6), goal [%next seed then.corn])
        (copy cost what.corn)
      ==
    ::
        %the
      ?-  -.goal
          %done
        =^  last  gen  rain
        =^  hasp  gen  rain
        =^  barf  gen  rain
        =^  tear  gen  (emit %tear ~ [%imm 0 last]~ %don last)
        =^  fear  gen  (emit %fear ~ [%imm 1 hasp]~ %don hasp)
        $(goal [%pick barf tear fear])
      ::
          %next
        ?:  ?=(%both -.what.goal)  (mine sass.what.goal then.goal)
        ?:  ?=(%none -.what.goal)
          =^  barf  gen  rain
          $(goal [%pick barf then.goal then.goal])
        =^  tare  gen  rain
        =/  tile  (vial %tile)
        =^  fare  gen  rain
        =/  file  (vial %file)  
        =^  thin  gen
          %:  emit
              %thin
              %:  ~(put by *(map @uvre (map bile @uvre)))
                  sass.what.goal
                  (~(gas by *(map bile @uvre)) ~[[tile tare] [file fare]])
              ==
              ~
              %hop  then.goal
          ==
        =^  tear  gen  (come tile thin)
        =^  fear  gen  (come file thin)
        =^  celt  gen  (emit %celt ~ [%imm 0 tare]~ %hop tear)
        =^  felt  gen  (emit %felt ~ [%imm 1 fare]~ %hop fear)
        =^  barf  gen  rain
        $(goal [%pick barf celt felt])
      ::
          %pick
        =^  coat  gen  rain
        =^  pith  gen  (emit %pith ~ ~ %clq coat [zero once]:goal)
        $(nomm pell.nomm, goal [%next [%this coat] pith], fax (peg fax 3))
      ==
    ::
        %for
      ?-  -.goal
          %done
        =^  rink  gen  rain
        =^  pink  gen  rain
        =^  tike  gen  (emit %tike ~ [%inc pink rink]~ %don rink)
        $(nomm mall.nomm, goal [%next [%this pink] tike], fax (peg fax 3))
      ::
         %pick
        =^  rink  gen  rain
        =^  pink  gen  rain
        =^  pike  gen
          (emit %pike ~ [%inc pink rink]~ %brn rink [zero once]:goal)
        $(nomm mall.nomm, goal [%next [%this pink] pike], fax (peg fax 3))
      ::
         %next
        ?:  ?=(%both -.what.goal)  (mine sass.what.goal then.goal)
        =^  rink  gen
          ?:  ?=(%none -.what.goal)
            rain
          [sass.what.goal gen]
        =^  pink  gen  rain
        =^  bike  gen
          (emit %bike ~ [%inc pink rink]~ %hop then.goal)
        $(nomm mall.nomm, goal [%next [%this pink] bike], fax (peg fax 3))
      ==
    ::
        %ivy
      ?-  -.goal
          %done
        =^  last  gen  rain
        =^  hasp  gen  rain
        =^  reek  gen  (emit %reek ~ [%imm 0 last]~ %don last)
        =^  riff  gen  (emit %riff ~ [%imm 1 hasp]~ %don hasp)
        =^  crap  gen  rain
        $(goal [%pick crap reek riff])
      ::
          %next
        ?:  ?=(%both -.what.goal)  (mine sass.what.goal then.goal)
        ?:  ?=(%none -.what.goal)
          =^  than  gen  $(nomm that.nomm, fax (peg fax 7))
          =^  thin  gen
            $(nomm this.nomm, fax (peg fax 6), then.goal then.than)
          (copy thin what.than)
        =^  tare  gen  rain
        =/  till  (vial %till)
        =^  fare  gen  rain
        =/  fill  (vial %fill)
        =^  ward  gen
          %:  emit
              %ward
              %:  ~(put by *(map @uvre (map bile @uvre)))
                  sass.what.goal
                  (~(gas by *(map bile @uvre)) ~[[till tare] [fill fare]])
              ==
              ~ 
              %hop 
              then.goal
          ==
        =^  weir  gen  (come till ward) 
        =^  mere  gen  (come fill ward)
        =^  ware  gen  (emit %ware ~ [%imm 0 tare]~ %hop weir)
        =^  mare  gen  (emit %mare ~ [%imm 1 fare]~ %hop mere)
        =^  crap  gen  rain
        $(goal [%pick crap ware mare])
      ::
          %pick
        =^  tire  gen  rain
        =^  tear  gen  rain
        =^  pare  gen  (emit %pare ~ ~ %eqq tire tear [zero once]:goal)
        =^  than  gen
          $(nomm that.nomm, goal [%next [%this tear] pare], fax (peg fax 7))
        =^  thin  gen
          $(nomm this.nomm, goal [%next [%this tire] then.than], fax (peg fax 6))
        (copy thin what.than)
      ==
    ::
        %six
      ?:  ?=(%next -.goal)
        =^  [teal=next feel=next]  gen  (phil goal)
        =^  fest  gen
          $(nomm else.nomm, fax (peg fax 15), goal feel)
        =^  zest  gen
          $(nomm then.nomm, fax (peg fax 14), goal teal)
        =^  [bead=need tile=bile file=bile]  gen  (sect zest fest)
        =^  lead  gen  rain
        =^  cond  gen
          $(nomm what.nomm, fax (peg fax 6), goal [%pick lead tile file])
        (copy cond bead)
      =^  fest  gen
        $(nomm else.nomm, fax (peg fax 15))
      =^  zest  gen
        $(nomm then.nomm, fax (peg fax 14))
      =^  [bead=need tile=bile file=bile]  gen  (sect zest fest)
      =^  barf  gen  rain
      =^  tool  gen  (emit %tool ~ [%ipb ~[barf]]~ %hop tile)
      =^  cond  gen
        $(nomm what.nomm, fax (peg fax 6), goal [%pick barf tool file])
      (copy cond bead)
    ::
        %eve
      =^  thin  gen  $(nomm then.nomm, fax (peg fax 7))
      $(nomm once.nomm, goal thin, fax (peg fax 6))
    ::
        %ten
      ?-  -.goal
          %done
        =^  last  gen  rain
        =^  dead  gen  (emit %dead ~ ~ %don last)
        $(goal [%next [%this last] dead])
      ::
          %pick
        ?.  =(here.nomm 1)  (mine sass.goal zero.goal)
        =^  flip  gen  rain
        =^  deep  gen  (emit %deep ~ ~ %brn flip [zero once]:goal)
        $(goal [%next [%this flip] deep])
      ::
          %next
        =^  [twig=need tree=need then=bile]  gen  (into here.nomm goal)
        =^  nest  gen
          $(nomm tree.nomm, fax (peg fax 15), goal [%next tree then])
        =^  eggs  gen
          $(nomm twig.nomm, fax (peg fax 14), goal [%next twig then.nest])
        (copy eggs what.nest)
      ==
    ::
        %sip
      ?+  hint.nomm  $(nomm then.nomm, fax (peg fax 7))
          %bout
        ?-  -.goal
            %done
          =^  last  gen  rain
          =^  dime  gen  (emit %dime ~ ~ %don last)
          $(goal [%next [%this last] dime])
        ::
            %pick
          =^  tome  gen  (emit %tome ~ [%tom ~]~ %hop zero.goal)
          =^  foam  gen  (emit %foam ~ [%tom ~]~ %hop once.goal)
          =^  race  gen
            $(nomm then.nomm, fax (peg fax 7), goal [%pick sass.goal tome foam])
          =^  tick  gen  (emit %tick ~ [%tim ~]~ %hop then.race)
          [race(then tick) gen]
        ::
            %next
          =^  stop  gen  (emit %stop ~ [%tom ~]~ %hop then.goal)
          =^  race  gen
            $(nomm then.nomm, fax (peg fax 7), then.goal stop)
          =^  goes  gen  (emit %goes ~ [%tim ~]~ %hop then.race)
          [race(then goes) gen]
        ==
      ::
          %meme
        =^  raft  gen  $(nomm then.nomm, fax (peg fax 7))
        =^  meme  gen  (emit %meme ~ [%mem ~]~ %hop then.raft)
        [raft(then meme) gen]
      ==
    ::
        %tip
      ?+    hint.nomm 
        =^  thin  gen  $(nomm then.nomm, fax (peg fax 7))
        =^  fake  gen
          $(nomm vice.nomm, fax (peg fax 13), goal [%next [%none ~] then.thin])
        (copy fake what.thin)
      ::
          ?(%hunk %hand %lose %mean %spot)
        =^  mane  gen  rain
        ?-  -.goal
            %done
          =^  real  gen  $(nomm then.nomm, fax (peg fax 7))
          =^  dint  gen
            (emit %dint ~ [%men hint.nomm mane]~ %hop then.real)
          =^  fake  gen
            $(nomm vice.nomm, fax (peg fax 14), goal [%next [%this mane] dint])
          (copy fake what.real)
        ::
            %pick
          =^  tame  gen  (emit %tame ~ [%man ~]~ %hop zero.goal)
          =^  fame  gen  (emit %fame ~ [%man ~]~ %hop once.goal)
          =^  real  gen
            $(nomm then.nomm, fax (peg fax 7), goal [%pick sass.goal tame fame])
          =^  dint  gen
            (emit %dint ~ [%men hint.nomm mane]~ %hop then.real)
          =^  fake  gen
            $(nomm vice.nomm, fax (peg fax 13), goal [%next [%this mane] dint])
          (copy fake what.real)
        ::
            %next
          =^  rugs  gen  (emit %rugs ~ [%man ~]~ %hop then.goal)
          =^  real  gen
            $(nomm then.nomm, fax (peg fax 7), then.goal rugs)
          =^  dint  gen
            (emit %dint ~ [%men hint.nomm mane]~ %hop then.real)
          =^  fake  gen
            $(nomm vice.nomm, fax (peg fax 13), goal [%next [%this mane] dint])
          (copy fake what.real)
        ==
      ::
          ?(%live %slog)
        =^  clue  gen  rain
        =^  real  gen  $(nomm then.nomm, fax (peg fax 7))
        =^  wave  gen
          ?:  ?=(%live hint.nomm)
            (emit %live ~ [%hit clue]~ %hop then.real)
          (emit %slog ~ [%slg clue]~ %hop then.real)
        =^  fake  gen
          $(nomm vice.nomm, fax (peg fax 13), goal [%next [%this clue] wave])
        (copy fake what.real)
      ::
          %memo
        =/  fork  (~(got by fizz) hare.nomm)
        =^  funk  gen  rain
        =^  sunk  gen  rain
        =^  gunk  gen  rain
        =/  body=(list pole)  ~[[%imm 0 gunk] [%imm fork funk]]
        ?-  -.goal
            %done
          =^  salt  gen  rain
          =^  mode  gen  (emit %mode ~ ~ %don salt)
          $(goal [%next [%this salt] mode])
        ::
            %pick
          =^  mere  gen  rain
          =^  chit  gen  (emit %chit ~ ~ %brn mere zero.goal once.goal)
          =^  loot  gen  rain
          =^  root  gen  rain
          =^  loam  gen  (emit %loam ~ ~[[%imm 0 loot] [%mew gunk sunk funk loot]] %hop zero.goal)
          =^  rome  gen  (emit %rome ~ ~[[%imm 1 root] [%mew gunk sunk funk root]] %hop once.goal)
          =^  moog  gen  $(nomm then.nomm, fax (peg fax 7), zero.goal loam, once.goal rome)
          =^  cast  gen  (emit %cast ~ body %mer gunk sunk funk mere chit then.moog)
          =^  fake  gen  $(nomm vice.nomm, fax (peg fax 13), goal [%next [%none ~] cast])
          =^  fear  gen  (copy fake what.moog)
          (copy fear [%this sunk])
        ::
            %next
          =^  [chit=next miss=next]  gen  (phil goal)
          =^  [chin=bile mere=@uvre]  gen  (kerf %chin chit)
          =^  [misc=bile salt=@uvre]  gen  (kerf %salt miss)
          =^  meow  gen  (emit %meow ~ [%mew gunk sunk funk salt]~ %hop misc)
          =^  real  gen  $(nomm then.nomm, fax (peg fax 7), goal [%next [%this salt] meow])
          =^  cake  gen  (emit %cake ~ body %mer gunk sunk funk mere chin misc)
          =^  fake  gen  $(nomm vice.nomm, fax (peg fax 13), goal [%next [%none ~] cake])
          =^  cope  gen  (copy fake what.real)
          (copy cope [%this sunk])
        ==
      ::
          %bout  ~|  %todo  !!
      ==
    ::
        %elf
      ?-  -.goal
          %done
        =^  last  gen  rain
        =^  deft  gen  (emit %deft ~ ~ %don last)
        $(goal [%next [%this last] deft])
      ::
          %pick
        =^  flip  gen  rain
        =^  heft  gen  (emit %heft ~ ~ %brn flip [zero once]:goal)
        $(goal [%next [%this flip] heft])
      ::
          %next
        =^  [weft=bile good=@uvre]  gen  (kerf %weft goal)
        =^  home  gen  rain
        =^  path  gen  rain
        =^  show  gen  (emit %show ~ ~ %spy home path good weft)
        =^  trot  gen
          $(nomm walk.nomm, fax (peg fax 7), goal [%next [%this path] show])
        =^  paid  gen
          $(nomm rent.nomm, fax (peg fax 6), goal [%next [%this home] then.trot])
        (copy paid what.trot)
      ==
    ==
  ::
  ::    redo callsite registerization
  ::
  ::  given recursion, we may not know the registerization for an arm
  ::  when we generate a direct call to it. Thus, once we have generated
  ::  code for all arms in the call tree and know their
  ::  registerizations, we return to callsites and generate
  ::  properly-registerized calls, without changing the registerization
  ::  of the calling arm.
  ++  redo
    |=  =bile
    ^-  _gen
    =.  fax  axe.bile
    =/  blob  (~(got by will.gen) bile)
    ?+  -.bend.blob  ~|  %redo-cant  !!
        %cal
      ?>  ?=(^ v.bend.blob)
      ?>  ?=(~ t.v.bend.blob)
      ?>  (~(has by like) a.bend.blob)
      =^  urge  gen  (args a.bend.blob)
      =^  reed  gen  (emit %reed ~ ~ bend.blob(v v.urge))
      =^  [rush=_bile i=@uvre]  gen  (kerf %rush [%next n.urge reed])
      =.  will.gen  (~(del by will.gen) bile)
      (emir bile ~ [%mov i.v.bend.blob i]~ %hop rush)
    ::
        %caf
      ?>  ?=(^ v.bend.blob)
      ?>  ?=(~ t.v.bend.blob)
      ?>  (~(has by like) a.bend.blob)
      =^  urge  gen  (args a.bend.blob)
      =^  reed  gen  (emit %reed ~ ~ bend.blob(v v.urge))
      =^  [rush=_bile i=@uvre]  gen  (kerf %rush [%next n.urge reed])
      =.  will.gen  (~(del by will.gen) bile)
      (emir bile ~ [%mov i.v.bend.blob i]~ %hop rush)
    ::
        %jmp
      ?>  ?=(^ v.bend.blob)
      ?>  ?=(~ t.v.bend.blob)
      ?>  (~(has by like) a.bend.blob)
      =^  urge  gen  (args a.bend.blob)
      =^  reed  gen  (emit %reed ~ ~ bend.blob(v v.urge))
      =^  [rush=_bile i=@uvre]  gen  (kerf %rush [%next n.urge reed])
      =.  will.gen  (~(del by will.gen) bile)
      (emir bile ~ [%mov i.v.bend.blob i]~ %hop rush)
    ::
        %jmf
      ?>  ?=(^ v.bend.blob)
      ?>  ?=(~ t.v.bend.blob)
      ?>  (~(has by like) a.bend.blob)
      =^  urge  gen  (args a.bend.blob)
      =^  reed  gen  (emit %reed ~ ~ bend.blob(v v.urge))
      =^  [rush=_bile i=@uvre]  gen  (kerf %rush [%next n.urge reed])
      =.  will.gen  (~(del by will.gen) bile)
      (emir bile ~ [%mov i.v.bend.blob i]~ %hop rush)
    ==
  ::   split register to need
  ::
  ::  given a destination, generate code which splits a noun in one
  ::  register to the registers described by the $need, and return the
  ::  one register and a label for the splitting code
  ++  kerf
    |=  [thus=@tas =next]
    ^-  [[bile @uvre] _gen]
    =^  ir  gen  (kern ~ what.next)
    ?~  pose.ir
      [[then.next out.ir] gen]
    =^  thin  gen  (emit thus ~ (flop pose.ir) %hop then.next)
    [[thin out.ir] gen]
  ::    split register to need (instruction list)
  :: 
  ::  like +kerf but return (reversed) instruction list instead of emitting basic block
  ++  kern
    |=  [pose=(list pole) =need]
    ^-  [[pose=(list pole) out=@uvre] _gen]
    =/  tack=(list _need)  ~[need]
    =/  ui  (sass need)
    ?~  ui
      =^  crap  gen  rain
      [[~ crap] gen]
    |-  ^-  [[pose=(list pole) out=@uvre] _gen]
    ?~  tack
      [[pose u.ui] gen]
    =*  n  i.tack
    ?:  ?=(%both -.n)
      =/  lure  (sass left.n)
      =/  rule  (sass rite.n)
      =?  pose  ?=(^ lure)
        [[%hed sass.n u.lure] pose]
      =?  pose  ?=(^ rule)
        [[%tal sass.n u.rule] pose]
      $(tack [left.n rite.n t.tack])
    $(tack t.tack)
  ::    emit basic block
  ::
  ::  given a fixed label and a basic block,
  ::  add the basic block to the code table
  ++  emit
    |=  [thus=@tas =blob]
    ^-  [bile _gen]
    =/  bill  [%bile fax thus bell]
    [bill (emir bill blob)]
  ::    emit basic block (raw label)
  ::
  :: given a raw bile and a basic block, add the basic block to the code
  :: tabel at that label.
  ++  emir
    |=  [=bile =blob]
    ^-  _gen
    gen(will (~(put by will.gen) bile blob))
  ::
  ::    generate a register
  ::
  ::  return the current next SSA register and increment the next SSA
  ::  register in the codegen state
  ++  rain
    ^-  [@uvre _gen]
    [sans.gen gen(sans .+(sans.gen))]
  ::
  ::    split need
  ::
  ::  split a need into two, generating cons instruction if necessary
  ++  lyse
    |=  =next
    ^-  [[bile need need] _gen]
    ?-  -.what.next
        %both :: discards sick flag which is OK since we know we will fulfill the need
      [[then.next left.what.next rite.what.next] gen]
    ::
        %none
      [[then.next [%none ~] %none ~] gen]
    ::
        %this
      =^  l  gen  rain
      =^  r  gen  rain
      =^  lizz  gen  (emit %lyse ~ [%con l r sass.what.next]~ %hop then.next)
      [[lizz [%this l] [%this r]] gen]
    ==
  ::
  ::    outermost register
  ::
  ::  get the outermost register of a need (or ~ if the need is %none):
  ::  used for noun-splitting code
  ++  sass
    |=  =need
    ^-  (unit @uvre)
    ?-  -.need
      %both  `sass.need
      %this  `sass.need
      %none  ~
    ==
  ::    intersect needs
  ::
  ::  match needs from branching control flow, generating noun-splitting
  ::  code for each branch as necessary
  ::
  ::  this generates the maximally common split of registers between
  ::  both branches. If one branch expects a cell at an axis but the other does
  ::  not, then we must expect that axis in a register so we do not
  ::  crash when the more permissive branch would be taken
  ++  sect
    |=  [zero=next once=next]
    =|  lose=(list pole)
    =|  rose=(list pole)
    =/  tack=(list (each r=@uvre [z=need o=need]))  [%| what.zero what.once]~
    =|  salt=(list need)
    |-  ^-  [[need bile bile] _gen]
    ?~  tack
      ?>  ?=(^ salt)
      ?>  ?=(~ t.salt)
      =^  loan  gen  (emit %loan ~ (flop lose) %hop then.zero)
      =^  roan  gen  (emit %roan ~ (flop rose) %hop then.once)
      [[i.salt loan roan] gen]
    ?-  -.i.tack
        %&
      ?>  ?=(^ salt)
      ?>  ?=(^ t.salt)
      $(tack t.tack, salt [[%both p.i.tack i.t.salt i.salt] t.t.salt])
    ::
        %|
      ?:  ?=(%none -.z.p.i.tack)
        :: z side has no requirements
        :: so we should do no splitting outside conditional
        ?:  ?=(%none -.o.p.i.tack)
          $(tack t.tack, salt [[%none ~] salt])
        =^  rr  gen  (kern rose o.p.i.tack) 
        =.  rose  pose.rr
        $(tack t.tack, salt [[%this out.rr] salt])
      ?:  ?=(%none -.o.p.i.tack)
        :: o side has no requirements
        :: so we should do no splitting outside conditional
        =^  lr  gen  (kern lose z.p.i.tack)
        =.  lose  pose.lr
        $(tack t.tack, salt [[%this out.lr] salt])
      ?:  ?=(%both -.z.p.i.tack)
        ::  z side splits
        ?:  ?=(%both -.o.p.i.tack)
          ::  both sides split, recursively build need
          %=  $
              tack
            :*  [%| left.z.p.i.tack left.o.p.i.tack]
                [%| rite.z.p.i.tack rite.o.p.i.tack]
                [%& sass.z.p.i.tack]
                t.tack
            ==
          ::
            rose  [[%mov sass.z.p.i.tack sass.o.p.i.tack] rose]
          ==
        ::  z side splits, o side this
        =^  lr  gen  (kern ~ z.p.i.tack)
        =.  lose  [[%mov sass.o.p.i.tack out.lr] lose]
        =.  lose  (weld pose.lr lose)
        $(tack t.tack, salt [o.p.i.tack salt])
      ?:  ?=(%both -.o.p.i.tack)
        ::  z side this, o side splits
        =^  rr  gen  (kern ~ o.p.i.tack)
        =.  rose  [[%mov sass.z.p.i.tack out.rr] rose]
        =.  rose  (weld pose.rr rose)
        $(tack t.tack, salt [z.p.i.tack salt])
      ::  both sides this
      =.  rose  [[%mov sass.z.p.i.tack sass.o.p.i.tack] rose]
      $(tack t.tack, salt [z.p.i.tack salt])
    ==
  ::
  ::    union needs
  ::
  ::  generate a need split as far as either input need is split,
  ::  generating cons code for less-split need. This is used when two
  ::  sequential subformulas read from the same subject
  ::
  ::  for correctness in crash handling it is vital that the needs are
  ::  ordered by the evaluation order of the computations, so that the
  ::  first need is from the first computation and the second need from
  ::  the second.
  ++  copy
    |=  [feed=next seed=need]
    =|  pose=(list pole)
    =/  tack=(list (each @uvre [l=need r=need]))  [%| what.feed seed]~
    =|  rack=(list need)
    |-  ^-  [next _gen]
    ?~  tack
      ?>  ?=(^ rack)
      ?>  ?=(~ t.rack)
      =^  cody  gen  (emit %copy ~ pose %hop then.feed)
      [[%next i.rack cody] gen]
    ?:  ?=(%& -.i.tack)
      ?>  ?=(^ rack)
      ?>  ?=(^ t.rack)
      $(rack [[%both p.i.tack i.t.rack i.rack] t.t.rack], tack t.tack)
    ?:  ?=(%none -.l.p.i.tack)  $(rack [r.p.i.tack rack], tack t.tack)
    ?:  ?=(%none -.r.p.i.tack)  $(rack [l.p.i.tack rack], tack t.tack)
    ?:  ?=(%this -.l.p.i.tack)
      ?:  ?=(%this -.r.p.i.tack)
        :: both this
        =?  pose  ?!  .=  sass.l.p.i.tack  sass.r.p.i.tack
          [[%mov sass.l.p.i.tack sass.r.p.i.tack] pose]
        $(rack [l.p.i.tack rack], tack t.tack)
      :: left this, right both
      :: 
      :: this case must be handled this way in case the code that needs
      :: l.p.i.tack will crash explicitly in some way.
      =^  rr  gen  (kern ~ r.p.i.tack)
      =.  pose  (weld (flop pose.rr) pose)
      =?  pose  ?!(=(sass.l.p.i.tack out.rr))
        [[%mov sass.l.p.i.tack out.rr] pose]
      $(tack t.tack, rack [[%this sass.l.p.i.tack] rack])
    ?:  ?=(%both -.r.p.i.tack)
      :: both both
      %=  $
          pose  [[%mov sass.l.p.i.tack sass.r.p.i.tack] pose]
          tack
        :*  [%| left.l.p.i.tack left.r.p.i.tack]
            [%| rite.l.p.i.tack rite.r.p.i.tack]
            [%& sass.l.p.i.tack]
            t.tack
        ==
      ==
    ::  left both, right this
    =/  lu  (sass left.l.p.i.tack)
    =/  ru  (sass rite.l.p.i.tack)
    =^  l  gen  ?~(lu rain [u.lu gen])
    =^  r  gen  ?~(ru rain [u.ru gen])
    %=  $
      pose  [[%con l r sass.r.p.i.tack] pose]
      tack
      :*  [%| left.l.p.i.tack %this l]
          [%| rite.l.p.i.tack %this r]
          [%& sass.l.p.i.tack]
          t.tack
      ==
    ==
  ::
  ::    crash
  ::
  ::  generate unconditional crashing code
  ++  bomb
    =^  b  gen  boom
    [[%next [%none ~] b] gen]
  ::
  ::    crash
  ::
  ::  like +bomb, but return only the label and not the need
  ++  boom
    (emit %boom ~ ~ %bom ~)
  ::  
  ::    Defer crash
  ::
  ::  Unconditionally poison the register.
  ::
  ::  This used when a value is known to not match the expectation of a
  ::  need
  ++  mine
    |=  [r=@uvre t=bile]
    ^-  [next _gen]
    =^  mile  gen  (emit %mine ~ [%poi r]~ %hop t)
    [[%next [%none ~] t] gen]
  ::  
  ::    create label
  ::
  ::  emit a label with the given fixed name in the current context
  ++  vial
    |=  t=@tas
    [%bile fax t bell]
  ::  
  ::    label come-from
  ::
  ::  emit an instruction which explicitly records the jump origin
  ::  useful for evaluating phi instructions in the jump destination
  ++  come
    |=  [f=bile t=bile]
    :-  f
    %=  gen
        will
      %+  ~(put by will.gen)  f 
      ^-  blob  [~ ~ %hip f t]
    ==
  ::
  ::    emit phi node
  ::
  ::  given a destination common to two branches, generate a phi node
  ::  and come-from blocks
  ++  phil
    |=  =next
    =/  tack=(list (each [zp=@uvre op=@uvre] need))  [%| what.next]~
    =|  salt=(list [z=need o=need]) 
    =|  biff=(map @uvre (map bile @uvre))
    =/  zb  (vial %zebu)
    =/  ob  (vial %oboe)
    |-  ^-  [[_next _next] _gen]
    ?~  tack
      ?>  ?=(^ salt)
      ?>  ?=(~ t.salt)
      =^  fill  gen  (emit %phil biff ~ %hop then.next)
      =^  zeke  gen  (come zb fill)
      =^  oaks  gen  (come ob fill)
      [[[%next z.i.salt zeke] [%next o.i.salt oaks]] gen]
    ?-  -.i.tack
        %&
      ?>  ?=(^ salt)
      ?>  ?=(^ t.salt)
      %=  $
          tack  t.tack
          salt 
        :_  t.t.salt
        :-  [%both zp.p.i.tack z.i.t.salt z.i.salt]
        [%both op.p.i.tack o.i.t.salt o.i.salt]
      ==
    ::
        %|
      ?-  -.p.i.tack
          %none  $(salt [[[%none ~] %none ~] salt], tack t.tack)
          %this
        =^  l  gen  rain
        =^  r  gen  rain
        =/  phi  (~(gas by *(map bile @uvre)) ~[[zb l] [ob r]])
        %=  $
            biff  (~(put by biff) sass.p.i.tack phi)
            tack  t.tack
            salt  [[[%this l] %this r] salt]
        ==
      ::
          %both
        =^  hurl  gen  rain
        =^  barf  gen  rain
        =/  phi  (~(gas by *(map bile @uvre)) ~[[zb hurl] [ob barf]])
        %=  $
            biff  (~(put by biff) sass.p.i.tack phi)
            tack
          :*  [%| left.p.i.tack]
              [%| rite.p.i.tack]
              [%& hurl barf]
              t.tack
          ==
        ==
      ==
    ==
  ::
  ::    direct call information
  ::
  ::  when we emit code for a direct call, we hope to know the
  ::  registerization already. If we don't, we need to add the call to
  ::  the redo set. If we do, then we need a linear list of poison
  ::  registers and a linear list of argument registers, as well as a
  ::  need which describes which parts of the call subject go in which
  ::  registers
  ++  args
    |=  =_bell
    ^-  [[v=(list @uvre) n=need r=?] _gen]
    =/  cn  (~(get by like) bell)
    =?  cn  ?=(~ cn)
      =/  dn  (~(get by hill) bell)
      ?~  dn  ~
      `want.u.dn
    ?~  cn
      =^  s  gen  rain
      [[~[s] [%this s] &] gen]
    =^  s  gen  (scar u.cn)
    [[v n |]:s gen]
  ::
  ::    generate fresh parameter lists
  ::
  ::  generate fresh parameter variables and provide them both in
  ::  argument list and need form
  ++  scar
    |=  n=need
    =|  rv=(list @uvre)
    =/  tack=(list (each @uvre need))  [%| n]~
    =|  salt=(list need)
    |-  ^-  [[v=(list @uvre) n=need] _gen]
    ?~  tack
      ?>  ?=(^ salt)
      ?>  ?=(~ t.salt)
      [[(flop rv) i.salt] gen]
    ?-  -.i.tack
        %&
      ?>  ?=(^ salt)
      ?>  ?=(^ t.salt)
      $(tack t.tack, salt [[%both p.i.tack i.t.salt i.salt] t.t.salt])
    ::
        %|
      ?-  -.p.i.tack
          %both
        =^  br  gen  rain
        %=  $
            tack
          :*  [%| left.p.i.tack]
              [%| rite.p.i.tack]
              [%& br] 
              t.tack
          ==
        ==
      ::
          %none  $(tack t.tack, salt [[%none ~] salt])
          %this
        =^  vr  gen  rain
        $(rv [vr rv], salt [[%this vr] salt], tack t.tack)
      ==
    ==
  ::    need at axis
  ::
  ::  push a need down by adding %both cases along the path described by
  ::  the axis. Used for nock 0 / %not.
  ++  from
    |=  [axe=@ =next]
    ?<  =(0 axe)
    =^  crap  gen
      =/  crop  (sass what.next)
      ?~  crop  rain
      [u.crop gen]
    =?  what.next  ?=(%none -.what.next)  [%this crap]
    =|  bait=(list [r=@uvre c=?(%2 %3)])
    |-  ^-  [_next _gen]
    ?.  =(1 axe)
      =^  barf  gen  rain
      $(bait [[barf (cap axe)] bait], axe (mas axe))
    =/  bits  (turn bait |=([r=@uvre *] r))
    =^  fram  gen  (emit %fram ~ [%ipb ~[crap]]~ %hop then.next)
    =/  feed
      %+  roll  bait
      |=  [[r=@uvre c=?(%2 %3)] n=_what.next]
      ?-  c
        %2  [%both r n %none ~]
        %3  [%both r [%none ~] n]
      ==
    [[%next feed fram] gen]
  ::
  ::    split need at axis
  ::
  ::  split a need along an axis to describe an edit operation.
  ::  the first returned need is for the patch noun, and the second is
  ::  for the noun to be edited
  ++  into
    |=  [axe=@ =next]
    =*  twig  what.next
    =|  tres=(list [lr=?(%2 %3) p=@uvre =need])
    =|  pose=(list pole)
    ?<  =(0 axe)
    |-  ^-  [[need need bile] _gen]
    ?.  =(1 axe)
      =^  p  gen  rain
      ?-  (cap axe)
          %2
        ?-  -.twig
            %both
          %=  $
            tres  [[%2 p rite.twig] tres]
            twig  left.twig
            axe   (mas axe)
            pose  [[%mov p sass.twig] pose]
          ==
        ::
            %this
          =^  l  gen  rain
          =^  r  gen  rain
          %=  $
            tres  [[%2 p %this r] tres]
            twig  [%this l]
            axe   (mas axe)
            pose  [[%con l r sass.twig] pose]
          ==
        ::
            %none
          %=  $
            tres  [[%2 p %none ~] tres]
            axe   (mas axe)
          ==
        ==
      ::
          %3
        ?-  -.twig
            %both
          %=  $
            tres  [[%3 p left.twig] tres]
            twig  rite.twig
            axe   (mas axe)
            pose  [[%mov p sass.twig] pose]
          ==
        ::
            %this
          =^  l  gen  rain
          =^  r  gen  rain
          %=  $
            tres  [[%3 p %this l] tres]
            twig  [%this r]
            axe   (mas axe)
            pose  [[%con l r sass.twig] pose]
          ==
        ::
            %none
          %=  $
            tres  [[%3 p %none ~] tres]
            axe   (mas axe)
          ==
        ==
      ==
    =^  flag  gen  rain
    =/  tree=need  [%this flag]
    |-  ^-  [[need need bile] _gen]
    ?~  tres 
      =^  tint  gen  (emit %into ~ [[%ipb ~[flag]] pose] %hop then.next)
      [[twig tree tint] gen]
    ?-  lr.i.tres
        %2
      $(tres t.tres, tree [%both p.i.tres tree need.i.tres])
    ::
        %3
      $(tres t.tres, tree [%both p.i.tres need.i.tres tree])
    ==
  ::
  ::    split immediate
  ::
  ::  given a noun and a need, generate instructions to emit that noun
  ::  into the registers of that need
  ++  mede
    |=  [=bile n=* =need]
    =|  todo=(list pole)
    =/  tack=(list [n=(unit *) =_need])  [`n need]~
    |-  ^-  [_bile _gen]
    ?~  tack
      (emit %mede ~ todo %hop bile)
    ?-  -.need.i.tack
        %none  $(tack t.tack)
        %this
      ?~  n.i.tack
        $(todo [[%poi sass.need.i.tack] todo], tack t.tack)
      $(todo [[%imm u.n.i.tack sass.need.i.tack] todo], tack t.tack)
      ::
        %both
      ?~  n.i.tack
        $(tack [[~ rite.need.i.tack] [~ left.need.i.tack] t.tack])
      ?@  u.n.i.tack
        $(tack [[~ rite.need.i.tack] [~ left.need.i.tack] t.tack])
      $(tack [[`+.u.n.i.tack rite.need.i.tack] [`-.u.n.i.tack left.need.i.tack] t.tack])
    ==
  --
::
::    lists of registers from a need
::  
::  the second list (walt) is the input registers in left-to-right order
++  sill
  |=  want=need
  =|  wart=(list @uvre)
  =/  tack=(list need)  ~[want]
  |-  ^-  (list @uvre)
  ?~  tack  wart
  ?-  -.i.tack
      %none  $(tack t.tack)
      %both
    %=  $
      tack  [rite.i.tack left.i.tack t.tack]
    ==
  ::
      %this
    %=  $
      wart  [sass.i.tack wart]
      tack  t.tack
    ==
  ==
::    
::    loop over redos
::
::  run redo:jean on each arm in the redo list, which will generate
::  code to properly registerize callsites whose registerization was
::  deferred, without changing the registerization of the calling arm
++  mill
  =|  todo=(list [=bell dire=next =gen])
  =|  like=(map bell need)
  =/  toil  work 
  =/  wurk  toil
  |-  ^-  _hill
  ?^  toil
    =/  [dire=next =gen]  ~(cuts jean i.toil *gen like)
    %=  $
       toil  t.toil
       todo  [[i.toil dire gen] todo]
       like  (~(put by like) i.toil what.dire)
    ==
  |-  ^-  _hill
  ?^  todo
    =/  r  redo.gen.i.todo
    |-  ^-  _hill
    ?^  r
      =.  gen.i.todo  (~(redo jean bell.i.todo gen.i.todo like) i.r)
      $(r t.r)
    =^  [wish=bile sire=@uvre]  gen.i.todo  (~(kerf jean bell.i.todo gen.i.todo like) %indy dire.i.todo)
    ?.  (~(has by will.gen.i.todo) wish)  ~&  %missing-wish  !!
    %=  ^$
        hill
      =/  walt  (sill what.dire.i.todo)
      %+  ~(put by hill)  bell.i.todo
      [then.dire.i.todo what.dire.i.todo walt wish sire [will sans]:gen.i.todo]
    ::
        todo  t.todo
    ==
  ::  XX temporary: turn hip/phi into mov so we can run this as-is
  ::  note that it's not safe to do mov coalescing on the output of this
  ::  since we may now have multiple %mov's that target one register
  =/  toil  wurk
  |-  ^-  _hill
  ?^  toil
    %=  $
        toil  t.toil
    ::
        hill
      %+  ~(jab by hill)  i.toil
      |=  =pile
      =/  queu=(list bile)  ~[long wish]:pile
      =|  back=(list bile)
      =|  will=(map bile blob)
      |-  ^-  _pile
      ?~  queu
        ?~  back  pile(will will)
        $(queu (flop back), back ~)
      =/  blob  (~(got by will.pile) i.queu)
      ?-  -.bend.blob
          %hip
        =/  movs
          %+  turn  ~(tap by biff:(~(got by will.pile) t.bend.blob))
          |=  [out=@uvre bin=(map bile @uvre)]
          [%mov (~(got by bin) c.bend.blob) out]
        %=  $
            queu  t.queu
            back  [t.bend.blob back]
            will
          %+  ~(put by will)  i.queu
          [biff.blob (welp body.blob movs) %hop t.bend.blob]
        ==
      ::
          %clq
        $(queu t.queu, back (weld ~[z o]:bend.blob back), will (~(put by will) i.queu blob))
      ::
          %eqq
        $(queu t.queu, back (weld ~[z o]:bend.blob back), will (~(put by will) i.queu blob))
      ::
          %brn 
        $(queu t.queu, back (weld ~[z o]:bend.blob back), will (~(put by will) i.queu blob))
      ::
          %hop
        $(queu t.queu, back [t.bend.blob back], will (~(put by will) i.queu blob))
      ::
          %lnk
        $(queu t.queu, back [t.bend.blob back], will (~(put by will) i.queu blob))
      ::
          %cal
        $(queu t.queu, back [t.bend.blob back], will (~(put by will) i.queu blob))
      ::
          %caf
        $(queu t.queu, back [t.bend.blob back], will (~(put by will) i.queu blob))
      ::
          %lnt
        $(queu t.queu, will (~(put by will) i.queu blob))
      ::
          %jmp
        $(queu t.queu, will (~(put by will) i.queu blob))
      ::
          %jmf
        $(queu t.queu, will (~(put by will) i.queu blob))
      ::
          %spy
        $(queu t.queu, back [t.bend.blob back], will (~(put by will) i.queu blob))
      ::
          %mer
        $(queu t.queu, back (weld ~[i m]:bend.blob back), will (~(put by will) i.queu blob))
      ::
          %don
        $(queu t.queu, will (~(put by will) i.queu blob))
      ::
          %bom
        $(queu t.queu, will (~(put by will) i.queu blob))
      ==
    ==
  hill
--
::    codegen interface
=+  %1
|%
::  
::    core reference
++  this  .
::
::    look for code
::
::  check if code exists for a given subject and formula
::  XX should optionally return a path to be checked against hot state, 
::  to invoke jets on indirect
++  peek
  |=  [s=* f=*]
  ^-  (unit [=bell hall=_hill])
  =/  moat  (~(get ja moan) f)
  |-
  ?~  moat  ~
  ?.  (~(huge so:sack soot.i.moat) [& s])
    $(moat t.moat)
  ?.    (~(has by hill) [soot.i.moat f])
    ~&  %not-in-hill  !!
  `[[soot.i.moat f] hill]
::
::    core state interface
::  [%comp ...]: generate code for given subject/formula pair
++  poke
  |=  =gist
  ~>  %bout
  ^-  [new=(set bell) old=(set bell) =_this]
  ::  %comp is the only case
  ::  analyze
  =.  sack  (rout:sack s.gist f.gist)
  ?<  =(~ moan)
  ::  save old codegen table keys
  =/  hole  ~(key by hill)
  ::  codegen
  =.  hill  mill
  ::  get entry label for new codegen
  =/  bell  
    =/  peep  (peek [s f]:gist)
    ?>  ?=(^ peep)
    bell.u.peep
  =/  heck  ~(key by hill) 
  [(~(dif in heck) hole) (~(dif in hole) heck) this]
::    pretty-printing door
++  xray
  |_  will=(map bile blob)
  ::
  ::    print a bell as an @ux-ed mug
  ++  ring
    |=  a=bell
    ^-  tank
    >`@ux`(mug a)<
  ::
  ::    print a bell as an @ux-ed mug + formula
  ++  rang
    |=  a=bell
    ^-  tank
    [%rose ["-" "" ""] (ring a) >form.a< ~]
  ::
  ::    print a bile as thus and axe + a pretty bell
  ++  rung
    |=  b=bile
    ^-  tank
    [%rose ["." "[" "]"] >thus.b< >axe.b< (ring +>+.b) ~]
  ::
  ::  print a register
  ++  near
    |=  r=@uvre
    ^-  tank
    [%leaf 'r' (a-co:co r)]
  ::
  ::    instruction print helper
  ++  pink
    |=  [t=@tas l=(list tank)]
    ^-  tank
    [%palm [" " "" "" ""] [%leaf (trip t)] l]
  ::
  ::   print a dataflow instruction
  ++  ping
    |=  i=pole
    ?-  -.i
        %imm
      (pink -.i >n.i< (near d.i) ~)
    ::
        %mov
      (pink -.i (near s.i) (near d.i) ~)
    ::
        %inc
      (pink -.i (near s.i) (near d.i) ~)
    ::
        %con
      (pink -.i (near h.i) (near t.i) (near d.i) ~)
    ::
        %hed
      (pink -.i (near s.i) (near d.i) ~)
    ::
        %tal
      (pink -.i (near s.i) (near d.i) ~)
    ::
        %men
      (pink -.i [%leaf (trip l.i)] (near s.i) ~)
    ::
        %man
      (pink -.i ~)
    ::
        %hit
      (pink -.i (near s.i) ~)
    ::
        %slg
      (pink -.i (near s.i) ~)
    ::
        %mew
      (pink -.i (near k.i) (near u.i) (near f.i) (near r.i) ~)
    ::
        %tim
      (pink -.i ~)
    ::
        %tom
      (pink -.i ~)
    ::
        %mem
      (pink -.i ~)
    ::
        %poi
      (pink -.i (near p.i) ~)
    ::
        %ipb
      (pink -.i (turn p.i near))
    ::
        %slo
      ~|  %todo  !!
    ::
        %sld
      ~|  %todo  !!
    ==
  ::
  ::   print a control flow instruction
  ++  pine
    |=  i=site
    ^-  tank
    ?-  -.i
        %clq
      (pink -.i (near s.i) (rung z.i) (rung o.i) ~)
    ::
        %eqq
      (pink -.i (near l.i) (near r.i) (rung z.i) (rung o.i) ~)
    ::
        %brn
      (pink -.i (near s.i) (rung z.i) (rung o.i) ~)
    ::
        %hop
      (pink -.i (rung t.i) ~)
    ::
        %hip
      (pink -.i (rung c.i) (rung t.i) ~)
    ::
        %lnk
      (pink -.i (near u.i) (near f.i) (near d.i) (rung t.i) ~)
    ::
        %cal
      (pink -.i (ring a.i) [%rose ["," "[" "]"] (turn v.i near)] (near d.i) (rung t.i) ~)
    ::
        %caf
      (pink -.i (ring a.i) [%rose ["," "[" "]"] (turn v.i near)] (near d.i) (rung t.i) (near u.i) >n.i< ~)
    ::
        %lnt
      (pink -.i (near u.i) (near f.i) ~)
    ::
        %jmp
      (pink -.i (ring a.i) [%rose ["," "[" "]"] (turn v.i near)] ~)
    ::
        %jmf
      (pink -.i (ring a.i) [%rose ["," "[" "]"] (turn v.i near)] (near u.i) >n.i< ~)
    ::
        %spy
      (pink -.i (near e.i) (near p.i) (near d.i) (rung t.i) ~)
    ::
        %mer
      (pink -.i (near k.i) (near u.i) (near f.i) (near d.i) (rung i.i) (rung m.i) ~)
    ::
        %don
      (pink -.i (near s.i) ~)
    ::
        %bom
      (pink -.i ~)
    ==
  ::
  ::   print a basic block
  ++  plop
    |=  =blob
    ^-  tank
    [%rose [";" "" ""] (snoc (turn body.blob ping) (pine bend.blob))]
  ::
  ::  topo-sort code in execution order, from an entry point
  ++  sore
    |=  tart=bile
    =/  queu=(list bile)  ~[tart]
    =|  back=(list bile)
    =|  code=(list [bile blob])
    =|  done=(set bile)
    |-  ^-  (list [bile blob])
    ?~  queu
      ?~  back
        (flop code)
      $(queu (flop back), back ~)
    ?:  (~(has in done) i.queu)
      $(queu t.queu)
    =/  blub  (~(got by will) i.queu)
    =/  ouch=(list bile)
      ?-  -.bend.blub
        %clq  ~[z o]:bend.blub
        %eqq  ~[z o]:bend.blub
        %brn  ~[z o]:bend.blub
        %hop  ~[t]:bend.blub
        %hip  ~[t]:bend.blub
        %lnk  ~[t]:bend.blub
        %cal  ~[t]:bend.blub
        %caf  ~[t]:bend.blub
        %lnt  ~
        %jmp  ~
        %jmf  ~
        %spy  ~[t]:bend.blub
        %mer  ~[i m]:bend.blub
        %don  ~
        %bom  ~
      ==
    %=  $
      queu  t.queu
      back  (weld ouch back)
      code  [[i.queu blub] code]
      done  (~(put in done) i.queu)
    ==
  ::
  ::   print the whole code for this arm
  ++  parm
    |=  tart=bile
    ^-  tank
    :*  %rose  [" " "" ""]
        (rang bell.tart)
        %+  turn  (sore tart)
        |=  [l=bile b=blob]
        [%palm ["" "" "->" ""] (rung l) (plop b) ~]
    ==
  ::
  ::   print register value assignments
  ++  vals
    |=  v=(map @uvre *)
    ^-  tank
    :*  %rose  [" " "" ""]
       %+  turn  ~(tap by v)
       |=  [r=@uvre n=*]
       [%palm ["=" "" "" ""] (near r) >n< ~]
    ==
  ::
  ::   print value assigned to register
  ++  gals
    |=  [x=@uvre v=(map @uvre *)]
    ^-  tank
    [%palm ["<--" "" "" ""] (near x) (vals v) ~]
  --
::
::   print code for an arm, if it exists
++  rake
  |=  [s=* f=*]
  ^-  tank
  =/  a  (peek s f)
  ?~  a  [%leaf "no code generated for arm"]
  =/  pile  (~(got by hall.u.a) bell.u.a)
  (~(parm xray will.pile) wish.pile)
::
::   debug-print code for an arm, if it exists
++  rack
  |=  [s=* f=*]
  ^-  ~
  ((slog (rake s f) ~) ~)
--
