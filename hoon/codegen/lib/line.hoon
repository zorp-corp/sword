/-  noir
/-  *gene
=<
|%
::  linearizer stateful core
++  lean
  =|  =hill
  |=  moan=(jar * hone:noir)
  =*  this  .
  ^-  [_hill _this]
  =>
  |%
  ::  enumerate difference in arms between hill and moan
  ++  peck
    ^-  [new=(set bell) old=(set bell)]
    =|  miel=(list bell)
    =/  foam  ~(tap by moan)
    |-  ^-  [new=(set bell) old=(set bell)]
    ?^  foam
      |-  ^-  [new=(set bell) old=(set bell)]
      ?^  q.i.foam
        $(q.i.foam t.q.i.foam, miel [[soot.i.q.i.foam p.i.foam] miel])
      ^$(foam t.foam)
    =/  jell  ~(key by hill)
    ((mif-in (~(gas in *(set bell)) miel)) jell)
  ::  get info about an arm from moan
  ++  puck
    |=  =bell
    ^-  (unit hone:noir)
    =/  hose  (~(get ja moan) +.bell)
    |-  ^-  (unit hone:noir)
    ?^  hose
      ?:  =(-.bell soot.i.hose)  `i.hose
      $(hose t.hose)
    ~
  ::  get a worklist, terminal arms first
  ++  work
    ^-  (list bell)
    =+  peck
    =/  wurk  ~(tap in new)
    =|  nose=(set bell)
    =|  kids=(jug bell bell)
    |-  ^-  (list bell)
    ?^  wurk
      =/  hues  (puck i.wurk)
      ?<  ?=(~ hues)
      =/  cads
        %-  ~(dif in (~(gas in *(set bell)) ~(val by ices.norm.u.hues)))
        loop.norm.u.hues
      =.  kids
        ?:  (~(has by kids) i.wurk)
          (~(jab by kids) i.wurk |=(a=(set bell bell) (~(uni in a) cads)))
        (~(put by kids) i.wurk cads)
      =.  nose  (~(uni by nose) cads)
      $(wurk t.wurk)
    =/  queu  ~(tap in (~(dif by new) nose))
    ?<  =(~ queu)
    =|  back=(list bell)
    =|  done=(set bell)
    =|  toil=(list bell)
    |-  ^-  (list bell)
    ?^  queu
      ?:  (~(has in done) i.queu)  $(queu t.queu)
      %=  $
        queu  t.queu
        toil  [i.queu toil]
        done  (~(put in done) i.queu)
        back  (weld ~(tap in (~(get ju kids) i.queu)) back)
      ==
    ?^  back
      $(queu (flop back), back ~)
    toil
  ::  first pass: turn nomm into linearized code
  ++  mill
    =|  redo=(set bile)
    |=  toil=(list bell)
    ^-  [(set bile) _this]
    ?^  toil
      =|  gen=[will=(map bile blob) sans=@stir redo=(set bile)]
      =/  =goal  [%done ~]  
      =/  fax  1
      |^
        =^  dire=next  gen  cuts
        =^  [wish=bile sire=@stir]  gen  (kerf dire)
        %=  ^$
            toil  t.toil
            redo  (~(uni in redo) redo.gen)
            hill
          %+  ~(put by hill)  i.toil
          [then.dire what.dire wish sire [will sans]:gen]
        ==
      ::  linearize nomm
      ++  cuts
        =+  =/  huns  (puck i.toil)
            ?>  ?=(^ huns)
            norm.u.huns
        |-  ^-  [next _gen]
        ?-  -.nomm
            %par
          ?-  -.goal
              %done
            =^  last  gen  rain
            =^  loch  gen  (emit %loch ~ %don last)
            $(goal [%next [%this last] loch])
          ::
              %pick  bomb
              %next
            =^  [bill=bile left=need rite=need]  gen  (lyse goal)
            =^  tale  gen
              $(nomm rite.nomm, goal [%next rite bill], fax (peg fax 3))
            =^  hale  gen
              $(nomm left.nomm, goal [%next left then.tale], fax (peg fax 2))
            (copy then.hale what.hale what.tale)
          ==
        ::
            %not
          ?:  =(0 here.nomm)  bomb
          ?-  -.goal
              %done
            =^  last  gen  rain
            =^  dear  gen  (emit %dear ~ %don last)
            $(goal [%next [%this last] dear])
          ::
              %pick
            =^  cove  gen  rain
            =^  cozy  gen  (emit %cozy ~ %brn cove [zero once]:goal)
            $(goal [%next [%this cove] cozy])
          ::
              %next
            [[%next (from here.nomm what.goal) then.goal] gen]
          ==
        ::
            %one
          ?-  -.goal
              %done
            =^  last  gen  rain
            =^  rime  gen  (emit %rime [%imm moan.nomm last]~ %don last)
            [[%next [%none ~] rime] gen]
          ::
              %pick
            ?:  =(0 moan.nomm)
              [[%next [%none ~] zero.goal] gen]
            ?:  =(1 moan.nomm)
              [[%next [%none ~] once.goal] gen]
            bomb
          ::
              %next
            =^  bill  gen  (mede then.goal moan.nomm what.goal)
            [[%next [%none ~] bill] gen]
          ==
        ::
            %two
          ?:  ?=(%pick -.goal)
            =^  pyre  gen  rain
            =^  pika  gen  (emit %pika ~ %brn pyre [zero once]:goal)
            $(goal [%next [%this pyre] pika])
          =^  [duke=bile sone=need fore=need]  gen
            =/  bull  (~(get by ices) rail.nomm)
            ?~  bull :: indirect
              =^  sofa  gen  rain
              =^  fora  gen  rain
              ?:  ?=(%done -.goal)
                =^  dune  gen  (emit %dune ~ %lnt sofa fora)
                [[dune [%this sofa] [%this fora]] gen]
              =^  [fine=bile rare=@stir]  gen  (reed goal)
              =^  dunk  gen  (emit %dunk ~ %lnk sofa fora rare fine)
              [[dunk [%this sofa] [%this fora]] gen]
            =/  hull  (~(get by hill) u.bull)
            ?~  hull :: direct but no registerization
              =^  sofa  gen  rain
              ?:  ?=(%done -.goal)
                =^  dude  gen  (emit %dude ~ %jmp u.bull ~[sofa])
                =.  redo.gen  (~(put in redo.gen) dude)  
                [[dude [%this sofa] [%none ~]] gen]
              =^  [fine=bile rare=@stir]  gen  (reed goal)
              =^  deed  gen  (emit %deed ~ %cal u.bull ~[sofa] rare fine)
              =.  redo.gen  (~(put in redo.gen) deed)
              [[deed [%this sofa] [%none ~]] gen]
            ::  direct with registerization
            ?:  ?=(%done -.goal)
              =^  kilt  gen  (cane want.u.hull u.bull ~)
              [[then.kilt what.kilt [%none ~]] gen]
            =^  kilt  gen  (cane want.u.hull u.bull `goal)
            [[then.kilt what.kilt [%none ~]] gen]
          =^  foes  gen  $(nomm corn.nomm, fax (peg fax 14), goal [%next fore duke])
          =^  suet  gen  $(nomm cost.nomm, fax (peg fax 6), goal [%next sone then.foes])
          (copy then.suet what.suet what.foes)
        ::
            %the
          ?-  -.goal
              %done
            =^  reef  gen  rain
            =^  tear  gen  (emit %tear [%imm 0 reef]~ %don reef)
            =^  fear  gen  (emit %fear [%imm 1 reef]~ %don reef)
            $(goal [%pick tear fear])
          ::
              %next
            ?:  ?=(%both -.what.goal)  bomb
            ?:  ?=(%none -.what.goal)
              $(goal [%pick then.goal then.goal])
            =^  tare  gen  rain
            =^  fare  gen  rain
            =^  thin  gen
              (emit %thin [%phi ~[tare fare] sass.what.goal]~ %hop then.goal)
            =^  tear  gen  (emit %tear [%imm 0 tare]~ %hop thin)
            =^  fear  gen  (emit %fear [%imm 1 fare]~ %hop thin)
            $(goal [%pick tear fear])
          ::
              %pick
            =^  coat  gen  rain
            =^  pith  gen  (emit %pith ~ %clq coat [zero once]:goal)
            $(nomm pell.nomm, goal [%next [%this coat] pith], fax (peg fax 3))
          ==
        ::
            %for
          ?-  -.goal
              %done
            =^  rink  gen  rain
            =^  pink  gen  rain
            =^  tike  gen  (emit %tike [%inc pink rink]~ %don rink)
            $(nomm mall.nomm, goal [%next [%this pink] tike], fax (peg fax 3))
          ::
              %pick
            =^  rink  gen  rain
            =^  pink  gen  rain
            =^  pike  gen
              (emit %pike [%inc pink rink]~ %brn rink [zero once]:goal)
            $(nomm mall.nomm, goal [%next [%this pink] pike], fax (peg fax 3))
          ::
              %next
            ?:  ?=(%both -.what.goal)  bomb
            =^  rink  gen
              ?:  ?=(%none -.what.goal)
                rain
              [sass.what.goal gen]
            =^  pink  gen  rain
            =^  bike  gen
              (emit %bike [%inc pink rink]~ %hop then.goal)
            $(nomm mall.nomm, goal [%next [%this pink] bike], fax (peg fax 3))
          ==
        ::
            %ivy
          ?-  -.goal
              %done
            =^  qual  gen  rain
            =^  reek  gen  (emit %reek [%imm 0 qual]~ %don qual)
            =^  riff  gen  (emit %riff [%imm 1 qual]~ %don qual)
            $(goal [%pick reek riff])
          ::
              %next
            ?:  ?=(%both -.what.goal)  bomb
            ?:  ?=(%none -.what.goal)
              =^  than  gen  $(nomm that.nomm, fax (peg fax 7))
              =^  thin  gen
                $(nomm this.nomm, fax (peg fax 6), then.goal then.than)
              (copy then.thin what.thin what.than)
            =^  tare  gen  rain
            =^  fare  gen  rain
            =^  ward  gen
              (emit %ward [%phi ~[tare fare] sass.what.goal]~ %hop then.goal)
            =^  ware  gen  (emit %ware [%imm 0 tare]~ %hop ward)
            =^  mare  gen  (emit %mare [%imm 1 fare]~ %hop ward)
            $(goal [%pick ware mare])
          ::
              %pick
            =^  tire  gen  rain
            =^  tear  gen  rain
            =^  pare  gen  (emit %pare ~ %eqq tire tear [zero once]:goal)
            =^  than  gen
              $(nomm that.nomm, goal [%next [%this tear] pare], fax (peg fax 7))
            =^  thin  gen
              %=  $
                nomm  this.nomm
                goal  [%next [%this tire] then.than]
                fax   (peg fax 6)
              ==
            (copy then.thin what.thin what.than)
          ==
        ::
            %six
          ~|  %todo  !!
        ::
            %eve
          =^  thin  gen  $(nomm then.nomm, fax (peg fax 7))
          $(nomm once.nomm, goal thin, fax (peg fax 6))
        ::
            %ten
          ~|  %todo  !!
        ::
            %sip
          ~|  %todo  !!
        ::
            %tip
          ~|  %todo  !!
        ::
            %elf
          ~|  %todo  !!
        ==
      ::  emit indirect entry code
      ++  kerf
        |=  dire=next
        ^-  [[bile @stir] _gen]
        ~|  %todo  !!
      :: emit a basic block
      ++  emit
        |=  [thus=@tas =blob]
        ^-  [bile _gen]
        =/  bill  [%bile fax thus i.toil]
        [bill gen(will (~(put by will.gen) bill blob))]
      ++  bomb
        ^-  [next _gen]
        =^  boom  gen  (emit %boom ~ %bom ~)
        [[%next [%none ~] boom] gen]
      ::  get a new SSA register
      ++  rain
        ^-  [@stir _gen]
        [sans.gen gen(sans +(sans.gen))]
      ::  combine two needs
      ++  copy
        |=  [then=bile left=need rite=need]
        ^-  [next _gen]
        =|  pose=(list pole)
        =/  tack=(list (unit [left=need rite=need]))  [`[left rite] ~]
        =|  salt=(list need)
        |-  ^-  [next _gen]
        ?^  tack
          ?~  i.tack
            ::  we use ~ as a stack marker to zip up needs
            ?>  ?=(^ salt)
            ?>  ?=(^ t.salt)
            $(tack t.tack, salt [[%both i.t.salt i.salt] t.t.salt])
          ?:  ?=(%none -.left.u.i.tack) :: left none, return rite
            $(tack t.tack, salt [rite.u.i.tack salt])
          ?:  ?=(%none -.rite.u.i.tack) :: rite none, return left
            $(tack t.tack, salt [left.u.i.tack salt])
          ?:  ?=(%this -.left.u.i.tack) :: left is this
            ?:  ?=(%this -.rite.u.i.tack) :: rite is this: return mov
              =?  pose  ?!(=(sass.left.u.i.tack sass.rite.u.i.tack))
                [[%mov sass.left.u.i.tack sass.rite.u.i.tack] pose]
              $(tack t.tack, salt [left.u.i.tack salt])
            :: left is this, rite is both: emit cons for left
            =^  hire  gen  rain
            =^  tire  gen  rain
            %=  $
                pose  [[%con hire tire sass.left.u.i.tack] pose]
                tack
              :*  `[[%this hire] left.rite.u.i.tack]
                  `[[%this tire] rite.rite.u.i.tack]
                  ~
                  t.tack
              ==
            ==
          :: left is both, rite is this, emit cons for rite
          ?:  ?=(%this -.rite.u.i.tack)
            =^  hire  gen  rain
            =^  tire  gen  rain
            %=  $
                pose  [[%con hire tire sass.rite.u.i.tack] pose]
                tack
              :*  `[left.left.u.i.tack [%this hire]]
                  `[rite.left.u.i.tack [%this tire]]
                  ~
                  t.tack
              ==
            ==
          :: both cons
          %=  $
              tack
            :*  `[left.left.u.i.tack left.rite.u.i.tack]
                `[rite.left.u.i.tack rite.rite.u.i.tack]
                ~
                t.tack
            ==
          ==
        ?>  ?=(^ salt)
        ?>  =(~ t.salt)
        =^  bill  gen
          ?~  pose  [then gen]
          (emit %copy pose %hop then)
        [[%next i.salt bill] gen]
      ::  split a need
      ++  lyse
        |=  =next
        ^-  [[bile need need] _gen]
        ?-  -.what.next
            %none  [[then.next what.next what.next] gen]
            %both  [[then.next left.what.next rite.what.next] gen]
            %this
          =^  hire  gen  rain
          =^  tire  gen  rain
          =^  bill  gen
            (emit %lyse [%con hire tire sass.what.next]~ %hop then.next)
          [[bill [%this hire] %this tire] gen]
        ==
      ::  push a need down an axis
      ++  from
        |=  [axe=@ =need]
        ?<  =(0 axe)
        |-  ^-  ^need
        ?:  =(1 axe)  need
        ?-  (cap axe)
          %2  [%both $(axe (mas axe)) [%none ~]]
          %3  [%both $(axe (mas axe)) [%none ~]]
        ==
      ::  split an immediate to a need
      ++  mede
        |=  [bill=bile a=* =need]
        ^-  [bile _gen]
        =/  tack=(list [a=* =^need])  [a need]~
        =|  pose=(list pole)
        |-  ^-  [bile _gen]
        ?^  tack
          ?-  -.need.i.tack
              %this
            $(tack t.tack, pose [[%imm a.i.tack sass.need.i.tack] pose])
          ::
              %both
            ?@  a.i.tack
              =^  boom  gen  bomb
              [then.boom gen]
            %=  $
                tack
              :*  [-.a.i.tack left.need.i.tack]
                  [+.a.i.tack rite.need.i.tack]
                  t.tack
              ==
            ==
          ::
              %none  $(tack t.tack)
          ==
        ?~  pose  [bill gen]
        (emit %mede pose %hop bill)
      ::  split a register to a need
      ++  reed
        |=  next
        ^-  [[bile @stir] _gen]
        =/  tack=(list (unit need))  ~[`what]
        =|  salt=(list @stir)
        =|  pose=(list pole)
        |-  ^-  [[bile @stir] _gen]
        ?^  tack
          ?~  i.tack
            ?>  ?=(^ salt)
            ?>  ?=(^ t.salt)
            =^  curl  gen  rain
            %=  $
              tack  t.tack
              salt  [curl t.t.salt]
              pose  [[%hed curl i.t.salt] [%tal curl i.salt] pose]
            ==
          ?-  -.u.i.tack
              %this
            $(tack t.tack, salt [sass.u.i.tack salt])
          ::
              %both
            $(tack [`left.u.i.tack `rite.u.i.tack ~ t.tack])
          ::
              %none
            =^  dumb  gen  rain
            $(tack t.tack, salt [dumb salt])
          ==
        ?>  ?=(^ salt)
        ?>  =(~ t.salt)
        ?~  pose  [[then i.salt] gen]
        =^  beer  gen  (emit %beer (flop pose) %hop then)
        [[beer i.salt] gen]
      ::  fresh variables for a call
      ++  cane
        |=  [=need =bell nuts=(unit next)]
        ^-  [next _gen]
        =|  rear=(list @stir)
        =/  tack=(list (unit ^need))  ~[`need]
        =|  news=(list ^need)
        |-  ^-  [next _gen]
        ?^  tack
          ?~  i.tack
            ?>  ?=(^ news)
            ?>  ?=(^ t.news)
            $(tack t.tack, news [[%both i.news i.t.news] t.t.news])
          ?-  -.u.i.tack
              %none  $(tack t.tack, news [u.i.tack news])
              %this
            =^  tear  gen  rain
            $(tack t.tack, news [[%this tear] news], rear [tear rear])
          ::
              %both
            ::  RLN traversal lets us build argument list in correct order
            $(tack [`rite.u.i.tack `left.u.i.tack ~ t.tack])
          ==
        ?>  ?=(^ news)
        ?>  =(~ t.news)
        =^  kale  gen
          ?~  nuts
            (emit %cave ~ %jmp bell rear)
          =^  [bill=bile rats=@stir]  gen  (reed u.nuts)
          (emit %kale ~ %cal bell rear rats bill)
        [[%next i.news kale] gen]
      ::  fresh variables for a phi node
      ++  phil
        |=  =next
        =|  pose=(list pole)
        =|  neat=(list [need need])
        =/  tack=(list (unit need))  ~[`what.next]
        |-  ^-  [[bile need need] _gen]
        ?^  tack
          ?~  i.tack
            ?>  ?=(^ neat)
            ?>  ?=(^ t.neat)
            %=  $
                tack  t.tack
                neat
              :_  neat
              [[%both -.i.neat -.i.t.neat] [%both +.i.neat +.i.t.neat]]
            ==
          ?-  -.u.i.tack
              %none  $(tack t.tack, neat [[u.i.tack u.i.tack] neat])
              %both  $(tack [`rite.u.i.tack `left.u.i.tack ~ t.tack])
              %this
            =^  beer  gen  rain
            =^  bore  gen  rain
            %=  $
              tack  t.tack
              neat  [[[%this beer] [%this bore]] neat]
              pose  [[%phi ~[beer bore] sass.u.i.tack] pose]
            ==
          ==
        ?>  ?=(^ neat)
        ?>  =(~ t.neat)
        =^  jill  gen
          ?~  pose  [then.next gen]
          (emit %jill pose %hop then.next)
        [[jill i.neat] gen]
      ::  intersect needs prior to conditional
      ++  sect
        |=  [true=next lies=next]
        =|  toes=(list pole)
        =|  foes=(list pole)
        =|  neat=(list need)
        ~|  %todo  !!  ::  XX START HERE need to run down two trees at once
      --
    [redo this]
  --
  [hill this]
--
|%
::  hacked-in version of as-yet-non-existent set xor
++  mif-in
  |*  a=(set)
  |*  b=(set)
  ^-  [p=_a q=_b]
  [(~(dif in a) b) (~(dif in b) a)]
++  one-in
  |*  a=(set)
  ^-  [(unit) _a]
  ?~  a  ~
  [n.a (~(uni by l.a) r.a)]
--
