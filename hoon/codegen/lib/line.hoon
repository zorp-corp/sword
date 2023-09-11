/-  noir
/-  *gene
=<
|%
::  linearizer stateful core
++  lean
  =|  =hill
  |=  moan=(jar * hone:noir)
  =>
  |%
  ++  this  .
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
  ::  first pass (reverse): turn nomm into linearized code
  ++  mill
    =|  redo=(list [a=bell r=(list bile)])
    |=  toil=(list bell)
    ^-  _this
    ?^  toil
      =|  gen=[will=(map bile blob) sans=@stir redo=(list bile)]
      =/  =goal  [%done ~]  
      =/  fax  1
      |^
        =^  dire=next  gen  cuts
        =^  [wish=bile sire=@stir]  gen  (kerf dire)
        %=  ^$
            toil  t.toil
            redo  [[i.toil redo.gen] redo]
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
            =^  loch  gen  (emit 0 %loch ~ %don last)
            $(goal [%next [%this last] loch])
          ::
              %pick  punt
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
            =^  dear  gen  (emit 0 %dear ~ %don last)
            $(goal [%next [%this last] dear])
          ::
              %pick
            =^  cove  gen  rain
            =^  pint  gen  punt
            =^  cozy  gen  (emit 0 %cozy ~ %brn cove then.pint [zero once]:goal)
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
            =^  rime  gen  (emit 0 %rime [%imm moan.nomm last]~ %don last)
            [[%next [%none ~] rime] gen]
          ::
              %pick
            ?:  =(0 moan.nomm)
              [[%next [%none ~] zero.goal] gen]
            ?:  =(1 moan.nomm)
              [[%next [%none ~] once.goal] gen]
            punt
          ::
              %next
            =^  bill  gen  (mede then.goal moan.nomm what.goal)
            [[%next [%none ~] bill] gen]
          ==
        ::
            %two
          ?:  ?=(%pick -.goal)
            =^  pyre  gen  rain
            =^  pint  gen  punt
            =^  pika  gen
              (emit 0 %pika ~ %brn pyre then.pint [zero once]:goal)
            $(goal [%next [%this pyre] pika])
          =^  [duke=bile sone=need fore=need]  gen
            =/  bull  (~(get by ices) rail.nomm)
            ?~  bull :: indirect
              =^  sofa  gen  rain
              =^  fora  gen  rain
              ?:  ?=(%done -.goal)
                =^  dune  gen  (emit 0 %dune ~ %lnt sofa fora)
                [[dune [%this sofa] [%this fora]] gen]
              =^  [fine=bile rare=@stir]  gen  (kerf goal)
              =^  dunk  gen  (emit 0 %dunk ~ %lnk sofa fora rare fine)
              [[dunk [%this sofa] [%this fora]] gen]
            =/  hull  (~(get by hill) u.bull)
            ?~  hull :: direct but no registerization
              =^  soap  gen  rain
              =^  sofa  gen  rain
              =^  hoof  gen  rain
              ?:  ?=(%done -.goal)
                =^  dude  gen  (emit 0 %dude ~ %jmp u.bull soap ~[sofa])
                =^  dawn  gen  (emit 0 %dawn [%con sofa hoof soap]~ %hop dude)
                =.  redo.gen  [dude redo.gen]
                [[dawn [%this sofa] [%this hoof]] gen]
              =^  [fine=bile rare=@stir]  gen  (kerf goal)
              =^  deed  gen  (emit 0 %deed ~ %cal u.bull soap ~[sofa] rare fine)
              =^  deem  gen  (emit 0 %deem [%con sofa hoof soap]~ %hop deed)
              =.  redo.gen  [deed redo.gen]
              [[deem [%this sofa] [%this hoof]] gen]
            ::  direct with registerization
            ?:  ?=(%done -.goal)
              =^  [kilt=next soap=@stir]  gen  (cane want.u.hull u.bull ~)
              =^  hose  gen  rain
              =^  hoof  gen  rain
              =^  down  gen
                (emit 0 %down [%con hose hoof soap]~ %hop then.kilt)
              =^  cops  gen  (copy down what.kilt [%this hose])
              [[then.cops what.cops [%this hoof]] gen]
            =^  [kilt=next soap=@stir]  gen  (cane want.u.hull u.bull `goal)
            =^  hose  gen  rain
            =^  hoof  gen  rain
            =^  doom  gen
              (emit 0 %doom [%con hose hoof soap]~ %hop then.kilt)
            =^  cops  gen  (copy doom what.kilt [%this hose])
            [[then.cops what.cops [%this hoof]] gen]
          =^  foes  gen  $(nomm corn.nomm, fax (peg fax 14), goal [%next fore duke])
          =^  suet  gen  $(nomm cost.nomm, fax (peg fax 6), goal [%next sone then.foes])
          (copy then.suet what.suet what.foes)
        ::
            %the
          ?-  -.goal
              %done
            =^  reef  gen  rain
            =^  tear  gen  (emit 0 %tear [%imm 0 reef]~ %don reef)
            =^  fear  gen  (emit 0 %fear [%imm 1 reef]~ %don reef)
            $(goal [%pick tear fear])
          ::
              %next
            ?:  ?=(%both -.what.goal)  bomb
            ?:  ?=(%none -.what.goal)
              $(goal [%pick then.goal then.goal])
            =^  tare  gen  rain
            =^  fare  gen  rain
            =^  thin  gen
              (emit 0 %thin [%phi ~[tare fare] sass.what.goal]~ %hop then.goal)
            =^  tear  gen  (emit 0 %tear [%imm 0 tare]~ %hop thin)
            =^  fear  gen  (emit 0 %fear [%imm 1 fare]~ %hop thin)
            $(goal [%pick tear fear])
          ::
              %pick
            =^  coat  gen  rain
            =^  pith  gen  (emit 0 %pith ~ %clq coat [zero once]:goal)
            $(nomm pell.nomm, goal [%next [%this coat] pith], fax (peg fax 3))
          ==
        ::
            %for
          ?-  -.goal
              %done
            =^  rink  gen  rain
            =^  pink  gen  rain
            =^  tike  gen  (emit 0 %tike [%inc pink rink]~ %don rink)
            =^  boom  gen  bomb
            =^  sike  gen  (emit 0 %sike ~ %clq pink then.boom tike) 
            $(nomm mall.nomm, goal [%next [%this pink] sike], fax (peg fax 3))
          ::
              %pick
            =^  rink  gen  rain
            =^  pink  gen  rain
            =^  pint  gen  punt
            =^  pike  gen
              (emit 0 %pike [%inc pink rink]~ %brn rink then.pint [zero once]:goal)
            =^  boom  gen  bomb
            =^  like  gen  (emit 0 %like ~ %clq pink then.boom pike)
            $(nomm mall.nomm, goal [%next [%this pink] like], fax (peg fax 3))
          ::
              %next
            ?:  ?=(%both -.what.goal)  punt
            =^  rink  gen
              ?:  ?=(%none -.what.goal)
                rain
              [sass.what.goal gen]
            =^  pink  gen  rain
            =^  bike  gen
              (emit 0 %bike [%inc pink rink]~ %hop then.goal)
            =^  boom  gen  bomb
            =^  mike  gen
              (emit 0 %mike ~ [%clq pink then.boom bike])
            $(nomm mall.nomm, goal [%next [%this pink] bike], fax (peg fax 3))
          ==
        ::
            %ivy
          ?-  -.goal
              %done
            =^  qual  gen  rain
            =^  reek  gen  (emit 0 %reek [%imm 0 qual]~ %don qual)
            =^  riff  gen  (emit 0 %riff [%imm 1 qual]~ %don qual)
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
              (emit 0 %ward [%phi ~[tare fare] sass.what.goal]~ %hop then.goal)
            =^  ware  gen  (emit 0 %ware [%imm 0 tare]~ %hop ward)
            =^  mare  gen  (emit 0 %mare [%imm 1 fare]~ %hop ward)
            $(goal [%pick ware mare])
          ::
              %pick
            =^  tire  gen  rain
            =^  tear  gen  rain
            =^  pare  gen  (emit 0 %pare ~ %eqq tire tear [zero once]:goal)
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
          ?:  ?=(%next -.goal)
            =^  [join=bile teal=need feel=need]  gen  (phil goal)
            =^  fest  gen
              $(nomm else.nomm, fax (peg fax 15), goal [%next feel join])
            =^  zest  gen
              $(nomm then.nomm, fax (peg fax 14), goal [%next teal join])
            =^  [bead=need tile=bile file=bile]  gen  (sect zest fest)
            =^  cond  gen
              $(nomm what.nomm, fax (peg fax 6), goal [%pick tile file])
            (copy then.cond what.cond bead)
          =^  fest  gen
            $(nomm else.nomm, fax (peg fax 15))
          =^  zest  gen
            $(nomm then.nomm, fax (peg fax 14))
          =^  [bead=need tile=bile file=bile]  gen  (sect zest fest)
          =^  cond  gen
            $(nomm what.nomm, fax (peg fax 6), goal [%pick tile file])
          (copy then.cond what.cond bead)
        ::
            %eve
          =^  thin  gen  $(nomm then.nomm, fax (peg fax 7))
          $(nomm once.nomm, goal thin, fax (peg fax 6))
        ::
            %ten
          ?-  -.goal
              %done
            =^  last  gen  rain
            =^  dead  gen  (emit 0 %dead ~ %don last)
            $(goal [%next [%this last] dead])
          ::
              %pick
            ?.  =(here.nomm 1)  punt
            =^  flip  gen  rain
            =^  pint  gen  punt
            =^  deep  gen  (emit 0 %deep ~ %brn flip then.pint [zero once]:goal)
            $(goal [%next [%this flip] deep])
          ::
              %next
            =^  [twig=need tree=need then=bile]  gen  (into here.nomm goal)
            =^  nest  gen
              $(nomm tree.nomm, fax (peg fax 15), goal [%next tree then])
            =^  eggs  gen
              $(nomm tree.nomm, fax (peg fax 14), goal [%next twig then.nest])
            (copy then.eggs what.eggs what.nest)
          ==
        ::
            %sip
          ?:  ?=(%bout hint.nomm)
            ?-  -.goal
                %done
              =^  last  gen  rain
              =^  dime  gen  (emit 0 %dime ~ %don last)
              $(goal [%next [%this last] dime])
            ::
                %pick
              =^  tome  gen  (emit 0 %tome [%tom ~]~ %hop zero.goal)
              =^  foam  gen  (emit 0 %foam [%tom ~]~ %hop once.goal)
              =^  race  gen
                $(nomm then.nomm, fax (peg fax 7), goal [%pick tome foam])
              =^  tick  gen  (emit 0 %tick [%tim ~]~ %hop then.race)
              [race(then tick) gen]
            ::
                %next
              =^  stop  gen  (emit 0 %stop [%tom ~]~ %hop then.goal)
              =^  race  gen
                $(nomm then.nomm, fax (peg fax 7), then.goal stop)
              =^  goes  gen  (emit 0 %goes [%tim ~]~ %hop then.race)
              [race(then goes) gen]
            ==
          ?:  ?=(%meme hint.nomm)
            =^  raft  gen  $(nomm then.nomm, fax (peg fax 7))
            =^  meme  gen  (emit 0 %meme [%mem ~]~ %hop then.raft)
            [raft(then meme) gen]
          $(nomm then.nomm, fax (peg fax 7))
        ::
            %tip
          ?:  ?=(?(%hunk %hand %lose %mean %spot) hint.nomm)
            =^  mane  gen  rain
            =^  teak  gen  rain
            =^  seek  gen  rain
            ?-  -.goal
                %done
              =^  real  gen  $(nomm then.nomm, fax (peg fax 15))
              =^  dint  gen
                %:  emit
                    0  %dint
                    :~  [%imm hint.nomm teak]
                        [%con teak mane seek]
                        [%men seek]
                    ==
                    %hop  then.real
                ==
              =^  fake  gen
                %=  $
                  nomm  vice.nomm
                  fax  (peg fax 14)
                  goal  [%next [%this mane] dint]
                ==
              (copy then.fake what.fake what.real)
            ::
                %pick
              =^  tame  gen  (emit 0 %tame [%man ~]~ %hop zero.goal)
              =^  fame  gen  (emit 0 %fame [%man ~]~ %hop once.goal)
              =^  real  gen
                $(nomm then.nomm, fax (peg fax 15), goal [%pick tame fame])
              =^  maps  gen
                %:  emit
                    0  %maps
                    :~  [%imm hint.nomm teak]
                        [%con teak mane seek]
                        [%men seek]
                    ==
                    %hop  then.real
                ==
              =^  fake  gen
                %=  $
                  nomm  vice.nomm
                  fax  (peg fax 14)
                  goal  [%next [%this mane] maps]
                ==
              (copy then.fake what.fake what.real)
            ::
                %next
              =^  pops  gen  (emit 0 %pops [%man ~]~ %hop then.goal)
              =^  real  gen
                $(nomm then.nomm, fax (peg fax 15), then.goal pops)
              =^  naps  gen
                %:  emit
                  0  %naps
                  :~  [%imm hint.nomm teak]
                      [%con teak mane seek]
                      [%men seek]
                  ==
                  %hop  then.real
                ==
              =^  fake  gen
                %=  $
                  nomm  vice.nomm
                  fax  (peg fax 14)
                  goal  [%next [%this mane] naps]
                ==
              (copy then.fake what.fake what.real)
            ==
          ?:  ?=(?(%live %slog) hint.nomm)
            =^  clue  gen  rain
            =^  real  gen  $(nomm then.nomm, fax (peg fax 15))
            =^  wave  gen
              ?:  ?=(%live hint.nomm)
                (emit 0 %wave [%hit clue]~ %hop then.real)
              (emit 0 %save [%slg clue]~ %hop then.real)
            =^  fake  gen
              $(nomm vice.nomm, fax (peg fax 14), goal real(then wave))
            (copy then.fake what.fake what.real)
          ::  XX we need to save the formula in %tip for this
          ~?  ?=(%memo hint.nomm)  '%memo codegen is TODO'
          =^  real  gen  $(nomm then.nomm, fax (peg fax 15))
          =^  fake  gen
            $(nomm vice.nomm, fax (peg fax 14), goal [%next [%none ~] then.real])
          (copy then.fake what.fake what.real)
        ::
            %elf
          ?-  -.goal
              %done
            =^  last  gen  rain
            =^  deft  gen  (emit 0 %deft ~ %don last)
            $(goal [%next [%this last] deft])
          ::
              %pick
            =^  pint  gen  punt
            =^  flip  gen  rain
            =^  heft  gen  (emit 0 %heft ~ %brn flip then.pint [zero once]:goal)
            $(goal [%next [%this flip] heft])
          ::
              %next
            =^  [weft=bile good=@stir]  gen  (kerf goal)
            =^  home  gen  rain
            =^  path  gen  rain
            =^  show  gen  (emit 0 %show ~ %spy home path good weft)
            =^  trot  gen
              $(nomm walk.nomm, fax (peg fax 6), goal [%next [%this path] show])
            =^  paid  gen
              %=  $
                nomm  rent.nomm
                fax  (peg fax 7)
                goal  [%next [%this home] then.trot]
              ==
            (copy then.paid what.paid what.trot)
          ==
        ==
      :: emit a basic block
      ++  emit
        |=  [tis=@ thus=@tas =blob]
        ^-  [bile _gen]
        =/  bill  [%bile fax tis thus i.toil]
        [bill gen(will (~(put by will.gen) bill blob))]
      ++  bomb
        ^-  [next _gen]
        =^  boom  gen  (emit 0 %boom ~ %bom ~)
        [[%next [%none ~] boom] gen]
      ++  punt
        ^-  [next _gen]
        =^  drop  gen  (emit 0 %drop ~ %pun ~)
        [[%next [%none ~] drop] gen]
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
          (emit 0 %copy pose %hop then)
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
            (emit 0 %lyse [%con hire tire sass.what.next]~ %hop then.next)
          [[bill [%this hire] %this tire] gen]
        ==
      ::  push a need down an axis
      ++  from
        |=  [axe=@ =need]
        ?<  =(0 axe)
        |-  ^-  _need
        ?:  =(1 axe)  need
        ?-  (cap axe)
          %2  [%both $(axe (mas axe)) [%none ~]]
          %3  [%both $(axe (mas axe)) [%none ~]]
        ==
      ::  split a need along an axis
      ++  into
        |=  [axe=@ feed=next]
        ?<  =(0 axe)
        =|  bead=(list [c=?(%2 %3) n=need])
        =|  pose=(list pole)
        |-  ^-  [[need need bile] _gen]
        ?.  =(1 axe)
          ?-  -.what.feed
              %none
            $(bead [[(cap axe) %none ~] bead], axe (mas axe))
          ::
              %both
            ?-  (cap axe)
                %2
              %=  $
                bead  [[%2 rite.what.feed] bead]
                what.feed  left.what.feed
                axe  (mas axe)
              ==
            ::
                %3
              %=  $
                bead  [[%3 left.what.feed] bead]
                what.feed  rite.what.feed
                axe  (mas axe)
              ==
            ==
          ::
              %this
            =^  l  gen  rain
            =^  r  gen  rain
            =.  pose  [[%con l r sass.what.feed] pose]
            ?-  (cap axe)
                %2
              $(bead [[%2 [%this r]] bead], what.feed [%this l], axe (mas axe))
            ::
                %3
              $(bead [[%3 [%this l]] bead], what.feed [%this r], axe (mas axe))
            ==
          ==
        =/  lead=need  [%none ~]
        |-  ^-  [[need need bile] _gen]
        ?^  bead
          ?-  c.i.bead
            %2  $(bead t.bead, lead [%both lead n.i.bead])
            %3  $(bead t.bead, lead [%both n.i.bead lead])
          ==
        =^  gill  gen
          ?~  pose  [then.feed gen]
          (emit 0 %gill pose %hop then.feed)
        [[what.feed lead gill] gen]
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
              =^  pint  gen  punt
              [then.pint gen]
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
        (emit 0 %mede pose %hop bill)
      ::  split a register to a need
      ++  kerf
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
        ?:  =(~ pose)  [[then i.salt] gen]
        =^  pint  gen  punt
        =|  bail=@
        =/  benz=site  [%hop then]
        =|  bock=(list pole)
        |-  ^-  [[bile @stir] _gen]
        ?^  pose
          ?:  ?=(%hed -.i.pose)
            =^  beer  gen  (emit bail %beer [i.pose bock] benz)
            %=  $
              bail  .+(bail)
              pose  t.pose
              bock  ~
              benz  [%clq +<.i.pose beer then.pint]
            ==
          ?:  ?=(%tal -.i.pose)
            =^  beer  gen  (emit bail %beer [i.pose bock] benz)
            %=  $
              bail  .+(bail)
              pose  t.pose
              bock  ~
              benz  [%clq +<.i.pose beer then.pint]
            ==
          $(bock [i.pose bock], pose t.pose)
        =^  wine  gen  (emit bail %wine bock benz)
        [[wine i.salt] gen]
      ::  fresh variables for a call
      ++  cane
        |=  [=need =bell nuts=(unit next)]
        ^-  [[next @stir] _gen]
        =|  rear=(list @stir)
        =/  tack=(list (unit ^need))  ~[`need]
        =|  news=(list ^need)
        |-  ^-  [[next @stir] _gen]
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
        =^  sofa  gen  rain
        =^  kale  gen
          ?~  nuts
            (emit 0 %cave ~ %jmp bell sofa rear)
          =^  [bill=bile rats=@stir]  gen  (kerf u.nuts)
          (emit 0 %kale ~ %cal bell sofa rear rats bill)
        [[[%next i.news kale] sofa] gen]
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
          (emit 0 %jill pose %hop then.next)
        [[jill i.neat] gen]
      ::  intersect needs prior to conditional
      ++  sect
        |=  [true=next lies=next]
        =|  toes=(list pole)
        =|  foes=(list pole)
        =/  note=(list (unit [t=need f=need]))  ~[`[what.true what.lies]]
        =|  neat=(list need)
        |-  ^-  [[need bile bile] _gen]  :: intersect needs, when only one is %both, build split code for it
        ?^  note
          ?^  i.note
            ?:  ?=(%both -.t.u.i.note)
              ?:  ?=(%both -.f.u.i.note)
                %=  $
                    note
                  :*  `[left.t left.f]:u.i.note
                      `[rite.t rite.f]:u.i.note
                      ~
                      t.note
                  ==
                ==
              =^  tare  gen
                ?:  ?=(%this -.f.u.i.note)
                  [sass.f.u.i.note gen]
                rain
              =/  spit=(list [n=need r=@stir])  [t.u.i.note tare]~
              |-  ^-  [[need bile bile] _gen]
              ?^  spit
                ?-  -.n.i.spit
                    %both
                  =^  l  gen  rain
                  =^  r  gen  rain
                  %=  $
                    toes  [[%hed r.i.spit l] [%tal r.i.spit r] toes]
                    spit  [[left.n.i.spit l] [rite.n.i.spit r] t.spit]
                  ==
                ::
                    %none
                  $(spit t.spit)
                ::
                    %this
                  %=  $
                    toes  [[%mov r.i.spit sass.n.i.spit] toes]
                    spit  t.spit
                  ==
                ==
              ^$(neat [[%this tare] neat], note t.note)
            ?:  ?=(%both -.f.u.i.note)
              =^  tare  gen
                ?:  ?=(%this -.t.u.i.note)
                  [sass.t.u.i.note gen]
                rain
              =/  spit=(list [n=need r=@stir])  [f.u.i.note tare]~
              |-  ^-  [[need bile bile] _gen]
              ?^  spit
                ?-  -.n.i.spit
                    %both
                  =^  l  gen  rain
                  =^  r  gen  rain
                  %=  $
                    foes  [[%hed r.i.spit l] [%tal r.i.spit r] foes]
                    spit  [[left.n.i.spit l] [rite.n.i.spit r] t.spit]
                  ==
                ::
                    %none
                  $(spit t.spit)
                ::
                    %this
                  %=  $
                    foes  [[%mov r.i.spit sass.n.i.spit] foes]
                    spit  t.spit
                  ==
                ==
              ^$(neat [[%this tare] neat], note t.note)
            ?:  ?=(%none -.t.u.i.note)  $(note t.note, neat [f.u.i.note neat])
            ?:  ?=(%none -.f.u.i.note)  $(note t.note, neat [t.u.i.note neat])
            %=  $
              foes  [[%mov [sass.t sass.f]:u.i.note] foes]
              note  t.note
              neat  [t.u.i.note neat]
            ==
          ?>  ?=(^ neat)
          ?>  ?=(^ t.neat)
          $(note t.note, neat [[%both i.t.neat i.neat] t.t.neat])
        ?>  ?=(^ neat)
        ?>  =(~ t.neat)
        =|  bale=@
        =|  bock=(list pole)
        =/  benz=site  [%hop then.true]
        =^  pint  gen  punt
        |-  ^-  [[need bile bile] _gen]  :: add clq operations before every hed or tal in toes
        ?^  toes  
          ?:  ?=(%hed -.i.toes)
            =^  tile  gen  (emit bale %tile [i.toes bock] benz)
            %=  $
              benz  [%clq +<.i.toes tile then.pint]
              bock  ~
              toes  t.toes
              bale  .+(bale)
            ==
          ?:  ?=(%tal -.i.toes)
            =^  tile  gen  (emit bale %tile [i.toes bock] benz)
            %=  $
              benz  [%clq +<.i.toes tile then.pint]
              bock  ~
              toes  t.toes
              bale  .+(bale)
            ==
          $(bock [i.toes bock], toes t.toes)
        =^  sits  gen  (emit bale %sits bock benz)
        =.  bale  0
        =.  bock  ~
        =.  benz  [%hop then.lies]
        |-  ^-  [[need bile bile] _gen]  :: add clq operations before every hed or tal in foes
        ?^  foes
          ?:  ?=(%hed -.i.foes)
            =^  file  gen  (emit bale %file [i.foes bock] benz)
            %=  $
              benz  [%clq +<.i.foes file then.pint]
              bock  ~
              foes  t.foes
              bale  .+(bale)
            ==
          ?:  ?=(%tal -.i.foes)
            =^  file  gen  (emit bale %file [i.foes bock] benz)
            %=  $
              benz  [%clq +<.i.foes file then.pint]
              bock  ~
              foes  t.foes
              bale  .+(bale)
            ==
          $(bock [i.foes bock], foes t.foes)
        =^  sift  gen  (emit bale %sift bock benz)
        [[i.neat sits sift] gen]
      --
    ::  XX redos
    ::    - generate new registers for call site
    ::    - split out registers generating appropriate clq branches
    ::    - make end of original block point to first register split
    ::      block
    |-  ^-  _this
    ?^  redo
      =/  pine  (~(got by hill) a.i.redo)
      |-
      ?^  r.i.redo
        =/  bock  (~(got by will.pine) i.r.i.redo)
        =*  fax  axe.i.r.i.redo
        ?:  ?=(%cal -.bend.bock)
          =*  coil  +<.bend.bock :: label to call
          =/  sill  +>+<.bend.bock :: so that we don't overconstrain the type
          ?>  ?=(^ sill)
          ?>  =(~ t.sill)
          =/  tack=(list [r=@stir n=need])  [i.sill want:(~(got by hill) coil)]~
          =|  args=(list @stir)
          =|  pose=(list pole)
          |-  ^-  _this  :: 
          ?^  tack 
            ?-  -.n.i.tack
                %none  $(tack t.tack)
                %this
              %=  $
                args  [r.i.tack args]
                tack  t.tack
              ==
            ::
                %both
              =^  l  sans.pine  [. .+(.)]:sans.pine
              =^  r  sans.pine  [. .+(.)]:sans.pine
              %=  $
                pose  [[%tal r.i.tack r] [%hed r.i.tack l] pose]
                tack  [[r rite.n.i.tack] [l left.n.i.tack] tack]
              ==
            ==
          =/  runt  [%bile fax 0 %runt a.i.redo]
          =.  will.pine
            (~(put by will.pine) runt [~ %pun ~])
          =|  bail=@
          =/  then=site  bend.bock(+>+< args)
          =|  body=(list pole)
          |-  ^-  _this
          ?^  pose
            ?:  ?=(%hed -.i.pose)
              =/  aged=bile  [%bile fax bail %ages a.i.redo]
              =.  will.pine
                (~(put by will.pine) aged [[i.pose body] then])
              %=  $
                pose  t.pose
                body  ~
                then  [%clq +<.i.pose aged runt]
              ==
            ?:  ?=(%tal -.i.pose)
              =/  aged=bile  [%bile fax bail %ages a.i.redo]
              =.  will.pine
                (~(put by will.pine) aged [[i.pose body] then])
              %=  $
                pose  t.pose
                body  ~
                then  [%clq +<.i.pose aged runt]
              ==
            $(body [i.pose body], pose t.pose)
          =/  aged=bile  [%bile fax bail %ages a.i.redo]
          =.  will.pine
            (~(put by will.pine) aged [body then])
          =.  will.pine
            (~(put by will.pine) i.r.i.redo [body.bock %hop aged])
          %=  ^^$
            r.i.redo  t.r.i.redo
          ==
        ?:  ?=(%jmp -.bend.bock)
          =*  coil  +<.bend.bock :: label to call
          =/  sill  +>+.bend.bock :: so that we don't overconstrain the type
          ?>  ?=(^ sill)
          ?>  =(~ t.sill)
          =/  tack=(list [r=@stir n=need])  [i.sill want:(~(got by hill) coil)]~
          =|  args=(list @stir)
          =|  pose=(list pole)
          |-  ^-  _this  :: 
          ?^  tack 
            ?-  -.n.i.tack
                %none  $(tack t.tack)
                %this
              %=  $
                args  [r.i.tack args]
                tack  t.tack
              ==
            ::
                %both
              =^  l  sans.pine  [. .+(.)]:sans.pine
              =^  r  sans.pine  [. .+(.)]:sans.pine
              %=  $
                pose  [[%tal r.i.tack r] [%hed r.i.tack l] pose]
                tack  [[r rite.n.i.tack] [l left.n.i.tack] tack]
              ==
            ==
          =/  runt  [%bile fax 0 %runt a.i.redo]
          =.  will.pine
            (~(put by will.pine) runt [~ %pun ~])
          =|  bail=@
          =/  then=site  bend.bock(+>+ args)
          =|  body=(list pole)
          |-  ^-  _this
          ?^  pose
            ?:  ?=(%hed -.i.pose)
              =/  aged=bile  [%bile fax bail %ages a.i.redo]
              =.  will.pine
                (~(put by will.pine) aged [[i.pose body] then])
              %=  $
                pose  t.pose
                body  ~
                then  [%clq +<.i.pose aged runt]
              ==
            ?:  ?=(%tal -.i.pose)
              =/  aged=bile  [%bile fax bail %ages a.i.redo]
              =.  will.pine
                (~(put by will.pine) aged [[i.pose body] then])
              %=  $
                pose  t.pose
                body  ~
                then  [%clq +<.i.pose aged runt]
              ==
            $(body [i.pose body], pose t.pose)
          =/  aged=bile  [%bile fax bail %ages a.i.redo]
          =.  will.pine
            (~(put by will.pine) aged [body then])
          =.  will.pine
            (~(put by will.pine) i.r.i.redo [body.bock %hop aged])
          %=  ^^$
            r.i.redo  t.r.i.redo
          ==
        ~|  '%redo-foul'  !!
      %=  ^$
        redo  t.redo
        hill  (~(put by hill) a.i.redo pine)
      ==
    this
  ::  second pass (forward):
  ::    - generate come-from graph
  ::    - track movs
  ::    - track cons/hed/tal
  ::    - track known cell/atom
  ::    - eliminate/rewrite mov
  ::    - rewrite hed/tal when register for (hed r/tal r) already exists
  ::    - eliminate clq on known cons/atom (rewrite to hop)
  ::    - eliminate unitary phi
  ::  third pass (backward):
  ::    -  track register uses
  ::    -  eliminate unused hed/tal/cons/inc/imm
  ::    -  unify blocks across single come-from-hop
  ::  these passes are done sequentially on each arm
  ::  to allow easier handoff of the come-from graph
  ::  there are no inter-arm dependencies in these passes
  ++  sand
    |=  toil=(list bell)
    ^-  [_hill _this]
    ?^  toil
      ~&  'todo: sand forward pass'
      ~&  'todo: sand backward pass'
      $(toil t.toil)
    [hill this]
  --
  ^-  [_hill _this]
  =/  toil  work
  =>  [toil (mill toil)]
  =>  (sand -)
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
