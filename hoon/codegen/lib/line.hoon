/-  *noir
/-  *gene
|_  =hill
::  linearize
::  discover un-linearized bells in topological order
++  pins
  |=  moan=(jar * hone)
  ^-  (list [=bell =food])
  =/  moat  ~(tap by moan)
  =|  topo  (map bell food)
  =|  hall  (map bell @hail)
  =|  tort  (set bell)
  |-  ^-  (list [=bell =food])
  ?^  moat
    |-  ^-  (list [=bell =food])
    ?^  q.i.moat
      =.  topo  (~(put by topo) [soot p.i.moat] [norm.i.q.i.moat])
      =/  melt  ~(tap by ices.norm.i.q.i.moat)
      |-  ^-  (list [=bell =food])
      ?^  melt
        =?  tort  ?!((~(has by topo) q.i.melt))  (~(put in tort) q.i.melt)
        $(melt t.melt)
      ^$(q.i.moat t.q.i.moat)
    ^$(moat t.moat)
  =/  salt
    %+  turn  ~(tap in (~(dif in ~(keys by topo)) tort))
    |=  bill=bell
    ^-  [=bell =food]
    [bill (~(got by topo) bill)]
  =/  tack  salt
  =|  done  (set bell)
  |-  ^-  (list =bell =food)
  ?^  tack
    =/  news  ~(val by ices.food.i.tack)
    =/  newb
      %+  murn  news
      |=  bill=bell
      ^-  (unit [=bell =food])
      ?:  (~(has in done) bill)  ~
      `[bill (~(got by topo) bill)]
    %=  $
      salt (weld newb salt)
      tack (weld newb t.tack)
      done (~(put in done) i.tack))
    ==
  (flop salt)
::  linearize a single formula
++  line
  |=  [=bell food]
  =|  seam  [code=(map bile blob) stir=@stir late=(set bile)]
  =/  goal=[%done ~]
  =/  fax  1
  =<
  |-  ^-  [next _seam]
  ?-  -.nomm
      %par
    ?-  -.goal
        %pick
      bomb
    ::
        %done
      =^  last  seam  whip
      =^  done  seam  (emit %care ~ %don last)
      $(goal [%next [%this last] done])
    ::
        %next
      =^  [leed=need reed=need pure=bile]  seam  (pare goal)
      =^  rite  seam  $(nomm rite.nomm, goal [%next reed pure], fax (peg fax 2))
      =^  left  seam
        $(nomm left.nomm, goal [%next leed then.rite], fax (peg fax 3))
      (copy what.left what.rite then.left)
    ==
  ::
      %not
    ?:  =(0 here.nomm)  bomb
    ?-  -.goal
        %pick
      =^  bean  seam  whip
      =^  cash  seam  (emit %cash ~ %brn bean [zero once]:goal)
      $(goal [%next [%this bean] cash])
    ::
        %done
      =^  last  seam  whip
      =^  doze  seam  (emit %doze ~ %don last)
      $(goal [%next [%this last] doze]
    ::
        %next
      [goal(what (grab here.nomm what.goal)) seam]
    ==
  ::
      %one
    ?-  -.goal
        %pick
      =^  bean  seam  whip
      =^  ways  seam  (emit %ways ~ %brn bean [zero once]:goal)
      $(goal [%next [%this bean] ways)
    ::
        %done
      =^  last  seam  whip
      =^  dune  seam  (emit %dune ~ %don last)
      $(goal [%next [%this last] dune)
    ::
        %next
      (divy moan.nomm goal)
    ==
  ::
      %two
    ?:  ?=(-.goal %pick)
      =^  bean  seam  whip
      =^  carb  seam  (emit %carb ~ %brn bean [zero once]:goal)
      $(goal [%next [%this bean] carb)
    =/  bulk  (~(get by ices) rail.nomm)
    ?~  bulk  :: indirect call
      ?-  -.goal
          %done
        =^  sure  seam  whip
        =^  fore  seam  whip
        =^  tiny  seam  (emit %tiny ~ %lnt sure fore)
        =^  golf  seam
          $(nomm corn.nomm, goal [%next [%this fore] tiny], fax (peg fax 12))
        =^  goes  seam
          %=  $
            nomm  cost.nomm
            goal  [%next [%this sure] then.golf]
            fax   (peg fax 13))
          ==
        (copy what.goes what.golf then.goes)
      ::
          %next
        =^  [salt=@stir thin=bile]  seam  (bite goal)
        =^  sure  seam  whip
        =^  fore  seam  whip
        =^  indy  seam  (emit %indy ~ lnk sure fore salt thin)
        =^  golf  seam
          $(nomm corn.nomm, goal [%next [%this fore] tiny], fax (peg fax 12))
        =^  goes  seam
          %=  $
            nomm cost.nomm
            goal [%next [%this sure] then.golf]
            fax (peg fax 13))
          ==
        (copy what.goes what.golf then.goes)
      ==
    =/  pull  (~(get by hill) u.bulk)
    =^  [seed=need sirs=(list @stir)]  seam
      ?~  pull
        =^  sure  seam  whip
        [[[%this sure] ~[sure]] seam]
      (noon want.u.pull)
    ?-  -.goal
        %done
      =^  leap  seam  (emit %leap ~ %jmp u.bulk sirs)
      =?  late.seam  =(~ pull)  (~(put in late.seam) leap)
      =^  golf  seam
        $(nomm corn.nomm, goal [%next [%none ~] leap], fax (peg fax 12))
      =^  goes  seam
        $(nomm cost.nomm, goal [%next seed then.golf], fax (peg fax 13))
      (copy what.goes what.golf then.goes)
    ::
        %next
      =^  thin  seam  (bite goal)
      =^  dive  seam  (emit %dive ~ %cal u.bulk sirs thin)
      =?  late.seam  =(~ pull)  (~(put in late.seam) dive)
      =^  golf  seam
        $(nomm corn.nomm, goal [%next [%none ~] dive], fax (peg fax 12))
      =^  goes  seam
        $(nomm cost.nomm, goal [%next seed then.golf], fax (peg fax 13))
      (copy what.goes what.golf then.goes)
    ==
  ::
      %the
    ?-  -.goal
        %done
      =^  last  seam  whip
      =^  zeal  seam  (emit %zeal [%imm 0 last] %don last)
      =^  tomb  seam  (emit %tomb [%imm 1 last] %don last)
      $(goal [%pick zeal tomb])
    ::
        %next
      ?-  -.what.goal
          %both  bomb
          %none  $(goal [%pick then.goal then.goal])
          %this
        =^  zest  seam  (emit %zest [%imm 0 sass.what.goal] %hop then.goal)
        =^  tone  seam  (emit %tone [%imm 1 sass.what.goal] %hop then.goal)
        $(goal [%pick zest tone])
      ==
    ::
        %pick
      =^  data  seam  whip
      =^  itch  seam  (emit %itch ~ %clq data [zero once]:goal)
      $(nomm pell.nomm, fax (peg fax 3), goal [%next [%this data] itch])
    ==
  ::
      %for
    ?-  -.goal
        %done
      =^  last  seam  whip
      =^  ford  seam  (emit %ford ~ %don last)
      $(goal [%next [%this last] ford])
    ::
        %pick
      =^  bean  seam  whip
      =^  forb  seam  (emit %forb ~ %brn bean [zero once]:goal)
      $(goal [%next [%this bean] forb])
    ::
        %next
      ?-  -.what.goal
          %both  bomb
          %none
        =^  crap  seam  whip
        $(what.goal [%this crap])
      ::
          %this
        =^  tome  seam  whip
        =^  rise  seam  (emit %rise [%inc tome sass.what.goal] %hop then.goal)
        $(nomm mall.nomm, fax (peg fax 3), goal [%next [%this tome] rise])
      ==
    ==
  ::
      %ivy
    ?-  -.goal
        %done
      =^  last  seam  whip
      =^  zeke  seam  (emit %zeke [%imm 0 last] %don last)
      =^  teak  seam  (emit %teke [%imm 1 last] %don last)
      $(goal [%pick zeke teak])
    ::
        %next
      ?-  -.what.goal
          %both  bomb
          %none  $(goal [%pick then.goal then.goal])
          %this
        =^  neck  seam  (emit %neck [%imm 0 sass.what.goal] %hop then.goal)
        =^  cone  seam  (emit %cone [%imm 1 sass.what.goal] %hop then.goal)
        $(goal [%pick neck cone])
      ==
    ::
        %pick
      =^  roan  seam  whip
      =^  rule  seam  whip
      =^  same  seam  (emit %same ~ %eqq roan rule [zero once]:goal)
      =^  goat  seam
        $(nomm that.nomm, fax (peg fax 7), goal [%next [%this rule] same])
      =^  gout  seam
        $(nomm this.nomm, fax (peg fax 6), goal [%next [%this roan] then.goal])
      (copy what.gout what.goat then.gout)
    ==
  ::
      %six
    ?.  ?=(-.goal %next)
      =^  fool  seam  $(nomm else.nomm, fax (peg fax 15))
      =^  tool  seam  $(nomm then.nomm, fax (peg fax 14))
      =^  [seed=need tile=bile file=bile]  seam  (sect tool fool)
      =^  cool  seam  $(nomm what.nomm, fax (peg fax 6), goal [%pick tile file])
      (copy what.cool seed then.cool)
    =^  [toll=next foal=next]  seam  (hype goal)
    =^  fool  seam  $(nomm else.nomm, fax (peg fax 15), goal foal)
    =^  tool  seam  $(nomm then.nomm, fax (peg fax 14), goal toll)
    =^  [seed=need tile=bile file=bile]  seam  (sect tool fool)
    =^  cool  seam  $(nomm what.nomm, fax (peg fax 6), goal [%pick tile file])
    (copy what.cool seed then.cool)
  ::
      %eve
    =^  thin  seam  $(nomm then.nomm, fax (peg fax 7)
    $(nomm once.nomm, fax (peg fax 6))
  ::
      %ten
    ?:  =(0 here.nomm)  bomb
    ?-  -.goal
        %pick
      ?.  =(1 here.nomm)  bomb
      =^  bean  seam  whip
      =^  bent  seam  (emit %bent ~ %brn bean [zero once]:goal)
      $(goal [%next [%this bean] bent])
    ::
        %done
      =^  last  seam  whip
      =^  rent  seam  (emit %rent ~ %don last)
      $(goal [%next [%this last] rent])
    ::
        %next
      =^  [seed=need reed=need tide=bile]  seam  (diet here.nomm goal)
      =^  huge  seam  $(nomm tree.nomm, fax (peg fax 7), goal [%next reed bent])
      =^  smol  seam  $(nomm twig.nomm, fax (peg fax 13), goal [%next seed then.huge])
      (copy what.huge what.smol then.smol)
    ==
  ::
      %sip
    ?-  -.goal
        %done
      =^  real  seam  $(nomm then.nomm, fax (peg fax 7))
      =^  hire  seam  whip
      =^  hive  seam  (emit %hive [%imm hint.nomm hire]~ %hin hire then.real)
      [[%next what.real hive] seam]
    ::
        %pick
      =^  tune  seam  (emit %tune ~ %hun zero.goal)
      =^  lune  seam  (emit %lune ~ %hun once.goal)
      =^  real  seam  $(nomm then.nomm, fax (peg fax 7), goal [%pick tune lune])
      =^  hire  seam  whip
      =^  hive  seam  (emit %hive [%imm hint.nomm hire]~ %hin hire then.real)
      [[%next what.real hive] seam]
    ::
        %next
      =^  dish  seam  (emit %dish ~ %hun then.goal)
      =^  real  seam  $(nomm then.nomm, fax (peg fax 7), then.goal dish)
      =^  hire  seam  whip
      =^  hive  seam  (emit %hive [%imm hint.nomm hire]~ %hin hire then.real)
      [[%next what.real hive] seam]
    ==
  ::
      %dip
    ?-  -.goal
        %done
      =^  real  seam  $(nomm then.nomm, fax (peg fax 7))
      =^  hire  seam  whip
      =^  sire  seam  whip
      =^  dire  seam  whip
      =^  hide  seam
        %:  emit  %hide
            ~[[%imm hint.nomm sire] [%con sire dire hire]]
            [%hin hire then.real]
        ==
      =^  fake  seam
        $(nomm vice.nomm, fax (peg fax 13), goal [%next [%this dire] hide])
      (copy what.fake what.real then.fake)
    ::
        %pick
      =^  tune  seam  (emit %tune ~ %hun zero.goal)
      =^  lune  seam  (emit %lune ~ %hun once.goal)
      =^  real  seam  $(nomm then.nomm, fax (peg fax 7), goal [%pick tune lune])
      =^  hire  seam  whip
      =^  sire  seam  whip
      =^  dire  seam  whip
      =^  hide  seam
        %:  emit  %hide
            ~[[%imm hint.nomm sire] [%con sire dire hire]]
            [%hin hire then.real]
        ==
      =^  fake  seam
        $(nomm vice.nomm, fax (peg fax 13), goal [%next [%this dire] hide])
      (copy what.fake what.real then.fake)
    ::
        %next
      =^  dish  seam  (emit %dish ~ %hun then.goal)
      =^  real  seam  $(nomm then.nomm, fax (peg fax 7), then.goal dish)
      =^  hire  seam  whip
      =^  sire  seam  whip
      =^  dire  seam  whip
      =^  hide  seam
        %:  emit  %hide
            ~[[%imm hint.nomm sire] [%con sire dire hire]]
            [%hin hire then.real]
        ==
      =^  fake  seam
        $(nomm vice.nomm, fax (peg fax 13), goal [%next [%this dire] hide])
      (copy what.fake what.real then.fake)
    ==
  ::
      %elf
    ?-  -.goal
        %done
      =^  last  seam  whip
      =^  deny  seam  (emit %deny ~ %don last)
      $(goal [%next [%this last] deny])
    ::
        %pick
      =^  bean  seam  whip
      =^  lots  seam  (emit %lots ~ %brn bean [zero once]:goal)
      $(goal [%next [%this last] lots])
    ::
        %next
      =^  [mare=@stir mind=bile]  seam  (bite goal)
      =^  ride  seam  whip
      =^  pins  seam  whip
      =^  afar  seam  (emit %afar ~ %spy ride pins mare mind)
      =^  pain  seam
        $(nomm walk.nomm, fax (peg fax 7), goal [%next [%this pins] afar])
      =^  rain  seam
        $(nomm rent.nomm, fax (peg fax 6), goal [%next [%this ride] then.pain])
      (copy what.rain what.pain then.rain)
    ==
  ==
  |%
  ::  new registers for a call
  ++  noon
    |=  feed=need
    ^-  [[need (list @stir)] seam]
    ?-  -.feed
        %this
      =^  sire  seam  whip
      [[[%this sire] ~[sire]] seam]
    ::
        %none  [[feed ~] seam]
        %both
      =^  [lead=need lire=(list @stir)]  seam  $(feed left.feed)
      =^  [reed=need rise=(list @stir)]  seam  $(feed rite.need)
      [[[%both lead reed] (weld lire rise)] seam]
    ==
  ::  split a need at an axis for editing
  ++  diet
    |=  [axe=@ next]
    ?<  =(axe 0)
    ^-  [[need need bile] seam]
    =|  lack  (list [cap=$?(%2 %3) =need])
    =|  post  (list pole)
    |-  ^-  [[need need bile] seam]
    ?.  =(1 axe)
      ?-  -.what
          %both
        ?-  (cap axe)
          %2  $(axe (mas axe), lack [[%2 rite.what] lack], what left.what)
          %3  $(axe (mas axe), lack [[%3 left.what] lack], what rite.what)
        ==
      ::
          %this
        =^  hire  seam  whip
        =^  tire  seam  whip
        =.  post  [[%con hire tire sass.what] post]
        ?-  (cap axe)
          %2  $(axe (mas axe), lack [[%2 %this tire] lack], what [%this hire])
          %3  $(axe (mas axe), lack [[%3 %this hire] lack], what [%this tire])
        ==
      ::
          %none
        ?-  (cap axe)
          %2  $(axe (mas axe), lack [[%2 what] lack])
          %3  $(axe (mas axe), lack [[%3 what] lack])
        ==
      ==
    =|  bead  [%none ~]
    |-  ^-  [[need need bile] seam]
    ?^  lack
      =.  bead
        ?-  cap.i.lack
          %2  [%both bead need.i.lack]
          %3  [%both need.i.lack bead]
        ==
      $(lack t.lack)
    ?~  post  [[bead what then] seam]
    =^  bale  seam  (emit %diet post %hop then)
    [[bead what bale] seam]
  ::  copy a need and write phi instructions
  ++  hype
    |=  nets=next
    ^-  [[next next] seam]
    =^  [pose=(list pole) thee=need leed=need]  seam
      |-  ^-  [[(list pole) need need] seam]
      ?-  -  what.nets
          %none  [[~ what.nets what.nets] seam]
          %this
        =^  test  seam  whip
        =^  fest  seam  whip
        [~[[%phi ~[test fest] sass.what.nets]] seam]
      ::
          %both
        =^  [lose=(list pole) lath=need sell=need]  seam
          $(what.nets left.what.nets)
        =^  [rose=(list pole) rats=need raft=need]  seam
        [[(weld lose rose) [%both lath rats] %both sell raft] seam]
      ==
    ?~  pose  [[nets nets] seam]
    =^  then  seam  (emit %hype pose %hop then.nets)
    [[[%next thee then] %next leed then] seam]
  ::  intersect needs from conditional branches
  ++  sect
    |=  [tool=next fool=next]
    ^-  [[need bile bile] seam]
    =^  [[toes=(list pole) foes=(list pole) thee=need]  seam
      |-  ^-  [[(list pole) need (list pole) need] seam]
      ?:  ?&(?=(-.what.tool %both) ?=(-.what.fool %both))
        =^  [[post=(list pole) pipe=(list pole) hate=need]  seam
          $(what.tool left.what.tool, what.fool left.what.fool)
        =^  [[pose=(list pole) pine=(list pole) tare=need]  seam
          $(what.tool rite.what.tool, what.fool rite.what.fool)
        :_  seam
        [(weld post pose) (weld pipe pine) %both hate tare]
      ?:  ?&(?=(-.what.tool %none) ?=(-.what.fool %none))
        [[~ ~ what.tool] seam]
      ?:  ?&(?=(-.what.tool %this) ?=(-.what.fool %this))
        ?:  =(sass.what.tool sass.what.fool)  [[~ what.tool ~ what.fool] seam]
        =^  mast  seam  whip
        :_  seam
        [[%mov mast sass.what.tool]~ [%mov mast sass.what.fool]~ %this mast]
      =^  mast  seam  whip
      =^  ties  seam  (bits mast what.tool)
      =^  flea  seam  (bits mast what.fool)
      :_  seam
      [ties flea %this mast]
    =^  true  seam  (emit %true ties %hop then.tool)
    =^  lies  seam  (emit %lies flea %hop then.fool)
    [[thee true lies] seam]
  ::  crash
  ++  bomb
    =^  boom  seam  (emit %boom ~ %bom ~)
    [[%next [%none ~] boom] seam]
  ::  split up a register among a need
  ++  bite
    |=  next
    ^-  [[@stir bile] seam]
    =^  star  seam  whip
    =^  post  seam  (bits star what)
    ?~  post  [[star then] seam]
    =^  pose  (emit %bite (flop post) %hop then)
    [[star pose] seam]
  ::  generate but do not emit instructions to split a register among a need.
  ::  emits in reverse order for better use in basic block building
  ++  bits
    |=  [stir=@stir neat=need]
    ^-  [(list pole) _seam]
    =|  post  (list pole)
    =/  tack=(list [stir=@stir need])  [stir neat]~
    |-  ^-  [(list pole) _seam]
    ?~  tack  [post seam]
    ?-  -<.i.tack
        %none  $(tack t.tack)
        %both
      =^  hire  seam  whip
      =^  tire  seam  whip
      %=  $
        post  [[%hed stir.i.tack hire] [%tal stir.i.tack tire] post] 
        tack  [[hire left.i.tack] [tire rite.i.tack] tack]
      ==
    ::
        %this
      ?:  =(stir.i.tack sass.i.tack)  $(tack t.tack)
      $(post [[%mov stir.i.tack sass.i.tack] post], tack t.tack)
    ==
  ::  split up an immediate noun among a need
  ++  divy
    |=  [moan=* next]
    ^-  [goal _seam]
    =/  tack=(list [moan=* =need])  [moan what]~
    =|  todo  (list pole)
    |-  ^-  [goal _seam]
    ?^  tack
      ?-  -.need.i.tack
          %none  $(tack t.tack)
          %this  $(tack t.tack, todo [[%imm moan.i.tack sass.need.i.tack] todo])
          %both
        ?@  moan.i.tack  bomb
        =.  tack
          :*  [-.moan.i.tack left.need.i.tack]
              [+.moan.i.tack rite.need.i.tack]
              t.tack
          ==
        $
      ==
    ?~  todo  [[%next [%none ~] then] seam]
    =^  bill  seam  (emit %divy todo %hop then)
    [[%next [%none ~] bill] seam]
  ::  split need
  ++  pare
    |=  next
    ^-  [[need need bile] _seam]
    ?-  -.what
        %both  [[left.what rite.what then] seam]
        %none  [[what what then] seam]
        %this
      =^  hire  seam  whip
      =^  tire  seam  whip
      =^  bill  seam  (emit %paco [%con hire tire sass.what] %hop then)
      [[hire tire bill] seam]
    ==
  ::  unify needs
  ++  copy
    |=  [one=need two=need then=bile]
    ^-  [next seam]
    =/  [cede=need todo=(list pole)]
      |-  ^-  [[need (list pole)] _seam]
      ?:  ?=(%none -.one) [[two ~] seam]
      ?:  ?=(%none -.two) [[one ~] seam]
      ?:  ?=(%this -.one)
        ?:  ?=(%this -.two)
          ?:  =(sass.one sass.two) [[one ~] seam]
          [[one [%mov sass.one sass.two]~] seam]
        =^  hire  seam  whip
        =^  tire  seam  whip
        =^  [leed=need hole=(list pole)]  seam
          $(one [%this hire], two left.two)
        =^  [reed=need toll=(list pole)]  seam
          $(one [%this tire], two rite.two)
        [[[%both leed reed] [[%con hire tire sass.one] (welp hole toll)]] seam]
      ?:  ?=(%this -.two)
        =^  hire  seam  whip
        =^  tire  seam  whip
        =^  [leed=need hole=(list pole)]  seam
          $(two [%this hire], one left.one)
        =^  [reed=need toll=(list pole)]  seam
          $(two [%this tire], one rite.one)
        [[[%both leed reed] [[%con hire tire sass.two] (welp hole toll)]] seam]
      =^  [leed=need hole=(list pole)]  seam
        $(two left.two, one left.one)
      =^  [reed=need toll=(list pole)]  seam
        $(two rite.two, one rite.one)
      [[[%both leed reed] (welp hole toll)] seam]
    ?~  todo  [[%next cede then] seam]
    =^  dupe  seam  (emit %dupe (flop todo) %hop then)
    [[cede dupe] seam]
  ::  emit code
  ++  emit
    |=  [thus=@tas =blob]
    ^-  [bile _seam]
    =/  bill  [fax thus bell]
    =.  code.seam  (~(put by code.seam) bill blob)
    [bill seam]
  ::  new register
  ++  whip
    ^-  [@stir _seam]
    [stir.seam seam(stir .+(stir.seam)]
  ::  push need down by axis
  ++  grab
    |=  [axe=@ =need]
    ^-  need
    ?<  =(axe 0)
    ?:  =(axe 1)  need
    ?-  (cap axe)
      %2  [%both $(here (mas axe)) %none ~]
      %3  [%both [%none ~] $(here (mas axe))]
    ==
  --
::  generate register relation maps and re-registerize calls
++  cure
  |=  [pill=pile late=(set @bile)]
  ^-  pile
  =/  work  ~(tap in late)
  =|  will  (map bile blob)
  |-  ^-  pile
  ?^  work
    ?:  ?=(%cal -.bend.q.i.work)
      =/  kyle  (~(got by hill) +<.bend.q.i.work)
      ?.  ~(has in late p.i.work)  $(work t.work, will (~(put by will) i.work))
      ?>  ?=(^ +>-.bend.q.i.work)
      ?>  =(~ t.+>-.bend.q.i.work)
      =/  held  i.+>-.bend.q.i.work
      =|  pale  (list pole)
      =|  rags  (list @stir)
      =/  hack=(list [stir=@stir =need])  [held want.kyle]~
      |-  ^-  pile
      ?^  hack
        ?-  -.need.i.hack
            %none  $(hack t.hack)
            %this
          $(rags [stir.i.hack rags], hack t.hack)
        ::
            %both
          =^  hire  sans.pill  [sans.pill .+(sans.pill)]
          =^  tire  sans.pill  [sans.pill .+(sans.pill)]
          %=  $
            pale [[%tal stir.i.hack tire] [%hed stir.i.hack hire] pale]
            hack [[tire rite.need.i.hack] [hire left.need.i.hack] t.hack]
          ==
        ==
      =/  bent  [%cal +<.q.i.work rags +>+.q.i.work]
      =.  will
        %+  ~(put in will)  t.i.work
        [(weld body.q.i.work (flop pale)) bent]
      ^$(work t.work)
    ?:  ?=(%jmp -.bend.q.i.work)
      =/  kyle  (~(got by hill) +<.bend.q.i.work)
      ?.  ~(has in late p.i.work)  $(work t.work, will (~(put by will) i.work))
      ?>  ?=(^ +>.bend.q.i.work)
      ?>  =(~ t.+>.bend.q.i.work)
      =/  held  i.+>.bend.q.i.work
      =|  pale  (list pole)
      =|  rags  (list @stir)
      =/  hack=(list [stir=@stir =need])  [held want.kyle]~
      |-  ^-  pile
      ?^  hack
        ?-  -.need.i.hack
            %none  $(hack t.ack)
            %this
          $(rags [stir.i.hack rags], hack t.hack)
        ::
            %both
          =^  hire  sans.pill  [sans.pill .+(sans.pill)]
          =^  tire  sans.pill  [sans.pill .+(sans.pill)]
          %=  $
            pale  [[%tal stir.i.hack tire] [%hed stir.i.hack hire] pale]
            hack  [[tire rite.need.i.hack] [hire left.need.i.hack] t.hack]
          ==
        ==
      =/  bent  [%jmp +<.q.i.work rags]
      =.  will
        %+  ~(put in will)  t.i.work
        [(weld body.q.i.work (flop pale)) bent]
      ^$(work t.work)
    =.  will  (~(put by will) i.work)
    $(work t.work)
  pill(will will)
::  rewrite registers and eliminate duplicate hed/tal instructions
++  coat
  |=  pill=pile
  =|  toga  (jug bile bile) :: predecessors
  =|  outs  (set bile) :: exit blocks
  =|  done  (set bile)
  =/  fore=(list bile)  ~[long.pill]
  =|  back  (list bile)
  |-  ^-  [[(set bile) (jug bile bile)] pile]
  ?^  fore
    ?:  (~(has in done) i.fore)
      $(fore t.fore)
    =.  done  (~(put in done) i.fore)
    =/  bend  bend:(~(got by will.pill) i.fore)
    ?-  -.bend
        %clq
      =.  toga  (~(gas ju toga) ~[[+>-.bend.bulb i.fore] [+>+.bend.bulb i.fore]])
      $(fore t.fore, back [+>-.bend.bulb +>+.bend.bulb back])
    ::
        %eqq
      =.  toga
        (~(gas ju toga) ~[[+>+<.bend.bulb i.fore] [+>+>.bend.bulb i.fore]])
      $(fore t.fore, back [+>+<.bend.bulb +>+>.bend.bulb back])
    ::
        %brn
      =.  toga  (~(gas ju toga) ~[[+>-.bend.bulb i.fore] [+>+.bend.bulb i.fore]])
      $(fore t.fore, back [+>-.bend.bulb +>+.bend.bulb back])
    ::
        %hop
      =.  toga  (~(put ju toga) +.bend.bulb i.fore)
      $(fore t.fore, back [+.bend.bulb back])
    ::
        %lnk
      =.  toga  (~(put ju toga) +>+>.bend.bulb i.fore)
      $(fore t.fore, back [+>+>.bend.bulb back])
    ::
        %cal
      =.  toga  (~(put ju toga) +>+>.bend.bulb i.fore)
      $(fore t.fore, back [+>+>.bend.bulb back])
    ::
        %lnt
      =.  outs  (~(put in outs) i.fore)
      $(fore t.fore)
    ::
        %jmp
      =.  outs  (~(put in outs) i.fore)
      $(fore t.fore)
    ::
        %spy
      =.  toga  (~(put ju toga) +>+>.bend.bulb i.fore)
      $(fore t.fore, back [+>+>.bend.bulb back])
    ::
        %hin
      =.  toga  (~(put ju toga) +>.bend.bulb i.fore)
      $(fore t.fore, back [+>.bend.bulb back])
    ::
        %hun
      =.  toga  (~(put ju toga) +.bend.bulb i.fore)
      $(fore t.fore, back [+.bend.bulb back])
    ::
        %don
      =.  outs  (~(put in outs) i.fore)
      $(fore t.fore)
    ::
        %bom
      =.  outs  (~(put in outs) i.fore)
      $(fore t.fore)
    ==
  ?^  back  $(fore (flop back), back ~)
  ::
  =|  subs  (map @stir @stir)
  =|  shun  (map pool @stir)
  =|  will  (map bile blob)
  =.  fore  ~[long.pill]
  =.  back  ~
  =.  done  ~
  |-  ^-  [[(set bile) (jug bile bile)] pile]
  ?^  fore
    =/  pred  (~(get ju toga) i.fore)
    ?.  =(~ (~(dif in pred) done))  :: kick to back if predecessors not done
      $(fore t.fore, back [i.fore back])
    =/  bulb  (~(got by will.pill) i.fore)
    =|  body  (list pole)
    |-  ^-  [[(set bile) (jug bile bile)] pile]
    ?^  body.bulb
      ?-  -.i.body.bulb
          %imm
        $(body.bulb t.body.bulb, body [i.body.bulb body])
      ::
          %mov
        =.  +<.i.body.bulb  (~(gut by subs) +<.i.body.bulb +<.i.body.bulb)
        =.  subs  (~(put by subs) +>.i.body.bulb +<.i.body.bulb)
        $(body.bulb t.body.bulb, body [i.body.bulb body])
      ::
          %phi
        =.  +<.i.body.bulb
          %+  turn  +<.i.body.bulb
          |=  stir=@stir
          (~(gut by subs) stir stir)
        ?>  +<.i.body.bulb
        =?    subs
            (all (turn t.+<.i.body.bulb |=(s=@stir =(i.+<.i.body.bulb s))))
          (~(put by subs) +>.i.body.bulb i.+<.i.body.bulb)
        $(body.bulb t.body.bulb, body [i.body.bulb body])
      ::
          %inc
        =.  +<.i.body.bulb  (~(gut by subs) +<.i.body.bulb +<.i.body.bulb)
        $(body.bulb t.body.bulb, body [i.body.bulb body])
      ::
          %con
        =.  +<.i.body.bulb  (~(gut by subs) +<.i.body.bulb +<.i.body.bulb)
        =.  +>-.i.body.bulb  (~(gut by subs) +>-.i.body.bulb +>-.i.body.bulb)
        =.  shun
          %-  ~(gas by shun)
          :~  [%hed +>+.i.body.bulb] +<.i.body.bulb]
              [%tal +>+.i.body.bulb] +>-.i.body.bulb]
          ==
        $(body.bulb t.body.bulb, body [i.body.bulb body])
      ::
          %hed
        =.  +<.i.body.bulb  (~(gut by subs) +<.i.body.bulb +<.i.body.bulb)
        =/  hoot  (~(get by shun) [%hed +<.i.body.bulb])
        ?~  hoot
          =.  shun  (~(put by shun) [%hed +<.i.body.bulb] +>.i.body.bulb)
          $(body.bulb t.body.bulb, body [i.body.bulb body])
        =.  subs  (~(put by subs) +>.i.body.bulb u.hoot)
        $(body.bulb t.body.bulb)
      ::
          %tal
        =.  +<.i.body.bulb  (~(gut by subs) +<.i.body.bulb +<.i.body.bulb)
        =/  hoot  (~(get by shun) [%tal +<.i.body.bulb])
        ?~  hoot
          =.  shun  (~(put by shun) [%tal +<.i.body.bulb] +>.i.body.bulb)
          $(body.bulb t.body.bulb, body [i.body.bulb body])
        =.  subs  (~(put by subs) +>.i.body.bulb u.hoot)
        $(body.bulb t.body.bulb)
      ==
    ?-  -.bend.bulb
        %clq
      =.  +<.bend.bulb  (~(gut by subs) +<.bend.bulb +<.bend.bulb)
      =.  will  (~(put by will) i.fore [(flop body) bend.bulb])
      ^$(fore t.fore, back [+>-.bend.bulb +>+.bend.bulb back])
    ::
        %eqq
      =.  +<.bend.bulb  (~(gut by subs) +<.bend.bulb +<.bend.bulb)
      =.  +>-.bend.bulb  (~(gut by subs) +>-.bend.bulb +>-.bend.bulb)
      =.  will  (~(put by will) i.fore [(flop body) bend.bulb])
      ^$(fore t.fore, back [+>+<.bend.bulb +>+>.bend.bulb back])
    ::
        %brn
      =.  +<.bend.bulb  (~(gut by subs) +<.bend.bulb +<.bend.bulb)
      =.  will  (~(put by will) i.fore [(flop body) bend.bulb])
      ^$(fore t.fore, back [+>-.bend.bulb +>+.bend.bulb back])
    ::
        %hop
      ^$(fore t.fore, back [+.bend.bulb back])
    ::
        %lnk
      =.  +<.bend.bulb  (~(gut by subs) +<.bend.bulb +<.bend.bulb)
      =.  +>-.bend.bulb  (~(gut by subs) +>-.bend.bulb +>-.bend.bulb)
      =.  will  (~(put by will) i.fore [(flop body) bend.bulb])
      ^$(fore t.fore, back [+>+>.bend.bulb back])
    ::
        %cal
      =.  +>-.bend.bulb
        %+  turn  +>-.bend.bulb
        |=  s=@stir
        (~(gut by subs) s s)
      =.  will  (~(put by will) i.fore [(flop body) bend.bulb])
      ^$(fore t.fore, back [+>+>.bend.bulb back])
    ::
        %lnt
      =.  +<.bend.bulb  (~(gut by subs) +<.bend.bulb +<.bend.bulb)
      =.  will  (~(put by will) i.fore [(flop body) bend.bulb])
      ^$(fore t.fore)
    ::
        %jmp
      =.  +>.bend.bulb
        %+  turn  +>.bend.bulb
        |=  s=@stir
        (~(gut by subs) s s)
      =.  will  (~(put by will) i.fore [(flop body) bend.bulb])
      ^$(fore t.fore)
    ::
        %spy
      =.  +<.i.body.bulb  (~(gut by subs) +<.i.body.bulb +<.i.body.bulb)
      =.  +>-.i.body.bulb  (~(gut by subs) +>-.i.body.bulb +>-.i.body.bulb)
      =.  will  (~(put by will) i.fore [(flop body) bend.bulb])
      ^$(fore t.fore, back [+>+>.bend.bulb back])
    ::
        %hin
      =.  +<.i.body.bulb  (~(gut by subs) +<.i.body.bulb +<.i.body.bulb)
      =.  will  (~(put by will) i.fore [(flop body) bend.bulb])
      ^$(fore t.fore, back [+>.bend.bulb back])
    ::
        %hun
      =.  will  (~(put by will) i.fore [(flop body) bend.bulb])
      ^$(fore t.fore, back [+.bend.bulb back])
    ::
        %don
      =.  +.i.body.bulb  (~(gut by subs) +.i.body.bulb +.i.body.bulb)
      =.  will  (~(put by will) i.fore [(flop body) bend.bulb])
      ^$(fore t.fore)
    ::
        %bom
      =.  will  (~(put by will) i.fore [(flop body) bend.bulb])
      ^$(fore t.fore)
    ==
  ?^  back  $(fore (flop back), back ~)
  [toga pill(will will)]
::  unify single-hop-entry blocks with predecessors and drop unused
::  safe defines
++  trim
  |=  [[exit=(set bile) toga=(jug bile bile)] pill=pile]
  =/  fore  ~(tap in exit)
  =|  back  (list bile)
  =|  done  (set bile)
  =|  used  (jug bile @stir) :: used at exit points
  =|  will (map bile blob)
  |-  ^-  pill
  ?^  fore
    =/  bulb  (~(got by will.pill) i.fore)
    ?.  %-  all  ::  kick to back if successors not done
        %+  turn
          ?-  -.bend.bulb
            %clq  ~[+>- +>+]:bend.bulb
            %eqq  ~[+>+< +>+>]:bend.bulb
            %brn  ~[+>- +>+]:bend.bulb
            %hop  ~[+.bend.bile]
            %lnk  ~[+>+>.bend.bile]
            %cal  ~[+>+>.bend.bile]
            %lnt  ~
            %jmp  ~
            %spy  ~[+>+>.bend.bile]
            %hin  ~[+>.bend.bile]
            %hun  ~[+.bend.bile]
            %don  ~
            %bom  ~
          ==
        ~(has in done)
      $(fore t.fore, back [i.fore back])
    =.  done  (~(put in done) i.fore)
    =/  uses  
      %+  ~(gas in (~(get ju used) i.fore))
      ?-  -.bend.bulb
        %clq  ~[+<.bend.bulb]
        %eqq  ~[+<.bend.bulb +>-.bend.bulb]
        %brn  ~[+<.bend.bulb]
        %hop  ~
        %lnk  ~[+<.bend.bulb +>-.bend.bulb]
        %cal  +>-.bend.bulb
        %lnt  ~[+<.bend.bulb]
        %jmp  +>.bend.bulb
        %spy  ~[+<.bend.bulb +>-.bend.bulb]
        %hin  ~[+<.bend.bulb]
        %hun  ~
        %don  ~[+<.bend.bulb]  
        %bom  ~
      ==
    =/  boyd  (flop body.bulb)
    |-  ^-  pile
    ?^  boyd
      ?-  -.i.boyd
          %imm
        =?  body  (~(has in uses) +>.i.boyd)  [i.boyd body]
        $(boyd t.boyd)
      ::
          %mov
        =?  body  (~(has in uses) +>.i.boyd)  [i.boyd body]
        =?  uses  (~(has in uses) +>.i.boyd)  (~(put in uses) +<.i.boyd)
        $(boyd t.boyd)
      ::
          %phi
        =?  body  (~(has in uses) +>.i.boyd)  [i.boyd body]
        =?  uses  (~(has in uses) +>.i.boyd)  (~(gas in uses) +<.i.boyd)
        $(boyd t.boyd)
      ::
          %inc
        =.  body  [i.boyd body]
        =.  uses  (~(put in uses) +<.i.boyd)
        $(boyd t.boyd)
      ::
          %con
        =?  body  (~(has in uses) +>.i.boyd)  [i.boyd body]
        =?  uses  (~(has in uses) +<.i.boyd)
          (~(gas in uses) ~[+<.i.boyd +>-.i.boyd])
        $(boyd t.boyd)
      ::
          %hed
        =.  body  [i.boyd body]
        =.  uses  (~(put in uses) +<.i.boyd)
        $(boyd t.boyd)
      ::
          %tal
        =.  body  [i.boyd body]
        =.  uses  (~(put in uses) +<.i.boyd)
        $(boyd t.boyd)
      ==
    =/  next  ~(tap in (~(get ju toga) i.fore))
    ?:  ?&(?=(^ next) =(~ t.next))
      =/  buns  (~(got by will.pill) i.next)
      ?:  ?=(%hop -.bend.buns)
        $(i.fore i.next, boyd (flop body.buns)
      =.  used
        (~(gas in used) (turn ~(tap in uses) |=(u=@stir [i.fore stir])))
      =.  will  (~(put by will) i.fore [body bend.bulb])
      ^$(fore t.fore)
    =.  back  (weld next back)
    |-  ^-  (jug bile @stir)
    ?^  next
      =/  user  (~(get ju used) i.next)
      =.  used  (~(put by used) i.next (~(uni in user) uses))
    =.  will  (~(put by will) i.fore [body bend.bulb])
    ^^$(fore t.fore)
  ?^  back  $(fore (flop back), back ~)
  pill(will will)
--
