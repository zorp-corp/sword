/-  *sock
/-  *gene
|%
:: linearize and registerize a formula
++  comb
  =/  tend=goal  [%done ~] :: this should be the bunt but see urbit/urbit#2688
  =/  deep  1
  =/  hall  1
  =|  hide=[will=(map bile blob) sans=@]
  |=  form=*
  ^-  pile
  =<
  =^  dent  hide
    |^
      ^-  [$>(%next goal) _hide]
      ?+  form  bomb
          [^ *]  :: Autocons
        ?-  -.tend
            %next
          =^  [left=need rite=need kale=bile]  hide  (coal tend)
          =^  ride  hide
            %=  $
              form  +.form
              deep  (peg deep 3)
              hall  (peg hall 3)
              tend  [%next rite kale]
            ==
          =^  deft  hide 
            %=  $
              form  -.form
              deep  (peg deep 2)
              hall  (peg hall 2)
              tend  [%next left then.ride]
            ==
          (copy what.deft what.ride then.deft)
        ::
            %pick  bomb
            %done
          =^  salt  hide  sear
          =^  cone  hide  (emit %cone ~ %don salt)
          $(tend [%next [%this salt %.n] cone])
        ==
      ::  Nock 0
          [%0 @]
        ?:  =(+.form 0)
          bomb
        ?-  -.tend
            %next
          :_  hide
          [%next (take +.form what.tend) then.tend]
        ::
            %pick
          =^  fork  hide  sear
          =^  pack  hide  (emit %pack ~ %brn fork [zero once]:tend)
          $(tend [%next [%this fork %.n] pack])
        ::
            %done
          =^  salt  hide  sear
          =^  gone  hide  (emit %gone ~ %don salt)
          $(tend [%next [%this salt %.n] gone])
        ==
      ::  Nock 1
          [%1 *]
        ?-  -.tend
            %next
          (hack +.form what.tend then.tend)
        ::
            %pick
          ?:  =(0 +.form)
            [[%next [%none ~] zero.tend] hide]
          ?:  =(1 +.form)
            [[%next [%none ~] once.tend] hide]
          bomb
        ::
            %done
          =^  salt  hide  sear
          =^  kine  hide  (emit %kine ~[[%imm +.form salt]] %don salt)
          [[%next [%none ~] kine] hide]
        ==
      ::  Nock 2
          [%2 * *]
        ?-  -.tend
            %next
          =^  [wish=bile sire=$>(%this need)]  hide  (divy what.tend then.tend)
          =^  raft  hide  sear
          =^  soft  hide  sear
          =^  link  hide  (emit %link ~ %lnk soft raft sass.sire wish)
          =^  rack  hide
            %=  $
              form  +>.form
              deep  (peg deep 7)
              hall  (peg hall 7)
              tend  [%next [%this raft %.y] link]
            ==
          =^  sack  hide
            %=  $
              form  +<.form
              deep  (peg deep 6)
              hall  (peg hall 6)
              tend  [%next [%this soft code.sire] then.rack]
            ==
          (copy what.sack what.rack then.sack)
        ::
            %pick
          =^  fork  hide  sear
          =^  bevy  hide  (emit %bevy ~ %brn fork [zero once]:tend)
          $(tend [%next [%this fork %.n] bevy])
        ::
            %done
          =^  raft  hide  sear
          =^  soft  hide  sear
          =^  tale  hide  (emit %tale ~ %lnt soft raft)
          =^  rack  hide
            %=  $
              form  +>.form
              deep  (peg deep 7)
              hall  (peg hall 7)
              tend  [%next [%this raft %.y] tale]
            ==
          =^  sack  hide
            %=  $
              form  +<.form
              deep  (peg deep 6)
              hall  (peg hall 6)
              tend  [%next [%this soft %.n] then.rack]
            ==
          (copy what.sack what.rack then.sack)
        ==
      :: Nock 3
          [%3 *]
        ?-  -.tend
            %next
          ?.  ?=(%this -.what.tend)
            bomb
          =^  ices  hide  (emit %ices ~[[%imm 0 sass.what.tend]] %hop then.tend)
          =^  miso  hide  (emit %miso ~[[%imm 1 sass.what.tend]] %hop then.tend)
          $(tend [%pick ices miso])
        ::
            %pick
          =^  thin  hide  sear
          =^  soup  hide  (emit %soup ~ %clq thin [zero once]:tend)
          %=  $
            form  +.form
            deep  (peg deep 3)
            hall  (peg hall 3)
            tend  [%next [%this thin %.n] soup]
          ==
        ::
            %done
          =^  salt  hide  sear
          =^  ices  hide  (emit %ices ~[[%imm 0 salt]] %don salt)
          =^  miso  hide  (emit %miso ~[[%imm 1 salt]] %don salt)
          $(tend [%pick ices miso])
        ==
      :: Nock 4
          [%4 *]
        ?-  -.tend
            %next
          ?.  ?=(%this -.what.tend)
            bomb
          =^  rink  hide  sear
          =^  sink  hide  (emit %sink ~[[%inc rink sass.what.tend]] %hop then.tend)
          %=  $
            form  +.form
            deep  (peg deep 3)
            hall  (peg hall 3)
            tend  [%next [%this rink %.n] sink]
          ==
        ::
            %pick
          =^  rink  hide  sear
          =^  fink  hide  sear
          =^  pink  hide  (emit %pink ~[[%inc rink fink]] %brn fink [zero once]:tend)
          %=  $
            form  +.form
            deep  (peg deep 3)
            hall  (peg hall 3)
            tend  [%next [%this rink %.n] pink]
          ==
        ::
            %done
          =^  rink  hide  sear
          =^  salt  hide  sear
          =^  wink  hide  (emit %wink ~[[%inc rink salt]] %don salt)
          %=  $
            form  +.form
            deep  (peg deep 3)
            hall  (peg hall 3)
            tend  [%next [%this rink %.n] wink]
          ==
        ==
      ::  Nock 5
          [%5 * *]
        ?-  -.tend
            %next
          ?.  ?=(%this -.what.tend)
            bomb
          =^  seek  hide  (emit %seek ~[[%imm 0 sass.what.tend]] %hop then.tend)
          =^  meek  hide  (emit %meek ~[[%imm 1 sass.what.tend]] %hop then.tend)
          $(tend [%pick seek meek])
        ::
            %pick
          =^  lame  hide  sear
          =^  tame  hide  sear
          =^  pear  hide  (emit %pear ~ %eqq lame tame [zero once]:tend)
          =^  rake  hide
            %=  $
              form  +>.form
              deep  (peg deep 7)
              hall  (peg hall 7)
              tend  [%next [%this tame %.n] pear]
            ==
          =^  lake  hide
            %=  $
              form  +<.form
              deep  (peg deep 6)
              hall  (peg hall 6)
              tend  [%next [%this lame %.n] then.rake]
            ==
          (copy what.lake what.rake then.lake)
        ::
            %done
          =^  salt  hide  sear
          =^  seed  hide  (emit %seed ~[[%imm 0 salt]] %don salt)
          =^  mead  hide  (emit %seed ~[[%imm 1 salt]] %don salt)
          $(tend [%pick seed mead])
        ==
      :: Nock 6
          [%6 * * *]
        =^  fall  hide
          %=  $
            form  +>+.form
            deep  (peg deep 15)
            hall  (peg hall 15)
          ==
        =^  tall  hide
          %=  $
            form  +>-.form
            deep  (peg deep 14)
            hall  (peg hall 14)
          ==
        =^  [pest=need zero=bile once=bile]  hide  (pony tall fall)
        =^  pike  hide
          %=  $
            form  +<.form
            deep  (peg deep 6)
            hall  (peg hall 6)
            tend  [%pick zero once]
          ==
        (copy what.pike pest then.pike)
      :: Nock 7
          [%7 * *]
        =^  pith  hide
          %=  $
            form  +>.form
            deep  (peg deep 7)
            hall  (peg hall 7)
          ==
        %=  $
          form  +<.form
          deep  (peg deep 6)
          hall  (peg hall 6)
          tend  pith
        ==
      :: Nock 8
          [%8 * *]
        =^  pith  hide
          %=  $
            form  +>.form
            deep  (peg deep 7)
            hall  (peg hall 7)
          ==
        =^  [left=need rite=need kale=bile]  hide  (coal pith)
        =^  hull  hide
          %=  $
            form  +<.form
            deep  (peg deep 6)
            hall  (peg hall 6)
            tend  [%next left kale]
          ==
        (copy what.hull rite then.hull)
      :: Nock 9
          [%9 @ *]
        ?:  =(+<.form 0)
          bomb
        ?-  -.tend
            %next
          =^  [wish=bile sire=$>(%this need)]  hide  (divy what.tend then.tend)
          =^  raft  hide  sear
          =^  soft  hide  sear
          =^  link  hide  (emit %link ~ %lnk soft raft sass.sire wish)
          =^  rack  hide  (copy [%this soft code.sire] (take +<.form [%this raft %.y]) link)
          %=  $
            form  +>.form
            deep  (peg deep 7)
            hall  (peg hall 7)
            tend  rack
          ==
        ::
            %pick
          =^  fork  hide  sear
          =^  bevy  hide  (emit %bevy ~ %brn fork [zero once]:tend)
          $(tend [%next [%this fork %.n] bevy])
        ::
            %done
          =^  raft  hide  sear
          =^  soft  hide  sear
          =^  tale  hide  (emit %tale ~ %lnt soft raft)
          =^  rack  hide  (copy [%this soft %.n] (take +<.form [%this raft %.y]) tale)
          %=  $
            form  +>.form
            deep  (peg deep 7)
            hall  (peg hall 7)
            tend  rack
          ==
        ==
      :: Nock 10
          [%10 [@ *] *]
        ?-  -.tend
            %next
          ?:  =(0 +<-.form)  bomb
          =^  [muon=need hole=need edit=bile]  hide  (vise +<-.form tend)
          =^  plug  hide
            %=  $
              form  +>.form
              deep  (peg deep 7)
              hall  (peg hall 7)
              tend  [%next muon edit]
            ==
          =^  tire  hide
            %=  $
              form  +<+.form
              deep  (peg deep 13)
              hall  (peg hall 13)
              tend  [%next hole then.plug]
            ==
          (copy what.tire what.plug then.tire)
        ::
            %pick
          ?.  =(1 +<-.form) :: any other case will crash
            bomb
          =^  real  hide  sear
          =^  seal  hide  (emit %seal ~ %brn real [zero once]:tend)
          $(tend [%next [%this real %.n] seal])
        ::
            %done
          =^  salt  hide  sear
          =^  ruck  hide  (emit %ruck ~ %don salt)
          $(tend [%next [%this salt %.n] ruck])
        ==
      :: Nock 11 (static)
          [%11 @ *]
        ?-  -.tend
            %next
          =^  huns  hide  (emit %huns ~ %hun then.tend)
          =^  work  hide
            %=  $
              form  +>.form
              deep  (peg deep 7)
              hall  (peg hall 7)
              tend  tend(then huns)
            ==
          =^  hind  hide  sear
          =^  hens  hide  (emit %hens ~[[%imm +<.form hind]] %hin hind then.work)
          :_  hide
          [%next what.work hens]
        ::
            %pick
          =^  hunt  hide  (emit %hunt ~ %hun zero.tend)
          =^  hoot  hide  (emit %hoot ~ %hun once.tend)
          =^  work  hide
            %=  $
              form  +>.form
              deep  (peg deep 7)
              hall  (peg hall 7)
              tend  [%pick hunt hoot]
            ==
          =^  hind  hide  sear
          =^  honk  hide  (emit %honk ~[[%imm +<.form hind]] %hin hind then.work)
          :_  hide
          [%next what.work honk]
        ::
            %done
          =^  work  hide
            %=  $
              form  +>.form
              deep  (peg deep 7)
              hall  (peg hall 7)
            ==
          =^  hind  hide  sear
          =^  hire  hide  (emit %hire ~[[%imm +<.form hind]] %hin hind then.work)
          :_  hide
          [%next what.work hire]
        ==
      ::  11  (dynamic)
          [%11 [@ *] *]
        ?-  -.tend
            %next
          =^  dune  hide  (emit %dune ~ %hun then.tend)
          =^  work  hide
            %=  $
              form  +>.form
              deep  (peg deep 7)
              hall  (peg hall 7)
              tend  tend(then dune)
            ==
          =^  hind  hide  sear
          =^  sine  hide  sear
          =^  dine  hide  sear
          =^  dens  hide 
            %:  emit
              %dens
              ~[[%imm +<-.form sine] [%con sine dine hind]]
              [%hin hind then.work]
            ==
          =^  idle  hide
            %=  $
              form  +<+.form
              deep  (peg deep 13)
              hall  (peg hall 13)
              tend  [%next [%this dine %.n] dens]
            ==
          (copy what.idle what.work then.idle)
        ::
            %pick
          =^  dint  hide  (emit %dint ~ %hun zero.tend)
          =^  diff  hide  (emit %diff ~ %hun once.tend)
          =^  work  hide
            %=  $
              form  +>.form
              deep  (peg deep 7)
              hall  (peg hall 7)
              tend  [%pick dint diff]
            ==
          =^  hind  hide  sear
          =^  sine  hide  sear
          =^  dine  hide  sear
          =^  dens  hide 
            %:  emit
              %dens
              ~[[%imm +<-.form sine] [%con sine dine hind]]
              [%hin hind then.work]
            ==
          =^  idle  hide
            %=  $
              form  +<+.form
              deep  (peg deep 13)
              hall  (peg hall 13)
              tend  [%next [%this dine %.n] dens]
            ==
          (copy what.idle what.work then.idle)
        ::
            %done
          =^  work  hide
            %=  $
              form  +>.form
              deep  (peg deep 7)
              hall  (peg hall 7)
            ==
          =^  hind  hide  sear
          =^  sine  hide  sear
          =^  dine  hide  sear
          =^  dens  hide 
            %:  emit
              %dens
              ~[[%imm +<-.form sine] [%con sine dine hind]]
              [%hin hind then.work]
            ==
          =^  idle  hide
            %=  $
              form  +<+.form
              deep  (peg deep 13)
              hall  (peg hall 13)
              tend  [%next [%this dine %.n] dens]
            ==
          (copy what.idle what.work then.idle)
        ==
      :: "Nock" 12
          [%12 * *]
        ?-  -.tend
            %next
          =^  [vile=bile hair=$>(%this need)]  hide  (divy what.tend then.tend)
          =^  rift  hide  sear
          =^  puff  hide  sear
          =^  look  hide  (emit %look ~ %spy rift puff sass.hair vile)
          =^  toad  hide
            %=  $
              form  +>.form
              deep  (peg deep 7)
              hall  (peg hall 7)
              tend  [%next [%this puff %.n] look]
            ==
          =^  goad  hide
            %=  $
              form  +<.form
              deep  (peg deep 6)
              hall  (peg hall 6)
              tend  [%next [%this rift %.n] then.toad]
            ==
          (copy what.goad what.toad then.goad)
        ::
            %pick
          =^  hair  hide  sear
          =^  rile  hide  (emit %rile ~ %brn hair [zero once]:tend)
          $(tend [%next [%this hair %.n] rile])
        ::
            %done
          =^  salt  hide  sear
          =^  dyed  hide  (emit %dyed ~ %don salt)
          $(tend [%next [%this salt %.n] dyed])
        ==
      ==
    ::  Split a need for a cons operation
    ++  coal
      |=  $>(%next goal)
      ^-  [[need need bile] _hide] 
      ?-  -.what
          %both  [[left.what rite.what then] hide]
          %none  [[what what then] hide]
          %this
        =^  hand  hide  sear
        =^  root  hide  sear
        =^  char  hide  (emit %char ~[[%con hand root sass.what]] %hop then)
        :_  hide
        [[%this hand code.what] [%this root code.what] char]
      ==
    ::  Crash
    ++  bomb
      ^-  [$>(%next goal) _hide]
      =^  boom  hide  (emit %boom ~ %bom ~)
      :_  hide
      [%next [%none ~] boom]
    ::  Push down a need by an axis
    ++  take
      |=  [axe=@ what=need]
      ^-  need
      ?<  =(axe 0)
      |-
      ?:  =(axe 1)  what
      ?-  (cap axe)
        %2  [%both $(axe (mas axe)) %none ~]
        %3  [%both [%none ~] $(axe (mas axe))]
      ==
    ::  Split immediate noun to registers
    ++  hack
      |=  [thin=* what=need then=bile]
      ^-  [$>(%next goal) _hide]
      =/  tins
        |-  ^-  (unit (list pole))
        ?-  -.what
            %this  `~[[%imm thin sass.what]]
            %none  `~
            %both
          ?.  ?=(^ thin)  ~
          =/  lack  $(thin -.thin, what left.what)
          =/  rack  $(thin +.thin, what rite.what)
          ?~  lack  ~
          ?~  rack  ~
          `(weld u.lack u.rack)
        ==
      ?~  tins  bomb
      =^  heck  hide  (emit %heck u.tins %hop then)
      :_  hide
      [%next [%none ~] heck]
    ::  Combine two needs for sequential basic blocks (eagerly split
    ::  forks)
    ++  copy
      |=  [what=need that=need then=bile]
      ^-  [$>(%next goal) _hide]
      =^  [want=need tins=(list pole)]  hide
        |-  ^-  [[need (list pole)] _hide]
        ?:  ?=(%none -.what)  [[that ~] hide]
        ?:  ?=(%none -.that)  [[what ~] hide]
        ?:  ?=(%this -.what)
          ?:  ?=(%this -.that)
            ?:  =(sass.what sass.that)
              [[[%this sass.what ?|(code.what code.that)] ~] hide]
            [[[%this sass.what ?|(code.what code.that)] ~[[%mov sass.what sass.that]]] hide]
          ::  that is %both
          =^  left  hide  sear
          =^  raft  hide  sear
          =^  [lift=need loll=(list pole)]  hide  $(what [%this left code.what], that left.that)
          =^  [rift=need role=(list pole)]  hide  $(what [%this raft code.what], that rite.that)
          :_  hide
          [[%both lift rift] [(welp [[%con left raft sass.what] role] loll)]]
        ?:  ?=(%this -.that)
          =^  left  hide  sear
          =^  raft  hide  sear
          =^  [lift=need loll=(list pole)]  hide  $(that [%this left code.that], what left.what)
          =^  [rift=need role=(list pole)]  hide  $(that [%this raft code.that], what rite.what)
          :_  hide
          [[%both lift rift] [(welp [[%con left raft sass.that] role] loll)]]
        =^  [lift=need loll=(list pole)]  hide  $(what left.what, that left.that)
        =^  [rift=need role=(list pole)]  hide  $(what rite.what, that rite.that)
        :_  hide
        [[%both lift rift] (weld role loll)]
      =^  cozy  hide  (emit %cozy (flop tins) %hop then)
      [[%next want cozy] hide]
    ::  Combine two needs for conditional (split forks only inside the
    ::  proper conditional branch)
    ++  pony
      |=  [tall=$>(%next goal) fall=$>(%next goal)]
      ^-  [[need bile bile] _hide]
      =/  that  what.tall
      =/  phat  what.fall
      =^  [want=need tins=(list pole) fins=(list pole)]  hide
        |^
          ^-  [[need (list pole) (list pole)] _hide]
          ?:  ?=(%none -.that)  [[phat ~ ~] hide] :: WRONG, must put other side in a register
          ?:  ?=(%none -.phat)  [[that ~ ~] hide]
          ?:  ?=(%this -.that)
            ?:  ?=(%this -.phat)
              ?:  =(sass.that sass.phat)  [[that ~ ~] hide]
              [[that ~ ~[[%mov sass.that sass.phat]]] hide]
            =^  fins  hide  (chip sass.that phat)
            [[that ~ fins] hide]
          ?:  ?=(%this -.phat)
            =^  tins  hide  (chip sass.phat that)
            [[phat tins ~] hide]
          =^  [lift=need tilt=(list pole) fill=(list pole)]  hide
            $(that left.that, phat left.phat)
          =^  [rift=need rile=(list pole) file=(list pole)]  hide
            $(that rite.that, phat rite.phat)
          [[[%both lift rift] (weld tilt rile) (weld fill file)] hide]
        ++  chip
          |=  [sass=@ wish=need]
          ^-  [(list pole) _hide]
          =^  tins  hide
            |-
            ^-  [(unit (list pole)) _hide]
            ?-  -.wish
                %none  [~ hide]
                %this  [`~[[%mov sass sass.wish]] hide]
                %both
              =^  left  hide  sear
              =^  raft  hide  sear
              =^  milk  hide  $(sass left, wish left.wish)
              =^  miff  hide  $(sass raft, wish rite.wish)
              =/  lire=(list pole)  ?~  milk  ~  [[%hed sass left] u.milk]
              =/  rile=(list pole)  ?~  miff  ~  [[%tal sass raft] u.miff]
              ?:  ?&(=(~ lire) =(~ rile))  [~ hide]
              :_  hide
              `(weld lire rile)
            ==
          :_  hide
          ?~  tins  ~  u.tins
        --
      =^  does  hide  (emit %does tins %hop then.tall)
      =^  dont  hide  (emit %dont fins %hop then.fall)
      [[want does dont] hide]
    ::  Split a need at an axis, for edits
    ::  XX it looks like this is wrong
    ++  vise
      |=  [axe=@ wish=$>(%next goal)]
      ^-  [[need need bile] _hide]
      ?<  =(axe 0)
      =^  [muon=need hole=need tins=(list pole)]  hide
        =/  what  what.wish
        |-  ^-  [[need need (list pole)] _hide]
        ?:  =(1 axe)  [[[%none ~] what ~] hide]
        ?-  -.what
            %this
          =^  left  hide  sear
          =^  raft  hide  sear
          ?-  (cap axe)
              %2
            =^  [rude=need loll=need fins=(list pole)]  hide
              $(axe (mas axe), what [%this left code.what])
            :_  hide
            [[%both rude [%this raft code.what]] loll [[%con left raft sass.what] fins]]
          ::
              %3
            =^  [lead=need role=need fins=(list pole)]  hide
              $(axe (mas axe), what [%this raft code.what])
            :_  hide
            [[%both [%this left code.what] lead] role [[%con left raft sass.what] fins]]
          ==
        ::
            %both
          ?-  (cap axe)
              %2
            =^  [rude=need loll=need fins=(list pole)]  hide
              $(axe (mas axe), what left.what)
            :_  hide
            [[%both rude rite.what] loll fins]
          ::
              %3
            =^  [lead=need role=need fins=(list pole)]  hide
              $(axe (mas axe), what rite.what)
            :_  hide
            [[%both left.what lead] role fins]
          ==
        ::
            %none  [[what what ~] hide]
        ==
      =^  flaw  hide  (emit %flaw (flop tins) %hop then.wish)
      [[muon hole flaw] hide]
    --
  =^  [wish=bile sire=$>(%this need)]  hide  (divy what.dent then.dent)
  [then.dent what.dent wish sass.sire will.hide sans.hide] 
  |%
  ::  emit a code block
  ++  emit
    |=  [thus=@tas loch=blob]
    ^-  [bile _hide]
    =/  bell  [%bile deep hall thus]
    :-  bell
    hide(will (~(put by will.hide) bell loch))
  ::  New SSA
  ++  sear  [sans.hide hide(sans .+(sans.hide))] 
  ::  Split a single SSA to a need
  ++  divy
    |=  [what=need then=bile]
    ^-  [[bile $>(%this need)] _hide]
    =^  [sins=(unit $>(%this need)) dole=(list pole)]  hide
      |-  ^-  [[(unit $>(%this need)) (list pole)] _hide]
      ?-  -.what
          %this  [[`what ~] hide]
          %none  [[~ ~] hide]
          %both
        =^  mine  hide  sear
        =^  [lans=(unit $>(%this need)) loll=(list pole)]  hide
          $(what left.what)
        =^  [runs=(unit $>(%this need)) role=(list pole)]  hide
          $(what rite.what)
        :_  hide
        :-
          ?~  lans
            ?~  runs  ~
            `[%this mine code.u.runs]
          ?~  runs  `[%this mine code.u.lans]
          `[%this mine ?|(code.u.runs code.u.lans)]
        %:  welp
          ?~  lans  ~  [[%hed mine sass.u.lans] loll]
          ?~  runs  ~  [[%tal mine sass.u.runs] role]
        ==
      ==
    ?~  sins
      =^  crap  hide  sear
      [[then [%this crap %.n]] hide]
    =^  vide  hide  (emit %vide dole %hop then)
    [[vide u.sins] hide]
  --
::  pretty printing for codegen
++  pc
  |_  pile
  ::  pretty print a bile (internal label)
  ++  pale
    |=  bile
    ^-  tank
    :*  %rose
        ["." "" ""]
        ~[leaf+(scow %tas thus) leaf+(a-co:co deep) leaf+(a-co:co hall)]
    ==
  ::  pretty print a list of blocks by label
  ++  boss
    |=  prog=(list bile)
    ^-  tank
    [%rose [" " "" ""] `tang`(turn prog balk)]
  ::  pretty print a block by label
  ++  balk
    |=  elbe=bile
    ^-  tank
    =/  lump  (~(got by will) elbe)
    :~  %rose
        [": " "" ""]
        (pale elbe)
        [%rose [" " "" ""] (weld (turn body.lump pulp) ~[(beep bend.lump)])]
    ==
  :: pretty print a block internal instruction
  ++  pulp
    |=  tint=pole
    ^-  tank
    :*  %rose
        [" " "" ";"]
        `cord`-.tint
        ?-  -.tint
          %imm  ~[(cain !>(+<.tint)) (gist +>.tint)]
          %mov  ~[(gist +<.tint) (gist +>.tint)]
          %inc  ~[(gist +<.tint) (gist +>.tint)]
          %unc  ~[(gist +<.tint) (gist +>.tint)]
          %con  ~[(gist +<.tint) (gist +>-.tint) (gist +>+.tint)]
          %hed  ~[(gist +<.tint) (gist +>.tint)]
          %hud  ~[(gist +<.tint) (gist +>.tint)]
          %tal  ~[(gist +<.tint) (gist +>.tint)]
          %tul  ~[(gist +<.tint) (gist +>.tint)]
        ==
    ==
  ::  pretty print an external label
  ++  ring
    |=  bell
    ^-  tank
    leaf+"{<(mug text)>}-{<(mug form)>}"
  ++  corn
    |=  talk=(list @)
    ^-  tank
    :*  %rose
        ["," "<" ">"]
        (turn talk gist) 
    ==
  ::  pretty print a block-ending instruction
  ++  beep
    |=  sine=site
    ^-  tank
    :*  %rose
        [" " "" ";"]
        `cord`-.sine
        ?-  -.sine
          %clq  ~[(gist +<.sine) (pale +>-.sine) (pale +>+.sine)]
          %eqq  ~[(gist +<.sine) (gist +>-.sine) (pale +>+<.sine) (pale +>+>.sine)]
          %brn  ~[(gist +<.sine) (pale +>-.sine) (pale +>+.sine)]
          %hop  ~[(pale +.sine)]
          %lnk  ~[(gist +<.sine) (gist +>-.sine) (gist +>+<.sine) (pale +>+>.sine)]
          %cal  ~[(ring +<.sine) (corn +>-.sine) (gist +>+<.sine) (pale +>+>.sine)]
          %lnt  ~[(gist +<.sine) (gist +>.sine)]
          %jmp  ~[(ring +<.sine) (corn +>.sine)]
          %spy  ~[(gist +<.sine) (gist +>-.sine) (gist +>+<.sine) (pale +>+>.sine)]  
          %hin  ~[(gist +<.sine) (pale +>.sine)]
          %hun  ~[(pale +.sine)]
          %don  ~[(gist +.sine)]
          %bom  ~
        ==
    ==
  ::  pretty print a register
  ++  gist
    |=  r=@
    ^-  tank
    [%leaf 'r' (a-co:co r)]
  ::  pretty print linearized code
  ++  prod
    ^-  tang
    =/  flow
      =/  queu=[font=(list bile) back=(list bile)]  [~[wish] ~]
      =|  seen=(set bile)
      =|  flow=(list bile)
      |-  ^-  _flow
      ?~  font.queu
        ?~  back.queu  (flop flow)
        $(queu [(flop back.queu) ~])
      ?:  (~(has in seen) i.font.queu)  $(font.queu t.font.queu)
      =.  seen  (~(put in seen) i.font.queu)
      =.  flow  [i.font.queu flow]
      =/  rent  (~(got by will) i.font.queu)
      ?-  bend.rent
          [%clq @ a=bile b=bile]
        $(font.queu t.font.queu, back.queu (weld ~[a b]:bend.rent back.queu))
      ::
          [%eqq @ @ a=bile b=bile]
        $(font.queu t.font.queu, back.queu (weld ~[a b]:bend.rent back.queu))
      ::
          [%brn @ a=bile b=bile]
        $(font.queu t.font.queu, back.queu (weld ~[a b]:bend.rent back.queu))
      ::
          [%hop a=bile]  $(font.queu t.font.queu, back.queu [a:bend.rent back.queu])
          [%lnk @ @ @ a=bile]  $(font.queu t.font.queu, back.queu [a:bend.rent back.queu])
          [%cal * * @ a=bile]  $(font.queu t.font.queu, back.queu [a:bend.rent back.queu])
          [%lnt *]  $(font.queu t.font.queu)
          [%jmp *]  $(font.queu t.font.queu)
          [%spy @ @ @ a=bile]  $(font.queu t.font.queu, back.queu [a:bend.rent back.queu])
          [%hin @ a=bile]  $(font.queu t.font.queu, back.queu [a:bend.rent back.queu])
          [%hun a=bile]  $(font.queu t.font.queu, back.queu [a:bend.rent back.queu])
          [%don *]  $(font.queu t.font.queu)
          [%bom *]  $(font.queu t.font.queu)
      ==
    =/  dead
      %~  tap  in
      %-  ~(dif in ~(key by will))
      (~(gas in `(set bile)`~) flow)
    %-  flop
    ^-  tang
    :~  'entry points'
        (pale wish)
        (pale long)
        'live blocks'
        (boss flow)
        'dead blocks'
        (boss dead)
     ==
  -- 
++  skep
  =|  hide=[will=(map bile blob) sans=@ come=(jug bile bile) gist=(map bile pose)]
  =<
  |=  pile
  ^-  [(jug bile bile) pile]
  ~|  %unimplemented  !!
  |%
  ++  sear  [sans.hide hide(sans .+(sans.hide)]
  ++  emir
    |=  [here=bile gist=pose what=(list pole) then=site]
    ^-  hide
    =/  fore
      ?-  -.then
        %clq  ~[[+>-.then here] [+>+.then here]]
        %eqq  ~[[+>+<.then here] [+>+>.then here]]
        %brn  ~[[+>-.then here] [+>+.then here]]
        %hop  [+.then here]~
        %lnk  [+>+>.then here]~
        %cal  [+>+>.then here]~
        %lnt  ~
        %jmp  ~
        %spy  [+>+>.then here]~
        %hin  [+>.then here]~
        %hun  [+.then here]~
        %don  ~
        %bom  ~
      ==
    ~|  %unimplemented  ?>  %.f
    %=    hide
      will  (~(put by will.hide) here (flop what) then)
      come  (~(gas ju come.hide) fore)
    ==
  --
--
