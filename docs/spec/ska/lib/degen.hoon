/+  ska
/-  *sock
/-  *gene
|%
++  plot
  =|  ski=farm
  |=  ent=barn
  ^-  [boot farm]
  =/  ext  (~(get by ski) ent)
  ?.  ?=(~ ext)  [says.u.ext ski]
  =;  [res sku]  [says.res sku(yard (~(put by yard.sku) ent res), wood [ent wood.sku]]
  =.  ski  ski(yard (~(put by yard.ski) ent [[%zer 0 %.n] [%risk %toss ~]])) :: blackhole, guard recursion
  |-
  ^-  [[does=nomm says=boot:ska] farm]
  =<
  ?+  for.ent  bomb
      [[* *] *]
    =^  [doth sath]  ski  $(for.ent -.for.ent)
    ?:  ?=([%boom ~] sath)  bomb
    =^  [toes tays]  ski  $(for.ent +.for.ent)
    ?:  ?=([%boom ~] tays)  bomb
    :_  ski
    :_  (cobb:ska sath tays)
    [%par doth sath]
      ::
      [%0 @]
    ::  we can decompose the axis into two axes, a safe axis which can
    ::  be implemented unchecked, and an unsafe axis which must be
    ::  checked. We then compose these two axes into safe %zer and
    ::  unsafe %zer composed by %sev
    =/  [saf rik ken]  (punt:ska +.for.ent sub.ent)
    ?:  =(0 saf)  bomb
    ?:  =(1 rik)  [[%zer saf %.y] [%safe ken]]
    ?:  =(1 saf)  [[%zer rik %.n] [%risk ken]] 
    :_  ski
    :_  [%risk ken]
    [%sev [%zer saf %.y] [%zer rik %.n]]
      ::
      [%1 *]
    :_  ski
    :_  [%safe %know +.for.ent]
    [%one +.for.ent]
      ::
      [%2 * *]
    =^  [dost sass]  ski  $(for.ent +<.for.ent)
    ?:  ?=([%boom ~] sass)  bomb
    =^  [doff faff]  ski  $(for.ent +>.for.ent)
    ?:  ?=([%boom ~] faff)  bomb
    =/  skun
      ?-  sass
        [%safe *]  sure.sass
        [%risk *]  hope.sass
      ==
    ?:  ?=([%safe %know *] faff)
      =^  ret  ski  ^$(ent [skun know.sure.faff])
      :_  ski
      :_  ?:  ?=([%safe *] sass)  ret  (dare:ska ret)
      [%two dost doff skun (some know.sure.faff) %.y]
    ?:  ?=([%risk %know *] faff)
      =^  ret  ski  ^$(ent [skun know.hope.faff])
      :_  ski
      :_  (dare:ska ret)
      [%two dost doff skun (some know.hope.faff) %.n]
    :_  ski
    :_  [%risk %toss ~]
    [%two dost doff skun ~ %.n]
      ::
      [%3 *]
    =^  [deft koob]  ski  $(for.ent +.for.ent)
    ?:  ?=([%boom ~] koob)  bomb
    :_  ski
    :_  (ques:ska koob)
    [%thr deft]
      ::
      [%4 *]
    =^  [dink sink]  ski  $(for.ent +.for.ent)
    ?:  ?=([%boom ~] sink)  bomb
    =/  rink
      ?-  sink
        [%safe *]  sure.sink
        [%risk *]  hope.sink
      ==
    :_  ski
    :_  (pile:ska sink)
    [%fou dink ?|(?=([%dice ~] rink) ?=([%flip ~] rink) ?=([%know @] rink))]
      ::
      [%5 * *]
    =^  [dome foam]  ski  $(for.ent +<.for.ent)
    ?:  ?=([%boom ~] foam)  bomb
    =^  [doot foot]  ski  $(for.ent +>.for.ent)
    ?:  ?=([%boom ~] foot)  bomb
    :_  ski
    :_  (bopp:ska foam foot)
    [%fiv dome doot]
      ::
      [%6 * * *]
    =^  [dawn sond]  ski  $(for.ent +<.for.ent)
    ?:  ?=([%safe %know %0] sond)  $(for.ent +>-.for.ent)
    ?:  ?=([%safe %know %1] sond)  $(for.ent +>+.for.ent)
    ?:  ?=([%safe %know *] sond) bomb
    ?:  ?=([%safe %bets *] sond) bomb
    ?:  ?=([%safe %flip ~] sond)
      =^  [drew slew]  ski  $(for.ent +>-.for.ent)
      =^  [darn song]  ski  $(for.ent +>+.for.ent)
      :_  ski
      :_  (gnaw:ska slew song)
      [%six dawn slew song]
    ?:  ?=([%risk %know %0] sond)
      =^  [drew slew]  ski  $(for.ent +>-.for.ent)
      :_  ski
      :_  (dare:ska slew)
      [%sev [dawn drew] [%zer 3 %.y]]
    ?:  ?=([%risk %know %1] sond)
      =^  [darn song]  ski  $(for.ent +>+.for.ent)
      :_  ski
      :_  (dare:ska song)
      [%sev [dawn darn] [%zer 3 %.y]]
    ?:  ?=([%risk %know *] sond) bomb
    ?:  ?=([%risk %bets *] sond) bomb
    =^  [drew slew]  ski  $(for.ent +>-.for.ent)
    =^  [darn song]  ski  $(for.ent +>+.for.ent)
    :_  ski
    :_  (dare:ska (gnaw:ska slew song))
    [%six dawn drew darn]
      ::
      [%7 * *]
    =^  [deck keck]  ski  $(for.ent +<.for.ent)
    ?:  ?=([%boom ~] keck)  bomb
    =/  news
      ?-  keck
        [%safe *]  sure.keck
        [%risk *]  hope.keck
      ==
    =^  [dest zest]  ski  $(sub.ent news, for.ent +>.for.ent)
    ?:  ?=([%boom ~] zest)  bomb
    :_  ski
    :_  ?:  ?=([%safe *] keck)  zest  (dare:ska zest)
    [%sev deck dest]
      ::
      [%8 * *]
    =^  [pink pest]  ski  $(for.ent +<.for.ent)
    ?:  ?=([%boom ~] pest)  bomb
    =/  nest
      ?-  pest
        [%safe *]  sure.pest
        [%risk *]  hope.pest
      ==
    =^  [dest zest]  ski  $(sub.ent (knit nest sub.ent), for.ent +>.for.ent)
    ?:  ?=([%boom ~] zest)  bomb
    :_  ski
    :_  ?:  ?=([%safe *] pest)  zest)  (dare:ska zest)
    [%eig pink dest]
      ::
      [%9 @ *]
    =^  [lore sore]  ski  $(for.ent +>.for.ent)
    ?:  ?=([%boom ~] sore)  bomb
    =/  news
      ?-  sore
        [%safe *]  sure.sore
        [%risk *]  hope.sore
      ==
    =/  fork  (pull:ska +<.for.ent news)
    ?:  ?=([%safe %know *] fork)
      =^  ret  ski  ^$(ent [news know.sure.fork])
      :_  ski
      :_  ?:  ?=([%safe *] sass)  ret  (dare:ska ret)
      [%nin +<.for.ent lore news (some know.sure.fork) %.y]
    ?:  ?=([%risk %know *] fork)
      =^  ret  ski  ^$(ent [news know.hope.fork])
      :_  ski
      :_  (dare:ska ret)
      [%nin +<.for.ent lore news (some know.hope.fork) %.n]
    :_  ski
    :_  [%risk %toss ~]
    [%nin +<.for.ent lore news ~ ?=([%safe *] fork)]
      ::
      [%10 [@ *] *]
    =^  [neat seat]  ski  $(for.ent +>.for.ent)
    ?:  ?=([%boom ~] seat)  bomb
    =^  [pace spat]  ski  $(for.ent +<+.for.ent)
    ?:  ?=([%boom ~] spat)  bomb
    =/  teak
      ?-  seat
        [%safe *]  sure.seat
        [%risk *]  hope.seat
      ==
    =/  [saf rik ken]  (punt +<-.for.ent teak)
    ?:  =(0 saf)  bomb
    ?:  =(1 rik)
      :_  [(welt +<-.for.ent spat seat) ski]
      :_  [%ten [+<-.for.ent pace] neat %.y]
    :_  [(welt +<-.for.ent spat seat) ski]
    :_  [%sev [neat pace] %ten [saf %ten [rik %zer 3 %.n] [%zer (peg 2 saf) %.y] %.y] [%zer 2 %.y] %.y]
      ::
      [%11 @ *]
    =^  [real seal]  ski  $(for.ent +>.for.ent)
    ?:  ?=([%boom ~] seal)  bomb
    :_  [seal ski]
    :_  [%els +<.for.ent real]
      ::
      [%11 [@ *] *]
    =^  [fake sake]  ski  $(for.ent +<+.for.ent)
    ?:  ?=([%boom ~] sake)  bomb
    =^  [real seal]  ski  $(for.ent +>.for.ent)
    ?:  ?=([%boom ~] seal)  bomb
    :_  ski
    ?:  ?=([%safe *] sake)
      [[%eld [+<-.for.ent fake] real %.y] seal]
    [[%eld [+<-.for.ent fake] real %.n] seal]
      ::
      [%12 * *]
    =^  [fear sear]  ski  $(for.ent +<.for.ent)
    ?:  ?=([%boom ~] sear)  bomb
    =^  [pack sack]  ski  $(for.ent +>.for.ent)
    ?:  ?=([%boom ~] sack)  bomb
    :_  [[%risk %toss ~] ski]
    [%twe fear pack]
  ==
  |%
  ++  bomb  [[%zer 0 %.n] [%boom ~] ski]
  --
++  till
  =|  burg=town
  |=  =farm
  ^-  town
  =/  work  (flop (skip wood.farm ~(has in ~(key by burg))))
  =<  burg
  |-
  ^-  _this
  ?:  =(~ work)  this
  =^  next  work  work
  =+  ~|  %next-miss  (~(got by yard.farm) next)
  ::  now we have the nock-- in does
  =/  dock  [lamb=lamb.burg lake=*lake]
  =|  flow=line
  =/  axle=@  1
  =/  fawn=does
  =+
    =<
    |-
    ^-  [[hat=sans her=berm] dock]
    ?-  fawn
        [%par * *]
      =^  [one two her]  dock  twin
      =^  [bat bit]  dock  $(fawn +>.fawn, axle (peg axle 3), flow [%dab her two])
      =^  [hat hit]  dock  $(fawn +<.fawn, axle (peg axle 2), flow [%dab bit one])
      (copy hat bat hit)
        ::
        [%zer *]
      ?-  flow
        [%moat *]
      =/  slow  (take +<.fawn what.flow +>.fawn)
      ?~  slow
        =^  hole  dock  bomb
        :_  dock
        [[%disc ~] hole]
      :_  dock
      [u.slow wher.flow]
        ::
        [%one *]
      (bang +.fawn) 
        ::
        [%two *]
      ~|  %todo  !!
        ::
    ==
    |%
    ++  bang
      |=  non=*
      ^-  [[hat=plow her=berm] _dock]
      ?-  flow
          [%pond ~]
        =^  ret  dock  wean
        =^  her  dock  (mend %rime ~[[%imm +.fawn ret]] [%don ret])
        :_  dock
        [[%disc ~] her]
          ::
          [%rift *]
        ?:  =(0 +.fawn)  [[[%disc ~] troo.flow] dock]
        ?:  =(1 +.fawn)  [[[%disc ~] fals.flow] dock]
        =^  hole  dock  bomb  :: XX maybe we should assert that SKA should have caught this?
        :_  dock
        [[%disc ~] hole]
          ::
          [%moat *]
        =/  what  what.flow
        =/  mitt
          |-
          ^-  (unit (list bran))
          ?-  what
              [%disc ~]
            (some ~)
              ::
              [%tine @]
            (some ~[[%imm non +.what]])
              ::
              [%fork *]
            ?@  non
              ?:  safe.what
                ~|  %safe-axis-atom  !!
              ~
            (clap $(what left.what, non -.non) $(what rite.what, non +.non) weld)
          ==
        ?~  mitt
          =^  hole  dock  bomb
          :_  dock
          [[%disc ~] hole]
        =^  rock  dock  (mend %toil u.mitt [%hop wher.flow])
        :_  dock
        [[%disc ~] rock]
      ==
    ++  take  :: axis
      |=  [sax=@ tow=plow row=?]  :: axis, destination, safety
      ^-  plow  :: nullary case = crash
      ?:  =(0 sax)  ~
      %-  some
      |-
      ?:  =(1 sax)  tow
      ?-  (cap sax)
        %2  [%fork $(sax (mas sax)) [%disc ~] row]
        %3  [%fork [%disc ~] $(sax (mas sax)) row]
      ==
    ++  milk  :: local label
      |=  gen=@
      ^-  berm
      [sub.does for.does axle gen]
    ++  wean  :: fresh ssa
      ^-  [@ _dock]
      [lamb.dock dock(lamb .+(lamb.dock))]
    ++  copy  :: replicate values to two destinations
      |=  [hat=plow bat=plow her=berm]
      ^-  [[hat=plow her=berm] _dock]
      =^  [tog moot]  dock
        |-
        ^-  [[tog=plow moot=(list tins)] _dock]
        ?:  ?=([%disc ~] hat)  [[bat ~] dock]
        ?:  ?=([%disc ~] bat)  [[hat ~] dock]
        ?-  hat
            [%tine @]
          ?-  bat
              [%tine @]
            ?:  =(+.hat +.bat)
              [[hat ~] dock]
            [[hat ~[[%mov +.hat +.bat]]] dock]
              ::
              [%fork *]
            =^  one  dock  wean
            =^  two  dock  wean
            =^  [hog hoot]  dock  $(hat [%tine one], bat hed.bat)
            =^  [log loot]  dock  $(hat [%tine two], bat tal.bat)
            [[[%fork hog log safe.bat] [[%con one two +.hat] (weld hoot loot)]] dock]
          ==
            ::
            [%fork *]
          ?-  bat
              [%tine @]
            =^  one  dock  wean
            =^  two  dock  wean
            =^  [hog hoot]  dock  $(hat hed.hat, bat [%ssa one])
            =^  [log loot]  dock  $(hat tal.hat, bat [%ssa two])
            [[[%fork hog log safe.hat] [[%con one two +.bat] (weld hoot loot)]] dock]
              ::
              [%fork *]
            =^  [hog hoot]  dock  $(hat hed.hat, bat hed.bat)
            =^  [log loot]  dock  $(hat tal.hat, bat tal.bat)
            [[%fork hog log ?&(safe.hat safe.bat)] (weld hoot loot)] dock]
          ==
        ==
      =/  blab  (milk %copy)
      :_  dock(pond (~(put by pond) blab [moot %hop her]))
      [tog blab]
    ++  twin  :: split sans from flow
      ^-  [[plow plow berm] _dock]
      ?-  flow
          [%rift *]
        =^  hole  dock  bomb
        :_  dock
        [[%disc ~] [%disc ~] hole]
          ::
          [%pond ~]
        =^  one  dock  wean
        =^  two  dock  wean
        =^  ret  dock  wean
        =^  her  dock  (mend %taco ~[[%con one two ret]] [%don ret])
        :_  dock
        [[%tine one] [%tine two] her]
          ::
          [%moat *]
        ?-  what.flow
            [%fork *]
          :_  dock
          [left.what.flow rite.what.flow wher.flow]
            ::
            [%disc ~]
          :_  dock
          [[%disc ~] [%disc ~] wher.flow]
            [%tine @]
          :_  dock
          =^  one  dock  wean
          =^  two  dock  wean
          =^  her  dock  (mend %cons ~[[%con one two +.what.flow]] [%hop wher.flow])
          :_  dock
          [[%tine one] [%tine two] her]
        ==
      ==
    ++  mend
      |=  [gen=@ =lock]
      ^-  [berm _dock]
      =/  curb  (milk gen)
      :-  curb
      dock(pond (~(put by pond.dock) berm lock))
    ++  bomb
      ^-  [berm _dock]
      (mend %boom ~ [%bom ~])
    --
--
