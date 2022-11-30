/-  *sock
/-  *gene
/+  ska
|%
++  plot
  =|  ski=farm
  |=  ent=barn
  ^-  [boot farm]
  =/  ext  (~(get by yard.ski) ent)
  ?.  ?=(~ ext)  [says.u.ext ski]
  =;  [res=[does=nomm says=boot:ska] sku=farm]
    [says.res sku(yard (~(put by yard.sku) ent res), wood [ent wood.sku])]
  :: blackhole, guard recursion
  =.  ski  ski(yard (~(put by yard.ski) ent [[%zer 0 %.n] [%risk %toss ~]]))
  |-  ^-  [[does=nomm says=boot:ska] farm]
  =<
  ?+  for.ent  bomb
      [[* *] *]
    =^  [doth=nomm sath=boot:ska]  ski  $(for.ent -.for.ent)
    ?:  ?=([%boom ~] sath)  bomb
    =^  [toes=nomm tays=boot:ska]  ski  $(for.ent +.for.ent)
    ?:  ?=([%boom ~] tays)  bomb
    :_  ski
    :_  (cobb:ska sath tays)
    [%par doth toes]
  ::
      [%0 @]
    ::  we can decompose the axis into two axes, a safe axis which can
    ::  be implemented unchecked, and an unsafe axis which must be
    ::  checked. We then compose these two axes into safe %zer and
    ::  unsafe %zer composed by %sev
    =+  [saf rik ken]=(punt:ska +.for.ent sub.ent)
    ?:  =(0 saf)  bomb
    :_  ski
    ?:  =(1 rik)  [[%zer saf %.y] [%safe ken]]
    ?:  =(1 saf)  [[%zer rik %.n] [%risk ken]]
    :_  [%risk ken]
    [%sev [%zer saf %.y] [%zer rik %.n]]
      ::
      [%1 *]
    :_  ski
    :_  [%safe %know +.for.ent]
    [%one +.for.ent]
  ::
      [%2 * *]
    =^  [dost=nomm sass=boot:ska]  ski  $(for.ent +<.for.ent)
    ?:  ?=([%boom ~] sass)  bomb
    =^  [doff=nomm faff=boot:ska]  ski  $(for.ent +>.for.ent)
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
    =^  [deft=nomm koob=boot:ska]  ski  $(for.ent +.for.ent)
    ?:  ?=([%boom ~] koob)  bomb
    :_  ski
    :_  (ques:ska koob)
    [%thr deft]
  ::
      [%4 *]
    =^  [dink=nomm sink=boot:ska]  ski  $(for.ent +.for.ent)
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
    =^  [dome=nomm foam=boot:ska]  ski  $(for.ent +<.for.ent)
    ?:  ?=([%boom ~] foam)  bomb
    =^  [doot=nomm foot=boot:ska]  ski  $(for.ent +>.for.ent)
    ?:  ?=([%boom ~] foot)  bomb
    :_  ski
    :_  (bopp:ska foam foot)
    [%fiv dome doot]
  ::
      [%6 * * *]
    =^  [dawn=nomm sond=boot:ska]  ski  $(for.ent +<.for.ent)
    ?:  ?=([%safe %know %0] sond)  $(for.ent +>-.for.ent)
    ?:  ?=([%safe %know %1] sond)  $(for.ent +>+.for.ent)
    ?:  ?=([%safe %know *] sond)  bomb
    ?:  ?=([%safe %bets *] sond)  bomb
    ?:  ?=([%safe %flip ~] sond)
      =^  [drew=nomm slew=boot:ska]  ski  $(for.ent +>-.for.ent)
      =^  [darn=nomm song=boot:ska]  ski  $(for.ent +>+.for.ent)
      :_  ski
      :_  (gnaw:ska slew song)
      [%six dawn drew darn]
    ?:  ?=([%risk %know %0] sond)
      =^  [drew=nomm slew=boot:ska]  ski  $(for.ent +>-.for.ent)
      :_  ski
      :_  (dare:ska slew)
      ::  run dawn in case it crashes, but throw it away
      [%sev [%par dawn drew] [%zer 3 %.y]]
    ?:  ?=([%risk %know %1] sond)
      =^  [darn=nomm song=boot:ska]  ski  $(for.ent +>+.for.ent)
      :_  ski
      :_  (dare:ska song)
      ::  run dawn in case it crashes, but throw it away
      [%sev [%par dawn darn] [%zer 3 %.y]]
    ?:  ?=([%risk %know *] sond)  bomb
    ?:  ?=([%risk %bets *] sond)  bomb
    =^  [drew=nomm slew=boot:ska]  ski  $(for.ent +>-.for.ent)
    =^  [darn=nomm song=boot:ska]  ski  $(for.ent +>+.for.ent)
    :_  ski
    :_  (dare:ska (gnaw:ska slew song))
    [%six dawn drew darn]
  ::
      [%7 * *]
    =^  [deck=nomm keck=boot:ska]  ski  $(for.ent +<.for.ent)
    ?:  ?=([%boom ~] keck)  bomb
    =/  news
      ?-  keck
        [%safe *]  sure.keck
        [%risk *]  hope.keck
      ==
    =^  [dest=nomm zest=boot:ska]  ski  $(sub.ent news, for.ent +>.for.ent)
    ?:  ?=([%boom ~] zest)  bomb
    :_  ski
    :_  ?:  ?=([%safe *] keck)  zest  (dare:ska zest)
    [%sev deck dest]
  ::
      [%8 * *]
    =^  [pink=nomm pest=boot:ska]  ski  $(for.ent +<.for.ent)
    ?:  ?=([%boom ~] pest)  bomb
    =/  nest
      ?-  pest
        [%safe *]  sure.pest
        [%risk *]  hope.pest
      ==
    =^  [dest=nomm zest=boot:ska]  ski
      $(sub.ent (knit:ska nest sub.ent), for.ent +>.for.ent)
    ?:  ?=([%boom ~] zest)  bomb
    :_  ski
    :_  ?:  ?=([%safe *] pest)
          zest
        (dare:ska zest)
    [%eig pink dest]
  ::
      [%9 @ *]
    =^  [lore=nomm sore=boot:ska]  ski  $(for.ent +>.for.ent)
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
      :_  ?:  ?=([%safe *] sore)
            ret
          (dare:ska ret)
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
    =^  [neat=nomm seat=boot:ska]  ski  $(for.ent +>.for.ent)
    ?:  ?=([%boom ~] seat)  bomb
    =^  [pace=nomm spat=boot:ska]  ski  $(for.ent +<+.for.ent)
    ?:  ?=([%boom ~] spat)  bomb
    =/  teak
      ?-  seat
        [%safe *]  sure.seat
        [%risk *]  hope.seat
      ==
    =+  [saf rik ken]=(punt:ska +<-.for.ent teak)
    ?:  =(0 saf)  bomb
    :_  ski
    :_  (welt:ska +<-.for.ent spat seat)
    ?:  =(1 rik)
      [%ten [+<-.for.ent pace] neat %.y]
    ^-  nomm
    :+  %sev  [%par neat pace]
    :+  %ten
      [saf %ten [rik %zer 3 %.n] [%zer (peg saf 2) %.y] %.y]
    [[%zer 2 %.y] %.y]
  ::
      [%11 @ *]
    =^  [real=nomm seal=boot:ska]  ski  $(for.ent +>.for.ent)
    ?:  ?=([%boom ~] seal)  bomb
    ^-  [[does=nomm says=boot:ska] farm]
    [[[%els +<.for.ent real] seal] ski]
  ::
      [%11 [@ *] *]
    =^  [fake=nomm sake=boot:ska]  ski  $(for.ent +<+.for.ent)
    ?:  ?=([%boom ~] sake)  bomb
    =^  [real=nomm seal=boot:ska]  ski  $(for.ent +>.for.ent)
    ?:  ?=([%boom ~] seal)  bomb
    :_  ski
    ?:  ?=([%safe *] sake)
      [[%eld [+<-.for.ent fake] real %.y] seal]
    [[%eld [+<-.for.ent fake] real %.n] seal]
  ::
      [%12 * *]
    =^  [fear=nomm sear=boot:ska]  ski  $(for.ent +<.for.ent)
    ?:  ?=([%boom ~] sear)  bomb
    =^  [pack=nomm sack=boot:ska]  ski  $(for.ent +>.for.ent)
    ?:  ?=([%boom ~] sack)  bomb
    :_  ski
    :_  [%risk %toss ~]
    [%twe fear pack]
  ==
  |%
  ++  bomb
    ^-  [[nomm boot:ska] farm]
    [[[%zer 0 %.n] [%boom ~]] ski]
  --
++  till
  =|  burg=town
  |=  =farm
  ^-  town
  =/  work  (flop (skip wood.farm ~(has in ~(key by land.burg))))
  |-  ^+  burg
  ?~  work  burg
  =/  next  i.work
  =+  ~|  %next-miss  (~(got by yard.farm) next)
  ::  now we have the nock-- in does
  =/  dock  [lamb=lamb.burg lake=*lake]
  =|  flow=line
  =/  axle=@  1
  =/  fawn  does
  =-  =.  lamb.burg  lamb.dock
      =.  land.burg
        %+  ~(put by land.burg)  next
        :_  says
        :-  (~(put by lake.dock) ~ [~ %hop her])
        |-  ^-  (list @)
        ?-  -.hat
          %fork  (weld $(hat left.hat) $(hat rite.hat))
          %tine  [+.hat]~
          %disc  ~
        ==
      $(work t.work)
  |^  ^-  [[hat=plow her=berm] dock=_dock]
  ?-    fawn
      [%par * *]
    =^  [one=plow two=plow her=berm]  dock  twin
    =^  [bat=plow bit=berm]  dock
      $(fawn +>.fawn, axle (peg axle 3), flow [%moat her two])
    =^  [hat=plow hit=berm]  dock
      $(fawn +<.fawn, axle (peg axle 2), flow [%moat bit one])
    (copy hat bat hit)
  ::
      [%zer *]
    ?-    -.flow
        %moat
      =/  slow  (take +<.fawn what.flow +>.fawn)
      ?~  slow
        fail
      :_  dock
      [u.slow wher.flow]
    ::
        %rift
      =^  miff  dock  wean
      =/  slow  (take +<.fawn [%tine miff] +>.fawn)
      ?~  slow
        fail
      =^  her  dock  (mend %miff ~ [%brn miff [troo fals]:flow])
      :_  dock
      [u.slow her]
    ::
        %pond
      =^  tend  dock  wean
      =/  slow  (take +<.fawn [%tine tend] +>.fawn)
      ?~  slow
        fail
      =^  her  dock  (mend %tend ~ [%don tend])
      :_  dock
      [u.slow her]
    ==
  ::
      [%one *]
    (bang +.fawn)
  ::
      [%two *]
    ~|  %todo  !!
  ::
      [%thr *]
    ?-    -.flow
        %moat
      ?-    -.what.flow
          %fork  fail
          %disc  $(fawn +.fawn, axle (peg axle 3))
          %tine
        =^  pear  dock  (mend %pear [%imm 0 +.what.flow]~ [%hop wher.flow])
        =^  bock  dock  (mend %bock [%imm 1 +.what.flow]~ [%hop wher.flow])
        =^  noon  dock  wean
        =^  keck  dock  (mend %keck ~ [%clq noon pear bock])
        $(fawn +.fawn, axle (peg axle 3), flow [%moat keck [%tine noon]])
      ==
    ::
        %rift
      =^  noon  dock  wean
      =^  keck  dock  (mend %keck ~ [%clq noon [troo fals]:flow])
      $(fawn +.fawn, axle (peg axle 3), flow [%moat keck [%tine noon]])
    ::
        %pond
      =^  tend  dock  wean
      =^  pear  dock  (mend %pear [%imm 0 tend]~ [%don tend])
      =^  bock  dock  (mend %bock [%imm 1 tend]~ [%don tend])
      =^  noon  dock  wean
      =^  keck  dock  (mend %keck ~ [%clq noon pear bock])
      $(fawn +.fawn, axle (peg axle 3), flow [%moat keck [%tine noon]])
    ==
  ::
      [%sev *]
    =^  [hit=plow his=berm]  dock  $(fawn +>.fawn, axle (peg axle 7))
    $(fawn +<.fawn, axle (peg axle 6), flow [%moat his hit])
  ::
      *
    ~|  %todo  !!
  ==
  ++  fail
    ^-  [[hat=plow her=berm] dock=_dock]
    =^  hole  dock  bomb
    :_  dock
    [[%disc ~] hole]
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
      :: XX maybe we should assert that SKA should have caught this?
      =^  hole  dock  bomb
      :_  dock
      [[%disc ~] hole]
    ::
        [%moat *]
      =/  what  what.flow
      =/  mitt
        |-  ^-  (unit (list bran))
        ?-    what
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
    ^-  (unit plow)  :: nullary case = crash
    ?:  =(0 sax)  ~
    %-  some
    |-  ^-  plow
    ?:  =(1 sax)  tow
    ?-  (cap sax)
      %2  [%fork $(sax (mas sax)) [%disc ~] row]
      %3  [%fork [%disc ~] $(sax (mas sax)) row]
    ==
  ++  milk  :: local label
    |=  gen=@
    ^-  berm
    ~!  next
    [sub.next for.next axle gen]
  ++  wean  :: fresh ssa
    ^-  [@ _dock]
    [lamb.dock dock(lamb .+(lamb.dock))]
  ++  copy  :: replicate values to two destinations
    |=  [hat=plow bat=plow her=berm]
    ^-  [[hat=plow her=berm] _dock]
    =^  [tog=plow moot=(list bran)]  dock
      |-
      ^-  [[tog=plow moot=(list bran)] _dock]
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
          =^  [hog=plow hoot=(list bran)]  dock
            $(hat [%tine one], bat left.bat)
          =^  [log=plow loot=(list bran)]  dock
            $(hat [%tine two], bat rite.bat)
          :_  dock
          :-  ^-  plow
              [%fork hog log safe.bat]
          [`bran`[%con one two +.hat] (weld hoot loot)]
        ==
          ::
          [%fork *]
        ?-  bat
            [%tine @]
          =^  one  dock  wean
          =^  two  dock  wean
          =^  [hog=plow hoot=(list bran)]  dock
            $(hat left.hat, bat [%tine one])
          =^  [log=plow loot=(list bran)]  dock
            $(hat rite.hat, bat [%tine two])
          :_  dock
          [[%fork hog log safe.hat] [%con one two +.bat] (weld hoot loot)]
        ::
            [%fork *]
          =^  [hog=plow hoot=(list bran)]  dock  $(hat left.hat, bat left.bat)
          =^  [log=plow loot=(list bran)]  dock  $(hat rite.hat, bat rite.bat)
          :_  dock
          [[%fork hog log ?&(safe.hat safe.bat)] (weld hoot loot)]
        ==
      ==
    =/  blab  (milk %copy)
    :_  dock(lake (~(put by lake.dock) `blab [moot %hop her]))
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
      ::
          [%tine @]
        =^  one  dock  wean
        =^  two  dock  wean
        =^  her  dock
          (mend %cons ~[[%con one two +.what.flow]] [%hop wher.flow])
        :_  dock
        [[%tine one] [%tine two] her]
      ==
    ==
  ++  mend
    |=  [gen=@ =lock]
    ^-  [berm _dock]
    =/  curb  (milk gen)
    :-  curb
    dock(lake (~(put by lake.dock) `curb lock))
  ++  bomb
    ^-  [berm _dock]
    (mend %boom ~ [%bom ~])
  --
--
