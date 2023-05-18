/-  *sock
/-  *noir
/+  ska
::  XX radically simplify socks by representing as [cape *], boots as
::  (unit [cape *])
=>
::  stdlib add-ons
|%
++  pin
  |=  [a=@ b=@]
  ?<  =(0 a)
  ?<  =(0 b)
  ^-  @
  =(a (rsh [1 (sub (met 0 b) (met 0 a))] b))
--
=>
::  analysis datastructures
|%
::  constraints
+$  mist  (map @hail rain)
::  reverse dependencies
+$  muck  (jug @hail @hail)
::  analysis results (hypotheses)
+$  mire  (map @hail sock)
::  formula variables
+$  haar  (set @hail)
::  call result variables
+$  silt  (set @hail)
::  mask
+$  cape  $@(? ^)
::  battery masks
+$  snow  (map @hail cape)
::  abstract stack entry
+$  fame  (jug * @hail)
::  compiled nomm
+$  mitt  (map [@hail @hail] nomm)
--
::  SACK: Subject-oriented Analysis of Constraints and Knowledge
=|  seam=[=mist =muck =mire =haar =silt hail=@hail]
|%
::  sack a subject and formula
++  raid
  |=  [soup=sock form=*]
  ^-  _seam
  =|  tack=fame
  =^  sail  seam  melt
  =^  dial  seam  melt
  =^  salt  seam  melt
  =.  mire.seam  (~(gas by mire.seam) ~[[sail soup] [dial [%know form]])
  =.  haar.seam  (~(put by haar.seam) dial)
  =.  silt.seam  (~(put by silt.seam) salt)
  |-  ^-  _seam
  =.  seam  (ruse sail dial tack salt)
  =.  seam  guns
  =^  maam  seam  club
  ?~  maam  seam
  $(sail sail.u.maam, dial dial.u.maam, tack tack.u.maam, salt -.u.maam)
::  generate nomm, constraints, and hypotheses for an arm
++  ruse
  |=  [sail=@hail dial=@hail tack=fame salt=@hail]
  =/  sofa  (~(gut by mire.seam) dial)
  ?>  ?=(%know -.sofa) :: don't call ruse on an unknown dial!
  =/  form  know.sofa
  =.  tack  (~(put ju tack) form sail) :: update abstract stack for any calls we make
  |^  ^-  _seam
    =^  [ire=nomm ale=@hail]  seam  fell 
    =.  mitt.seam  (~(put by mitt.seam) [sail dial] ire)
    (gain salt [%thus ale])
  ++  fell
    ^-  [[nomm @hail] _seam]
    ?+  form  bomb
        [left=^ rite=*]
      =^  [lire=nomm lice=@hail]  seam  fell(form left.form)
      =^  [rise=nomm rice=@hail]  seam  fell(form rite.form)
      =^  mice  seam  (game [%cons lice rice])
      [[[%par lire rise] mice] seam]
    ::
        [%0 axe=@]
      =/  soup  (hast sail)
      =/  nein  (cull axe.form soup)
      ?-  -.nein
          %|
        =^  mice  seam  (game [%neat axe.form sail])
        [[[%not axe.form] mice] seam]
      ::
          %&
        =^  moat  seam  (game [%neat crax.form sail])
        =^  mice  seam  (game [%bomb moat])
        [[[%not 0] mice] seam]
      ==
    ::
        [%1 tis=*]
      =^  mice  seam  (game [%mede tis.form])
      [[[%one tis] mice] seam]
    ::
        [%2 sumo=* cafe=*]
      =^  [sums=nomm seal=@hail]  seam  $(form sumo.form)
      =^  [from=nomm fair=@hail]  seam  $(form cafe.form)
      =^  salt  seam  (game [%call seal fair tack])
      =.  haar.seam  (~(put in haar.seam) fair)
      =.  silt.seam  (~(put in silt.seam) salt)
      [[[%two sums seal from fair] salt] seam]
    ::
        [%3 seat=*]
      =^  [sate=nomm sell=@hail]  seam  $(form seat.form)
      =^  char  seam  (game [%eats sell])
      [[[%the sate] char] seam]
    ::
        [%4 sink=*]
      =^  [sunk=nomm sill=@hail]  seam  $(form sink.form)
      =^  more  seam  (game [%meat sill])
      [[[%for sunk] more] seam]
    ::
        [%5 one=* two=*]
      =^  [lump=nomm late=@hail]  seam  $(form one.form)
      =^  [rump=nomm rate=@hail]  seam  $(form two.form)
      =^  bale  seam  (game [%eels late rate])
      [[[%ivy lump rump] bale] seam]
    ::
        [%6 what=* then=* else=*]
      =^  [when=nomm wait=@hail]  seam  $(form what.form)
      =^  [thus=nomm tale=@hail]  seam  $(form then.form)
      =^  [even=nomm fate=@hail]  seam  $(form else.form)
      =^  cool  seam  (game [%both thus even])
      [[[%six when thus even] cool] seam]
    ::
        [%7 with=* that=*]
      =^  [wine=nomm wait=@hail]  seam  $(form with.form)
      =^  [tile=nomm tale=@hail]  seam  $(sail wait, form that.form)
      [[[%eve wine tile] tale] seam]
    ::
        [%8 whip=* that=*]
      $(form [%7 [whip %0 1] that]
    ::
        [%9 fax=@ that=*]
      $(form [%7 that %2 [%0 1] %0 fax]
    ::
        [%10 [axe=@ limp=*] trip=*]
      =^  [limb=nomm late=@hail]  seam  $(form limp.form)
      =^  [tree=nomm tale=@hail]  seam  $(form trip.form)
      =^  diet  seam  (game [%edit axe.form late tale])
      [[[%ten axe.form limb tree] diet] seam]
    ::
        [%11 tag=@ real=*]
      =^  [rump=nomm hump=@hail]  seam  $(form real.form)
      [[[%sip tag rump] hump] seam]
    ::
        [%11 [tag=@ fake=*] real=*]
      =^  [ramp=nomm rail=@hail]  seam  $(form fake.form)
      =^  [rump=nomm hump=@hail]  seam  $(form real.form)
      =^  bump  seam  (game [%hint rail hump])
      [[[%tip tag ramp rump] bump] seam]
    ::
        [%12 cafe=* pass=*]
      =^  [camp=nomm cane=@hail]  seam  $(form cafe.form)
      =^  [pump=nomm pain=@hail]  seam  $(form pass.form)
      =^  kite  seam  (game [%scry cane pain])
      [[[%elf camp pump] kite] seam]
    ==
  ++  hast
    |=  ale=@hail
    ^-  sock
    (~(gut by mire.seam) ale [%toss ~])
  ++  bomb  ~|  %todo  !!  :: generate a crash variable
  ++  gain  ~|  %todo  !!  :: add a constraint
  ++  game  ~|  %todo  !!  :: generate a variable and constraint
  --
::  check for non-recursive direct calls
++  club
  ^-  [(unit [@hail $>(%call rain)]) _seam]
  =/  fore  ~(tap in silt)
  |-  ^-  [(unit [@hail $>(%call rain)]) _seam]
  ?~  fore  [~ seam]
  =/  aint  (~(got by mist.seam) i.fore)
  ?:  ?=(%turn -.aint)  $(fore t.fore) :: XX can we drop this from silt here?
  ?>  ?=(%call -.aint) :: this should be respected by nomm generation
  =/  sofa  (~(gut by mire.seam) dial.aint [%toss ~])
  ?.  ?=(%know -.sofa)  $(fore t.fore) :: indirect, check the next one
  =/  soup  (~(gut by mire.seam) sail.aint)
  =/  bats  ~(tap in (~(get ju tack.aint) know.sofa)
  |-  ^-  [(unit [@hail $>(call rain)]) _seam]
  ?~  bats  [`[i.fore aint] seam] :: this one is non-recursive
  =/  bass  (~(gut by mire.seam) i.bats [%toss ~])
  =/  bake  (~(got by snow.seam) i.bats)
  =/  brie  (~(app ca bake) bass)
  ?:  (nail brie soup)
    ^$(fore t.fore) :: recursive, check the next one
  $(bats t.bats)
::  locate the battery
++  guns
  ^-  _seam
  =/  fore  ~(tap in haar.seam)
  =/  fare  fore
  |^  ^-  _seam
    ?~  fare  fire
    =/  hypo  (~(gut by mire.seam) i.fare [%toss ~])
    =?  snow.seam  ?=(%know -.hypo)  (~(put by snow.seam) i.fare ~[1])
    $(fare t.fare)
  ++  fire
    =/  queu=[fore=(list @hail) back=(list @hail)]  [fore ~]
    |^  ^-  _seam
      ?~  fore.queu
        ?~  back.queu  seam
        $(queu [(flop back.queu) ~])
      =/  aint  (~(got by mist.seam) i.fore)
      =/  cope  (~(got by snow.seam) i.fore)
      ?-  -.aint
          %neat
        =.  seam  (fact sail.aint (~(pat ca cope) axe.aint))
        $(fore.queu t.fore.queu, back.queu [sail.aint back.queu])
      ::
          %mede
        $(fore.queu t.fore.queu)
          %cons
        =/  [l=cape r=cape]  ~(rip ca cope)
        =.  seam  (fact left.aint l)
        =.  seam  (fact rite.aint r)
        $(fore.queu t.fore.queu, back.queu [left.aint rite.aint back.queu])
      ::
          %eats
        =.  seam  (fact eats.aint &)
        $(fore.queu t.fore.queu, back.queu [eats.aint back.queu])
      ::
          %meat
        =.  seam  (fact meat.aint &)
        $(fore.queu t.fore.queu, back.queu [meat.aint back.queu])
      ::
          %eels
        =.  seam  (fact left.aint &)
        =.  seam  (fact rite.aint &)
        $(fore.queu t.fore.queu, back.queu [left.aint rite.aint back.queu])
      ::
          %edit
        =/  [limb=cape tree=cape]  (~(awl ca cope) axe.aint)
        =.  seam  (fact limb.aint limb)
        =.  seam  (fact tree.aint tree)
        $(fore.queu t.fore.queu, back.queu [limb.aint tree.aint back.queu])
      ::
          %both
        =.  seam  (fact then.aint cope)
        =.  seam  (fact else.aint cope)
        $(fore.queu t.fore.queu, back.queu [then.aint else.aint back.queu])
      ::
          %call
        $(fore.queu t.fore.queu)
      ::
          %turn
        =.  seam  (fact turn.aint cope)
        $(fore.queu t.fore.queu, back.queu [turn.aint back.queu])
      ::
          %hint
        =.  seam  (fact real.aint cope)
        $(fore.queu t.fore.queu, back.queu [real.aint back.queu])
      ::
          %scry
        $(fore.queu t.fore.queu)
      ::  XX we shouldn't always fix this, only when a conditional
      ::  depends on it
          %bomb
        =.  seam  (fact mine.aint &)
        $(fore.queu t.fore.queu, back.queu [mine.aint back.queu])
      ==
    ++  fact
      |=  [ale=@hail ape=cape]
      ^-  _seam
      %=    seam
          snow
        ?:  (~(has by snow.seam) ale)
          (~(jab by snow.seam) ale |=(ope=cape (~(uni ca ope) ape)))
        (~(put by snow.seam) ale ape)
      ==
    --
  --
::  operations on $cape
++  ca
  |_  one=cape
  ++  app
    |=  know=sock
    ^-  sock
    ?-  one
        %|  [%toss ~]
        %&  know
        ^
      ?+  -.sock  !!
          %know
        ?>  ?=(^ know.sock)
        %+  knit:ska
          $(one -.one, know [%know -.know.sock])
        $(one +.one, know [%know +.know.sock])
      ::
          %bets
        (knit:ska $(one -.one, know left.know) $(one +.one, know rite.know))
      ::
          %toss
        [%toss ~]
      ==
    ==
  ::  unify two capes
  ++  uni
    |=  two=cape
    ^-  cape
    ?-  one
        %|  two
        %&  one
        ^
      ?-  two
        %|  one
        %&  two
        ^
      =/  l  $(one -.one, two -.two)
      =/  r  $(one +.one, two +.two)
      ?:(?&(?=(@ l) =(l r)) l [l r])
    ==
  ::  push a cape down to an axis
  ++  pat
    |=  axe=@
    ?<  =(0 axe)
    |-  ^-  cape
    ?:  =(1 axe)  one
    ?-  (cap axe)
      %2  [$(axe (mas axe)) |]
      %3  [| $(axe (mas axe))]
    ==
  ::  split a cape
  ++  rip
    ^-  [cape cape]
    ?-  one
      %|  [| |]
      %&  [& &]
      ^   one
  ::  poke a hole in a cape
  ++  awl
    |=  axe=@
    ?<  =(0 axe)
    |-  ^-  [cape cape]
    ?:  ?=(%| one) [| |]
    ?:  =(1 axe)  [one |]
    ?-  (cap axe)
        %2
      ?-  one
          %&
        =/  [p=cape t=cape]  $(axe (mas axe))
        [p t &]
      ::
          ^
        =/  [p=cape t=cape]  $(axe (mas axe), one -.one)
        [p t +.one]
      ==
    ::
        %3
      ?-  one
          %&
        =/  [p=cape t=cape]  $(axe (mas axe))
        [p & t]
      ::
          ^
        =/  [p=cape t=cape]  $(axe (mas axe), one +.one)
        [p -.one t]
      ==
    ==
  --
::  fresh hail
++  melt
  ^-  [@hail _seam]
  [hail.seam seam(hail .+(hail.seam)]
--
