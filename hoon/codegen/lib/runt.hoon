/-  *noir
/+  sack
=*  moan  moan.sack
=*  cole  cole.sack
=>
::  "jets"
|%
++  jdec
  |=  s=*
  ^-  (unit)
  ?.  ?=([* @ *] s)  ~
  ?:  =(0 +6.s)  ~
  `(dec +6.s)
--
::  hot state: path->jet matching
=/  heat
  %-  ~(gas in *(map [path @] $-(* (unit))))
  :~
  [[/dec/test100 1] jdec]
  ==
::  warm state: label->jet matching
=|  warm=(map [sock *] [p=path q=$-(* (unit))])
|%
++  morn
  |=  [s=* f=*]
  =*  this  .
  ^-  (unit *)
  =|  tack=(list) :: result stack
  =/  silt=(list)  ~[s] :: subject stack
  =<
  =^  h  this  (dyn s f)
  ?:  ?=(%| -.h)
    =/  jolt  ~&  jet+p.p.h  (q.p.h s) 
    ~?  =(~ jolt)  %jet-bail
    jolt
  =/  toll=(list (each nomm toms))  [%& nomm.norm.p.h]~ :: instruction stack
  =/  icey=(list (map @hail [=sock form=*]))  ~[ices.norm.p.h] :: code table stack
  |-
  ?~  toll
    ?>  ?=(^ tack)
    ?>  ?=(~ t.tack)
    `i.tack
  ?:  ?=(%& -.i.toll)
    =*  form  p.i.toll
    ?-  -.form
        %par  $(toll [[%& left.form] [%& rite.form] [%| %par] t.toll])
        %not
      =/  salt  (get here.form)
      ?~  salt  ~
      $(toll t.toll, tack [u.salt tack])
    ::
        %one  $(toll t.toll, tack [moan.form tack])
        %two  $(toll [[%& cost.form] [%& corn.form] [%| %two rail.form] t.toll])
        %the  $(toll [[%& pell.form] [%| %the] t.toll])
        %for  $(toll [[%& mall.form] [%| %for] t.toll])
        %ivy  $(toll [[%& this.form] [%& that.form] [%| %ivy] t.toll])
        %six
      $(toll [[%& what.form] [%| %six] [%& then.form] [%& else.form] t.toll])
    ::
        %eve  $(toll [[%& once.form] [%| %eve] [%& then.form] [%| %vee] t.toll])
        %ten  $(toll [[%& twig.form] [%& tree.form] [%| %ten here.form] t.toll])
        %sip  $(toll [[%& then.form] t.toll])
        %tip  $(toll [[%& vice.form] [%| %tip hint.form rail.form] [%& then.form] t.toll])
        %elf  $(toll [[%& rent.form] [%& walk.form] [%| %elf] t.toll])
    ==
  =*  fern  p.i.toll
  ?-  fern
      %par
    ?>  ?=(^ tack)
    ?>  ?=(^ t.tack)
    $(toll t.toll, tack [[i.t.tack i.tack] t.t.tack])
  ::
      %wot
    ?>  ?=(^ icey)
    ?>  ?=(^ silt)
    $(toll t.toll, icey t.icey, silt t.silt)
  ::
      %the
    ?>  ?=(^ tack)
    $(toll t.toll, tack [?=(^ i.tack) t.tack])
  ::
      %for
    ?>  ?=(^ tack)
    ?^  i.tack  ~
    $(toll t.toll, tack [.+(i.tack) t.tack])
  ::
     %ivy
    ?>  ?=(^ tack)
    ?>  ?=(^ t.tack)
    $(toll t.toll, tack [=(i.t.tack i.tack) t.t.tack])
  ::
      %six
    ?>  ?=(^ tack)
    ?>  ?=(^ t.toll)
    ?>  ?=(^ t.t.toll)
    ?.  ?=(? i.tack)  ~
    ?:  i.tack
      $(toll [i.t.toll t.t.t.toll], tack t.tack)
    $(toll [i.t.t.toll t.t.t.toll], tack t.tack)
  ::
      %eve
    ?>  ?=(^ tack)
    $(toll t.toll, silt [i.tack silt], tack t.tack)
  ::
      %vee
    ?>  ?=(^ silt)
    $(toll t.toll, silt t.silt)
  ::
      %elf  ~
      [%two *]
    ?>  ?=(^ icey)
    ?>  ?=(^ tack)
    ?>  ?=(^ t.tack)
    =/  snow  (~(get by i.icey) rail.fern)
    ?~  snow
      =^  honk  this  (dyn i.t.tack i.tack)
      ?:  ?=(%| -.honk)
        =/  jolt  ~&(jet+p.p.honk (q.p.honk i.t.tack))
        ?~  jolt  ~&  %jet-bail  ~
        %=  $
          toll  t.toll
          tack  [u.jolt t.t.tack]
        ==
      %=  $
        toll  [[%& nomm.norm.p.honk] [%| %wot] t.toll]
        icey  [ices.norm.p.honk icey]
        silt  [i.t.tack silt]
        tack  t.t.tack
      ==
    =/  honk  (fan u.snow)
    ?>  ?=(^ honk)
    ?:  ?=(%| -.u.honk)
      =/  jolt  ~&(jet+p.p.u.honk (q.p.u.honk i.t.tack))
      ?~  jolt  ~&  %jet-bail  ~
      %=  $
        toll  t.toll
        tack  [u.jolt t.t.tack]
      ==
    %=  $
      toll  [[%& nomm.norm.p.u.honk] [%| %wot] t.toll]
      icey  [ices.norm.p.u.honk icey]
      silt  [i.t.tack silt]
      tack  t.t.tack
    ==
  ::
      [%ten *]
    ?>  ?=(^ tack)
    ?>  ?=(^ t.tack)
    =/  salt  (put here.fern i.t.tack i.tack)
    ?~  salt  ~
    $(toll t.toll, tack [u.salt t.t.tack])
  ::
      [%tip *]
    ?>  ?=(^ tack)
    $(toll t.toll, tack t.tack) 
  ==
  |%
  ++  put
    |=  [axe=@ twig=* tree=*]
    ^-  (unit)
    ?:  =(0 axe)  ~
    |-  ^-  (unit)
    ?:  =(1 axe)  `twig
    ?@  tree  ~
    ?-  (cap axe)
        %2
      =/  l  $(axe (mas axe), tree -.tree)
      ?~(l ~ `[u.l +.tree])
    ::
        %3
      =/  r  $(axe (mas axe), tree +.tree)
      ?~(r ~ `[-.tree u.r])
    ==
  ++  get
    |=  axe=@ 
    ^-  (unit)
    ?:  =(0 axe)  ~
    ?>  ?=(^ silt)
    |-  ^-  (unit)
    ?:  =(1 axe)  `i.silt
    ?@  i.silt  ~
    ?-  (cap axe)
      %2  $(i.silt -.i.silt, axe (mas axe))
      %3  $(i.silt +.i.silt, axe (mas axe))
    ==
  ++  dyn
    |=  [s=* f=*]
    ^-  [(each hone [p=path q=$-(* (unit))]) _this]
    =/  honk  (fin s f)
    ?^  honk  [u.honk this]
    =.  sack  (rout:sack [& s] f)
    =.  warm  wag
    =/  hunk  (fin s f)
    ?>  ?=(^ hunk)
    [u.hunk this]
  ++  wag
    ^-  _warm
    =/  jets  ~(tap by heat)
    =.  warm  ~  :: full regeneration
    |-  ^-  _warm
    ?^  jets
      =/  labs  ~(tap in (~(get ju call.cole) -.i.jets))
      |-  ^-  _warm
      ?^  labs
        $(warm (~(put by warm) i.labs [-< +]:i.jets), labs t.labs)
      ^$(jets t.jets)
    warm
  ++  fin
    |=  [s=* f=*]
    ^-  (unit (each hone [p=path q=$-(* (unit))]))
    (fan [& s] f)
  ++  fan
    |=  [s=sock f=*]
    ^-  (unit (each hone [p=path q=$-(* (unit))]))
    =/  huns  (~(get ja moan) f)
    |-  ^-  (unit (each hone [p=path q=$-(* (unit))]))
    ?~  huns  ~
    ?:  (~(huge so:sack soot.i.huns) s)
      =/  jute  (~(get by warm) [soot.i.huns f])
      ?^  jute  `[%| u.jute] 
      `[%& i.huns]
    $(huns t.huns)
  --
--
