/-  *noir
/+  sack
=*  moan  moan.sack
|%
++  morn
  |=  [s=* f=*]
  ^-  (unit *)
  =|  tack=(list) :: result stack
  =/  silt=(list)  ~[s] :: subject stack
  =<
  =^  h  moan  (dyn s f)
  =/  toll=(list (each nomm toms))  [%& nomm.norm.h]~ :: instruction stack
  =/  icey=(list (map @hail [=sock form=*]))  ~[ices.norm.h] :: code table stack
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
        %tip  $(toll [[%& vice.form] [%| %tip hint.form] [%& then.form] t.toll])
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
      =^  honk  moan  (dyn i.t.tack i.tack)
      %=  $
        toll  [[%& nomm.norm.honk] [%| %wot] t.toll]
        icey  [ices.norm.honk icey]
        silt  [i.t.tack silt]
        tack  t.t.tack
      ==
    =/  honk  (fan u.snow)
    ~?  =(~ honk)  missingarm+u.snow
    ~?  =(~ honk)  moan+~(key by moan)
    ?>  ?=(^ honk)
    %=  $
      toll  [[%& nomm.norm.u.honk] [%| %wot] t.toll]
      icey  [ices.norm.u.honk icey]
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
    ^-  [hone _moan]
    =/  honk  (fin s f)
    ?^  honk  [u.honk moan]
    =.  moan  (rout:sack [& s] f)
    =/  hunk  (fin s f)
    ?>  ?=(^ hunk)
    [u.hunk moan]
  ++  fin
    |=  [s=* f=*]
    ^-  (unit hone)
    (fan [& s] f)
  ++  fan
    |=  [s=sock f=*]
    =/  huns  (~(get ja moan.sack) f)
    |-  ^-  (unit hone)
    ?~  huns  ~
    ?:  (~(huge so:sack soot.i.huns) s)
      `i.huns
    $(huns t.huns)
  --
--
