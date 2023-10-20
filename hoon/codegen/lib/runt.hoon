/-  *noir
/-  gene
/+  sack
/+  line
=*  moan  moan.sack
=*  cole  cole.sack
=/  lena  (lean:line moan)
=*  hill  hill.lena
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
++  lane
  |=  [s=* f=*]
  =*  this  .
  ^-  (unit *)
  =|  tack=(list [salt=@ turn=bile:gene s=* f=* mean=(list ^) stir=(map @uvre *) dirt=pile:gene])
  =<
  =^  tear  this  (dyn s f)
  ?:  ?=(%| -.tear)
    (q.p.tear s)
  =/  fram=[sick=(set @) stir=(map @ *) dirt=pile:gene]  [*(set @) *(map @ *) p.tear]
  =|  mean=(list ^)
  =|  come=bile
  =/  [todo=(list pole:gene) then=site:gene]  (~(got by will.dirt.fram) wish.dirt.fram)
  =.  stir.fram  (~(put by stir.fram) sire.dirt.fram s)
  |-  ^-  (unit *)
  =>  |%  ++  r  |=(a=@uvre (~(got by stir.fram) a))  --
  =|  sick=(set @uvre) :: poisoned registers
  ?^  todo
    ~&  i.todo
    =>  |%  ++  go  $(todo t.todo)  --
    ?+  -.i.todo  ~&("skip/todo: {<i.todo>}" go)
        %imm
      =.  stir.fram  (~(put by stir.fram) +>.i.todo +<.i.todo)
      go
    ::
        %mov
      ?:  (~(has in sick.fram) +<.i.todo)
        =.  sick.fram  (~(put in sick.fram) +>.i.todo)
      =.  stir.fram  (~(put by stir.fram) +>.i.todo (r +<.i.todo))
      go
    ::
        %phi
      |-  ^-  (unit *)
      ?~  +<.i.todo  !!
      ?.  =(come -.i.+<.i.todo)  $(+<.i.todo t.+<.i.tod)
      ?:  (~(has by sick.fram) +.i.+<.todo)
        =.  sick.fram  (~(put in sick.fram) +.i.+<.todo)
      =/  v  (~(got by stir.fram) +.i.+<.i.todo)
      =.  stir.fram  (~(put by stir.fram) +>.i.todo v)
      go
    ::
        %inc
      =/  a  (r +<.i.todo)
      ?^  a  ~
      =.  stir.fram  (~(put by stir.fram) +>.i.todo .+(a))
      go
    ::
        %con
      =.  stir.fram  (~(put by stir.fram) +>+.i.todo [(r +<.i.todo) (r +>-.i.todo)])
      go
    ::
        %hed
      =/  n  (r +<.i.todo)
      ?@  n
        =.  sick.fram  (~(put in sick.fram) +<.i.todo)
        go
      =.  stir.fram  (~(put by stir.fram) +>.i.todo -:(r +<.i.todo))
      go
    ::
        %tal
      =/  n  (r +<.i.todo)
      ?@  n
        =.  sick.fram  (~(put in sick.fram) +<.i.todo)
        go
      =.  stir.fram  (~(put by stir.fram) +>.i.todo +:(r +<.i.todo))
      go
    ::
        %pol
      ?:  (~(has in sick.fram) +<.i.todo)
        =.  sick.fram  (~(put in sick.fram) +>.i.todo)
        go
      go
    ::
        %poi
      =.  sick.fram  (~(put in sick.fram) +.i.todo)
      go
    ::
        %ipb
      ?:  (~(has in sick.fram) +.i.todo)
        ~
      go
    ::  XX todo men/man
    ==
  ~&  then
  =>  |%
      ++  go  |=(b=bile:gene =+((~(got by will.dirt.fram) b) ^$(todo body, then bend)))
      ++  sr
        |=  [a=(list @uvre) n=need:gene]
        =/  nack=(list need:gene)  ~[n]
        =|  whip=(set @uvre)
        |-  ^-  (map @uvre)
        ?^  nack
          ?-  -.i.nack
              %none  $(nack t.nack)
              %both  $(nack [left.i.nack rite.i.nack t.nack])
              %this
            ?>  ?=(^ a)
            ?:  (~(has in sick.fram) i.a)
              $(whip (~(put in whip) sass.i.nack), a t.a, nack t.nack)
            $(a t.a, nack t.nack)
          ==
        whip
      ++  ar
        |=  [a=(list @uvre) n=need:gene]
        =/  nack=(list need:gene)  ~[n]
        =|  whip=(map @uvre *)
        |-  ^-  (map @uvre *)
        ?^  nack
          ?-  -.i.nack
              %none  $(nack t.nack)
              %both  $(nack [left.i.nack rite.i.nack t.nack])
              %this
            ?>  ?=(^ a)
            ?:  (~(has in sick.fram) i.a)
              $(a t.a, nack t.nack)
            $(whip (~(put by whip) sass.i.nack (r i.a)), a t.a, nack t.nack)
          ==
        whip
      ++  re
        |=  x=*
        ?^  tack
          =.  stir.i.tack  (~(put by stir.i.tack) salt.i.tack x)
          =/  post  (~(got by will.dirt.i.tack) turn.i.tack)
          %=  ^$
            tack  t.tack
            stir.fram  stir.i.tack
            dirt.fram  dirt.i.tack
            s  s.i.tack
            f  f.i.tack
            mean  mean.i.tack
            todo  body.post
            then  bend.post
          ==
        `x
      --
  ?-  -.then
      %clq
    ?:  (~(has in sick.fram) +<.then)
      ~
    ?^  (r +<.then)
      (go +>-.then)
    (go +>+.then)
  ::
      %eqq
    ?:  (~(has in sick.fram) +<.then)
      ~
    ?:  =((r +<.then) (r +>-.then)) 
      (go +>+<.then)
    (go +>+>.then)
  ::
      %brn
    ?:  (~(has in sick.fram) +<.then)
      ~
    =/  c  (r +<.then)
    ?:  =(0 c)
      (go +>+<.then)
    ?:  =(1 c)
      (go +>+>.then)
    (go +>-.then)
  ::
      %hop
    (go +.then)
  ::
      %lnk
    ?:  (~(has in sick.fram) +<.then)
      ~
    ?:  (~(has in sick.fram) +>-.then)
      ~
    =/  s  (r +<.then)
    =/  f  (r +>-.then)
    =^  tear  this  (dyn s f)
    ?:  ?=(%| -.tear)
      =/  silt  (q.p.tear s)
      ?~  silt  ~
      =.  stir.fram  (~(put by stir.fram) +>+<.then u.silt)
      (go +>+>.then)
    =/  wish  (~(got by will.p.tear) wish.p.tear)
    %=  $
      tack  [[+>+<.then +>+>.then ^s ^f mean fram] tack]
      s  s
      f  f
      dirt.fram  p.tear
      stir.fram  (~(put by *(map @ *)) sire.p.tear s)
      mean  ~
      todo  body.wish
      then  bend.wish
    ==
  ::
      %cal
    =/  jute  (~(get by warm) +<.then)
    ?^  jute
      =/  silt  (q.u.jute s)
      ?~  silt  ~
      =.  stir.fram  (~(put by stir.fram) +>+>-.then u.silt)
      (go +>+>+.then)
    =/  pill  (~(got by hill) +<.then)
    =/  long  (~(got by will.pill) long.pill)
    %=  $
      tack  [[+>+>-.then +>+>+.then ^s ^f mean fram] tack]
      s  s
      f  f
      sick.fram  (sr +>+<.then want.pill) 
      stir.fram  (ar +>+<.then want.pill)
      dirt.fram  pill
      mean  ~
      todo  body.long
      then  bend.long
    ==
  ::
      %lnt
    ?:  (~(has in sick.fram) +<.then)
      ~
    ?:  (~(has in sick.fram) +>.then)
      ~
    =/  s  (r +<.then)
    =/  f  (r +>.then)
    =^  tear  this  (dyn s f)
    ?:  ?=(%| -.tear)
      =/  silt  (q.p.tear s)
      ?~  silt  ~
      (re u.silt)
    =/  wish  (~(got by will.p.tear) wish.p.tear)
    %=  $
      s  s
      f  f
      dirt.fram  p.tear
      stir.fram  (~(put by *(map @ *)) sire.p.tear s)
      todo  body.wish
      then  bend.wish
    ==
  ::
      %jmp
    =/  jute  (~(get by warm) +<.then)
    ?^  jute
      =/  silt  (q.u.jute s)
      ?~  silt  ~
      (re u.silt)
    =/  pill  (~(got by hill) +<.then)
    =/  long  (~(got by will.pill) long.pill)
    %=  $
      s  s
      f  f
      sick.fram  (sr +>+.then want.pill)
      stir.fram  (ar +>+.then want.pill)
      dirt.fram  pill
      todo  body.long
      then  bend.long
    ==
  ::
      %hip
    =.  come  +<.then
    (go +>.then)
  ::
      %spy
    ~&  'todo: spy'  ~
  ::
      %mer
    ~&  'todo: mer'  (go +>+>+>.then)
  ::
      %don
    ?:  ?=(^ sick.fram)  ~
    (re (r +.then))
  ::
      %bom  ~
  ==
  |%
  ++  dyn
    |=  [s=* f=*]
    ^-  [(each pile:gene [p=path q=$-(* (unit))]) _this]
    =/  mile  (fin s f)
    ?^  mile  [u.mile this]
    =.  sack  (rout:sack [& s] f)
    =.  warm  wag
    =.  lena  (lena moan)
    =/  milt  (fin s f)
    ?>  ?=(^ milt)
    [u.milt this]
  ++  fin  |=([s=* f=*] (fan [& s] f))
  ++  fan
    |=  [s=sock f=*]
    ^-  (unit (each pile:gene [p=path q=$-(* (unit))]))
    =/  huns  (~(get ja moan) f)
    |-  ^-  (unit (each pile:gene [p=path q=$-(* (unit))]))
    ?~  huns  ~
    ?:  (~(huge so:sack soot.i.huns) s)
      =/  jute  (~(get by warm) [soot.i.huns f])
      ?^  jute  `[%| u.jute]
      =/  mile  (~(get by hill) [soot.i.huns f])
      ?~  mile  ~
      `[%& u.mile]
    $(huns t.huns)
  ++  wag
    ^-  _warm
    =/  jets  ~(tap by heat)
    =.  warm  ~
    |-  ^-  _warm
    ?^  jets
      =/  labs  ~(tap in (~(get ju call.cole) -.i.jets))
      |-  ^-  _warm
      ?^  labs
        $(warm (~(put by warm) i.labs [-< +]:i.jets), labs t.labs)
      ^$(jets t.jets)
    warm
  --
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
