/-  *sock
/-  *gene
|%
++  run
  |=  [prog=tabl tart=labl ject=*]
  ^-  (unit *)
  =/  uuff  (~(get by prog) tart)
  ?~  uuff
    ~
  =/  buff  does.u.uuff
  =|  resu=*
  =|  stac=(list (list *))
  |^  ?~  buff
        ~
      =/  inst  i.buff
      =.  buff  t.buff
      ?-  inst
          ::
          [%con *]
        $(resu +.inst)
          ::
          [%axe *]
        =/  r  (axes +.inst)
        ?~  r
          ~
        $(resu u.r)
          ::
          [%cel @]
        =/  r  (both (segt +.inst (some resu)))
        ?~  r
          ~
        $(resu u.r)
          ::
          [%clq @]
        $(resu .?(resu))
          ::
          [%inc ~]
        $(res .+(resu))
          ::
          [eqq @]
        =/  r  (mate (segt +.inst) (some resu))
        ?~  r
          ~
        $(resu u.r)
          ::
          [%br1 *]
        ?:  ?=  %0  resu
          $
        ?:  ?=  %1  resu
          $(buff (julp +.inst))
        ~
          ::
          [%bru *]
        $(buff (julp +.inst))
          ::
          [%brh *]
        $
          ::
          [%sub ~]
        $(ject resu)
          ::
          [%ext ~]
        $(ject [resu ject])
          ::
          [%dxt ~]
        ?:  ?=  ject  [* *]
          $(ject +.ject)
        ~
          ::
          [%noc ~]
        ?:  ?=  resu  [* *]
          $(ject -.resu, resu +.resu)
        ~
          ::
          [%lnk ~]
        ~|  'TODO: run lnk'
          ::
          [%cal *]
        =/  stuc  (sput 0 t.buff)
        ?~  stuc
          ~
        =.  stac  u.stuc
        =/  nuuf  (~(get by prog) +.inst)
        ?~  nuuf
          ~
        $(buff does.u.nuuf)
          ::
          [%lnt ~]
        ~|  'TODO: run lnt'
          ::
          [%jmp *]
        =/  nuuf  (~(get by prog) +.inst)
        ?~  nuuf
          ~
        $(buff does.u.nuuf)
          ::
          [%edt *]
        =/  edut  (edit +.inst resu ject)
        ?~  edut
          ~
        $(resu u.edut)
          ::
          [%spy ~]
        ~|  'TODO: run spy'
          ::
          [%puh @]
        $(stac [(reap +.inst ~) stac])
          ::
          [%put @]
        =/  stuc  (sput +.inst resu)
        ?~  stuc
          ~
        $(stac u.stuc)
          ::
          [%get @]
        =/  guts  (segt +.inst) 
        ?~  guts
          ~
        $(resu u.guts)
          ::
          [%sav @]
        =/  stuc  (sput +.inst ject)
        ?~  stuc
          ~
        $(stac u.stuc)
          ::
          [%reo @]
        =/  guts  (segt +.inst) 
        ?~  guts
          ~
        $(ject u.guts)
          ::
          [%don ~]
        ?~  stac
          (some resu)
        =/  guts  (segt 0)
        ?~  guts
          ~
        $(buff u.guts)
          ::
          [%bom ~]
        ~
      ==
  :: get an axis from the subject
  ++  axes
    |=  ax=@
    ^-  (unit *)
    ?:  ?=  %0  ax
      ~
    |-
    ^-  (unit *)
    ?:  ?=  %1  ax
      (some ject)
    ?.  ?=  [* *]  ject
      ~
    ?-  (cap ax)
        ::
        %2
      $(ax (mas ax), ject -.ject)
        ::
        %3
      $(ax (mas ax), ject +.ject)
    ==
  :: get the value of a slot in the stack frame
  ++  segt
    |=  slot=@
    ^-  (unit *)
    ?~  stac
      ~
    =/  fra  (slag slot i.stac)
    ?~  fra
      ~
    i.fra
  :: put a value into the frame
  ++  sput
    |=  [slot=@ cont=*]
    ^-  (unit _stac)
    ?~  stac
      ~
    ?.  (gth (lent i.stac) slot)
      ~
    =.  i.stac  (snap i.stac slot cont)
    (some stac)
  ++  edit
    |=  [ax=@ pat=* tre=*]
    ^-  (unit *)
    ?:  ?=  %0  ax
      ~
    |-
    ^-  (unit *)
    ?:  ?=  %1  ax
      (some pat)
    ?.  ?=  [* *] tre
      ~
    ?-  (cap ax)
        ::
        %2
      :-  $(ax (mas ax), tre -.tre)  +.tre
        ::
        %3
      :-  -.tre  $(ax (mas ax), tre +.tre)
    ==
  :: jump to a local label brh
  ++  julp
    |=  mabl=labl
    ^-  _buff
    ?~  buff
      ~
    ?:  .=  i.buff  [%brh mabl]
      t.buff
    $(buff t.buff)
  --
--
