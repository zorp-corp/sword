/-  *gene
/+  degen
|%
++  real
  |=  [ject=* form=*]
  =/  labl  [[%toss ~] form]
  =/  prog  prog:+:(gene:degen labl)
  (play ject prog labl)
++  play
  |=  [ject=* prog=tinn entr=labl]
  ^-  *
  =/  tend=*  [[0 0] ject]
  =|  tack=(list [togo=linn rend=_tend])
  =/  inst=linn  
    =/  entu  (~(get by prog) entr)
    ?~  entu
      ~|  'No entry for given labl'  !!
    does.u.entu
  |^
    ^-  *
    =^  next  inst  ~|(%empty-instruction-list ?>(?=(^ inst) inst))
    ?+  next  ~|('TODO: full instruction set' !!)
        [%don ~]
      ?:  =(tack ~)
        (gett 4)  :: TODO pop stack
      =/  res  (gett 4)
      =^  [pins=linn pend=_tend]  tack  tack
      =.  inst  pins
      =.  tend  pend
      =.  this  (putt 4 res)
      $
        ::
        [%jmp *]
      =/  entu  (~(get by prog) +.next)
      ?~  entu
        ~|  'No entry for given labl'  !!
      $:this(inst does.u.entu)
        ::
        [%cal *]
      =/  entu  (~(get by prog) +.next)
      ?~  entu
        ~|  'No entry for given labl'  !!
      =/  ject  (gett 3)
      =/  mend  tend
      =.  this  (putt 2 [0 0])
      =.  tack  [[inst mend] tack]
      =.  inst  does.u.entu
      $
        ::
        [%lnt ~]
      =/  dorm  (gett 4)
      =.  prog  prog:+:(gene:degen(prog prog) [[%toss ~] dorm])
      =/  entu  (~(get by prog) [[%toss ~] dorm])
      ?~  entu
        ~|  'No entry for given labl'  !!
      $:this(inst does.u.entu)
        ::
        [%lnk ~]
      =/  dorm  (gett 4)
      =.  prog  prog:+:(gene:degen(prog prog) [[%toss ~] dorm])
      =/  entu  (~(get by prog) entr)
      ?~  entu
        ~|  'No entry for given labl'  !!
      =/  ject  (gett 3)
      =/  mend  tend
      =/  thin  (putt 2 [0 0])
      $:thin(tack [[inst mend] tack], inst does.u.entu)
        ::
        [%bom *]
      ~|  'Crashed on command'  !!
        ::
        [%imm * @]
      $:(putt +>.next +<.next)
        ::
        [%mov @ @]
      $:(putt +>.next (gett +<.next))
        ::
        [%clq * *]
      ?@  (gett 4)
        $:(find +>.next)
      $:(find +<.next)
        ::
        [%inc @]
      =/  ting  (gett +.next)
      ?@  ting
        $:(putt +.next .+(ting))
      ~|  'Increment of cell'  !!
        ::
        [%eqq * *]
      ?:  =((gett 8) (gett 9))
        $:(find +<.next)
      $:(find +>.next)
        ::
        [%brn * *]
      =/  ting  (gett 4)
      ?:  =(ting 0)
        $:(find +<.next)
      ?:  =(ting 1)
        $:(find +>.next)
      ~|  'Branch on something not a loobean'  !!
        ::
        [%hop *]
      $:(find +.next)
        ::
        [%her *]
      ~&  'Running over label'
      $
    ==
  ++  this  .
  ++  find
    |=  wher=dabl
    ^-  _this
    ?~  inst
      ~|  'Empty instruction list'  !!
    ?:  ?=  [%her *]  -.inst
      ?:  =(wher ->.inst)
        this(inst +.inst)
      $(inst +.inst)
    $(inst +.inst)
  ++  gett
    |=  wher=@
    ^-  *
    ?:  =(1 wher)
      tend
    ?@  tend
      ~|  'Get axis from atom'  !!
    ?-  (cap wher)
      %2  $(wher (mas wher), tend -.tend)
      %3  $(wher (mas wher), tend +.tend)
    ==
  ++  putt
    |=  [wher=@ what=*]
    ^-  _this
    %=    this
        tend
      |-
      ?:  =(1 wher)
        what
      ?-  (cap wher)
          %2
        ?@  tend
          :-  $(wher (mas wher))
          0
        :-  $(wher (mas wher), tend -.tend)
        +.tend
          %3
        ?@  tend
          :-  0
          $(wher (mas wher))
        :-  -.tend
        $(wher (mas wher), tend +.tend)
      ==
    ==
  --
--
