|%
::
::   run nock
::
::  interpret nock, generating code for the outer invocation and any
::  indirect calls encountered. This should be considered the formal
::  entry point for Ares codegen
++  wink
  =*  thus  .
  |=  [h=heat j=(map @ $-(* (unit))) p=$-(^ (unit (unit))) s=* f=*]
  =*  wink  .
  ^-  [tone _this]
  =/  hull  (peek s f)
  =?  thus  ?=(~ hull)  this:(poke %comp ~ s f)
  =?  hull  ?=(~ hull)  (peek s f)
  ?>  ?=(^ hull)
  =/  bell  bell.u.hull
  =*  hill  hall.u.hull
  =|  from=bile
  =/  pyre  (~(got by hill) bell)
  =/  fram=[will=(map bile blob) regs=(map @uvre *) mean=(list [@ta *]) sick=(set @uvre)]
    [will.pyre (~(put in *(map @uvre *)) sire.pyre s) *(list [@ta *]) *(set @uvre)] 
  =|  tack=(list [then=bile r=@uvre _fram])
  =/  bloc  (~(got by will.pyre) wish.pyre)
  ~%  %wink-loop  wink  ~
  |^  ^-  [tone _this]
    :: XX dedent
    ?^  body.bloc
      =>  |%
          :: next instruction
          ++  go  $(body.bloc t.body.bloc)
          --
      =*  x  i.body.bloc
      ?-  -.i.body.bloc
          %imm
        =.  regs.fram  (~(put by regs.fram) d.x n.x)
        go
      ::
          %mov
        =.  regs.fram  (~(put by regs.fram) d.x (r s.x))
        go
      ::
          %phi
        |-  ^-  [tone _this]
        ?^  s.x
          ?.  =(from -.i.s.x)
            $(s.x t.s.x)
          =.  regs.fram  (~(put by regs.fram) d.x (r +.i.s.x))
          go
        ~|  %bad-phi  !!
      ::
          %inc
        =/  a  (r s.x)
        ?@  a
          =.  regs.fram  (~(put by regs.fram) d.x +(a))
          go
        no
      ::
          %con
        =.  regs.fram  (~(put by regs.fram) d.x [(r h.x) (r t.x)])
        go
      ::
          %cop
        =?  sick.fram  ?=(@ (r s.x))  (~(put in sick.fram) s.x)
        go
      ::
          %lop
        =?  sick.fram  ?!  ?=(? (r s.x))  (~(put in sick.fram) s.x)
        go
      ::
          %coc
        ?@  (r s.x)
          no
        go
      ::
          %hed
        =/  c  (r s.x)
        =?  sick.fram  ?=(@ c)  (~(put in sick.fram) s.x)
        =?  regs.fram  ?=(^ c)  (~(put by regs.fram) d.x -.c) 
        go
      ::
          %hci
        =/  c  (r s.x)
        ?@  c
          no
        =.  regs.fram  (~(put by regs.fram) d.x -.c)
        go
      ::
          %tal
        =/  c  (r s.x)
        =?  sick.fram  ?=(@ c)  (~(put in sick.fram) s.x)
        =?  regs.fram  ?=(^ c)  (~(put by regs.fram) d.x +.c) 
        go
      ::
          %tci
        =/  c  (r s.x)
        ?@  c
          no
        =.  regs.fram  (~(put by regs.fram) d.x +.c)
        go
      ::
          %men
        =.  mean.fram  [[l.x (r s.x)] mean.fram]
        go
      ::
          %man
        ?>  ?=(^ mean.fram)
        $(mean.fram t.mean.fram) :: =. here would hit the TMI problem
      ::
          %sld
        ~|  %todo  !!
      ::
          %slo
        ~|  %todo  !!
      ::
          %hit
        go
      ::
          %slg
        ~&  (r s.x)  go
      ::
      :: XX need to feed in global cache from outside
      :: mew and mer influence which code we actually run, which could
      :: affect whether dynamic calls are made, which could influence
      :: the state of the codegen core.
      :: So we do need to handle them properly and not just ignore them
          %mew
        ~|  %todo  !!
      ::
      ::  side-effect only
          %tim
        go
      ::
      ::  side-effect only
          %tom
        go
      ::
      ::  side-effect only
          %mem
        go
      ::
          %pol
        =?  sick.fram  (~(has in sick.fram) s.x)  (~(put in sick.fram) d.x)
        go
      ::
          %poi
        =.  sick.fram  (~(put in sick.fram) d.x)
        go
      ::
          %ipb
        |-  ^-  [tone _this]
        ?^  s.x
          ?:  (~(has in sick.fram) i.s.x)  no
          $(s.x t.s.x)
        go
      ==
    =*  x  bend.bloc
    =>  |%
        :: jump to given bile
        ++  goto  |=  =bile  ^$(bloc (~(got by will.fram) bile))
        --
    ?-  -.bend.bloc
        %clq
      ?^  (r s.x)
        (goto z.x)
      (goto o.x)
    ::
        %eqq
      ?:  =((r l.x) (r r.x))
        (goto z.x)
      (goto o.x)
    ::
        %brn
      =/  b  (r s.x)
      ?+  b  no 
        %0  (goto z.x)
        %1  (goto o.x)
      ==
    ::
        %hop
      (goto t.x)
    ::
        %hip
      =.  from  c.x
      (goto t.x)
    ::
        %lnk
      =/  s  (r u.x)
      =/  f  (r f.x)
      =/  hull  (peek s f)
      =?  thus  ?=(~ hull)  this:(poke %comp ~ s f)
      =?  hull  ?=(~ hull)  (peek s f)
      ?>  ?=(^ hull)
      =.  hill  hall.u.hull
      =/  bell  bell.u.hull
      =/  pyre  (~(got by hill) bell)
      =.  tack  [[t.x d.x fram] tack]
      =.  sick.fram  ~
      =.  regs.fram  (~(put by *(map @uvre *)) sire.pyre s)
      =.  will.fram  will.pyre
      (goto wish.pyre)
    ::
        %cal
      =/  call-sick  (turn b.x ~(has in sick.fram))
      =/  call-args  (turn v.x r)
      =/  pyre  (~(got by hill) a.x)
      =+  (args want.pyre call-sick call-args)
      =.  tack  [[t.x d.x fram] tack]
      =.  sick.fram  sick
      =.  regs.fram  regs
      =.  will.fram  will.pyre
      (goto long.pyre)
    ::
        %caf  :: XX check hot state rather than not in hill
      ?.  (~(has by hill) a.x)
        =/  tore  (mink [(r u.x) form.a.x] p)
        ?-  -.tore
            %1  [tore this]
            %2  no
            %0
          =.  regs.fram  (~(put by regs.fram) d.x product.tore)
          (goto t.x)
        ==
      $(bend.bloc [%cal [a b v d t]:x])
    ::
        %lnt
      =/  s  (r u.x)
      =/  f  (r f.x)
      =/  hull  (peek s f)
      =?  thus  ?=(~ hull)  this:(poke %comp ~ s f)
      =?  hull  ?=(~ hull)  (peek s f)
      ?>  ?=(^ hull)
      =.  hill  hall.u.hull
      =/  bell  bell.u.hull
      =/  pyre  (~(got by hill) bell)
      =.  sick.fram  ~
      =.  regs.fram  (~(put by *(map @uvre *)) sire.pyre s)
      =.  will.fram  will.pyre
      (goto wish.pyre)
    ::
        %jmf  :: XX check hot state rather than not in hill
      ?.  (~(has by hill) a.x)
        =/  tore  (mink [(r u.x) form.a.x] p)
        ?-  -.tore
          %1  [tore this]
          %2  no
          %0  [tore this]
        ==
      $(bend.bloc [%jmp [a b v]:x])
    ::
        %jmp
      =/  call-sick  (turn b.x ~(has in sick.fram))
      =/  call-args  (turn v.x r)
      =/  pyre  (~(got by hill) a.x)
      =+  (args want.pyre call-sick call-args)
      =.  sick.fram  sick
      =.  regs.fram  regs
      =.  will.fram  will.pyre
      ~_  (~(vals xray will.fram) regs.fram)
      (goto long.pyre)
    ::
        %spy
      ~|  %todo  !!
    ::
    ::  XX we need to pass persistent caches in
        %mer
      ~|  %todo  !!
    ::
        %don
      ?^  tack
        =.  regs.i.tack  (~(put by regs.i.tack) r.i.tack (r s.x))
        =.  fram  +>.i.tack
        =/  then  then.i.tack
        $(bloc (~(got by will.fram) then), tack t.tack)
      [[%0 (r s.x)] this]
    ::
      %bom  no
    ==
  ::
  ::    get register value
  ++  r  |=(x=@uvre ~_((~(gals xray will.fram) x regs.fram) (~(got by regs.fram) x)))
  ::
  ::    crash with the mean stack
  ++  no  [[%2 mean.fram] this]
  ::
  ::    match up call arguments
  ++  args
    |=  [=need call-sick=(list ?) call-args=(list *)]
    =|  [sick=(set @uvre) regs=(map @uvre *)]
    =/  tack=(list _need)  ~[need]
    |-  ^-  [sick=(set @uvre) regs=(map @uvre *)]
    ?~  tack
      [sick regs]
    ?-  -.i.tack
        %both
      ?>  ?=(^ call-sick)
      =?  sick  i.call-sick  (~(put in sick) sass.i.tack)
      $(tack [left.i.tack rite.i.tack t.tack], call-sick t.call-sick)
    ::
        %this
      ?>  ?=(^ call-args)
      =.  regs  (~(put by regs) sass.i.tack i.call-args)
      $(tack t.tack, call-args t.call-args)
    ::
        %none  $(tack t.tack)
    ==
  --
--
