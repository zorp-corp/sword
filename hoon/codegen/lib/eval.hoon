=*  line  .
|%
++  this  .
++  hour
  =*  thus  .
  |=  [s=* f=*]
  ^-  [(unit) _this]
  =<
  =^  cone  thus  (cope s f)
  =*  n  nomm.norm.cone
  |-  ^-  [(unit) _this]
  ?-  -.n
      %par
    =^  l  thus  $(n left.n)
    ?~  l  [~ thus]
    =^  r  thus  $(n rite.n)
    ?~  r  [~ thus]
    [`[u.l u.r] thus]
  ::
      %not
    ?:  =(0 here.n)  ~&  %axe-none-crash  [~ this]
    |-  ^-  [(unit) _this]
    ?:  =(1 here.n)  [`s this]
    ?@  s  ~&  %axe-miss-crash  [~ this]
    ?-  (cap here.n)
      %2   $(s -.s, here.n (mas here.n))
      %3   $(s +.s, here.n (mas here.n))
    ==
  ::
      %one
    [`moan.n this]
  ::
      %two
    =^  t  thus  $(n cost.n)
    ?~  t  [~ this]
    =^  g  thus  $(n corn.n)
    ?~  g  [~ this]
    =/  bull  (~(get by ices.norm.cone) rail.n)
    =^  cane  thus
      ?~  bull
        ~&  [%indirect t g]  (cope u.t u.g)
      ?>  (~(huge so sock.u.bull) [& s])
      =/  kine  (mose u.bull)
      ?>  ?=(^ kine)
      [u.kine this]
    $(s u.t, cone cane)
  ::
      %the
    =^  p  thus  $(n pell.n)
    ?~  p  [~ this]
    [`?^(u.p 0 1) this]
  ::
      %for
    =^  m  thus  $(n mall.n)
    ?~  m  [~ this]
    ?^  u.m  ~&  %inc-cell-crash  [~ this]
    [`.+(u.m) this]
  ::
      %ivy
    =^  i  thus  $(n this.n)
    ?~  i  [~ this]
    =^  o  thus  $(n that.n)
    ?~  o  [~ this]
    [`.=(u.i u.o) this]
  ::
      %six
    =^  c  thus  $(n what.n)
    ?~  c  [~ this]
    ?.  ?=(? u.c)  ~&  %cond-not-loobean  [~ this]
    ?:  u.c  $(n then.n)
    $(n else.n)
  ::
      %eve
    =^  t  thus  $(n once.n)
    ?~  t  [~ this]
    $(s u.t, n then.n)
  ::
      %ten
    ?:  =(0 here.n)  ~&  %edit-none-crash  [~ this]
    =^  w  thus  $(n twig.n)
    ?~  w  [~ this]
    =^  r  thus  $(n tree.n)
    ?~  r  [~ this]
    =|  tack=(list [?(%2 %3) *])
    |-  ^-  [(unit) _this]
    ?.  =(1 here.n)
      ?@  u.r  ~&  %edit-miss-crash  [~ this]
      ?-  (cap here.n)
        %2  $(u.r -.u.r, tack [[%2 +.u.r] tack], here.n (mas here.n))
        %3  $(u.r +.u.r, tack [[%3 -.u.r] tack], here.n (mas here.n))
      ==
    |-  ^-  [(unit) _this]
    ?~  tack  [w this]  
    ?-  -.i.tack
      %2  $(tack t.tack, u.w [u.w +.i.tack])
      %3  $(tack t.tack, u.w [+.i.tack u.w])
    ==
  ::
      %sip
    $(n then.n)
  ::
      %tip
    =^  h  thus  $(n vice.n)
    ?~  h  [~ this]
    $(n then.n)
  ::
      %elf
    ~|  %no-scry  !!
  ==
  |%
  ++  mose
    |=  [s=sock f=*]
    ^-  (unit hone)
    =/  huns  (~(get ja moan) f)
    |-  ^-  (unit hone)
    ?~  huns  ~
    ?:  =(s soot.i.huns)  `i.huns
    $(huns t.huns)
  ++  mope
    |=  [s=* f=*]
    ^-  (unit hone)
    =/  huns  (~(get ja moan) f)
    |-  ^-  (unit hone)
    ?~  huns  ~
    ~&  [%mope-i soot.i.huns]
    ?:  (~(huge so soot.i.huns) [& s])
      `i.huns
    $(huns t.huns)
  ++  cope
    |=  [s=* f=*]
    ^-  [hone _this]
    =/  roan  (mope s f)
    ?:  ?=(^ roan)  [u.roan this]
    =.  sack  (rout:sack s f)
    ~&  [%cope-moan moan]
    =/  sewn  (mope s f)
    ?.  ?=(^ sewn)  ~|  [%mope-miss s f]  !!
    [u.sewn this]
  --
++  tine
  |=  [s=* f=*]
  =<
  =^  [=bell hall=_hill]  line  (peep s f)
  =/  pill  (~(got by hall) bell)
  =/  blob  (~(got by will.pill) wish.pill)
  =/  rasp  (star sire.pill s)
  =|  vile=(set @uvre)
  |^  ^-  (unit *)
    ?^  body.blob
      =*  i  i.body.blob
      ~&  i
      ?-  -.i
          %imm  $(rasp (p d.i n.i), body.blob t.body.blob)
          %mov  $(rasp (mov s.i d.i), body.blob t.body.blob)
          %inc
        =/  v  (g s.i)
        ?^  v  ~&  [%inc-cell-crash v]  ~
        $(rasp (p d.i .+(v)), body.blob t.body.blob)
      ::
          %con  $(rasp (p d.i [(g h.i) (g t.i)]), body.blob t.body.blob)
          %cop  =?(vile ?=(^ (g s.i)) (~(put in vile) s.i) $(body.blob t.body.blob))
          %lop  =?(vile ?=(? (g s.i)) (~(put in vile) s.i) $(body.blob t.body.blob))
          %coc  ?@((g s.i) ~ $(body.blob t.body.blob))
          %hed
        =/  c  (g s.i)
        ?@  c
          ~&  [%poison s.i c]
          $(vile (~(put in vile) s.i), body.blob t.body.blob)
        $(rasp (p d.i -.c), body.blob t.body.blob)
      ::
          %hci  =/(c (g s.i) ?@(c ~ $(rasp (p d.i -.c), body.blob t.body.blob)))
          %tal
        =/  c  (g s.i)
        ?@  c
          ~&  [%poison s.i c]
          $(vile (~(put in vile) s.i), body.blob t.body.blob)
        $(rasp (p d.i +.c), body.blob t.body.blob)
      ::
          %tci  =/(c (g s.i) ?@(c ~ $(rasp (p d.i +.c), body.blob t.body.blob)))
          %men  ~&  %mean-todo  $(body.blob t.body.blob)
          %man  ~&  %mean-todo  $(body.blob t.body.blob)
          %slo  ~&  %slow-todo  $(body.blob t.body.blob)
          %sld  ~&  %slow-todo  $(body.blob t.body.blob)
          %hit  ~&  %skip-hit  $(body.blob t.body.blob)
          %slg  ~&  (g s.i)  $(body.blob t.body.blob)
          %mew  ~&  %memo-todo  $(body.blob t.body.blob)
          %tim  ~&  %skip-tim  $(body.blob t.body.blob)
          %tom  ~&  %skip-tom  $(body.blob t.body.blob)
          %mem  ~&  %skip-mem  $(body.blob t.body.blob)
          %pol  =?(vile (~(has in vile) p.i) (~(put in vile) q.i) $(body.blob t.body.blob))
          %poi  $(vile (~(put in vile) p.i), body.blob t.body.blob)
          %ipb 
        |-  ^-  (unit *)
        ?~  p.i  ^$(body.blob t.body.blob)
        ?:  (~(has in vile) i.p.i)  ~
        $(p.i t.p.i)
      ==
    =*  i  bend.blob
    ~&  i
    ?-  -.i
        %clq  ?^((g s.i) (goto z.i) (goto o.i))
        %eqq  ?:(=((g l.i) (g r.i)) (goto z.i) (goto o.i))
        %brn  
      =/  c  (g s.i)
      ?-  c 
        %0  (goto z.i) 
        %1  (goto o.i) 
        *  ~
      ==
    ::
        %hop  (goto t.i)
        %hip  ~&  %no-hip  !!
        %lnk
      =/  s  (g u.i)
      =/  f  (g f.i)
      =/  r  (tine s f)
      ?~  r  ~
      =.  rasp  (p d.i u.r)
      (goto t.i)
    ::
        %cal
      =/  pins  (~(got by hill) a.i)
      =/  blub  (~(got by will.pins) long.pins)
      =/  r
        %=  $
          pill  pins
          blob  blub
          rasp  (afar v.i walt.pins)
          vile  (soil b.i bait.pins)
        ==
      ?~  r  ~
      =.  rasp  (p d.i u.r)
      (goto t.i)
    ::
        %caf  ~|  %caf-todo  !!
        %lnt  (tine (g u.i) (g f.i))
        %jmp
      =/  pins  (~(got by hill) a.i)
      =/  blub  (~(got by will.pins) long.pins)
      %=  $
        pill  pins
        blob  blub
        rasp  (afar v.i walt.pins)
        vile  (soil b.i bait.pins)
      ==
    ::
        %jmf  ~|  %jmf-todo  !!
        %spy  ~&  %no-scry  !!
        %mer  ~&  %skip-mem  (goto m.i)
        %don  `(g s.i)
        %bom  ~
    ==
  ++  g
    |=  r=@uvre
    (~(got by rasp) r)
  ++  p
    |=  [r=@uvre v=*]
    ~&  [%p r v]
    (~(put by rasp) r v)
  ++  mov
    |=  [s=@uvre d=@uvre] 
    ^-  _rasp
    =/  mv  (~(get by rasp) s)
    ?~  mv  ~&  [%rasp-miss s (~(has in vile) s)]  rasp
    ~&  [%p d u.mv]  (~(put by rasp) d u.mv)
  ++  goto  |=(b=bile ^$(blob (~(got by will.pill) b)))
  ++  afar
    |=  [v=(list @uvre) walt=(list @uvre)]
    =|  m=(map @uvre *)
    |-  ^-  (map @uvre *)
    ?~  v  ?>  =(~ walt)  ~&  [%c m]  m
    ?>  ?=(^ walt)
    =/  mv  (~(get by rasp) i.v)
    ?~  mv
      ~&  [%rasp-miss-afar i.v (~(has in vile) i.v)]
      $(v t.v, walt t.walt)
    $(m (~(put by m) i.walt u.mv), v t.v, walt t.walt)
  ++  soil
    |=  [b=(list @uvre) bait=(list @uvre)]
    =|  p=(set @uvre)
    |-  ^-  (set @uvre)
    ?~  b  ?>  =(~ bait)  p
    ?>  ?=(^ bait)
    =?  p  (~(has in vile) i.b)  (~(put in p) i.bait)
    $(b t.b, bait t.bait)
  --
  |%
  ++  peep
    |=  [s=* f=*]
    ^-  [[=bell hall=_hill] _line]
    =/  bull  (peek:line s f)
    ?:  ?=(^ bull)  [u.bull line]
    =.  +.line  this:(poke:line [%comp ~ s f])
    =/  ball  (peek:line s f)
    ?>  ?=(^ ball)
    [u.ball line]
  ++  star
    |=  [r=@uvre s=*]
    (~(put by *(map @uvre *)) r s)
  --
--
