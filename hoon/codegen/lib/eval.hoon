/+  line
=>  $:line
=*  line  .
=/  ip=?  |
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
      ?>  (~(huge so sock.u.bull) [& u.t])
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
    ~!  moan
    =/  huns  (~(get ja moan) f)
    |-  ^-  (unit hone)
    ?~  huns  ~
    :: ~&  [%mope-i soot.i.huns]
    ~!  so
    ?:  (~(huge so soot.i.huns) [& s])
      `i.huns
    $(huns t.huns)
  ++  cope
    |=  [s=* f=*]
    ^-  [hone _this]
    =/  roan  (mope s f)
    ?:  ?=(^ roan)  [u.roan this]
    =.  sack  (rout:sack s f)
    ~&  %rout-done
    :: ~&  [%cope-moan moan]
    =/  sewn  (mope s f)
    ?.  ?=(^ sewn)  ~|  [%mope-miss s f]  !!
    [u.sewn this]
  --
++  tine
  |=  [s=* f=*]
  =<
  =^  [indy=@uxor mont=_fuji]  line  (peep s f)
  =/  pile  (~(got by hill.mont) indy)
  =/  blob  (~(got by will.pile) `@uwoo`0)
  =/  rasp=(map @uvre (unit))  [[0v0 `s] ~ ~]
  =|  mean=(list [@ta *])
  =|  jets=(map [path @] $-(* *))
  |^  ^-  tone
    ?^  body.blob
      =*  i  i.body.blob
      ~?  ip  i
      ?-  -.i
          %imm  $(rasp (p d.i `n.i), body.blob t.body.blob)
          %mov  $(rasp (mov s.i d.i), body.blob t.body.blob)
          %inc
        =/  c  (g s.i)
        %=  $
          rasp       (p d.i ?.(?=([~ @] c) ~ `+(u.c)))
          body.blob  t.body.blob
        ==
      ::
          %con
        $(rasp (p d.i (both (g h.i) (g t.i))), body.blob t.body.blob)
          %hed
        =/  c  (g s.i)
        %=  $
          rasp       (p d.i ?.(?=([~ ^] c) ~ `-.u.c))
          body.blob  t.body.blob
        ==
      ::
          %tal
        =/  c  (g s.i)
        %=  $
          rasp       (p d.i ?.(?=([~ ^] c) ~ `+.u.c))
          body.blob  t.body.blob
        ==
      ::
          %men
        %=  $
          body.blob  t.body.blob
          mean       [[l.i (g s.i)] mean]
        ==
          %man
        %=  $
          body.blob  t.body.blob
          mean       ~|(%mean-miss +.mean)
        ==
          %slo  ~&  %slow-todo  $(body.blob t.body.blob)
          %sld  ~&  %slow-todo  $(body.blob t.body.blob)
          %hit  ~&  %skip-hit  $(body.blob t.body.blob)
          %slg  ~&  (g s.i)  $(body.blob t.body.blob)
          %mew  ~&  %memo-todo  $(body.blob t.body.blob)
          %tim  ~&  %skip-tim  $(body.blob t.body.blob)
          %tom  ~&  %skip-tom  $(body.blob t.body.blob)
          %mem  ~&  %skip-mem  $(body.blob t.body.blob)
          %poi  $(rasp (p p.i ~), body.blob t.body.blob)
          %ipb
        |-  ^-  tone
        ?~  p.i  ^$(body.blob t.body.blob)
        ?~  (g i.p.i)  [%2 mean]
        $(p.i t.p.i)
      ==
    =*  i  bend.blob
    ~?  ip  i
    ?-  -.i
        %clq
      =/  c  +:(g s.i)
      ?^(c (goto z.i) (goto o.i))
        %eqq  ?:(=(+:(g l.i) +:(g r.i)) (goto z.i) (goto o.i))
        %brn
      =/  c  +:(g s.i)
      ?-  c
        %0  (goto z.i)
        %1  (goto o.i)
        *  [%2 mean]
      ==
    ::
        %hop  (goto t.i)
        %hip  ~&  %no-hip  !!
        %lnk
      =/  r  (tine +:(g u.i) +:(g f.i))
      ?.  ?=(%0 -.r)  r
      =.  rasp  (p d.i `product.r)
      (goto t.i)
    ::
        %cal
      =/  pyle  (~(got by hill.mont) a.i)
      =/  r
        %=  $
          blob  (~(got by will.pyle) 0w1)
          pile  pyle
          rasp  (afar v.i walt.pyle)
        ==
      ?.  ?=(%0 -.r)  r
      =.  rasp  (p d.i `product.r)
      (goto t.i)
    ::
        %caf
      =/  gate=(unit $-(* *))  (~(get by jets) n.i)
      =/  subj  +:(g u.i)
      ?.  ?=(~ gate)
        =/  r  (mack gate +6.subj)
        ?~  r  [%2 mean]  [%0 u.r]
      =/  pyle  (~(got by hill.mont) a.i)
      =/  r
        %=  $
          blob  (~(got by will.pyle) 0w1)
          pile  pyle
          rasp  (afar v.i walt.pyle)
        ==
      ?.  ?=(%0 -.r)  r
      =.  rasp  (p d.i `product.r)
      (goto t.i)
        %lnt  (tine +:(g u.i) +:(g f.i))
        %jmp
      =/  pyle  (~(got by hill.mont) a.i)
      %=  $
        pile  pyle
        blob  (~(got by will.pyle) 0w1)
        rasp  (afar v.i walt.pyle)
      ==
    ::
        %jmf
      =/  gate=(unit $-(* *))  (~(get by jets) n.i)
      =/  subj  +:(g u.i)
      ?.  ?=(~ gate)
        =/  r  (|=(s=* (mack gate +6.subj)))
        ?~  r  [%2 mean]  [%0 u.r]
      =/  pyle  (~(got by hill.mont) a.i)
      %=  $
        pile  pyle
        blob  (~(got by will.pyle) 0w1)
        rasp  (afar v.i walt.pyle)
      ==
        %spy  ~&  %no-scry  !!
        %mer  ~&  %skip-mem  (goto m.i)
        %don  [%0 +:(g s.i)]
        %bom  [%2 mean]
    ==
  ++  g
    |=  r=@uvre
    ~|  r   (~(got by rasp) r)
  ++  p
    ::  XX assert not present (bc SSA)
    |=  [r=@uvre v=(unit)]
    ~?  ip  [%p r v]
    (~(put by rasp) r v)
  ++  mov
    |=  [s=@uvre d=@uvre]
    ^-  _rasp
    (~(put by rasp) d (~(got by rasp) s))
  ++  goto  |=(b=@uwoo ^$(blob (~(got by will.pile) b)))
  ++  afar
    |=  [v=(list @uvre) walt=(list @uvre)]
    =|  m=(map @uvre (unit))
    |-  ^+  m
    ?~  v  ?>  =(~ walt)  ~?  ip  [%c m]  m
    ?>  ?=(^ walt)
    $(m (~(put by m) i.walt (g i.v)), v t.v, walt t.walt)
  --
  |%
  ++  peep
    |=  [s=* f=*]
    ^-  [[@uxor ^fuji] _line]
    =/  bull  (peek:line s f)
    ?:  ?=(^ bull)  [u.bull line]
    =.  line  this:(poke:line [%comp ~ s f])
    =/  ball  (peek:line s f)
    ?>  ?=(^ ball)
    [u.ball line]
  --
--
