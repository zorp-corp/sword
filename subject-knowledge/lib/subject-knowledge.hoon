=<
::  a memoization for formula analysis, updated/inspected at eval
::  boundaries (2/9)
=|  memo=(map [sock *] [(unit sock) (unit @tas)])
::  A jet dashboard
::
::  The @tas tag is used only for debugging
=|  jute=(map * (list [sock [$-(* *) @tas]]))
|%
::  Partial knowledge of a noun
+$  sock  ^sock
++  bord  *(map * (list [sock [$-(* *) @tas]]))
::  test whether a sock nests in another sock
::  a=sock nests in b=sock if b has the same information as a, or
::  strictly more information
++  mous
  |=  [a=sock b=sock]
  ^-  ?
  |-
  ^-  ?
  ?:  ?=  [%gues ~]  a
    %.y
  ?-  a
      ::
      [%know *]
    ?|
      ?&
        ?=  [%know *]  b
        =(k.a k.b)
      ==
      ?&
        ?=  [%bets * *]  b
        ?=  [* *]  k.a
        $(a [%know -.k.a], b p.b)
        $(a [%know +.k.a], b q.b)
      ==
    ==
      ::
      [%bets * *]
    ?|
      ?&
        ?=  [%know * *]  b
        $(a p.a, b [%know -.k.b])
        $(a q.a, b [%know +.k.b])
      ==
      ?&
        ?=  [%bets * *]  b
        $(a p.a, b p.b)
        $(a q.a, b q.b)
      ==
    ==
  ==
::  Check for a jet
++  juke
  |=  [s=sock f=*]
  ^-  (unit [$-(* *) @tas])
  =/  jets
    =/  j  (~(get by jute) f)
    ^-  (list [sock [$-(* *) @tas]])
    ?~  j  ~  u.j
  |-
  ^-  (unit [$-(* *) @tas])
  ?~  jets
    ~
  ?:  (mous -<.jets s)
    `->.jets
  $(jets +.jets)
::  learn a noun at an address
++  darn
  |=  [s=sock b=@ n=*]
  ^-  sock
  ?:  =(b 1)
    [%know n]
  ?:  ?=  [%know *]  s
    :-  %know
    .*  [k.s n]  [%10 [b [0 3]] [0 2]] 
  ?-  (cap b)
      ::
      %2
    ?-  s
        ::
        [%gues ~]
      [%bets p=$(b (mas b), s [%gues ~]) q=[%gues ~]]
        ::
        [%bets p=* q=*]
      [%bets p=$(b (mas b), s p.s) q=q.s]
    ==
      ::
      %3
    ?-  s
        ::
        [%gues ~]
      [%bets p=[%gues ~] q=$(b (mas b), s [%gues ~])]
        ::
        [%bets p=* q=*]
      [%bets p=p.s q=$(b (mas b), s q.s)]
    ==
  ==
::  axis into a sock
++  yarn
  |=  [s=sock b=@]
  ^-  sock
  ?<  =(b 0)
  |-
  ?:  =(b 1)
    s
  ?-  s
      ::
      [%know *]
    :-  %know
    .*  k.s  [0 b]
      ::
      [%gues ~]
    [%gues ~]
      ::
      [%bets p=* q=*]
     ?-  (cap b)
         ::
         %2
       $(b (mas b), s p.s)
         ::
         %3
       $(b (mas b), s q.s)
     ==
  ==
::  make a new sock from 2 socks (and an axis)
++  knit
  |=  [s=sock b=@ t=sock]
  ^-  sock
  ?:  =(b 1)
    t
  ?-  s
      ::
      [%know @]
    ~|  %know-atom  !!
      ::
      [%know * *]
    ?-  (cap b)
        ::
        %2
      =/  r  $(s [%know +<.s], b (mas b))
      ?:  ?=  [%know k=*]  r
        [%know +.r +>.s]
      [%bets r [%know +.k.s]]
        ::
        %3
      =/  r  $(s [%know +>.s], b (mas b))
      ?:  ?=  [%know k=*]  r
        [%know +<.s +.r]
      [%bets [%know +<.s] r]
    ==
      ::
      [%bets p=* q=*]
    ?-  (cap b)
        ::
        %2
      =/  r  $(s p.s, b (mas b))
      ?:  ?=  [%know k=*]  r
        ?:  ?=  [%know k=*]  q.s
          [%know +.r +.q.s]
        [%bets r q.s]
      [%bets r q.s]
        ::
        %3
      =/  r  $(s q.s, b (mas b))
      ?:  ?=  [%know k=*]  r
        ?:  ?=  [%know k=*]  p.s
          [%know +.p.s +.r]
        [%bets p.s r]
      [%bets p.s r]
    ==
      ::
      [%gues ~]
    =/  r  $(b (mas b))
    ?:  ?=  [%gues ~]  r
      [%gues ~]
    ?-  (cap b)
        ::
        %2
      [%bets r [%gues ~]]
        ::
        %3
      [%bets [%gues ~] r]
    ==
  ==
++  pear
  |=  [a=sock b=sock]
  ^-  sock
  ?:  ?=  [%know *]  a
    ?:  ?=  [%know *]  b
      [%know =(a b)]
    [%gues ~]
  [%gues ~]
::  Compute what we know of a Nock formula's result
++  wash
  |=  [s=sock f=*]
  ^-  [sock _memo]
  |-
  ^-  [sock _memo]
  =/  sockf
    |=  [s=sock f=sock] 
    ^-  [sock _memo]
    ?.  ?=  [%know *]  f
      [[%gues ~] memo]
    =/  m  (~(get by memo) s k.f)
    ?~  m
      :: memo miss
      =.  memo  (~(put by memo) [s k.f] [~ ~])
      =^  r  memo  ^$(s s, f +.f)
      [r (~(put by memo) [s k.f] [`r ~])] 
    ?~  -.u.m
      ::  memo blackhole
      [[%gues ~] memo]
    ::  memo hit]
    [u.-.u.m memo]
  ?+  f  ~|  %wash-bonk  !!
      ::
      [[* *] *]
    =^  pres  memo  $(f -.f)
    =^  qres  memo  $(f +.f)
    [(knit [%bets pres [%gues ~]] 3 qres) memo]
      ::
      [%0 b=@]
    [(yarn s b.f) memo]
      ::
      [%1 b=*]
    [[%know b.f] memo]
      ::
      [%2 b=* c=*]
    =^  bres  memo  $(f b.f)
    =^  cres  memo  $(f c.f)
    (sockf s=bres f=cres)
      ::
      [%3 b=*]
    =^  bres  memo  $(f b.f)
    :_  memo
    ?-  bres
        ::
        [%know @]
      [%know 1]
        ::
        [%know * *]
      [%know 0]
        ::
        [%bets * *]
      [%know 0]
        :: maybe we want to distinguish unknown noun and unknown atom?
        :: (bet is unknown cell...)
        [%gues ~]
      [%gues ~]
    ==
      ::
      [%4 b=*]
    =^  bres  memo  $(f b.f)
    [[%gues ~] memo]
      ::
      [%5 b=* c=*]
    =^  bres  memo  $(f b.f)
    =^  cres  memo  $(f c.f)
    [(pear bres cres) memo]
      ::
      [%6 b=* c=* d=*]
    =^  bres  memo  $(f b.f)
    ?+  bres  ~|  %wash-nest  !!
        ::
        [%know %0]
      $(f c.f)
        ::
        [%know %1]
      $(f d.f)
        :: can we merge them somehow in this case? things the branches
        :: agree on?
        [%gues ~]
      [[%gues ~] memo]
    ==
      ::
      [%7 b=* c=*]
    =^  bres  memo  $(f b.f)
    $(s bres, f c.f)
      ::
      [%8 b=* c=*]
    =^  bres  memo  $(f b.f)
    $(s (knit [%bets [%gues ~] s] 2 bres), f c.f)
      ::
      [%9 b=@ c=*]
    =^  cres  memo  $(f c.f)
    (sockf cres (yarn cres b.f))
      ::
      [%10 [b=@ c=*] d=*]
    =^  cres  memo  $(f c.f)
    =^  dres  memo  $(f d.f)
    [(knit dres b.f cres) memo]
      ::
      [%11 b=@ c=*]
    $(f c.f)
      ::
      [%11 [b=@ c=*] d=*]
    =^  cres  memo  $(f c.f)
    ?:  =(b.f %data)
      =/  c  c.f
      |-
      ?:  ?=  [[%0 @] *]  c
        =.  s  (knit s ->.c [%gues ~])
        $(c +.c)
      ^$(f d.f)
    $(f d.f)
      ::
      [%12 ref=* path=*]
    =^  rres  memo  $(f ref.f)
    =^  pres  memo  $(f path.f)
    [[%gues ~] memo]
  ==
::  Description of knowledge at a call site
::
::  Codegen behavior
::  %dyn means we will generate a full eval: check cache or codegen,
::  guard on stored sock
::
::  %mis means we will do fresh inline codegen of a formula with a new
::  label
::
::  %rec and %hit both mean we will generate to jumps to labels
::  They differ because for %rec the analysis must treat the call as
::  unknowable, while for %hit we do not re-analyze the call but return
::  the memoized sock
::
::  %jet corresponds to a jump to a jet
+$  coot
  $%
    [%dyn =sock]     :: we don't know the formula
    [%mis =foot]     :: we know the formula, it's not memoized
    [%rec =sock f=*] :: a recursive call, the memo table has a blackhole for this sock/formula pair
    [%hit res=sock]  :: a memoized call, the memo table has an entry for this sock/formula pair
    [%jet jet=@tas]  :: call would be jetted 
  ==
::  Annotated Nock tree with subject knowledge
+$  foot
  $:
    $%
      [%1 b=*]
      [%2 b=foot c=foot =coot]
      [%3 b=foot]
      [%4 b=foot]
      [%5 b=foot c=foot]
      [%6 b=foot c=(unit foot) d=(unit foot)]
      [%7 b=foot c=foot]
      [%8 b=foot c=foot]
      [%9 b=@ c=foot =coot]
      [%10 [b=@ c=foot] d=foot]
      [%11 b=@ c=foot]
      [%11 [b=@ c=foot] d=foot]
      [%12 ref=foot path=foot]
      [%cell p=foot q=foot]
      [%0 b=@]
    ==
    $=  s  sock
    $=  r  sock
    $=  h  (unit [@ (unit sock)])
  ==
++  cort
  |=  =coot
  ^-  sock
  ?-  coot
      ::
      [%dyn *]
    [%gues ~]
      ::
      [%mis *]
    r.foot.coot
      ::
      [%rec *]
    [%gues ~]
      ::
      [%hit *]
    res.coot
      ::
      [%jet *]
    [%gues ~]
  ==
++  pull-eval
  |=  [s=sock f=sock h=(unit [@ (unit sock)])]
  ^-  [coot _memo]
  =/  nam
    ?~  h  ~
    ?.  =(-.u.h %fast)  ~
    ?~  +.u.h  ~
    =/  namsock  (yarn u.+.u.h 2)
    ?.  ?=  [%know @]  namsock  ~
    `k.namsock
  ?.  ?=  [%know *]  f
    ~&  "Dyn: {<s>}"
    [[%dyn s] memo]
  =/  jet  (juke s k.f)
  ?.  ?=  ~  jet
    ::  found a jet
    ~&  "Jet: {<+.u.jet>}"
    [[%jet +.u.jet] memo]
  =/  mem  (~(get by memo) [s k.f])
  ?~  mem
    :: memo miss
    =.  memo  (~(put by memo) [s k.f] [~ nam]) :: blackhole for recursive eval
    =^  res  memo  (pull s k.f)
    ~&  "Miss: sock {<s>} formula {<k.f>}"
    =.  memo  (~(jab by memo) [s k.f] |=([(unit sock) nm=(unit @tas)] [`r.res nm]))
    [[%mis res] memo] :: fill in result
  ?~  -.u.mem
    :: memo blackhole
    ~&  "Recur: sock {<[s]>} formula {<k.f>}"
    [[%rec s k.f] memo]
  :: memo hit
  ~&  "Hit: sock {<[s]>} formula {<k.f>} result {<u.u.mem>}"
  [[%hit u.-.u.mem] memo] 
++  pull
  |=  [s=sock f=*]
  ^-  [foot _memo]
  ?+  f  ~|  "Unrecognized nock {<f>}"  ~|  %pull-bonk  !!
      ::
      [[* *] *]
    =^  pfoot  memo  $(f -.f)
    =^  qfoot  memo  $(f +.f)
    [[[%cell pfoot qfoot] s=s r=(knit [%bets r.pfoot [%gues ~]] 3 r.qfoot) h=~] memo]
      ::
      [%0 b=@]
    [[[%0 b.f] s=s r=(yarn s b.f) h=~] memo]
      ::
      [%1 b=*]
    [[[%1 b.f] s=s r=[%know b.f] h=~] memo]
      ::
      [%2 b=* c=*]
    =^  bfoot  memo  $(f b.f)
    =^  cfoot  memo  $(f c.f)
    =^  coot  memo  (pull-eval r.bfoot r.cfoot h.cfoot)
    [[[%2 bfoot cfoot coot] s (cort coot) h=~] memo]
      ::
      [%3 b=*]
    =^  bfoot  memo  $(f b.f)
    =/  r
      ?-  r.bfoot
          ::
          [%know @]
        [%know 1]
          ::
          [%know * *]
        [%know 0]
          ::
          [%bets * *]
        [%know 0]
          ::
          [%gues ~]
        [%gues ~]
      ==
    [[[%3 bfoot] s r h=~] memo]
      ::
      [%4 b=*]
    =^  bfoot  memo  $(f b.f)
    [[[%4 bfoot] s [%gues ~] h=~] memo]
      ::
      [%5 b=* c=*]
    =^  bfoot  memo  $(f b.f)
    =^  cfoot  memo  $(f c.f)
    =/  r
      ?:  ?=  [%know *]  r.bfoot
        ?:  ?=  [%know *]  r.cfoot
          [%know =(k.r.bfoot k.r.cfoot)]
        [%gues ~]
      [%gues ~]
    [[[%5 bfoot cfoot] s r h=~] memo]
      ::
      [%6 b=* c=* d=*]
    =^  bfoot  memo  $(f b.f)
    ?+  r.bfoot  ~|  %pull-nest  !!
        ::
        [%know %0]
      =^  cfoot  memo  $(f c.f)
      [[[%6 bfoot `cfoot ~] s r.cfoot h=~] memo]
        ::
        [%know %1]
      =^  dfoot  memo  $(f d/f)
      [[[%6 bfoot ~ `dfoot] s r.dfoot h=~] memo]
        ::
        [%gues ~]
      =^  cfoot  memo  $(f c.f)
      =^  dfoot  memo  $(f d.f)
      [[[%6 bfoot `cfoot `dfoot] s [%gues ~] h=~] memo]
    ==
      ::
      [%7 b=* c=*]
    =^  bfoot  memo  $(f b.f)
    =^  cfoot  memo  $(s r.bfoot, f c.f)
    [[[%7 bfoot cfoot] s r.cfoot h=~] memo]
      ::
      [%8 b=* c=*]
    =^  bfoot  memo  $(f b.f)
    =^  cfoot  memo  $(s (knit [%bets [%gues ~] s] 2 r.bfoot), f c.f)
    [[[%8 bfoot cfoot] s=s r=r.cfoot h=~] memo]
      ::
      [%9 b=@ c=*]
    =^  cfoot  memo  $(f c.f)
    =^  coot  memo  (pull-eval r.cfoot (yarn r.cfoot b.f) h.cfoot)
    [[[%9 b.f cfoot coot] s (cort coot) h=h.cfoot] memo]
      ::
      [%10 [b=@ c=*] d=*]
    =^  cfoot  memo  $(f c.f)
    =^  dfoot  memo  $(f d.f)
    [[[%10 [b.f cfoot] dfoot] s (knit r.dfoot b.f r.cfoot) h=h.dfoot] memo]
      ::
      [%11 b=@ c=*]
    =^  cfoot  memo  $(f c.f)
    [[[%11 b.f cfoot] s r=r.cfoot h=`[b.f ~]] memo]
      ::
      [%11 [b=@ c=*] d=*]
    =^  cfoot  memo  $(f c.f)
    ::  implement the %data hint
    ::  delete the axes which are looked up in the hint from the subject
    ::  knowledge for the hinted computation
    ?:  =(b.f %data)
      =/  c  c.f
      |-
      ?:  ?=  [[%0 @] *]  c
        =.  s  (knit s ->.c [%gues ~])
        $(c +.c)
      =^  dfoot  memo  ^$(f d.f)
      [[[%11 [b.f cfoot] dfoot] s r.dfoot h=`[b.f `r.cfoot]] memo]
    =^  dfoot  memo  $(f d.f)
    [[[%11 [b.f cfoot] dfoot] s r.dfoot h=`[b.f `r.cfoot]] memo]
      ::
      [%12 ref=* path=*]
    =^  reffoot  memo  $(f ref.f)
    =^  pathfoot  memo  $(f path.f)
    [[[%12 reffoot pathfoot] s [%gues ~] h=~] memo]
  ==
::  example nocks for testing
++  nocs
  |%
  ++  dec
    !=
    !.
    =>
    =>  %ed
    |%
    ++  dec
      ~/  %dec
      |=  x=@
      ~>  %data.[x ~]
      ^-  @
      =|  d=@
      |-
      ^-  d=@
      ?:  =(.+(d) x)
        d
      $(d .+(d))
    --
    (dec 8)
  ++  ad
    !=
    !.
    =>
    =>  %ed
    |%
    ++  dec
      ~/  %dec
      |=  x=@
      ~>  %data.[x ~]
      ^-  @
      =|  d=@
      |-
      ^-  d=@
      ?:  =(.+(d) x)
        d
      $(d .+(d))
    ++  add
      ~/  %add
      |=  [x=@ y=@]
      ~>  %data.[x y ~]
      ^-  @
      |-
      ^-  @
      ?:  =(x 0)
        y
      $(x (dec x), y .+(y))
    --
    (add 5 8)
  --
--
|%
+$  sock
  $%
    [%know k=*]
    [%hint s=@ h=sock r=sock]
    [%bets p=sock q=sock]
    [%gues ~]
  ==
--
