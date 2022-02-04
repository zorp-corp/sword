|%
::  type representing partial information about a noun
+$  sock
  $%
    [%know k=*]
    [%bets p=sock q=sock]
    [%gues ~]
  ==
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
  |-
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
       $(b (mas b), s +<.s)
         ::
         %3
       $(b (mas b), s +>.s)
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
      [%know [p=* q=*]]
    ?-  (cap b)
        ::
        %2
      =/  r  $(s [%know +<.s], b (mas b))
      ?:  ?=  [%know k=*]  r
        [%know +.r +>.s]
      [%bets r [%know q.k.s]]
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
::  annotate a Nock formula with what we know of its result
++  wash
  |=  [s=sock f=*]
  ^-  sock
  |-
  ^-  sock
  =/  sockf
    |=  [s=sock f=sock] 
    ?.  ?=  [%know *]  f
      [%gues ~]
    ^$(s s, f +.f)
  ?+  f  ~|  %wash-bonk  !!
      ::
      [[* *] *]
    =/  pres  $(f -.f)
    =/  qres  $(f +.f)
    (knit [%bets pres [%gues ~]] 3 qres)
      ::
      [%0 b=@]
    (yarn s b.f)
      ::
      [%1 b=*]
    [%know b.f]
      ::
      [%2 b=* c=*]
    =/  bres  $(f b.f)
    =/  cres  $(f c.f)
    (sockf s=bres f=cres)
      ::
      [%3 b=*]
    =/  bres  $(f b.f)
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
    =/  bres  $(f b.f)
    ?+  bres  ~|  %wash-nest  !!
        :: do we want to do this?
        [%know a=@]
      [%know +(+.bres)]
        ::
        [%gues ~]
      [%gues ~]
    ==
      ::
      [%5 b=* c=*]
    =/  bres  $(f b.f)
    =/  cres  $(f c.f)
    (pear bres cres)
      ::
      [%6 b=* c=* d=*]
    =/  bres  $(f b.f)
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
      [%gues ~]
    ==
      ::
      [%7 b=* c=*]
    =/  bres  $(f b.f)
    $(s bres, f c.f)
      ::
      [%8 b=* c=*]
    =/  bres  $(f b.f)
    $(s (knit [%bets [%gues ~] s] 2 bres), f c.f)
      ::
      [%9 b=@ c=*]
    =/  cres  $(f c.f)
    (sockf cres (yarn cres b.f))
      ::
      [%10 [b=@ c=*] d=*]
    =/  cres  $(f c.f)
    =/  dres  $(f d.f)
    (knit dres b.f cres)
      ::
      [%11 b=@ c=*]
    $(f c.f)
      ::
      [%11 [b=@ c=*] d=*]
    +:[$(f c.f) $(f d.f)]
      ::
      [%12 ref=* path=*]
    +>:[$(f ref.f) $(f path.f) [%gues ~]]
  ==
+$  foot
  $:
    $%
      [%1 b=*]
      [%2 b=foot c=foot f=(each sock foot)]
      [%3 b=foot]
      [%4 b=foot]
      [%5 b=foot c=foot]
      [%6 b=foot c=(unit foot) d=(unit foot)]
      [%7 b=foot c=foot]
      [%8 b=foot c=foot]
      [%9 b=@ c=foot f=(each sock foot)]
      [%10 [b=@ c=foot] d=foot]
      [%11 b=@ c=foot]
      [%11 [b=@ c=foot] d=foot]
      [%12 ref=foot path=foot]
      [%cell p=foot q=foot]
      [%0 b=@]
    ==
    $=  s  sock
    $=  r  sock
  ==
++  pull
  |=  [s=sock f=*]
  ^-  foot
  |-
  ^-  foot
  =/  sockf
    |=  [s=sock f=sock]
    ^-  (each sock foot)
    ?:  ?=  [%know *]  f
      [%| ^$(f +.f)]
    [%& s]
  ?+  f  ~|  %pull-bonk  !!
      ::
      [[* *] *]
    =/  pfoot  $(f -.f)
    =/  qfoot  $(f +.f)
    [[%cell pfoot qfoot] s (knit [%bets r.pfoot [%gues ~]] 3 r.qfoot)]
      ::
      [%0 b=@]
    [[%0 b.f] s=s r=(yarn s b.f)]
      ::
      [%1 b=*]
    [[%1 b.f] s=s r=[%know b.f]]
      ::
      [%2 b=* c=*]
    =/  bfoot  $(f b.f)
    =/  cfoot  $(f c.f)
    =/  rfoot  (sockf r.bfoot r.cfoot)
    =/  r
      ^-  sock
      ?-  -.rfoot
          ::
          %&
        [%gues ~]
          ::
          %|
        +>+.rfoot
      ==
    [[%2 bfoot cfoot rfoot] s r]
      ::
      [%3 b=*]
    =/  bfoot  $(f b.f)
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
    [[%3 bfoot] s r]
      ::
      [%4 b=*]
    =/  bfoot  $(f b.f)
    =/  r
      ?+  r.bfoot  ~|  %pull-bonk  !!
          ::
          [%know @]
        [%know +(+.r.bfoot)]
          ::
          [%gues ~]
        [%gues ~]
      ==
    [[%4 bfoot] s r]
      ::
      [%5 b=* c=*]
    =/  bfoot  $(f b.f)
    =/  cfoot  $(f c.f)
    =/  r
      ?:  ?=  [%know *]  r.bfoot
        ?:  ?=  [%know *]  r.cfoot
          [%know =(k.r.bfoot k.r.cfoot)]
        [%gues ~]
      [%gues ~]
    [[%5 bfoot cfoot] s r]
      ::
      [%6 b=* c=* d=*]
    =/  bfoot  $(f b.f)
    ?+  r.bfoot  [[%6 bfoot `$(f c.f) `$(f d.f)] s [%gues ~]]
        ::
        [%know %0]
      =/  cfoot  $(f c.f)
      [[%6 bfoot `cfoot ~] s r.cfoot]
        ::
        [%know %1]
      =/  dfoot  $(f d/f)
      [[%6 bfoot ~ `dfoot] s r.dfoot]
    ==
      ::
      [%7 b=* c=*]
    =/  bfoot  $(f b.f)
    =/  cfoot  $(s r.bfoot, f c.f)
    [[%7 bfoot cfoot] s r.cfoot]
      ::
      [%8 b=* c=*]
    =/  bfoot  $(f b.f)
    =/  cfoot  $(s (knit [%bets [%gues ~] s] 2 r.bfoot), f c.f)
    [[%8 bfoot cfoot] s=s r=r.cfoot]
      ::
      [%9 b=@ c=*]
    =/  cfoot  $(f c.f)
    =/  rfoot  (sockf r.cfoot (yarn r.cfoot b.f))
    =/  r
      ?-  -.rfoot
          ::
          %&
        [%gues ~]
          %|
        +>+.rfoot
      ==
    [[%9 b.f cfoot rfoot] s r]
      ::
      [%10 [b=@ c=*] d=*]
    =/  cfoot  $(f c.f)
    =/  dfoot  $(f d.f)
    [[%10 [b.f cfoot] dfoot] s (knit r.dfoot b.f r.cfoot)]
      ::
      [%11 b=@ c=*]
    =/  cfoot  $(f c.f)
    [[%11 b.f cfoot] s r=r.cfoot]
      ::
      [%11 [b=@ c=*] d=*]
    =/  cfoot  $(f c.f)
    =/  dfoot  $(f d.f)
    [[%11 [b.f cfoot] dfoot] s r.dfoot]
      ::
      [%12 ref=* path=*]
    =/  reffoot  $(f ref.f)
    =/  pathfoot  $(f path.f)
    [[%12 reffoot pathfoot] s [%gues ~]]
  ==
--
