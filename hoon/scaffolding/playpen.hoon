::  This file aims to approach hoon.hoon, as the pieces necessary to run a live
::  ship with Ares as Serf are written. Required to run toddler.hoon as an Arvo.
::
!.
=>  %a50
~%  %a.50  ~  ~
|%
::
::  Types
::
+$  cord  @t
+$  knot  @ta
+$  ship  @p
+$  life  @ud
+$  rift  @ud
+$  pass  @
+$  bloq  @
+$  step  _`@u`1
+$  bite  $@(bloq [=bloq =step])
+$  octs  [p=@ud q=@]
+$  dime  [p=@ta q=@]
+$  pint  [p=[p=@ q=@] q=[p=@ q=@]]
+$  spot  [p=path q=pint]
+$  mold  $~(* $-(* *))
++  unit  |$  [item]  $@(~ [~ u=item])
++  list  |$  [item]  $@(~ [i=item t=(list item)])
++  lest  |$  [item]  [i=item t=(list item)]
+$  tape  (list @tD)
+$  path  (list knot)
+$  coin  $~  [%$ %ud 0]
          $%  [%$ p=dime]
              [%blob p=*]
              [%many p=(list coin)]
          ==
+$  tone  $%  [%0 product=*]
              [%1 block=*]
              [%2 trace=(list [@ta *])]
          ==
++  trel
  |$  [first second third]
  [p=first q=second r=third]
++  qual
  |$  [first second third fourth]
  [p=first q=second r=third s=fourth]
+$  tank
  $+  tank
  $~  leaf/~
  $@  cord
  $%  [%leaf p=tape]
      [%palm p=(qual tape tape tape tape) q=(list tank)]
      [%rose p=(trel tape tape tape) q=(list tank)]
  ==
+$  toon  $%  [%0 p=*]
              [%1 p=*]
              [%2 p=(list tank)]
          ==
++  tree  |$  [node]  $@(~ [n=node l=(tree node) r=(tree node)])
++  pair  |$  [head tail]  [p=head q=tail]
::
++  map
  |$  [key value]
  $|  (tree (pair key value))
  |=(a=(tree (pair)) ?:(=(~ a) & ~(apt by a)))
::
++  set
  |$  [item]
  $|  (tree item)
  |=(a=(tree) ?:(=(~ a) & ~(apt in a)))
::
++  jug   |$  [key value]  (map key (set value))
::
::  Basic arithmetic
::
++  add                                                 ::  unsigned addition
  ~/  %add
  |=  [a=@ b=@]
  ~>  %sham.%add
  ^-  @
  ?:  =(0 a)  b
  $(a (dec a), b +(b))
::
++  dec                                                 ::  decrement
  ~/  %dec
  |=  a=@
  ~>  %sham.%dec
  ~_  leaf+"decrement-underflow"
  ?<  =(0 a)
  =+  b=0
  |-  ^-  @
  ?:  =(a +(b))  b
  $(b +(b))
::
++  div                                                 ::  divide
  ~/  %div
  |:  [a=`@`1 b=`@`1]
  ~>  %sham.%div
  ^-  @
  ~_  leaf+"divide-by-zero"
  ?<  =(0 b)
  =+  c=0
  |-
  ?:  (lth a b)  c
  $(a (sub a b), c +(c))
::
++  dvr                                                 ::  divide w/remainder
  ~/  %dvr
  |:  [a=`@`1 b=`@`1]
  ~>  %sham.%dvr
  ^-  [p=@ q=@]
  [(div a b) (mod a b)]
::
++  gte                                                 ::  greater or equal
  ~/  %gte
  |=  [a=@ b=@]
  ~>  %sham.%gte
  ^-  ?
  !(lth a b)
::
++  gth                                                 ::  greater
  ~/  %gth
  |=  [a=@ b=@]
  ~>  %sham.%gth
  ^-  ?
  !(lte a b)
::
++  lte                                                 ::  less or equal
  ~/  %lte
  |=  [a=@ b=@]
  ~>  %sham.%lte
  |(=(a b) (lth a b))
::
++  lth                                                 ::  less
  ~/  %lth
  |=  [a=@ b=@]
  ~>  %sham.%lth
  ^-  ?
  ?&  !=(a b)
      |-
      ?|  =(0 a)
          ?&  !=(0 b)
              $(a (dec a), b (dec b))
  ==  ==  ==
::
++  mod                                                 ::  modulus
  ~/  %mod
  |:  [a=`@`1 b=`@`1]
  ~>  %sham.%mod
  ^-  @
  ?<  =(0 b)
  (sub a (mul b (div a b)))
::
++  mul                                                 ::  multiply
  ~/  %mul
  |:  [a=`@`1 b=`@`1]
  ~>  %sham.%mul
  ^-  @
  =+  c=0
  |-
  ?:  =(0 a)  c
  $(a (dec a), c (add b c))
::
++  sub                                                 ::  subtract
  ~/  %sub
  |=  [a=@ b=@]
  ~>  %sham.%sub
  ~_  leaf+"subtract-underflow"
  ::  difference
  ^-  @
  ?:  =(0 b)  a
  $(a (dec a), b (dec b))
::
::  Tree addressing
::
++  cap                                                 ::  index in head or tail
  ~/  %cap                                              
  |=  a=@
  ~>  %sham.%cap
  ^-  ?(%2 %3)
  ?-  a
    %2        %2
    %3        %3
    ?(%0 %1)  !!
    *         $(a (div a 2))
  ==
::
++  mas                                                 ::  axis within head/tail
  ~/  %mas
  |=  a=@
  ~>  %sham.%mas
  ^-  @
  ?-  a
    ?(%2 %3)  1
    ?(%0 %1)  !!
    *         (add (mod a 2) (mul $(a (div a 2)) 2))
  ==
::
::  List logic
::
++  flop                                                ::  reverse
  ~/  %flop
  |*  a=(list)
  =>  .(a (homo a))
  ^+  a
  =+  b=`_a`~
  |-
  ?~  a  b
  $(a t.a, b [i.a b])
::
++  homo                                                ::  homogenize
  |*  a=(list)
  ^+  =<  $
    |@  ++  $  ?:(*? ~ [i=(snag 0 a) t=$])
    --
  a
::
++  lent                                                ::  length
  ~/  %lent
  |=  a=(list)
  ^-  @
  =+  b=0
  |-
  ?~  a  b
  $(a t.a, b +(b))
::
++  reap                                                ::  replicate
  ~/  %reap
  |*  [a=@ b=*]
  |-  ^-  (list _b)
  ?~  a  ~
  [b $(a (dec a))]
::
++  roll                                                ::  left fold
  ~/  %roll
  |*  [a=(list) b=_=>(~ |=([* *] +<+))]
  |-  ^+  ,.+<+.b
  ?~  a
    +<+.b
  $(a t.a, b b(+<+ (b i.a +<+.b)))
::
++  scag                                                ::  prefix
  ~/  %scag
  |*  [a=@ b=(list)]
  |-  ^+  b
  ?:  |(?=(~ b) =(0 a))  ~
  [i.b $(b t.b, a (dec a))]
::
++  slag                                                ::  suffix
  ~/  %slag
  |*  [a=@ b=(list)]
  |-  ^+  b
  ?:  =(0 a)  b
  ?~  b  ~
  $(b t.b, a (dec a))
::
++  snag                                                ::  index
  ~/  %snag
  |*  [a=@ b=(list)]
  |-  ^+  ?>(?=(^ b) i.b)
  ?~  b
    ~_  leaf+"snag-fail"
    !!
  ?:  =(0 a)  i.b
  $(b t.b, a (dec a))
::
++  turn                                                ::  transform
  ~/  %turn
  |*  [a=(list) b=gate]
  =>  .(a (homo a))
  ^-  (list _?>(?=(^ a) (b i.a)))
  |-
  ?~  a  ~
  [i=(b i.a) t=$(a t.a)]
::
++  weld                                                ::  concatenate
  ~/  %weld
  |*  [a=(list) b=(list)]
  =>  .(a ^.(homo a), b ^.(homo b))
  |-  ^+  b
  ?~  a  b
  [i.a $(a t.a)]
::
++  welp                                                ::  concatenate
  ~/  %welp
  =|  [* *]
  |@
  ++  $
    ?~  +<-
      +<-(. +<+)
    +<-(+ $(+<- +<->))
  --
::
::  Bit arithmetic
::
++  bex                                                 ::  binary exponent
  ~/  %bex
  |=  a=bloq
  ~>  %sham.%bex
  ^-  @
  ?:  =(0 a)  1
  (mul 2 $(a (dec a)))
++  can                                                 ::  assemble
  ~/  %can
  |=  [a=bloq b=(list [p=step q=@])]
  ~>  %sham.%can
  ^-  @
  ?~  b  0
  (add (end [a p.i.b] q.i.b) (lsh [a p.i.b] $(b t.b)))
::
++  cat                                                 ::  concatenate
  ~/  %cat
  |=  [a=bloq b=@ c=@]
  ~>  %sham.%cat
  (add (lsh [a (met a b)] c) b)
::
++  cut                                                 ::  slice
  ~/  %cut
  |=  [a=bloq [b=step c=step] d=@]
  ~>  %sham.%cut
  (end [a c] (rsh [a b] d))
::
++  end                                                 ::  tail
  ~/  %end
  |=  [a=bite b=@]
  ~>  %sham.%end
  =/  [=bloq =step]  ?^(a a [a *step])
  (mod b (bex (mul (bex bloq) step)))
::
++  fil                                                 ::  fill bloqstream
  ~/  %fil
  |=  [a=bloq b=step c=@]
  =|  n=@ud
  =.  c  (end a c)
  =/  d  c
  |-  ^-  @
  ?:  =(n b)
    (rsh a d)
  $(d (add c (lsh a d)), n +(n))
::
++  lsh                                                 ::  left-shift
  ~/  %lsh
  |=  [a=bite b=@]
  ~>  %sham.%lsh
  =/  [=bloq =step]  ?^(a a [a *step])
  (mul b (bex (mul (bex bloq) step)))
::
++  met                                                 ::  measure
  ~/  %met
  |=  [a=bloq b=@]
  ~>  %sham.%met
  ^-  @
  =+  c=0
  |-
  ?:  =(0 b)  c
  $(b (rsh a b), c +(c))
::
++  rap                                                 ::  assemble variable
  ~/  %rap
  |=  [a=bloq b=(list @)]
  ~>  %sham.%rap
  ^-  @
  ?~  b  0
  (cat a i.b $(b t.b))
::
++  rep                                                 ::  assemble fixed
  ~/  %rep
  |=  [a=bite b=(list @)]
  ~>  %sham.%rep
  =/  [=bloq =step]  ?^(a a [a *step])
  =|  i=@ud
  |-  ^-  @
  ?~  b   0
  %+  add  $(i +(i), b t.b)
  (lsh [bloq (mul step i)] (end [bloq step] i.b))
::
++  rev                                                 ::  reverse block order
  ~/  %rev
  |=  [boz=bloq len=@ud dat=@]
  ~>  %sham.%rev
  ^-  @
  =.  dat  (end [boz len] dat)
  %+  lsh
    [boz (sub len (met boz dat))]
  (swp boz dat)
::
++  rip                                                 ::  disassemble
  ~/  %rip
  |=  [a=bite b=@]
  ~>  %sham.%rip
  ^-  (list @)
  ?:  =(0 b)  ~
  [(end a b) $(b (rsh a b))]
::
++  rsh                                                 ::  right-shift
  ~/  %rsh
  |=  [a=bite b=@]
  ~>  %sham.%rsh
  =/  [=bloq =step]  ?^(a a [a *step])
  (div b (bex (mul (bex bloq) step)))
::
++  swp                                                 ::  naive rev bloq order
  ~/  %swp
  |=  [a=bloq b=@]
  (rep a (flop (rip a b)))
::
::  Modular arithmetic
::
++  fe                                                  ::  modulo bloq
  |_  a=bloq
  ++  dif                                               ::  difference
    |=([b=@ c=@] (sit (sub (add out (sit b)) (sit c))))
  ++  inv  |=(b=@ (sub (dec out) (sit b)))              ::  inverse
  ++  net  |=  b=@  ^-  @                               ::  flip byte endianness
           =>  .(b (sit b))
           ?:  (lte a 3)
             b
           =+  c=(dec a)
           %+  con
             (lsh c $(a c, b (cut c [0 1] b)))
           $(a c, b (cut c [1 1] b))
  ++  out  (bex (bex a))                                ::  mod value
  ++  rol  |=  [b=bloq c=@ d=@]  ^-  @                  ::  roll left
           =+  e=(sit d)
           =+  f=(bex (sub a b))
           =+  g=(mod c f)
           (sit (con (lsh [b g] e) (rsh [b (sub f g)] e)))
  ++  ror  |=  [b=bloq c=@ d=@]  ^-  @                  ::  roll right
           =+  e=(sit d)
           =+  f=(bex (sub a b))
           =+  g=(mod c f)
           (sit (con (rsh [b g] e) (lsh [b (sub f g)] e)))
  ++  sum  |=([b=@ c=@] (sit (add b c)))                ::  wrapping add
  ++  sit  |=(b=@ (end a b))                            ::  enforce modulo
  --
::
::  Bit logic
::
++  con                                                 ::  binary or
  ~/  %con
  |=  [a=@ b=@]
  ~>  %sham.%con
  =+  [c=0 d=0]
  |-  ^-  @
  ?:  ?&(=(0 a) =(0 b))  d
  %=  $
    a   (rsh 0 a)
    b   (rsh 0 b)
    c   +(c)
    d   %+  add  d
          %+  lsh  [0 c]
          ?&  =(0 (end 0 a))
              =(0 (end 0 b))
          ==
  ==
::
++  dis                                                 ::  binary and
  ~/  %dis
  |=  [a=@ b=@]
  ~>  %sham.%dis
  =|  [c=@ d=@]
  |-  ^-  @
  ?:  ?|(=(0 a) =(0 b))  d
  %=  $
    a   (rsh 0 a)
    b   (rsh 0 b)
    c   +(c)
    d   %+  add  d
          %+  lsh  [0 c]
          ?|  =(0 (end 0 a))
              =(0 (end 0 b))
          ==
  ==
::
++  mix                                                 ::  binary xor
  ~/  %mix
  |=  [a=@ b=@]
  ~>  %sham.%mix
  ^-  @
  =+  [c=0 d=0]
  |-
  ?:  ?&(=(0 a) =(0 b))  d
  %=  $
    a   (rsh 0 a)
    b   (rsh 0 b)
    c   +(c)
    d   (add d (lsh [0 c] =((end 0 a) (end 0 b))))
  ==
::
::  Hashes
::
++  mug                                                 ::  mug with murmur3
  ~/  %mug
  |=  a=*
  ~>  %sham.%mug
  |^  ?@  a  (mum 0xcafe.babe 0x7fff a)
      =/  b  (cat 5 $(a -.a) $(a +.a))
      (mum 0xdead.beef 0xfffe b)
  ::
  ++  mum
    |=  [syd=@uxF fal=@F key=@]
    =/  wyd  (met 3 key)
    =|  i=@ud
    |-  ^-  @F
    ?:  =(8 i)  fal
    =/  haz=@F  (muk syd wyd key)
    =/  ham=@F  (mix (rsh [0 31] haz) (end [0 31] haz))
    ?.(=(0 ham) ham $(i +(i), syd +(syd)))
  --
::
++  muk                                                 ::  standard murmur3
  ~%  %muk  ..muk  ~
  =+  ~(. fe 5)
  |=  [syd=@ len=@ key=@]
  =.  syd      (end 5 syd)
  =/  pad      (sub len (met 3 key))
  =/  data     (welp (rip 3 key) (reap pad 0))
  =/  nblocks  (div len 4)  ::  intentionally off-by-one
  =/  h1  syd
  =+  [c1=0xcc9e.2d51 c2=0x1b87.3593]
  =/  blocks  (rip 5 key)
  =/  i  nblocks
  =.  h1  =/  hi  h1  |-
    ?:  =(0 i)  hi
    =/  k1  (snag (sub nblocks i) blocks)  ::  negative array index
    =.  k1  (sit (mul k1 c1))
    =.  k1  (rol 0 15 k1)
    =.  k1  (sit (mul k1 c2))
    =.  hi  (mix hi k1)
    =.  hi  (rol 0 13 hi)
    =.  hi  (sum (sit (mul hi 5)) 0xe654.6b64)
    $(i (dec i))
  =/  tail  (slag (mul 4 nblocks) data)
  =/  k1    0
  =/  tlen  (dis len 3)
  =.  h1
    ?+  tlen  h1  ::  fallthrough switch
      %3  =.  k1  (mix k1 (lsh [0 16] (snag 2 tail)))
          =.  k1  (mix k1 (lsh [0 8] (snag 1 tail)))
          =.  k1  (mix k1 (snag 0 tail))
          =.  k1  (sit (mul k1 c1))
          =.  k1  (rol 0 15 k1)
          =.  k1  (sit (mul k1 c2))
          (mix h1 k1)
      %2  =.  k1  (mix k1 (lsh [0 8] (snag 1 tail)))
          =.  k1  (mix k1 (snag 0 tail))
          =.  k1  (sit (mul k1 c1))
          =.  k1  (rol 0 15 k1)
          =.  k1  (sit (mul k1 c2))
          (mix h1 k1)
      %1  =.  k1  (mix k1 (snag 0 tail))
          =.  k1  (sit (mul k1 c1))
          =.  k1  (rol 0 15 k1)
          =.  k1  (sit (mul k1 c2))
          (mix h1 k1)
    ==
  =.  h1  (mix h1 len)
  |^  (fmix32 h1)
  ++  fmix32
    |=  h=@
    =.  h  (mix h (rsh [0 16] h))
    =.  h  (sit (mul h 0x85eb.ca6b))
    =.  h  (mix h (rsh [0 13] h))
    =.  h  (sit (mul h 0xc2b2.ae35))
    =.  h  (mix h (rsh [0 16] h))
    h
  --
::
::  Noun Ordering
::
++  dor                                                 ::  tree order
  ~/  %dor
  |=  [a=* b=*]
  ~>  %sham.%dor
  ^-  ?
  ?:  =(a b)  &
  ?.  ?=(@ a)
    ?:  ?=(@ b)  |
    ?:  =(-.a -.b)
      $(a +.a, b +.b)
    $(a -.a, b -.b)
  ?.  ?=(@ b)  &
  (lth a b)
::
++  gor                                                 ::  mug order
  ~/  %gor
  |=  [a=* b=*]
  ~>  %sham.%gor
  ^-  ?
  =+  [c=(mug a) d=(mug b)]
  ?:  =(c d)
    (dor a b)
  (lth c d)
::
++  mor                                                 ::  more mug order
  ~/  %mor
  |=  [a=* b=*]
  ~>  %sham.%mor
  ^-  ?
  =+  [c=(mug (mug a)) d=(mug (mug b))]
  ?:  =(c d)
    (dor a b)
  (lth c d)
::
::  Unsigned powers
::
++  pow                                                 ::  unsigned exponent
  ~/  %pow
  |=  [a=@ b=@]
  ?:  =(b 0)  1
  |-  ?:  =(b 1)  a
  =+  c=$(b (div b 2))
  =+  d=(mul c c)
  ?~  (dis b 1)  d  (mul d a)
::
::  Set logic
::
++  in
  ~/  %in
  =|  a=(tree)  :: (set)
  ~>  %sham.%in
  |@
  ++  apt
    =<  $
    ~/  %apt
    =|  [l=(unit) r=(unit)]
    ~>  %sham.%apt
    |.  ^-  ?
    ?~  a   &
    ?&  ?~(l & (gor n.a u.l))
        ?~(r & (gor u.r n.a))
        ?~(l.a & ?&((mor n.a n.l.a) $(a l.a, l `n.a)))
        ?~(r.a & ?&((mor n.a n.r.a) $(a r.a, r `n.a)))
    ==
  ::
  ++  del
    ~/  %del
    |*  b=*
    ~>  %sham.%del
    |-  ^+  a
    ?~  a
      ~
    ?.  =(b n.a)
      ?:  (gor b n.a)
        a(l $(a l.a))
      a(r $(a r.a))
    |-  ^-  [$?(~ _a)]
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (mor n.l.a n.r.a)
      l.a(r $(l.a r.l.a))
    r.a(l $(r.a l.r.a))
  ::
  ++  put
    ~/  %put
    |*  b=*
    ~>  %sham.%put
    |-  ^+  a
    ?~  a
      [b ~ ~]
    ?:  =(b n.a)
      a
    ?:  (gor b n.a)
      =+  c=$(a l.a)
      ?>  ?=(^ c)
      ?:  (mor n.a n.c)
        a(l c)
      c(r a(l r.c))
    =+  c=$(a r.a)
    ?>  ?=(^ c)
    ?:  (mor n.a n.c)
      a(r c)
    c(l a(r l.c))
  --
::
::  Map logic
::
++  by
  ~/  %by
  =|  a=(tree (pair))  ::  (map)
  ~>  %sham.%by
  =*  node  ?>(?=(^ a) n.a)
  |@
  ++  del
    ~/  %del
    |*  b=*
    ~>  %sham.%del
    |-  ^+  a
    ?~  a
      ~
    ?.  =(b p.n.a)
      ?:  (gor b p.n.a)
        a(l $(a l.a))
      a(r $(a r.a))
    |-  ^-  [$?(~ _a)]
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (mor p.n.l.a p.n.r.a)
      l.a(r $(l.a r.l.a))
    r.a(l $(r.a l.r.a))
  ::
  ++  apt
    =<  $
    ~/  %apt
    =|  [l=(unit) r=(unit)]
    ~>  %sham.%apt
    |.  ^-  ?
    ?~  a   &
    ?&  ?~(l & &((gor p.n.a u.l) !=(p.n.a u.l)))
        ?~(r & &((gor u.r p.n.a) !=(u.r p.n.a)))
        ?~  l.a   &
        &((mor p.n.a p.n.l.a) !=(p.n.a p.n.l.a) $(a l.a, l `p.n.a))
        ?~  r.a   &
        &((mor p.n.a p.n.r.a) !=(p.n.a p.n.r.a) $(a r.a, r `p.n.a))
    ==
  ::
  ++  get
    ~/  %get
    |*  b=*
    ~>  %sham.%get
    =>  .(b `_?>(?=(^ a) p.n.a)`b)
    |-  ^-  (unit _?>(?=(^ a) q.n.a))
    ?~  a
      ~
    ?:  =(b p.n.a)
      `q.n.a
    ?:  (gor b p.n.a)
      $(a l.a)
    $(a r.a)
  ::
  ++  put
    ~/  %put
    |*  [b=* c=*]
    ~>  %sham.%put
    |-  ^+  a
    ?~  a
      [[b c] ~ ~]
    ?:  =(b p.n.a)
      ?:  =(c q.n.a)
        a
      a(n [b c])
    ?:  (gor b p.n.a)
      =+  d=$(a l.a)
      ?>  ?=(^ d)
      ?:  (mor p.n.a p.n.d)
        a(l d)
      d(r a(l r.d))
    =+  d=$(a r.a)
    ?>  ?=(^ d)
    ?:  (mor p.n.a p.n.d)
      a(r d)
    d(l a(r l.d))
  --
::
::  Jug logic
::
++  ju
  =|  a=(tree (pair * (tree)))  ::  (jug)
  |@
  ++  del
    |*  [b=* c=*]
    ^+  a
    =+  d=(get b)
    =+  e=(~(del in d) c)
    ?~  e
      (~(del by a) b)
    (~(put by a) b e)
  ::
  ++  get
    |*  b=*
    =+  c=(~(get by a) b)
    ?~(c ~ u.c)
  ::
  ++  put
    |*  [b=* c=*]
    ^+  a
    =+  d=(get b)
    (~(put by a) b (~(put in d) c))
  --
::
::  SHA hashing
::
++  shay                                                ::  sha-256 with length
  ~/  %shay
  |=  [len=@u ruz=@]  ^-  @
  =>  .(ruz (cut 3 [0 len] ruz))
  =+  [few==>(fe .(a 5)) wac=|=([a=@ b=@] (cut 5 [a 1] b))]
  =+  [sum=sum.few ror=ror.few net=net.few inv=inv.few]
  =+  ral=(lsh [0 3] len)
  =+  ^=  ful
      %+  can  0
      :~  [ral ruz]
          [8 128]
          [(mod (sub 960 (mod (add 8 ral) 512)) 512) 0]
          [64 (~(net fe 6) ral)]
      ==
  =+  lex=(met 9 ful)
  =+  ^=  kbx  0xc671.78f2.bef9.a3f7.a450.6ceb.90be.fffa.
                 8cc7.0208.84c8.7814.78a5.636f.748f.82ee.
                 682e.6ff3.5b9c.ca4f.4ed8.aa4a.391c.0cb3.
                 34b0.bcb5.2748.774c.1e37.6c08.19a4.c116.
                 106a.a070.f40e.3585.d699.0624.d192.e819.
                 c76c.51a3.c24b.8b70.a81a.664b.a2bf.e8a1.
                 9272.2c85.81c2.c92e.766a.0abb.650a.7354.
                 5338.0d13.4d2c.6dfc.2e1b.2138.27b7.0a85.
                 1429.2967.06ca.6351.d5a7.9147.c6e0.0bf3.
                 bf59.7fc7.b003.27c8.a831.c66d.983e.5152.
                 76f9.88da.5cb0.a9dc.4a74.84aa.2de9.2c6f.
                 240c.a1cc.0fc1.9dc6.efbe.4786.e49b.69c1.
                 c19b.f174.9bdc.06a7.80de.b1fe.72be.5d74.
                 550c.7dc3.2431.85be.1283.5b01.d807.aa98.
                 ab1c.5ed5.923f.82a4.59f1.11f1.3956.c25b.
                 e9b5.dba5.b5c0.fbcf.7137.4491.428a.2f98
  =+  ^=  hax  0x5be0.cd19.1f83.d9ab.9b05.688c.510e.527f.
                 a54f.f53a.3c6e.f372.bb67.ae85.6a09.e667
  =+  i=0
  |-  ^-  @
  ?:  =(i lex)
    (run 5 hax net)
  =+  ^=  wox
      =+  dux=(cut 9 [i 1] ful)
      =+  wox=(run 5 dux net)
      =+  j=16
      |-  ^-  @
      ?:  =(64 j)
        wox
      =+  :*  l=(wac (sub j 15) wox)
              m=(wac (sub j 2) wox)
              n=(wac (sub j 16) wox)
              o=(wac (sub j 7) wox)
          ==
      =+  x=:(mix (ror 0 7 l) (ror 0 18 l) (rsh [0 3] l))
      =+  y=:(mix (ror 0 17 m) (ror 0 19 m) (rsh [0 10] m))
      =+  z=:(sum n x o y)
      $(wox (con (lsh [5 j] z) wox), j +(j))
  =+  j=0
  =+  :*  a=(wac 0 hax)
          b=(wac 1 hax)
          c=(wac 2 hax)
          d=(wac 3 hax)
          e=(wac 4 hax)
          f=(wac 5 hax)
          g=(wac 6 hax)
          h=(wac 7 hax)
      ==
  |-  ^-  @
  ?:  =(64 j)
    %=  ^$
      i  +(i)
      hax  %+  rep  5
           :~  (sum a (wac 0 hax))
               (sum b (wac 1 hax))
               (sum c (wac 2 hax))
               (sum d (wac 3 hax))
               (sum e (wac 4 hax))
               (sum f (wac 5 hax))
               (sum g (wac 6 hax))
               (sum h (wac 7 hax))
           ==
    ==
  =+  l=:(mix (ror 0 2 a) (ror 0 13 a) (ror 0 22 a))    ::  s0
  =+  m=:(mix (dis a b) (dis a c) (dis b c))            ::  maj
  =+  n=(sum l m)                                       ::  t2
  =+  o=:(mix (ror 0 6 e) (ror 0 11 e) (ror 0 25 e))    ::  s1
  =+  p=(mix (dis e f) (dis (inv e) g))                 ::  ch
  =+  q=:(sum h o p (wac j kbx) (wac j wox))            ::  t1
  $(j +(j), a (sum q n), b a, c b, d c, e (sum d q), f e, g f, h g)
::
::  Scrambling
::
++  ob
  ~%  %ob  ..ob
    ==
      %fein  fein
      %fynd  fynd
    ==
  |%
  ::
  ::  +fein: conceal structure, v3.
  ::
  ::    +fein conceals planet-sized atoms.  The idea is that it should not be
  ::    trivial to tell which planet a star has spawned under.
  ::
  ++  fein
    ~/  %fein
    |=  pyn=@  ^-  @
    ?:  &((gte pyn 0x1.0000) (lte pyn 0xffff.ffff))
      (add 0x1.0000 (feis (sub pyn 0x1.0000)))
    ?:  &((gte pyn 0x1.0000.0000) (lte pyn 0xffff.ffff.ffff.ffff))
      =/  lo  (dis pyn 0xffff.ffff)
      =/  hi  (dis pyn 0xffff.ffff.0000.0000)
      %+  con  hi
      $(pyn lo)
    pyn
  ::
  ::  +fynd: restore structure, v3.
  ::
  ::    Restores obfuscated values that have been enciphered with +fein.
  ::
  ++  fynd
    ~/  %fynd
    |=  cry=@  ^-  @
    ?:  &((gte cry 0x1.0000) (lte cry 0xffff.ffff))
      (add 0x1.0000 (tail (sub cry 0x1.0000)))
    ?:  &((gte cry 0x1.0000.0000) (lte cry 0xffff.ffff.ffff.ffff))
      =/  lo  (dis cry 0xffff.ffff)
      =/  hi  (dis cry 0xffff.ffff.0000.0000)
      %+  con  hi
      $(cry lo)
    cry
  ::  +feis: a four-round generalised Feistel cipher over the domain
  ::         [0, 2^32 - 2^16 - 1].
  ::
  ::    See: Black & Rogaway (2002), Ciphers for arbitrary finite domains.
  ::
  ++  feis
    |=  m=@
    ^-  @
    (fee 4 0xffff 0x1.0000 (mul 0xffff 0x1.0000) eff m)
  ::
  ::  +tail: reverse +feis.
  ::
  ++  tail
    |=  m=@
    ^-  @
    (feen 4 0xffff 0x1.0000 (mul 0xffff 0x1.0000) eff m)
  ::
  ::  +fee: "Fe" in B&R (2002).
  ::
  ::    A Feistel cipher given the following parameters:
  ::
  ::    r:    number of Feistel rounds
  ::    a, b: parameters such that ab >= k
  ::    k:    value such that the domain of the cipher is [0, k - 1]
  ::    prf:  a gate denoting a family of pseudorandom functions indexed by
  ::          its first argument and taking its second argument as input
  ::    m:    an input value in the domain [0, k - 1]
  ::
  ++  fee
    |=  [r=@ a=@ b=@ k=@ prf=$-([j=@ r=@] @) m=@]
    ^-  @
    =/  c  (fe r a b prf m)
    ?:  (lth c k)
      c
    (fe r a b prf c)
  ::
  ::  +feen: "Fe^-1" in B&R (2002).
  ::
  ::    Reverses a Feistel cipher constructed with parameters as described in
  ::    +fee.
  ::
  ++  feen
    |=  [r=@ a=@ b=@ k=@ prf=$-([j=@ r=@] @) m=@]
    ^-  @
    =/  c  (fen r a b prf m)
    ?:  (lth c k)
      c
    (fen r a b prf c)
  ::
  ::  +fe:  "fe" in B&R (2002).
  ::
  ::    An internal function to +fee.
  ::
  ::    Note that this implementation differs slightly from the reference paper
  ::    to support some legacy behaviour.  See urbit/arvo#1105.
  ::
  ++  fe
    |=  [r=@ a=@ b=@ prf=$-([j=@ r=@] @) m=@]
    =/  j  1
    =/  ell  (mod m a)
    =/  arr  (div m a)
    |-  ^-  @
    ::
    ?:  (gth j r)
      ?.  =((mod r 2) 0)
        (add (mul arr a) ell)
      ::
      :: Note that +fe differs from B&R (2002)'s "fe" below, as a previous
      :: implementation of this cipher contained a bug such that certain inputs
      :: could encipher to the same output.
      ::
      :: To correct these problem cases while also preserving the cipher's
      :: legacy behaviour on most inputs, we check for a problem case (which
      :: occurs when 'arr' is equal to 'a') and, if detected, use an alternate
      :: permutation instead.
      ::
      ?:  =(arr a)
        (add (mul arr a) ell)
      (add (mul ell a) arr)
    ::
    =/  f  (prf (sub j 1) arr)
    ::
    =/  tmp
      ?.  =((mod j 2) 0)
        (mod (add f ell) a)
      (mod (add f ell) b)
    ::
    $(j +(j), ell arr, arr tmp)
  ::
  ::  +fen:  "fe^-1" in B&R (2002).
  ::
  ::    Note that this implementation differs slightly from the reference paper
  ::    to support some legacy behaviour.  See urbit/arvo#1105.
  ::
  ++  fen
    |=  [r=@ a=@ b=@ prf=$-([j=@ r=@] @) m=@]
    =/  j  r
    ::
    =/  ahh
      ?.  =((mod r 2) 0)
        (div m a)
      (mod m a)
    ::
    =/  ale
      ?.  =((mod r 2) 0)
        (mod m a)
      (div m a)
    ::
    :: Similar to the comment in +fe, +fen differs from B&R (2002)'s "fe^-1"
    :: here in order to preserve the legacy cipher's behaviour on most inputs.
    ::
    :: Here problem cases can be identified by 'ahh' equating with 'a'; we
    :: correct those cases by swapping the values of 'ahh' and 'ale'.
    ::
    =/  ell
      ?:  =(ale a)
        ahh
      ale
    ::
    =/  arr
      ?:  =(ale a)
        ale
      ahh
    ::
    |-  ^-  @
    ?:  (lth j 1)
      (add (mul arr a) ell)
    =/  f  (prf (sub j 1) ell)
    ::
    ::  Note that there is a slight deviation here to avoid dealing with
    ::  negative values.  We add 'a' or 'b' to arr as appropriate and reduce
    ::  'f' modulo the same number before performing subtraction.
    ::
    =/  tmp
      ?.  =((mod j 2) 0)
        (mod (sub (add arr a) (mod f a)) a)
      (mod (sub (add arr b) (mod f b)) b)
    ::
    $(j (sub j 1), ell tmp, arr ell)
  ::
  ::  +eff: a murmur3-based pseudorandom function.  'F' in B&R (2002).
  ::
  ++  eff
    |=  [j=@ r=@]
    ^-  @
    (muk (snag j raku) 2 r)
  ::
  ::  +raku: seeds for eff.
  ::
  ++  raku
    ^-  (list @ux)
    :~  0xb76d.5eed
        0xee28.1300
        0x85bc.ae01
        0x4b38.7af7
    ==
  ::
  --
::
::  Exotic bases
::
++  po                                                  ::  phonetic base
  ~/  %po
  =+  :-  ^=  sis                                       ::  prefix syllables
      'dozmarbinwansamlitsighidfidlissogdirwacsabwissib\
      /rigsoldopmodfoglidhopdardorlorhodfolrintogsilmir\
      /holpaslacrovlivdalsatlibtabhanticpidtorbolfosdot\
      /losdilforpilramtirwintadbicdifrocwidbisdasmidlop\
      /rilnardapmolsanlocnovsitnidtipsicropwitnatpanmin\
      /ritpodmottamtolsavposnapnopsomfinfonbanmorworsip\
      /ronnorbotwicsocwatdolmagpicdavbidbaltimtasmallig\
      /sivtagpadsaldivdactansidfabtarmonranniswolmispal\
      /lasdismaprabtobrollatlonnodnavfignomnibpagsopral\
      /bilhaddocridmocpacravripfaltodtiltinhapmicfanpat\
      /taclabmogsimsonpinlomrictapfirhasbosbatpochactid\
      /havsaplindibhosdabbitbarracparloddosbortochilmac\
      /tomdigfilfasmithobharmighinradmashalraglagfadtop\
      /mophabnilnosmilfopfamdatnoldinhatnacrisfotribhoc\
      /nimlarfitwalrapsarnalmoslandondanladdovrivbacpol\
      /laptalpitnambonrostonfodponsovnocsorlavmatmipfip'
      ^=  dex                                           ::  suffix syllables
      'zodnecbudwessevpersutletfulpensytdurwepserwylsun\
      /rypsyxdyrnuphebpeglupdepdysputlughecryttyvsydnex\
      /lunmeplutseppesdelsulpedtemledtulmetwenbynhexfeb\
      /pyldulhetmevruttylwydtepbesdexsefwycburderneppur\
      /rysrebdennutsubpetrulsynregtydsupsemwynrecmegnet\
      /secmulnymtevwebsummutnyxrextebfushepbenmuswyxsym\
      /selrucdecwexsyrwetdylmynmesdetbetbeltuxtugmyrpel\
      /syptermebsetdutdegtexsurfeltudnuxruxrenwytnubmed\
      /lytdusnebrumtynseglyxpunresredfunrevrefmectedrus\
      /bexlebduxrynnumpyxrygryxfeptyrtustyclegnemfermer\
      /tenlusnussyltecmexpubrymtucfyllepdebbermughuttun\
      /bylsudpemdevlurdefbusbeprunmelpexdytbyttyplevmyl\
      /wedducfurfexnulluclennerlexrupnedlecrydlydfenwel\
      /nydhusrelrudneshesfetdesretdunlernyrsebhulryllud\
      /remlysfynwerrycsugnysnyllyndyndemluxfedsedbecmun\
      /lyrtesmudnytbyrsenwegfyrmurtelreptegpecnelnevfes'
  |%
  ++  ins  ~/  %ins                                     ::  parse prefix
           |=  a=@tas
           =+  b=0
           |-  ^-  (unit @)
           ?:(=(256 b) ~ ?:(=(a (tos b)) [~ b] $(b +(b))))
  ++  ind  ~/  %ind                                     ::  parse suffix
           |=  a=@tas
           =+  b=0
           |-  ^-  (unit @)
           ?:(=(256 b) ~ ?:(=(a (tod b)) [~ b] $(b +(b))))
  ++  tos  ~/  %tos                                     ::  fetch prefix
           |=(a=@ ?>((lth a 256) (cut 3 [(mul 3 a) 3] sis)))
  ++  tod  ~/  %tod                                     ::  fetch suffix
           |=(a=@ ?>((lth a 256) (cut 3 [(mul 3 a) 3] dex)))
  --
::
++  fa                                                  ::  base58check
  =+  key='123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
  =/  yek=@ux  ~+
      =-  yek:(roll (rip 3 key) -)
      =+  [a=*char b=*@ yek=`@ux`(fil 3 256 0xff)]
      |.
      [+(b) (mix yek (lsh [3 `@u`a] (~(inv fe 3) b)))]
  |%
  ++  cha  |=(a=char `(unit @uF)`=+(b=(cut 3 [`@`a 1] yek) ?:(=(b 0xff) ~ `b)))
  ++  tok
    |=  a=@ux  ^-  @ux
    =+  b=(pad a)
    =-  (~(net fe 5) (end [3 4] (shay 32 -)))
    (shay (add b (met 3 a)) (lsh [3 b] (swp 3 a)))
  ::
  ++  pad  |=(a=@ =+(b=(met 3 a) ?:((gte b 21) 0 (sub 21 b))))
  ++  enc  |=(a=@ux `@ux`(mix (lsh [3 4] a) (tok a)))
  ++  den
    |=  a=@ux  ^-  (unit @ux)
    =+  b=(rsh [3 4] a)
    ?.  =((tok b) (end [3 4] a))
      ~
    `b
  --
::
::  Text processing
::
++  crip                                                ::  tape to cord
  |=  a=tape
  ^-  @t
  (rap 3 a)
++  trip                                                ::  cord to tape
  ~/  %trip
  |=  a=@  ^-  tape
  ?:  =(0 (met 3 a))
    ~
  [^-(@ta (end 3 a)) $(a (rsh 3 a))]
::
::  Parsing
:: 
++  ne
  |_  tig=@
  ++  c  (cut 3 [tig 1] key:fa)
  ++  d  (add tig '0')
  ++  x  ?:((gte tig 10) (add tig 87) d)
  ++  v  ?:((gte tig 10) (add tig 87) d)
  ++  w  ?:(=(tig 63) '~' ?:(=(tig 62) '-' ?:((gte tig 36) (add tig 29) x)))
  --
::
::  Atom printing
::
++  co
  !:
  ~%  %co  ..co  ~
  =<  |_  lot=coin
      ++  rear  |=(rom=tape rend(rep rom))
      ++  rent  ~+  `@ta`(rap 3 rend)
      ++  rend
        ^-  tape
        ~+
        ?:  ?=(%blob -.lot)
          ['~' '0' ((v-co 1) (jam p.lot))]
        ?:  ?=(%many -.lot)
          :-  '.'
          |-  ^-  tape
          ?~   p.lot
            ['_' '_' rep]
          ['_' (weld (trip (wack rent(lot i.p.lot))) $(p.lot t.p.lot))]
        =+  [yed=(end 3 p.p.lot) hay=(cut 3 [1 1] p.p.lot)]
        |-  ^-  tape
        ?+    yed  (z-co q.p.lot)
            %p
          =+  sxz=(fein:ob q.p.lot)
          =+  dyx=(met 3 sxz)
          :-  '~'
          ?:  (lte dyx 1)
            (weld (trip (tod:po sxz)) rep)
          =+  dyy=(met 4 sxz)
          =|  imp=@ud
          |-  ^-  tape
          ?:  =(imp dyy)
            rep
          %=  $
            imp  +(imp)
            rep  =/  log  (cut 4 [imp 1] sxz)
                 ;:  weld
                   (trip (tos:po (rsh 3 log)))
                   (trip (tod:po (end 3 log)))
                   ?:(=((mod imp 4) 0) ?:(=(imp 0) "" "--") "-")
                   rep
          ==     ==
        ::
            %u
          ?:  ?=(%c hay)
            %+  welp  ['0' 'c' (reap (pad:fa q.p.lot) '1')]
            (c-co (enc:fa q.p.lot))
          ::
          =;  gam=(pair tape tape)
            (weld p.gam ?:(=(0 q.p.lot) `tape`['0' ~] q.gam))
          ?+  hay  [~ ((ox-co [10 3] |=(a=@ ~(d ne a))) q.p.lot)]
            %b  [['0' 'b' ~] ((ox-co [2 4] |=(a=@ ~(d ne a))) q.p.lot)]
            %i  [['0' 'i' ~] ((d-co 1) q.p.lot)]
            %x  [['0' 'x' ~] ((ox-co [16 4] |=(a=@ ~(x ne a))) q.p.lot)]
            %v  [['0' 'v' ~] ((ox-co [32 5] |=(a=@ ~(x ne a))) q.p.lot)]
            %w  [['0' 'w' ~] ((ox-co [64 5] |=(a=@ ~(w ne a))) q.p.lot)]
          ==
      --
  =|  rep=tape
  =<  |%
      ++  c-co  (em-co [58 1] |=([? b=@ c=tape] [~(c ne b) c]))
      ++  d-co  |=(min=@ (em-co [10 min] |=([? b=@ c=tape] [~(d ne b) c])))
      --
  |%
  ::  +em-co: format in numeric base
  ::
  ::    in .bas, format .min digits of .hol with .par
  ::
  ::    - .hol is processed least-significant digit first
  ::    - all available digits in .hol will be processed, but
  ::      .min digits can exceed the number available in .hol
  ::    - .par handles all accumulated output on each call,
  ::      and can edit it, prepend or append digits, &c
  ::    - until .hol is exhausted, .par's sample is [| digit output],
  ::      subsequently, it's [& 0 output]
  ::
  ++  em-co
    |=  [[bas=@ min=@] par=$-([? @ tape] tape)]
    |=  hol=@
    ^-  tape
    ?:  &(=(0 hol) =(0 min))
      rep
    =/  [dar=@ rad=@]  (dvr hol bas)
    %=  $
      min  ?:(=(0 min) 0 (dec min))
      hol  dar
      rep  (par =(0 dar) rad rep)
    ==
  ::
  ::  +ox-co: format '.'-separated digit sequences in numeric base
  ::
  ::    in .bas, format each digit of .hol with .dug,
  ::    with '.' separators every .gop digits.
  ::
  ::    - .hol is processed least-significant digit first
  ::    - .dug handles individual digits, output is prepended
  ::    - every segment but the last is zero-padded to .gop
  ::
  ++  ox-co
    |=  [[bas=@ gop=@] dug=$-(@ @)]
    %+  em-co
      [(pow bas gop) 0]
    |=  [top=? seg=@ res=tape]
    %+  weld
      ?:(top ~ `tape`['.' ~])
    %.  seg
    %+  em-co(rep res)
      [bas ?:(top 0 gop)]
    |=([? b=@ c=tape] [(dug b) c])
::
::  Formatting functions
::
++  scow
  ~/  %scow
  |=(mol=dime ~(rend co %$ mol))
::
++  smyt                                                ::  pretty print path
  |=  bon=path  ^-  tank
  :+  %rose  [['/' ~] ['/' ~] ~]
  (turn bon |=(a=@ [%leaf (trip a)]))
::
::  Virtualization
::
++  mink  !.                                            ::  raw virtual nock
  ~/  %mink
  |=  $:  [subject=* formula=*]
          scry=$-(^ (unit (unit)))
      ==
  ~/  %sham.%mink
  =|  trace=(list [@ta *])
  |^  ^-  tone
      ?+  formula  [%2 trace]
          [^ *]
        =/  head  $(formula -.formula)
        ?.  ?=(%0 -.head)  head
        =/  tail  $(formula +.formula)
        ?.  ?=(%0 -.tail)  tail
        [%0 product.head product.tail]
      ::
          [%0 axis=@]
        =/  part  (frag axis.formula subject)
        ?~  part  [%2 trace]
        [%0 u.part]
      ::
          [%1 constant=*]
        [%0 constant.formula]
      ::
          [%2 subject=* formula=*]
        =/  subject  $(formula subject.formula)
        ?.  ?=(%0 -.subject)  subject
        =/  formula  $(formula formula.formula)
        ?.  ?=(%0 -.formula)  formula
        %=  $
          subject  product.subject
          formula  product.formula
        ==
      ::
          [%3 argument=*]
        =/  argument  $(formula argument.formula)
        ?.  ?=(%0 -.argument)  argument
        [%0 .?(product.argument)]
      ::
          [%4 argument=*]
        =/  argument  $(formula argument.formula)
        ?.  ?=(%0 -.argument)  argument
        ?^  product.argument  [%2 trace]
        [%0 .+(product.argument)]
      ::
          [%5 a=* b=*]
        =/  a  $(formula a.formula)
        ?.  ?=(%0 -.a)  a
        =/  b  $(formula b.formula)
        ?.  ?=(%0 -.b)  b
        [%0 =(product.a product.b)]
      ::
          [%6 test=* yes=* no=*]
        =/  result  $(formula test.formula)
        ?.  ?=(%0 -.result)  result
        ?+  product.result
              [%2 trace]
          %&  $(formula yes.formula)
          %|  $(formula no.formula)
        ==
      ::
          [%7 subject=* next=*]
        =/  subject  $(formula subject.formula)
        ?.  ?=(%0 -.subject)  subject
        %=  $
          subject  product.subject
          formula  next.formula
        ==
      ::
          [%8 head=* next=*]
        =/  head  $(formula head.formula)
        ?.  ?=(%0 -.head)  head
        %=  $
          subject  [product.head subject]
          formula  next.formula
        ==
      ::
          [%9 axis=@ core=*]
        =/  core  $(formula core.formula)
        ?.  ?=(%0 -.core)  core
        =/  arm  (frag axis.formula product.core)
        ?~  arm  [%2 trace]
        %=  $
          subject  product.core
          formula  u.arm
        ==
      ::
          [%10 [axis=@ value=*] target=*]
        ?:  =(0 axis.formula)  [%2 trace]
        =/  target  $(formula target.formula)
        ?.  ?=(%0 -.target)  target
        =/  value  $(formula value.formula)
        ?.  ?=(%0 -.value)  value
        =/  mutant=(unit *)
          (edit axis.formula product.target product.value)
        ?~  mutant  [%2 trace]
        [%0 u.mutant]
      ::
          [%11 tag=@ next=*]
        =/  next  $(formula next.formula)
        ?.  ?=(%0 -.next)  next
        :-  %0
        .*  subject
        [11 tag.formula 1 product.next]
      ::
          [%11 [tag=@ clue=*] next=*]
        =/  clue  $(formula clue.formula)
        ?.  ?=(%0 -.clue)  clue
        =/  next
          =?    trace
              ?=(?(%hunk %hand %lose %mean %spot) tag.formula)
            [[tag.formula product.clue] trace]
          $(formula next.formula)
        ?.  ?=(%0 -.next)  next
        :-  %0
        .*  subject
        [11 [tag.formula 1 product.clue] 1 product.next]
      ::
          [%12 ref=* path=*]
        =/  ref  $(formula ref.formula)
        ?.  ?=(%0 -.ref)  ref
        =/  path  $(formula path.formula)
        ?.  ?=(%0 -.path)  path
        =/  result  (scry product.ref product.path)
        ?~  result
          [%1 product.path]
        ?~  u.result
          [%2 [%hunk product.ref product.path] trace]
        [%0 u.u.result]
      ==
  ::
  ++  frag
    |=  [axis=@ noun=*]
    ^-  (unit)
    ?:  =(0 axis)  ~
    |-  ^-  (unit)
    ?:  =(1 axis)  `noun
    ?@  noun  ~
    =/  pick  (cap axis)
    %=  $
      axis  (mas axis)
      noun  ?-(pick %2 -.noun, %3 +.noun)
    ==
  ::
  ++  edit
    |=  [axis=@ target=* value=*]
    ^-  (unit)
    ?:  =(1 axis)  `value
    ?@  target  ~
    =/  pick  (cap axis)
    =/  mutant
      %=  $
        axis    (mas axis)
        target  ?-(pick %2 -.target, %3 +.target)
      ==
    ?~  mutant  ~
    ?-  pick
      %2  `[u.mutant +.target]
      %3  `[-.target u.mutant]
    ==
  --
::
++  mock                                                ::  virtual nock
  |=  [[sub=* fol=*] gul=$-(^ (unit (unit)))]
  (mook (mink [sub fol] gul))
::
++  mook                                                ::  convert %tone to %toon
  |=  ton=tone
  ^-  toon
  ?.  ?=([%2 *] ton)
    ton
  |^  [%2 (turn skip rend)]
  ::
  ++  skip
    ^+  trace.ton
    =/  yel  (lent trace.ton)
    ?.  (gth yel 1.024)  trace.ton
    %+  weld
      (scag 512 trace.ton)
    ^+  trace.ton
    :_  (slag (sub yel 512) trace.ton)
    :-  %lose
    (crip "[skipped {(scow %ud (sub yel 1.024))} frames]")
  ::
  ::  +rend: raw stack frame to tank
  ::
  ::    $%  [%hunk ref=* path]            ::  failed scry ([~ ~])
  ::        [%lose cord]                  ::  skipped frames
  ::        [%hand *]                     ::  mug any
  ::        [%mean $@(cord (trap tank))]  ::  ~_ et al
  ::        [%spot spot]                  ::  source location
  ::    ==
  ::
  ++  rend
    |=  [tag=@ta dat=*]
    ^-  tank
    ?+    tag
    ::
      leaf+"mook.{(rip 3 tag)}"
    ::
        %hunk
      ?@  dat  leaf+"mook.hunk"
      =/  sof=(unit path)  ((soft path) +.dat)
      ?~  sof  leaf+"mook.hunk"
      (smyt u.sof)
    ::
        %lose
      ?^  dat  leaf+"mook.lose"
      leaf+(rip 3 dat)
    ::
        %hand
      leaf+(scow %p (mug dat))
    ::
        %mean
      ?@  dat  leaf+(rip 3 dat)
      =/  mac  (mack dat -.dat)
      ?~  mac  leaf+"####"
      =/  sof  ((soft tank) u.mac)
      ?~  sof  leaf+"mook.mean"
      u.sof
    ::
        %spot
      =/  sof=(unit spot)  ((soft spot) dat)
      ?~  sof  leaf+"mook.spot"
      :+  %rose  [":" ~ ~]
      :~  (smyt p.u.sof)
          =*  l   p.q.u.sof
          =*  r   q.q.u.sof
          =/  ud  |=(a=@u (scow %ud a))
          leaf+"<[{(ud p.l)} {(ud q.l)}].[{(ud p.r)} {(ud q.r)}]>"
      ==
    ==
  --
::
++  mole                                                ::  typed unitary virtual
  ::  XX: needs real jet dashboard to jet
  ~/  %mole
  |*  tap=(trap)
  ^-  (unit _$:tap)
  =/  mur  (mure tap)
  ?~(mur ~ `$:tap)
::
++  mure                                                ::  untyped unitary virtual
  |=  tap=(trap)
  ^-  (unit)
  =/  ton  (mink [tap %9 2 %0 1] |=(a=^ ``.*(a [%12 [%0 2] %0 3])))
  ?.(?=(%0 -.ton) ~ `product.ton)
::
++  soft                                                ::  virtual clam
  |*  han=$-(* *)
  |=(fud=* (mole |.((han fud))))
--
