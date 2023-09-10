::  A working pill that grows towards arvo.hoon. Requires playpen.hoon from this
::  scaffolding directory.
::
/+  cradle
!.
=/  core
  =>  cradle
  !=
  =>
  |%
  +$  card  (cask)
  ++  cask  |$  [a]  (pair mark a)
  +$  knot  @ta
  ++  list  |$  [item]  $@(~ [i=item t=(list item)])
  +$  mark  @tas
  +$  ovum  [=wire =card]
  ++  pair  |$  [head tail]  [p=head q=tail]
  +$  path  (list knot)
  +$  wire  path
  ::  mutually recursive Ackermann functions
  ::  test turning %spot hints on/off
  ++  wack
    ::  re-enable %spot hints
    !:
    |=  [m=@ud n=@ud]
    ::  %mean hint
    ~_  [%leaf "I am a %mean hint via ~_ from +wack"]
    ::  %hela hint
    ~>  %hela
    ::  %memo hint
    ~+
    ?~  m  +(n)
    ?~  n
      (tack (dec m) 1)
    (tack (dec m) $(n (dec n)))
  ++  tack
    ::  disable %spot hints
    !.
    |=  [m=@ud n=@ud]
    ::  %hela hint
    ~>  %hela
    ::  %memo hint
    ~+
    ?~  m  +(n)
    ?~  n
      (wack (dec m) 1)
    (wack (dec m) $(n (dec n)))
  --  =>
  ::
  |%
  ++  load  !!
  ++  peek  _~
  ++  wish  !!
  ++  poke
    |=  [now=@da ovo=ovum]
    ^-  ^
    ~>  %slog.[0 (wack 2 1)]
    [~ ..poke]
  --
  ::
  |=  [now=@da ovo=ovum]
  ^-  *
  .(+> +:(poke now ovo))
::
|%
++  aeon
  ^-  *
  =>  *[arvo=* epic=*]
  !=
  |-  ^-  *
  ?@  epic  arvo
  %=  $
    epic  +.epic
    arvo  .*([arvo -.epic] [%9 2 %10 [6 %0 3] %0 2])
  ==
--
[%pill %baby [aeon .*(cradle core) ~] ~ ~]
