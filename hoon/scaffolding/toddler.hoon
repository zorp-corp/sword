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
  ++  ack
    |=  [m=@ud n=@ud]
    ?~  m  +(n)
    ?~  n
      $(m (dec m), n 1)
    $(m (dec m), n $(n (dec n)))
  --  =>
  ::
  |%
  ++  load  !!
  ++  peek  _~
  ++  wish  !!
  ++  poke
    |=  [now=@da ovo=ovum]
    ^-  ^
    :: ~>  %slog.[0 'got']
    :: ~>  %slog.[0 -.card.ovo]
    =/  fec  [//term/1 %blit [%put (snoc "A(2,1) = " (add 48 (ack 2 1)))] [%nel ~] ~]
    [[fec ~] ..poke]
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
