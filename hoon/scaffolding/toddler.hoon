::  A working pill that grows towards arvo.hoon. Requires playpen.hoon from this
::  scaffolding directory.
::
/+  playpen
!.
=/  core
  =>  playpen
  !=
  =>
  |%
  +$  card  (cask)
  ++  cask  |$  [a]  (pair mark a)
  +$  goof  [mote=term =tang]
  +$  mark  @tas
  +$  ovum  [=wire =card]
  +$  vere  [[non=@ta rev=path] kel=wynn]
  +$  wire  path
  +$  wasp
    ::  %crud: reroute $ovum with $goof
    ::  %wack: iterate entropy
    ::  %wyrd: check/record runtime kelvin stack
    ::
    $%  [%crud =goof =ovum]
        [%wack p=@uvJ]
        [%wyrd p=vere]
    ==
  +$  weft  [lal=@tas num=@ud]
  +$  wynn  (list weft)
  ::  mutually recursive Ackermann functions
  ::  test turning %spot hints on/off
  ++  wack
    ::  re-enable %spot hints
    !:
    |=  [m=@ud n=@ud]
    ::  %mean hint
    ~_  [%leaf "I am a %mean hint via ~_ from +wack"]
    ::  %hela hint
    :: ~>  %hela
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
    :: ~>  %hela
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
    ::
    ?.  ?=(?(%crud %wack %wyrd) p.card.ovo)
      ~>  %slog.[0 leaf+(scow %ud (wack 1 1))]
      [~ ..poke]
    ::
    =/  buz
      ~>  %mean.'pith: bad wasp'
      ;;(wasp card.ovo)
    ?+  -.buz
      ~>  %slog.[0 leaf+"default $wasp"]
      [~ ..poke]
    ::
      %crud
        ~>  %slog.[0 goof.buz]
        [~ ..poke]
    ==
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
[%pill %toddler [aeon .*(playpen core) ~] ~ ~]
