/+  hoon
=>  hoon
|%
::  emits a trap that will produce the Ares codegen core.
::
::  This trap can be built and included in Ares, where it will be run if
::  an Ares instance does not have an existing codegen core
++  make-codegen-core
  |=  cg=path
  ^-  [%cg * (trap vase)]
  =|  sub=(trap vase)
  =.  sub  (build-lib cg sub %hoon)
  ::  this should follow the order of the ford runes in the files
  =.  sub  (build-sur cg sub %sock)
  =.  sub  (build-lib cg sub %soak)
  =.  sub  (build-sur cg sub %noir)
  =.  sub  (build-lib cg sub %sack)
  =.  sub  (build-sur cg sub %gene)
  =.  sub  (build-lib cg sub %line)
  =/  cg-kick
    =>  sub  !=  !:  q:$
  [%cg cg-kick sub]
::  build a library file
++  build-lib
  |=  [cg=path sub=(trap vase) nam=term]  ^-  (trap vase)
  ~>  %slog.[0 leaf+"make: building /lib/{(trip nam)}"]
  =/  hun=hoon
    %+  mist  /lib/[nam]/hoon
    .^(@t cx+(welp cg /lib/[nam]/hoon))
  (swut sub hun nam)
::  build an interface file
++  build-sur
  |=  [cg=path sub=(trap vase) nam=term]  ^-  (trap vase)
  ~>  %slog.[0 leaf+"make: building /sur/{(trip nam)}"]
  =/  hun=hoon
    %+  mist  /sur/[nam]/hoon
    .^(@t cx+(welp cg /sur/[nam]/hoon))
  (swut sub hun nam)
::  +mist: +rain but skipping past ford runes
::
::  copied from urbit lib/pill.hoon as it's not exported from there
++  mist
  |=  [bon=path txt=@]
  ^-  hoon
  =+  vas=(vang [& |] bon)
  ~|  bon
  %+  scan  (trip txt)
  %-  full
  =;  fud
    (ifix [;~(plug gay fud) gay] tall:vas(wer bon))
  %-  star
  ;~  pose  vul
    %+  ifix  [fas (just `@`10)]
    (star ;~(less (just `@`10) next))
  ==
--
