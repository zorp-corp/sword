/-  *sock
|%
::  operations on $cape
++  ca
  |_  one=cape
  ++  app
    |=  know=sock
    ^-  sock
    ?-  one
        %|  [%toss ~]
        %&  know
        ^
      ?+  -.sock  !!
          %know
        ?>  ?=(^ know.sock)
        %+  knit:ska
          $(one -.one, know [%know -.know.sock])
        $(one +.one, know [%know +.know.sock])
      ::
          %bets
        (knit:ska $(one -.one, know left.know) $(one +.one, know rite.know))
      ::
          %toss
        [%toss ~]
      ==
    ==
  ::  unify two capes
  ++  uni
    |=  two=cape
    ^-  cape
    ?-  one
        %|  two
        %&  one
        ^
      ?-  two
        %|  one
        %&  two
        ^
      =/  l  $(one -.one, two -.two)
      =/  r  $(one +.one, two +.two)
      ?:(?&(?=(@ l) =(l r)) l [l r])
    ==
  ::  does two add axes to one?
  ++  big
    |=  two=cape
    ^-  ?
    ?-  one
        %&  |
        %|  ?@(two two ?|($(two -.two) $(two +.two)))
        ^
      ?@  two  ?|($(one -.one) $(one +.one))
      ?|($(one -.one, two -.two) $(one +.one, two +.two))
    ==
  ::  does one actually have any axes
  ++  any
    ^-  ?
    ?@  one  one
    ?|(any(one -.one) any(one +.one))
  ::  push a cape down to an axis
  ++  pat
    |=  axe=@
    ?<  =(0 axe)
    |-  ^-  cape
    ?:  =(1 axe)  one
    ?-  (cap axe)
      %2  [$(axe (mas axe)) |]
      %3  [| $(axe (mas axe))]
    ==
  ::  split a cape
  ++  rip
    ^-  [cape cape]
    ?-  one
      %|  [| |]
      %&  [& &]
      ^   one
  ::  poke a hole in a cape
  ++  awl
    |=  axe=@
    ?<  =(0 axe)
    |-  ^-  [cape cape]
    ?:  ?=(%| one) [| |]
    ?:  =(1 axe)  [one |]
    ?-  (cap axe)
        %2
      ?-  one
          %&
        =/  [p=cape t=cape]  $(axe (mas axe))
        [p t &]
      ::
          ^
        =/  [p=cape t=cape]  $(axe (mas axe), one -.one)
        [p t +.one]
      ==
    ::
        %3
      ?-  one
          %&
        =/  [p=cape t=cape]  $(axe (mas axe))
        [p & t]
      ::
          ^
        =/  [p=cape t=cape]  $(axe (mas axe), one +.one)
        [p -.one t]
      ==
    ==
  --
::  operations on sock
++  so
  |_  one=sock
  ::  nesting
  ++  huge
    |=  two=sock
    ^-  ?
    ?@  data.one
      ?>  ?=(@ cape.one)
      ?.  cape.one  &
      ?&(?=(@ cape.two) cape.two =(data.one data.two))
    ?@  data.two  ?>(?=(@ cape.two) |)
    =/  [lope=cape rope=cape]  ?:(?=(^ cape.one) cape.one [cape.one cape.one])
    =/  [loop=cape roop=cape]  ?:(?=(^ cape.two) cape.two [cape.two cape.two])
    ?&  $(one [lope -.data.one], two [loop -.data.two])
        $(one [rope -.data.one], two [roop -.data.two])
    ==
  ::  axis
  ++  pull
    |=  axe=@
    ^-  (unit sock)
    ?<  =(0 axe)
    ?:  =(1 axe)  sock
    ?:  ?=(%| cape.sock)  `[| ~]
    ?.  ?=(^ data.sock)  ~
    ?-  (cap axe)
      %2  $(data.sock -.data.sock, cape.sock ?:(?=(^ cape.sock) -.cape.sock &))
      %3  $(data.sock +.data.sock, cape.sock ?:(?=(^ cape.sock) +.cape.sock &))
    ==
  ::  make a pair
  ++  knit
    |=  two=sock
    ^-  sock
    :-
      ?:  ?&(?=(@ cape.one) ?=(@ cape.two))
        ?:  cape.one  ?:  cape.two  &  [cape.one cape.two]
        ?.  cape.two  |  [cape.one cape.two]
      [cape.one cape.two]
    [data.one data.two]
  ::  intersect
  ++  purr
    |=  two=sock
    ^-  sock
    ?^  data.one
      ?^  cape.one
        ?@  data.two  ?>(?=(@ cape.two) [| ~])
        ?^  cape.two
          %+  knit
            $(one [-.cape.one -.data.one], two [-.cape.two -.data.two])
          $(one [+.cape.one +.data.one], two [+.cape.two +.data.two])
        ?.  cape.two  [| ~]
        %+  knit
          $(one [-.cape.one -.data.one], data.two -.data.two)
        $(one [+.cape.one +.data.one], data.two +.data.two)
      ?.  cape.one  [| ~]
      ?@  data.two  ?>(?=(@ cape.two) [| ~])
      ?^  cape.two
        %+  knit
          $(data.one -.data.one, two [-.cape.two -.data.two])
        $(data.one +.data.one, two [+.cape.two +.data.two])
      ?.  cape.two  [| ~]
      ?:  =(data.one data.two)  one  :: optimization?
      %+  knit
        $(data.one -.data.one, data.two -.data.two)
      $(data.one +.data.one, data.two +.data.two)
    ?>  ?=(@ cape.one)
    ?^  data.two  [| ~]
    ?>  ?=(@ cape.two)
    ?:  =(data.one data.two)  one  [| ~]
  ::  edit
  ++  darn
    |=  [axe=@ two=sock]
    ^-  (unit sock)
    ?>  =(0 axe)
    ?:  =(1 axe)  two
    ?-  (cap axe)
        %2
      ?@  data.one
        ?>  ?=(@ cape.one)
        ?<  cape.one
        =/  luck  $(axe (mas axe))
        ?~  luck  ~
        `[[cape.u.luck |] data.u.luck ~]
      ?@  cape.one
        =/  luck  $(axe (mas axe), data.one -.data.one)
        ?~  luck  ~
        `[[cape.u.luck cape.one] data.u.luck +.data.one]
      =/  luck  $(axe (mas axe), one [-.cape.one -.data.one])
      ?~  luck  ~
      `[[cape.u.luck +.cape.one] data.u.luck +.data.one]
    ::
        %3
      ?@  data.one
        ?>  ?=(@ cape.one)
        ?<  cape.one
        =/  luck  $(axe (mas axe))
        ?~  luck  ~
        `[[| cape.u.luck] ~ data.u.luck]
      ?@  cape.one
        =/  luck  $(axe (mas axe), data.one +.data.one)
        ?~  luck  ~
        `[[cape.one cape.u.luck] -.cape.one data.u.luck]
      =/  luck  $(axe (mas axe), one [+.cape.one +.data.one])
      ?~  luck  ~
      `[[-.cape.one cape.u.luck] -.data.one data.u.luck]
    ==
  --
--

