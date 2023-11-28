|%
::  operations on $cape
++  ca
  |_  one=cape
  ::    axes of yes
  ::
  ::  list all axes of %.y in a cape
  ++  yea
    ^-  (list @)
    =/  axe  1
    |-  ^-  (list @)
    ?-  one
      %|  ~
      %&  ~[axe]
      ^  (weld $(one -.one, axe (peg axe 2)) $(one +.one, axe (peg axe 3)))
    ==
  ::    cape intersection
  ::
  ::  intersect two capes
  ++  int
    |=  two=cape
    ^-  cape
    ?-  one
        %|  %|
        %&  two
        ^
      ?-  two
          %|  %|
          %&  one
          ^
        =/  l   $(one -.one, two -.two)
        =/  r   $(one +.one, two +.two)
        ?:(?&(?=(@ l) =(l r)) l [l r])
      ==
    ==
  ::    apply a cape as a mask to a sock
  ::  
  ::  mask unknown axes in a cape out of a sock
  ++  app
    |=  know=sock
    |-  ^-  sock
    ?-  one
        %|  [%| ~]
        %&  know
        ^
      ?:  ?=(%| cape.know)  [%| ~]
      ?>  ?=(^ data.know)
      ?:  ?=(^ cape.know)
        =/  l  $(one -.one, cape.know -.cape.know, data.know -.data.know)
        =/  r  $(one +.one, cape.know +.cape.know, data.know +.data.know)
        [[cape.l cape.r] data.l data.r]
      =/  l  $(one -.one, data.know -.data.know)
      =/  r  $(one +.one, data.know +.data.know)
      [[cape.l cape.r] data.l data.r]
    ==
  ::    union two capes
  ::
  ::  
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
    ==
  ::    Added axes?
  ::
  ::  big returns true if any subaxis of a masked axis in one
  ::  is unmasked in two. Note that this is not an ordering relation as
  ::  it is not antisymmetric
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
  ::    non-null?
  ::
  ::  true if there are any unmasked axes
  ++  any
    ^-  ?
    ?@  one  one
    ?|(any(one -.one) any(one +.one))
  ::    push a cape down to an axis
  ::
  ::  this builds a path described by the input axis with one at the
  ::  bottom
  ++  pat
    |=  axe=@
    ?<  =(0 axe)
    |-  ^-  cape
    ?:  =(1 axe)  one
    ?-  (cap axe)
      %2  [$(axe (mas axe)) |]
      %3  [| $(axe (mas axe))]
    ==
  ::    split a cape
  ::
  ::  assume a cape will be applied to a cell,
  ::  and provide capes for the head and tail of the cell.
  ++  rip
    ^-  [cape cape]
    ?-  one
      %|  [| |]
      %&  [& &]
      ^   one
    ==
  ::    poke a hole in a cape
  ::
  ::  mask an axis out of a cape, and return a cape
  ::  describing which subaxes were unmasked
  ++  awl
    |=  axe=@
    ?<  =(0 axe)
    |-  ^-  [cape cape]
    ?:  ?=(%| one)  [| |]
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
  ::    valid?
  ++  apt
    |-  ^-  ?
    ?@  cape.one
      &
    ?@  data.one
      |
    ?&  $(cape.one -.cape.one, data.one -.data.one)
        $(cape.one +.cape.one, data.one +.data.one)
    ==
  ::    normalize
  ::  throw away unknown axes in data (setting to ~)
  ++  norm
    |-  ^-  sock
    ?-  cape.one
        %|  [%| ~]
        %&  one
        ^
      ?>  ?=(^ data.one)
      =/  l  $(cape.one -.cape.one, data.one -.data.one)
      =/  r  $(cape.one +.cape.one, data.one +.data.one)
      ?:  ?&(=(& cape.l) =(& cape.r))
        [& data.l data.r]
      ?:  ?&(=(| cape.l) =(| cape.r))
        [| ~]
      [[cape.l cape.r] data.l data.r]
    ==
  ::    nesting
  ::
  ::  true if there are no axes unmasked in two but masked in one,
  ::  and if all axes unmasked in both have the same data
  ++  huge
    |=  two=sock
    ^-  ?
    ?@  data.one
      ?.  ?=(@ cape.one)  ~|  badone+one  !!
      ?.  cape.one  &
      ?&(?=(@ cape.two) cape.two =(data.one data.two))
    ?@  data.two  ?>(?=(@ cape.two) |)
    =/  [lope=cape rope=cape]  ?:(?=(^ cape.one) cape.one [cape.one cape.one])
    =/  [loop=cape roop=cape]  ?:(?=(^ cape.two) cape.two [cape.two cape.two])
    ?&  $(one [lope -.data.one], two [loop -.data.two])
        $(one [rope +.data.one], two [roop +.data.two])
    ==
  ::    axis
  ::
  ::  create a sock that is known to be cells down the given axis
  ::  and at that axis is one
  ++  pull
    |=  axe=@
    ^-  (unit sock)
    ?:  =(0 axe)  ~
    |-  ^-  (unit sock)
    ?:  =(1 axe)  `one
    ?:  ?=(%| cape.one)  `[| ~]
    ?.  ?=(^ data.one)  ~
    ?-  (cap axe)
      %2  $(data.one -.data.one, cape.one ?:(?=(^ cape.one) -.cape.one &), axe (mas axe))
      %3  $(data.one +.data.one, cape.one ?:(?=(^ cape.one) +.cape.one &), axe (mas axe))
    ==
  ::    pair
  ::
  ::  takes a pair of socks to a sock of a pair.
  ++  knit
    |=  two=sock
    ^-  sock
    :-
      ?:  ?&(?=(@ cape.one) ?=(@ cape.two))
        ?:  cape.one  ?:  cape.two  &  [cape.one cape.two]
        ?.  cape.two  |  [cape.one cape.two]
      [cape.one cape.two]
    [data.one data.two]
  ::    intersect
  ::
  ::  output is unmasked only where both one and two are unmasked and
  ::  they both agree in data
  ++  purr
    |=  two=sock
    |-  ^-  sock
    ?^  data.one
      ?@  data.two  ?>(?=(@ cape.two) [| ~])
      ?^  cape.one
        ?^  cape.two
          %-  %~  knit  so
            $(one [-.cape.one -.data.one], two [-.cape.two -.data.two])
          $(one [+.cape.one +.data.one], two [+.cape.two +.data.two])
        ?.  cape.two  [| ~]
        %-  %~  knit  so
          $(one [-.cape.one -.data.one], data.two -.data.two)
        $(one [+.cape.one +.data.one], data.two +.data.two)
      ?.  cape.one  [| ~]
      ?^  cape.two
        %-  %~  knit  so 
          $(data.one -.data.one, two [-.cape.two -.data.two])
        $(data.one +.data.one, two [+.cape.two +.data.two])
      ?.  cape.two  [| ~]
      ?:  =(data.one data.two)  one  :: optimization?
      %-  %~  knit  so
        $(data.one -.data.one, data.two -.data.two)
      $(data.one +.data.one, data.two +.data.two)
    ?>  ?=(@ cape.one)
    ?^  data.two  [| ~]
    ?>  ?=(@ cape.two)
    ?:  =(data.one data.two)  one  [| ~]
  ::    edit
  ::
  ::  update mask and data at an axis into a sock
  ++  darn
    |=  [axe=@ two=sock]
    ^-  (unit sock)
    ?:  =(0 axe)  ~
    |-  ^-  (unit sock)
    ?:  =(1 axe)  `two
    ?@  data.one
      ?>  ?=(@ cape.one)
      ?:  cape.one  ~
      =/  luck  $(axe (mas axe))
      ?~  luck  ~
      ?-  (cap axe)
        %2  `[[cape.u.luck |] data.u.luck ~]
        %3  `[[| cape.u.luck] ~ data.u.luck]
      ==
    ?@  cape.one
      ?-  (cap axe)
          %2
        =/  luck  $(axe (mas axe), data.one -.data.one)
        ?~  luck  ~
        `[[cape.u.luck cape.one] data.u.luck +.data.one]
      ::
          %3
        =/  luck  $(axe (mas axe), data.one +.data.one)
        ?~  luck  ~
        `[[cape.one cape.u.luck] -.data.one data.u.luck]
      ==
    ?-  (cap axe)
        %2
      =/  luck  $(axe (mas axe), cape.one -.cape.one, data.one -.data.one)
      ?~  luck  ~
      `[[cape.u.luck +.cape.one] data.u.luck +.data.one]
    ::
        %3
      =/  luck  $(axe (mas axe), cape.one +.cape.one, data.one +.data.one)
      ?~  luck  ~
      `[[-.cape.one cape.u.luck] -.data.one data.u.luck]
    ==
  --
::    apt assertion
::
::  assert a sock is apt:so
++  sap
  |=  know=sock
  ?>  ~(apt so know)
  know
--

