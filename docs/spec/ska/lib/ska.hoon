/-  *sock
|%
:: Split an axis into a sock into safe and unsafe components
++  punt
  |=  [axe=@ =sock]
  ^-  [@ @ sock]
  ?:  =(0 axe)
    [0 0 %toss ~]
  =/  saf  1
  |-
    ?:  =(axe 1)
      [saf 1 sock]
    ?+  sock  [0 0 %toss ~]
      [%know * *]
    ?-  (cap axe)
      %2  $(axe (mas axe), sock [%know -.know.sock], saf (peg saf 2))
      %3  $(axe (mas axe), sock [%know +.know.sock], saf (peg saf 3))
    ==
      ::
      [%bets *]
    ?-  (cap axe)
      %2  $(axe (mas axe), sock hed.sock, saf (peg saf 2))
      %3  $(axe (mas axe), sock tal.sock, saf (peg saf 3))
    ==
      ::
      [%toss ~]
    [saf axe %toss ~]
  ==
:: Get an axis from a sock
++  pull
  |=  arg=[@ sock]
  ^-  boot
  =/  [saf rik ken]  (punt arg)
  ?:  =(0 saf)  [%boom ~]
  ?:  =(1 rik)  [%safe ken]
  [%risk ken]
++  yank
  |=  [axe=@ =boot]
  ?-  boot
    [%safe *]  (pull axe sure.boot)
    [%risk *]  (dare (pull axe hope.boot))
    [%boom ~]  [%boom ~]
  ==
:: Test if sock is atom or cell, or unknown
++  fits
  |=  =sock
  ^-  ^sock
  ?-  sock
      ::
      [%know @]
    [%know 1]
      ::
      [%know * *]
    [%know 0]
      ::
      [%bets *]
    [%know 0]
      ::
      [%dice ~]
    [%know 1]
      ::
      [%flip ~]
    [%know 1]
      ::
      [%toss ~]
    [%flip ~]
  ==
:: Test if we can know two socks are equal
++  pear
  |=  [a=sock b=sock]
  ^-  sock
  ?:  ?&(?=([%know *] a) ?=([%know *] b))
    ?:  =(know.a know.b)
      [%know 0]
    [%know 1]
  [%flip ~]
:: Test if we can know two boots are equal
++  bopp
  |=  [a=boot b=boot]
  ^-  boot
  ?:  ?=  [%boom ~]  a
    [%boom ~]
  ?:  ?=  [%boom ~]  b
    [%boom ~]
  ?-  a
      ::
      [%safe *]
    ?-  b
        ::
        [%safe *]
      [%safe (pear sure.a sure.b)]
        ::
        [%risk *]
      [%risk (pear sure.a hope.b)]
    ==
      ::
      [%risk *]
    ?-  b
        ::
        [%safe *]
      [%risk (pear hope.a sure.b)]
        ::
        [%risk *]
      [%risk (pear hope.a hope.b)]
    ==
  ==
:: combine two socks into a sock of a cell
++  knit
  |=  [a=sock b=sock]
  ^-  sock
  ?.  ?=  [%know *]  a
    [%bets a b]
  ?.  ?=  [%know *]  b
    [%bets a b]
  [%know [know.a know.b]]
:: combine two boots into a boot of a cell
++  cobb
  |=  [hed=boot tal=boot]
  ^-  boot
  ?:  ?=  [%boom ~]  hed
    [%boom ~]
  ?:  ?=  [%boom ~]  tal
    [%boom ~]
  ?-  hed
      ::
      [%safe *]
    ?-  tal
        ::
        [%safe *]
      [%safe (knit sure.hed sure.tal)]
        ::
        [%risk *]
      [%risk (knit sure.hed hope.tal)]
    ==
      ::
      [%risk *]
    ?-  tal
        ::
        [%safe *]
      [%risk (knit hope.hed sure.tal)]
        ::
        [%risk *]
      [%risk (knit hope.hed hope.tal)]
    ==
  ==
:: patch a sock
++  darn
  |=  [axe=@ pat=sock =sock]
  ^-  boot
  ?:  .=  0  axe
    [%boom ~]
  |-
  ^-  boot
  ?:  =(axe 1)
    [%safe pat]
  ?:  ?=  [%dice ~]  sock
    [%boom ~]
  ?:  ?=  [%flip ~]  sock
    [%boom ~]
  ?:  ?=  [%know @]  sock
    [%boom ~]
  ?-  (cap axe)
      ::
      %2
    ?-  sock
        ::
        [%know * *]
      (cobb $(axe (mas axe), sock [%know -.know.sock]) [%safe %know +.know.sock])
        ::
        [%bets * *]
      (cobb $(axe (mas axe), sock hed.sock) [%safe tal.sock])
        ::
        [%toss ~]
      (cobb $(axe (mas axe)) [%risk %toss ~])
    ==
      ::
      %3
    ?-  sock
        ::
        [%know * *]
      (cobb [%safe %know -.know.sock] $(axe (mas axe), sock [%know +.know.sock]))
        ::
        [%bets * *]
      (cobb [%safe hed.sock] $(axe (mas axe), sock tal.sock))
        ::
        [%toss ~]
      (cobb [%risk %toss ~] $(axe (mas axe)))
    ==
  ==
:: Stitch a boot into another boot
++  welt
  |=  [axe=@ pach=boot wole=boot]
  ^-  boot
  ?:  ?=  [%boom ~]  pach
    [%boom ~]
  ?:  ?=  [%boom ~]  wole
    [%boom ~]
  =/  poch
    ?-  pach
        ::
        [%safe *]
      sure.pach
        ::
        [%risk *]
      hope.pach
    ==
  =/  wool
    ?-  wole
        ::
        [%safe *]
      sure.wole
        ::
        [%risk *]
      hope.wole
    ==
  ?:  ?&  ?=  [%safe *]  wole  ?=  [%safe *]  pach  ==
    (darn axe poch wool)
  (dare (darn axe poch wool))

:: Pessimize a boot by making it %risk even if it's %safe
++  dare
  |=  =boot
  ?-  boot
      ::
      [%boom ~]
    [%boom ~]
      ::
      [%risk *]
    [%risk hope.boot]
      ::
      [%safe *]
    [%risk sure.boot]
  ==
:: Weaken a %know
++  fray
  |=  a=*
  ^-  sock
  ?:  ?=  @  a
    [%dice ~]
  [%bets [%know -.a] [%know +.a]]
:: Produce the intersection of two socks
++  mous
  |=  [a=sock b=sock]
  ?:  ?&(?=([%know *] a) ?=([%know *] b))
    ?:  =(know.a know.b)
      a
    $(a (fray a), b (fray b))
  ?:  ?=([%know *] a)
    $(a (fray a))
  ?:  ?=([%know *] b)
    $(b (fray b))
  ?:  ?&(?=([%bets *] a) ?=([%bets *] b))
    [%bets $(a hed.a, b hed.b) $(a tal.a, b tal.b)]
  ?:  ?&(?=([%dice ~] a) ?|(?=([%dice ~] b) ?=([%flip ~] b)))
    [%dice ~]
  ?:  ?&(?=([%dice ~] b) ?=([%flip ~] a))
    [%dice ~]
  ?:  ?&(?=([%flip ~] a) ?=([%flip ~] b))
    [%flip ~]
  [%toss ~]
::  Produce the intersection of two boots
::
::  Note that the intersection of a safe or risk
::  boot and a boom boot is a risk boot, since
::  in a branch between a possibly non-crashing computation
::  and a crashing computation, we might crash and we might not.
::
::  In particular, we have to handle assertions and
::  error cases where it is intended that one branch of a conditional
::  will crash
++  gnaw
  |=  [a=boot b=boot]
  ?:  ?=  [%safe *]  a
    ?:  ?=  [%safe *]  b
      [%safe (mous sure.a sure.b)]
    ?:  ?=  [%risk *]  b
      [%risk (mous sure.a hope.b)]
    [%risk sure.a]
  ?:  ?=  [%risk *]  a
    ?:  ?=  [%safe *]  b
      [%risk (mous hope.a sure.b)]
    ?:  ?=  [%risk *]  b
      [%risk (mous hope.a hope.b)]
    [%risk hope.a]
  ?:  ?=  [%safe *]  b
    [%risk sure.b]
  ?:  ?=  [%risk *]  b
    [%risk hope.b]
  [%boom ~]
::  Produce a boot of whether a given boot is a cell or atom
++  ques
  |=  non=boot
  ^-  boot
  ?:  ?=([%boom ~] non)
    [%boom ~]
  ?-  non
      ::
      [%safe %know @]
    [%safe %know 1]
      ::
      [%safe %know * *]
    [%safe %know 0]
      ::
      [%safe %bets *]
    [%safe %know 0]
      ::
      [%safe %dice ~]
    [%safe %know 1]
      ::
      [%safe %flip ~]
    [%safe %know 1]
      ::
      [%safe %toss ~]
    [%safe %flip ~]
      ::
      [%risk %know @]
    [%risk %know 1]
      ::
      [%risk %know * *]
    [%risk %know 0]
      ::
      [%risk %bets *]
    [%risk %know 0]
      ::
      [%risk %dice ~]
    [%risk %know 1]
      ::
      [%risk %flip ~]
    [%risk %know 1]
      ::
      [%risk %toss ~]
    [%risk %flip ~]
  ==
++  pile
  |=  tom=boot
  ^-  boot
  ?+  tom  [%boom ~]
      ::
      [%safe %know @]
    [%safe %dice ~]
      ::
      [%safe %dice ~]
    [%safe %dice ~]
      ::
      [%safe %flip ~]
    [%safe %dice ~]
      ::
      [%safe %toss ~]
    [%risk %dice ~]
      ::
      [%risk %know @]
    [%risk %dice ~]
      ::
      [%risk %dice ~]
    [%risk %dice ~]
      ::
      [%risk %flip ~]
    [%risk %dice ~]
      ::
      [%risk %toss ~]
    [%risk %dice ~]
  ==
::  Produce knowledge of the result given knowledge of the subject
++  wash
  |=  [subj=sock form=*]
  ^-  boot
  =+  ward=(map [sock *] boot)
  ?+  form  [%boom ~]
      ::
      [[* *] *]
    (cobb $(form -.form) $(form +.form))
      ::
      [%0 @]
    (pull +.form subj)
      ::
      [%1 *]
    [%safe %know +.form]
      ::
      [%2 * *]
    =/  subn  $(form +<.form)
    ?:  ?=([%boom ~] subn)
      [%boom ~]
    =/  forn  $(form +>.form)
    ?:  ?=([%boom ~] forn)
      [%boom ~]
    ?:  ?=  [%safe %dice ~]  forn
      [%boom ~]
    ?:  ?=  [%safe %flip ~]  forn
      [%boom ~]
    ?:  ?=  [%risk %dice ~]  forn
      [%boom ~]
    ?:  ?=  [%risk %flip ~]  forn
      [%boom ~]
    ?+  forn  [%risk %toss ~]
        ::
        [%safe %know *]
      ?-  subn
          ::
          [%safe *]
        $(subj sure.subn, form know.sure.forn)
          ::
          [%risk *]
        (dare $(subj hope.subn, form know.sure.forn))
      ==
        ::
        [%risk %know *]
      ?-  subn
          ::
          [%safe *]
        (dare $(subj sure.subn, form know.hope.forn))
          ::
          [%risk *]
        (dare $(subj hope.subn, form know.hope.forn))
      ==
    ==
      ::
      [%3 *]
    (ques $(form +.form))
      ::
      [%4 *]
    (pile $(form +.form))
      ::
      [%5 * *]
    (bopp $(form +<.form) $(form +>.form))
      ::
      [%6 * * *]
    =/  cond  $(form +<.form)
    ?+  cond  [%boom ~]
        ::
        [%safe *]
      ?+  sure.cond  [%boom ~]
          ::
          [%know %0]
        $(form +>-.form)
          ::
          [%know %1]
        $(form +>+.form)
          ::
          [%flip ~]
        (gnaw $(form +>-.form) $(form +>+.form))
          ::
          [%dice ~]
        (dare (gnaw $(form +>-.form) $(form +>+.form)))
          ::
          [%toss ~]
        (dare (gnaw $(form +>-.form) $(form +>+.form)))
      ==
        ::
        [%risk *]
      ?+  hope.cond  [%boom ~]
          ::
          [%know %0]
        (dare $(form +>-.form))
          ::
          [%know %1]
        (dare $(form +>+.form))
          ::
          [%flip ~]
        (dare (gnaw $(form +>-.form) $(form +>+.form)))
          ::
          [%dice ~]
        (dare (gnaw $(form +>-.form) $(form +>+.form)))
          ::
          [%toss ~]
        (dare (gnaw $(form +>-.form) $(form +>+.form)))
      ==
    ==
      ::
      [%7 * *]
    =/  news  $(form +<.form)
    ?+  news  [%boom ~]
        ::
        [%safe *]
      $(subj sure.news, form +>.form)
        ::
        [%risk *]
      (dare $(subj hope.news, form +>.form))
    ==
      ::
      [%8 * *]
    =/  news  $(form +<.form)
    ?+  news  [%boom ~]
        ::
        [%safe *]
      $(subj (knit sure.news subj), form +>.form)
        ::
        [%risk *]
      (dare $(subj (knit hope.news subj), form +>.form))
    ==
      ::
      [%9 @ *]
    =/  news  $(form +>.form)
    ?+  news  [%boom ~]
        ::
        [%safe *]
      =/  newf  (pull +<.form sure.news)
      ?+  newf  [%boom ~]
          ::
          [%safe %know *]
        $(subj sure.news, form know.sure.newf)
          ::
          [%risk %know *]
        (dare $(subj sure.news, form know.hope.newf))
          ::
          [%safe *]
        [%risk %toss ~]
          ::
          [%risk *]
        [%risk %toss ~]
      ==
        ::
        [%risk *]
      =/  newf  (pull +<.form hope.news)
      ?+  newf  [%boom ~]
          ::
          [%safe %know *]
        (dare $(subj hope.news, form know.sure.newf))
          ::
          [%risk %know *]
        (dare $(subj hope.news, form know.hope.newf))
          ::
          [%safe *]
        [%risk %toss ~]
          ::
          [%risk *]
        [%risk %toss ~]
      ==
    ==
      ::
      [%10 [@ *] *]
    (welt +<-.form $(form +<+.form) $(form +>.form))
      ::
      [%11 @ *]
    $(form +>.form)
      ::
      [%11 [* *] *]
    =/  hint  $(form +<+.form)
    ?+  hint  [%boom ~]
        ::
        [%safe *]
      $(form +>.form)
        ::
        [%risk *]
      (dare $(form +>.form))
    ==
      ::
      [%12 *]
    [%risk %toss ~]
  ==
  ++  cuff
    |=  =sock
    =/  axe  1
    |-
    ^-  (list @)
    ?-  sock
        ::
        [%know *]
      (limo [axe ~])
        ::
        [%bets *]
      (weld $(axe (add axe axe), sock hed.sock) $(axe (add (add axe axe) 1), sock tal.sock))
        ::
        [%dice ~]
      (limo [axe ~])
        ::
        [%flip ~]
      (limo [axe ~])
        ::
        [%toss ~]
      (limo [axe ~])
    ==
--
