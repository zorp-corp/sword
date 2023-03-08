/-  *sock
!:
|%
++  trip
  |=  toob=$<(%boom boot)
  ^-  (unit *)
  ?-  -.toob
    %safe  (stub sure.toob)
    %risk  (stub hope.toob)
  ==
++  stub
  |=  =sock
  ^-  (unit *)
  ?:  ?=(%know -.sock)
    `know.sock
  ~
:: Split an axis into a sock into safe and unsafe components
++  punt
  |=  [axe=@ =sock]
  ^-  [@ @ ^sock]
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
  =+  [saf rik ken]=(punt arg)
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
    $(a (fray know.a), b (fray know.b))
  ?:  ?=([%know *] a)
    $(a (fray know.a))
  ?:  ?=([%know *] b)
    $(b (fray know.b))
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
  =|  ward=(map [sock *] boot)
  =.  ward  (~(put by ward) [subj form] [%risk %toss ~])
  |^
    -:swab
  ++  swab
    |-
    ^-  [boot _ward]
    ?>  ?=(^ form)
    ?+  form  [[%boom ~] ward]
        ::
        [[* *] *]
      =^  l  ward  $(form -.form)
      =^  r  ward  $(form +.form)
      :_  ward
      (cobb l r)
        ::
        [%0 @]
      :_  ward
      (pull +.form subj)
        ::
        [%1 *]
      :_  ward
      [%safe %know +.form]
        ::
        [%2 * *]
      =^  subn  ward  $(form +<.form)
      ?:  ?=([%boom ~] subn)
        [[%boom ~] ward]
      =^  forn  ward  $(form +>.form)
      ?:  ?=([%boom ~] forn)
        [[%boom ~] ward]
      ?:  ?=  [%safe %dice ~]  forn
        [[%boom ~] ward]
      ?:  ?=  [%safe %flip ~]  forn
        [[%boom ~] ward]
      ?:  ?=  [%risk %dice ~]  forn
        [[%boom ~] ward]
      ?:  ?=  [%risk %flip ~]  forn
        [[%boom ~] ward]
      ?+  forn  [[%risk %toss ~] ward]
          ::
          [%safe %know *]
        ?-  subn
            ::
            [%safe *]
          =/  nubs  sure.subn
          =/  norm  know.sure.forn
          =/  mem  (~(get by ward) [nubs norm])
          ?.  ?=(~ mem)  [u.mem ward]
          =.  ward  (~(put by ward) [nubs norm] [%risk %toss ~])
          =^  r  ward  $(subj nubs, form norm)
          [r (~(put by ward) [nubs norm] r)]
            ::
            [%risk *]
          =/  nubs  hope.subn
          =/  norm  know.sure.forn
          =/  mem  (~(get by ward) [nubs norm])
          ?.  ?=(~ mem)  [u.mem ward]
          =.  ward  (~(put by ward) [nubs norm] [%risk %toss ~])
          =^  r  ward  $(subj nubs, form norm)
          [(dare r) (~(put by ward) [nubs norm] (dare r))]
        ==
          ::
          [%risk %know *]
        ?-  subn
            ::
            [%safe *]
          =/  nubs  sure.subn
          =/  norm  know.hope.forn
          =/  mem  (~(get by ward) [nubs norm])
          ?.  ?=(~ mem)  [u.mem ward]
          =.  ward  (~(put by ward) [nubs norm] [%risk %toss ~])
          =^  r  ward  $(subj nubs, form norm)
          [(dare r) (~(put by ward) [nubs norm] (dare r))]
            ::
            [%risk *]
          =/  nubs  hope.subn
          =/  norm  know.hope.forn
          =/  mem  (~(get by ward) [nubs norm])
          ?.  ?=(~ mem)  [u.mem ward]
          =.  ward  (~(put by ward) [nubs norm] [%risk %toss ~])
          =^  r  ward  $(subj nubs, form norm)
          [(dare r) (~(put by ward) [nubs norm] (dare r))]
        ==
      ==
        ::
        [%3 *]
      =^  s  ward  $(form +.form)
      :_  ward
      (ques s)
        ::
        [%4 *]
      =^  s  ward  $(form +.form)
      :_  ward
      (pile s)
        ::
        [%5 * *]
      =^  l  ward  $(form +<.form)
      =^  r  ward  $(form +>.form)
      :_  ward
      (bopp l r)
        ::
        [%6 * * *]
      =^  cond  ward  $(form +<.form)
      ?+  cond  [[%boom ~] ward]
          ::
          [%safe *]
        ?+  sure.cond  [[%boom ~] ward]
            ::
            [%know %0]
          $(form +>-.form)
            ::
            [%know %1]
          $(form +>+.form)
            ::
            [%flip ~]
          =^  t  ward  $(form +>-.form)
          =^  f  ward  $(form +>+.form)
          :_  ward
          (gnaw t f)
            ::
            [%dice ~]
          =^  t  ward  $(form +>-.form)
          =^  f  ward  $(form +>+.form)
          :_  ward
          (dare (gnaw t f))
            ::
            [%toss ~]
          =^  t  ward  $(form +>-.form)
          =^  f  ward  $(form +>+.form)
          :_  ward
          (dare (gnaw t f))
        ==
          ::
          [%risk *]
        ?+  hope.cond  [[%boom ~] ward]
            ::
            [%know %0]
          =^  t  ward  $(form +>-.form)
          :_  ward
          (dare t)
            ::
            [%know %1]
          =^  f  ward  $(form +>+.form)
          :_  ward
          (dare f)
            ::
            [%flip ~]
          =^  t  ward  $(form +>-.form)
          =^  f  ward  $(form +>+.form)
          :_  ward
          (dare (gnaw t f))
            ::
            [%dice ~]
          =^  t  ward  $(form +>-.form)
          =^  f  ward  $(form +>+.form)
          :_  ward
          (dare (gnaw t f))
            ::
            [%toss ~]
          =^  t  ward  $(form +>-.form)
          =^  f  ward  $(form +>+.form)
          :_  ward
          (dare (gnaw t f))
        ==
      ==
        ::
        [%7 * *]
      =^  news  ward  $(form +<.form)
      ?+  news  [[%boom ~] ward]
          ::
          [%safe *]
        $(subj sure.news, form +>.form)
          ::
          [%risk *]
        =^  r  ward  $(subj hope.news, form +>.form)
        :_  ward
        (dare r)
      ==
        ::
        [%8 * *]
      =^  news  ward  $(form +<.form)
      ?+  news  [[%boom ~] ward]
          ::
          [%safe *]
        $(subj (knit sure.news subj), form +>.form)
          ::
          [%risk *]
        =^  r  ward  $(subj (knit hope.news subj), form +>.form)
        :_  ward
        (dare r)
      ==
        ::
        [%9 @ *]
      =^  news  ward  $(form +>.form)
      ?+  news  [[%boom ~] ward]
          ::
          [%safe *]
        =/  newf  (pull +<.form sure.news)
        ?+  newf  [[%boom ~] ward]
            ::
            [%safe %know *]
          =/  nubs  sure.news
          =/  norm  know.sure.newf
          =/  mem  (~(get by ward) [nubs norm])
          ?.  ?=(~ mem)  [u.mem ward]
          =.  ward  (~(put by ward) [nubs norm] [%risk %toss ~])
          =^  r  ward  $(subj nubs, form norm)
          :_  (~(put by ward) [nubs norm] r)
          r
            ::
            [%risk %know *]
          =/  nubs  sure.news
          =/  norm  know.hope.newf
          =/  mem  (~(get by ward) [nubs norm])
          ?.  ?=(~ mem)  [u.mem ward]
          =.  ward  (~(put by ward) [nubs norm] [%risk %toss ~])
          =^  r  ward  $(subj nubs, form norm)
          :_  (~(put by ward) [nubs norm] (dare r))
          (dare r)
            ::
            [%safe *]
          [[%risk %toss ~] ward]
            ::
            [%risk *]
          [[%risk %toss ~] ward]
        ==
          ::
          [%risk *]
        =/  newf  (pull +<.form hope.news)
        ?+  newf  [[%boom ~] ward]
            ::
            [%safe %know *]
          =/  nubs  hope.news
          =/  norm  know.sure.newf
          =/  mem  (~(get by ward) [nubs norm])
          ?.  ?=(~ mem)  [u.mem ward]
          =.  ward  (~(put by ward) [nubs norm] [%risk %toss ~])
          =^  r  ward  $(subj nubs, form norm)
          :_  (~(put by ward) [nubs norm] (dare r))
          (dare r)
            ::
            [%risk %know *]
          =/  nubs  hope.news
          =/  norm  know.hope.newf
          =/  mem  (~(get by ward) [nubs norm])
          ?.  ?=(~ mem)  [u.mem ward]
          =.  ward  (~(put by ward) [nubs norm] [%risk %toss ~])
          =^  r  ward  $(subj nubs, form norm)
          :_  (~(put by ward) [nubs norm] (dare r))
          (dare r)
            ::
            [%safe *]
          [[%risk %toss ~] ward]
            ::
            [%risk *]
          [[%risk %toss ~] ward]
        ==
      ==
        ::
        [%10 [@ *] *]
      =^  p  ward  $(form +<+.form)
      =^  w  ward  $(form +>.form)
      :_  ward
      (welt +<-.form p w)
        ::
        [%11 @ *]
      $(form +>.form)
        ::
        [%11 [* *] *]
      =^  hint  ward  $(form +<+.form)
      ?+  hint  [[%boom ~] ward]
          ::
          [%safe *]
        $(form +>.form)
          ::
          [%risk *]
        =^  r  ward  $(form +<.form)
        :_  ward
        (dare r)
      ==
        ::
        [%12 *]
      [[%risk %toss ~] ward]
    ==
  --
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
