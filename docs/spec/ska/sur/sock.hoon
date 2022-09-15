|%
+$  sock
  $%  [%know know=*]            :: We know everything about this noun
      [%bets hed=sock tal=sock] :: This noun is a cell, with partial knowledge of its head and tail
      [%dice ~]                 :: This noun is an atom
      [%flip ~]                 :: This noun is an atom, specifically 0 or 1
      [%gues ~]                 :: We know nothing about this noun
  ==
+$  boot
  $%  [%safe sure=sock]         :: The Nock that produces this noun will not crash
      [%risk hope=sock]         :: The Nock that produces this noun might crash
      [%boom ~]                 :: The Nock will crash
  ==
--
