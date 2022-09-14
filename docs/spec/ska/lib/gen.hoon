:: TODO:
:: ?= in wide form
:: fix ?- and ?+
/-  *sock
/-  *gene
/+  ska
=|  prog=tabl
=|  buff=lock
=|  bust=(list [buff=lock])
=|  gab=gabl
=*  this  .
=<
|%
:: Write an instruction to the program buffer
++  inst
  |=  [=lick]
  ^-  _this
  this(buff [lick buff])
++  gibl
  ^-  [gabl _this]
  [gab this(gab .+(gab))]
:: Finish writing a procedure
++  done
  |=  says=boot
  ^-  [boot _this]
  :-  says
  this(prog (~(put by prog) bloc [(flop buff) says]), buff ~)]
:: Procrastinate on the current procedure, start
:: writing the one with a new label
++  proc
  ^-  _this
  this(bust [buff bust], buff ~)
:: Done procrastinating, work on the procedure we
:: were doing
++  crop
  ^-  _this
  ?<  ?~  bust
  this(buff buff.i.bust, bust t.bust)
:: Generate code for a formula
::
:: TODO: add crash instruction %bom, generate whenever
:: returning [%boom ~]
++  gene
  |=  blos=labl
  =|  mod=mode
  =/  bloc  blos
  |^
    ^-  [boot _this]
    ?-  mod
        ::
        %save
      ?+  for.bloc  boom
          ::
          [[* *] *]
        =.  this  (inst [%puh 1])
        =^  hed  this  $(for.bloc -.for.bloc)
        =.  this  (inst [%put 0])
        =^  tal  this  $(for.bloc +.for.bloc)
        =.  this  (inst [%cel 0])
        =.  this  (inst [%pop ~])
        [(cobb:ska hed tal) this]
          ::
          [%0 @]
        ?:  .=  +.for.bloc  0
          bomb
        =. (inst [%axe +.for.bloc])
        [(pull:ska +.for.bloc sub.bloc) this]
          ::
          [%1 *]
        =.  (inst [%con +.for.bloc])
        [[%safe %know +.for.bloc] this]
          ::
          [%2 * *]
        =.  this  (inst [%puh 3])
        =.  this  (inst [%sav 1])
        =^  news  this  $(for.bloc +<.for.bloc, mod %step)
        ?:  ?=  [%boom ~]  news
          boom
        =/  nows
          ?-  news
            [%safe *]  sure.news
            [%risk *]  risk.news
          ==
        =/  shis  this
        =.  this  (inst [%put 2])
        =.  this  (inst [%reo 1])
        =^  newf  this  $(for.bloc +>.for.bloc, mod %step)
        =.  this  (inst [%reo 2])
        ?:  ?=  [%boom ~]  newf
          bomb
        ?:  ?=  [%safe %know *]  newf
          =.  this  shis
          =.  this  (inst [%sub ~])
          =/  nabl  [nows know.sure.newf]
          =^  res   this  (fics nabl)
          =.  this  (inst [%cal nabl])
          =.  this  (inst [%reo 1])
          =.  this  (inst [%pop ~])
          ?:  ?=  [%safe *]  news
            [res this]
          [(dare res) this]
        ?:  ?=  [%risk %know *]  newf
          =/  nabl  [nows know.hope.newf]
          =^  res  this  (fics nabl)
          =.  this  (inst [%cal nabl])
          =.  this  (inst [%reo 1])
          =.  this  (inst [%pop ~])
          [(dare res) this]
        =.  this  (inst [%lnk ~])
        =.  this  (inst [%reo 1])
        =.  this  (inst [%pop ~])
        [[%risk %gues ~] this]
          ::
          [%3 *]
        =^  non  this  $(for.bloc +.for.bloc)
        =.  this  (inst [%clq ~])
        [(ques:ska non) this]
          ::
          [%4 *]
        =^  num  this  $(for.bloc +.for.bloc)
        =.  this  (inst [%inc ~])
        [(pile:ska num) this]
          ::
          [%5 * *]
        =.  this  (inst [%puh 1])
        =^  nox  this  $(for.bloc +<.for.bloc)
        =.  this  (inst [%put 0])
        =^  noy  this  $(for.bloc +>.for.bloc)
        =.  this  (inst [%eqq 0])
        =.  this  (inst [%pop ~])
        [(bopp nox noy) this]
          ::
          [%6 * * *]
        =/  shis  this
        =^  tes  this  $(for.bloc +<.for.bloc)
        ?:  ?=  [%boom ~]  tes
          boom
        ?:  ?=  [%safe %know 0]  tes
          =.  this  shis
          =^  res  this  $(for.bloc +>-.for.bloc)
          [res this]
        ?:  ?=  [%safe %know 1]  tes
          =.  this  shis
          =^  res  this  $(for.bloc +>+.for.bloc)
          [res this]
        ?:  ?=  [%risk %know 0]  tes
          =^  res  this  $(for.bloc +>-.for.bloc)
          [res this]
        ?:  ?=  [%risk %know 1]  tes
          =^  res  this  $(for.bloc +>+.for.bloc)
          [res this]
        ?:  ?|  ?=  [%safe %know *]  tes
                ?=  [%safe %bets *]  tes
                ?=  [%risk %know *]  tes
                ?=  [%risk %bets *]  tes
            ==
          boom
        =^  gib  this  gibl
        =^  geb  this  gibl
        =.  this  (inst [%br1 gib])
        =^  roo  this  $(for.bloc +>-.for.bloc)
        =.  this  (inst [%bru geb])
        =.  this  (inst [%brh gib])
        =^  ral  this  $(for.bloc +>+.for.bloc)
        =.  this  (inst [%brh geb])
        ?:  ?=  [%safe %flip ~]  tes
          [(gnaw roo ral) this]
        [(dare (gnaw roo ral)) this]
          ::
          [%7 * *]
        =.  this  (inst [%puh 1])
        =.  this  (inst [%sav 0])
        =^  news  this  $(for.bloc +<.for.bloc, mod %step)
        ?:  ?=  [%boom ~]  news
          boom
        =/  nows
          ?-  news
              ::
              [%safe *]
            sure.news
              ::
              [%risk *]
            hope.news
          ==
        =.  this  (inst [%sub ~])
        =^  res  this  $(for.bloc +>.for.bloc, sub.bloc nows, mod %step)
        =.  this  (inst [%reo 0])
        =.  this  (inst [%pop ~])
        ?:  ?=  [%safe *]  news
          [res this]
        [(dare res) this]
          ::
          [%8 * *]
        =.  this  (inst [%puh 1])
        =.  this  (inst [%sav 0])
        =^  newh  this  $(for.bloc +<.for.bloc, mod %step)
        ?:  ?=  [%boom ~]  newh
          boom
        =/  nowh
          ?-  newh
              ::
              [%safe *]
            sure.newh
              ::
              [%risk *]
            hope.newh
          ==
        =.  this  (inst [%ext ~])
        =^  res  $(for.bloc +>.for.bloc, sub.bloc (knit nows sub.bloc), mod %step)
        =.  this  (inst [%reo 0])
        =.  this  (inst [%pop ~])
        ?:  ?=  [%safe *]  newh
          [res this]
        [(dare res) this]
          ::
          [%9 @ *]
        =.  this  (inst [%puh 2])
        =.  this  (inst [%sav 1])
        =^  newc  this  $(for.bloc +>.for.bloc, mod %step)
        ?:  ?=  newc  [%boom ~]
          boom
        =/  nowc
          ?-  newc
              ::
              [%safe *]
            sure.newc
              ::
              [%risk *]
            hope.newc
          ==
        =.  this  (inst [%sub ~])
        =/  newf  (pull +<.for.bloc nowc)
        ?:  ?=  [%boom ~]  newf
          boom
        =/  shis  this
        =.  this  (inst [%axe +<.for.bloc])
        ?:  ?=  [%safe %know *]  newf
          =.  this  shis
          =/  nabl  [nowc know.sure.newf]
          =^  res  this  (fics nabl)
          =.  this  (inst [%cal nabl])
          =.  this  (inst [%reo 1])
          =.  this  (inst [%pop ~])
          ?:  ?=  [%safe *]  newc
            [res this]
          [(dare res) this]
        ?:  ?=  [%risk %know *]  newf
          =/  nabl  [nowc know.hope.newf]
          =^  res  this  (fics nabl)
          =.  this  (inst [%cal nabl])
          =.  this  (inst [%reo 1])
          =.  this  (inst [%pop ~])
          [(dare res) this]
        =.  this  (inst [%lnk ~])
        =.  this  (inst [%reo 1])
        =.  this  (inst [%pop ~])
        [[%risk %gues ~] this]
          ::
          [%10 [@ *] *]
        =.  this  (inst [%puh 2])
        =.  this  (inst [%sav 0])
        =^  wole  this  $(for.bloc +>.for.bloc, mod %step)
        ?:  ?=  [%boom ~]  wole
          boom
        =.  this  (inst [%put 1])
        =.  this  (inst [%reo 0])
        =^  pach  this  $(for.bloc +<+.for.bloc, mod %step)
        ?:  ?=  [%boom ~]  pach
          boom
        =.  this  (inst [%reo 1])
        =.  this  (inst [%edt +<-.for.bloc])
        =.  this  (inst [%reo 0])
        =.  this  (inst [%pop ~])
        [(welt axe pach wole) this]
          ::
          [%11 @ *]
        $(for.bloc +>.for.bloc)
          ::
          [%11 [* *] *]
        =/  shis  this
        =^  hnt  this  $(for.bloc +<+.for.bloc)
        ?:  ?=  [%safe *]  hnt
          =.  this  shis
          $(for.bloc +>.for.bloc)
        $(for.bloc +>.for.bloc)
          ::
          [%12 * *]
        =.  this  (inst [%puh 1])
        =^  ref  this  $(for.bloc +<.for.bloc)
        ?:  ?=  [%boom ~]  ref
          boom
        =.  this  (inst [%put 0])
        =^  pat  this  $(for.bloc +<
        ?:  ?=  [%boom ~]  pat
          boom
        =.  this  (inst [%cel 0])
        =.  this  (inst [%spy ~])
        [[%risk %gues ~] this]
      ==
        ::
        %step
      ?+  for.bloc  boom
          ::
          [[* *] *]
        =.  this  (inst [%puh 1])
        =^  hed  this  $(for.bloc -.for.bloc, mod %save)
        =.  this  (inst [%put 0])
        =^  tal  this  $(for.bloc +.for.bloc)
        =.  this  (inst [%cel 0])
        =.  this  (inst [%pop ~])
        [(cobb:ska hed tal) this]
          ::
          [%0 @]
        =.  (inst [%axe +.for.bloc])
        [(pull:ska +.for.bloc sub.bloc) this]
          ::
          [%1 *]
        =.  (inst [%con +.for.bloc])
        [[%safe %know +.for.bloc] this]
          ::
          [%2 * *]
        =.  this  (inst [%puh 2])
        =^  news  this  $(for.bloc +<.for.bloc, mod %save)
        ?:  ?=  [%boom ~]  news
          bomb
        =/  nows
          ?-  news
              ::
              [%safe *]
            sure.news
              ::
              [%risk *]
            hope.news
          ==
        =/  shis  this
        =.  this  (inst [%put 1]) 
        =^  newf  this  $(for.bloc +>.for.bloc)
        =.  this  (inst [%reo 1])
        ?:  ?=  [%boom ~]  newf
          bomb
        ?:  ?=  [%safe %know *]  newf
          =.  this  shis
          =.  this  (inst [%sub ~])
          =/  nabl  [nows know.sure.newf)
          =^  res  this  (fics nabl)
          =.  this  (inst [%cal nabl])
          =.  this  (inst [%pop ~])
          ?:  ?=  [%safe *]  news
            [res this]
          [(dare res) this]
        ?:  ?=  [%risk %know *]  newf
          =/  nabl  [nows know.hope.newf]
          =^  res  this  (fics nabl)
          =.  this  (inst [%cal nabl])
          =.  this  (inst [%pop ~])
          [(dare res) this]
        =.  this  (inst [%lnk ~])
        =.  this  (inst [%pop ~])
        [[%risk %gues ~] this]
          ::
          [%3 *]
        =^  non  this  $(for.bloc +.for.bloc)
        =.  this  (inst [%clq ~])
        [(ques:ska non) this]
          ::
          [%4 *]
        =^  num  this  $(for.bloc +.for.bloc)
        =.  this  (inst [%inc ~]
        [(pile:ska num) this]
          ::
          [%5 * *]
        =.  this  (inst [%puh 1])
        =^  nox  this  $(for.bloc +<.for.bloc, mod %save)
        =.  this  (inst [%put 0])
        =^  noy  this  $(for.bloc +>.for.bloc)
        =.  this  (inst [%eqq 0])
        =.  this  (inst [%pop ~])
        [(bopp nox noy) this]
          ::
          [%6 * * *]
        =/  shis  this
        =^  tes  this  $(for.bloc +<.for.bloc, mod %save)
        ?:  ?=  [%boom ~]  tes
          boom
        ?:  ?=  [%safe %know 0]  tes
          =.  this  shis
          =^  res  this  $(for.bloc +>-.for.bloc)
          [res this]
        ?:  ?=  [%safe %know 1]  tes
          =.  this  shis
          =^  res  this  $(for.bloc +>+.for.bloc)
          [res this]
        ?:  ?=  [%risk %know 0]  tes
          =^  res  this  $(for.bloc +>-.for.bloc)
          [res this]
        ?:  ?=  [%risk %know 1]  tes
          =^  res  this  $(for.bloc +>+.for.bloc)
          [res this]
        ?:  ?|  ?=  [%safe %know *]  tes
                ?=  [%safe %bets *]  tes
                ?=  [%risk %know *]  tes
                ?=  [%risk %bets *]  tes
            ==
          boom
        =^  gib  this  gibl
        =^  geb  this  gibl
        =.  this  (inst [%br1 gib])
        =^  roo  this  $(for.bloc +>-.for.bloc)
        =.  this  (inst [%bru geb])
        =.  this  (inst [%brh gib])
        =^  ral  this  $(for.bloc +>+.for.bloc)
        =.  this  (inst [%brh geb])
        ?:  ?=  [%safe %flip ~]  tes
          [(gnaw roo ral) this]
        [(dare (gnaw roo ral)) this]
          ::
          [%7 * *]
        =^  news  this  $(for.bloc +<.for.bloc)
        ?:  ?=  [%boom ~]  news
          boom
        =/  nows
          ?-  news
              ::
              [%safe *]
            sure.news
              ::
              [%risk *]
            hope.news
          ==
        =.  this  (inst [%sub ~])
        =^  res  $(for.bloc +>.for.bloc, sub.bloc nows)
        ?:  ?=  [%safe *]  news
          [res this]
        [(dare res) this]
          ::
          [%8 * *]
        =^  newh  this  $(for.bloc +<.for.bloc, mod %save)
        ?:  ?=  [%boom ~]  newh
          boom
        =/  nowh
          ?-  newh
              ::
              [%safe *]
            sure.newh
              ::
              [%risk *]
            hope.newh
          ==
        =.  this  (inst [%ext ~])
        =^  res  $(for.bloc +>.for.bloc, sub.bloc (knit nows sub.bloc))
        ?:  ?=  [%safe *]  newh
          [res this]
        [(dare res) this]
          ::
          [%9 @ *]
        =.  this  (inst [%puh 1])
        =^  newc  this  $(for.bloc +>.for.bloc)
        ?:  ?=  [%boom ~]  newc
          boom
        =/  nowc
          ?-  newc
              ::
              [%safe *]
            sure.newc
              ::
              [%risk *]
            hope.newc
          ==
        =.  this  (inst [%sub ~])
        =/  newf  (pull +<.for.bloc nowc)
        ?:  ?=  [%boom ~]  newf
          boom
        =/  shis  this
        =.  this  (inst [%axe +<.for.bloc])
        ?:  ?=  [%safe %know *]  newf
          =.  this  shis
          =/  nabl  [nowc know.sure.newf]
          =^  res  this  (fics nabl)
          =.  this  (inst [%cal nabl])
          =.  this  (inst [%pop ~])
          ?:  ?=  [%safe *]  newc
            [res this]
          [(dare res) this]
        ?:  ?=  [%risk %know *]  newf
          =/  nabl  [nowc know.hope.newf]
          =^  res  this  (fics nabl)
          =.  this  (inst [%cal nabl])
          =.  this  (inst [%pop ~])
          [(dare res) this]
        =.  this  (inst [%lnk ~])
        =.  this  (inst [%pop ~])
        [[%risk %gues ~] this]
          ::
          [%10 [@ *] *]
        =.  this  (inst [%puh 1])
        =^  wole  this  $(for.bloc +>.for.bloc, mod %save)
        ?:  ?=  [%boom ~]  wole
          boom
        =.  this  (inst [%put 0])
        =^  pach  this  $(for.bloc +<+.for.bloc)
        ?:  ?=  [%boom ~]  pach
          boom
        =.  this  (inst [%reo 0])
        =.  this  (inst [%edt +<-.for.bloc])
        =.  this  (inst [%pop ~])
        [(welt axe pach wole) this]
          ::
          [%11 @ *]
        $(for.bloc +>.for.bloc)
          ::
          [%11 [* *] *]
        =/  shis  this
        =^  hnt  this  $(for.bloc +<+.for.bloc, mod %save)
        ?:  ?=  [%safe *]  hnt
          =.  this  shis
          $(for.bloc +>.for.bloc)
        $(for.bloc +>.for.bloc)
          ::
          [%12 * *]
        =.  this  (inst [%puh 1])
        =^  ref  this  $(for.bloc +<.for.bloc, mod %save)
        =.  this  (inst [%put 0])
        =^  pat  this  $(for.bloc +>.for.bloc)
        =.  this  (inst [%cel 0])
        =.  this  (inst [%pop ~])
        =.  this  (inst [%spy ~])
        [[%risk %gues ~] this]
      ==
        ::
        %butt
      ?+  for.bloc  boom
          ::
          [[* *] *]
        =.  this  (inst [%puh 1])
        =^  hed  this  $(for.bloc -.for.bloc, mod %save)
        =.  this  (inst [%put 0])
        =^  tal  this  $(for.bloc +.for.bloc, mod %step)
        =.  this  (inst [%cel 0])
        =.  this  (inst [%pop ~])
        [(cobb:ska hed tal) this]
          ::
          [%0 @]
        =.  (inst [%axe +.for.bloc])
        [(pull:ska +.for.bloc sub.bloc) this]
          ::
          [%1 *]
        =.  (inst [%con +.for.bloc])
        [[%safe %know +.for.bloc] %this]
          ::
          [%2 * *]
        =^  news  this  $(for.bloc +<.for.bloc, mod %save)
        ?:  ?=  [%boom ~]  news
          bomb
        =/  nows
          ?-  news
              ::
              [%safe *]
            sure.news
              ::
              [%risk *]
            hope.news
          ==
        =/  shis  this
        =.  this  (inst [%puh 1])
        =.  this  (inst [%put 0])
        =^  newf  this  $(for.bloc +>.for.bloc, mod %step)
        =.  this  (inst [%cel 0])
        =.  this  (inst [%pop ~])
        =.  this  (inst [%noc ~])
        ?:  ?=  [%boom ~]  newf
          bomb
        ?:  ?=  [%safe %know *]  newf
          =.  this  shis
          =.  this  (inst [%sub ~])
          =/  nabl  [nows know.sure.newf]
          =^  res  this  (fics nabl)
          =.  this  (inst [%jmp nabl])
          ?:  ?=  [%safe *]  news
            [res this]
          [(dare res) this]
        ?:  ?=  [%risk %know *]  newf
          =/  nabl  [nows know.hope.newf]
          =^  res  this  (fics nabl)
          =.  this  (inst [%jmp nabl])
          [(dare res) this]
        =.  this  (inst [%lnt ~])
        [[%risk %gues ~] this]
          ::
          [%3 *]
        =^  non  this  $(for.bloc +.for.bloc, mod %step)
        =.  this  (inst [%clq ~])
        [(ques:ska non) this]
          ::
          [%4 *]
        =^  num  this  $(for.bloc +.for.bloc, mod %step)
        =.  this  (inst [%inc ~])
        [(pile:ska num) this]
          ::
          [%5 * *]
        =.  this  (inst [%puh 1])
        =^  nox  this  $(for.bloc +<.for.bloc, mod %save)
        =.  this  (inst [%put 0])
        =^  noy  this  $(for.bloc +>.for.bloc, mod %step)
        =.  this  (inst [%eqq 0])
        =.  this  (inst [%pop ~])
        [(bopp nox noy) this]
          ::
          [%6 * * *]
        =/  shis  this
        =^  tes  this  $(for.bloc +<.for.bloc, mod %save)
        ?:  ?=  [%boom ~]  tes
          bomb
        ?:  ?=  [%safe %know 0]  tes
          =.  this  shis
          =^  res  this  $(for.bloc +>-.for.bloc)
          [res this]
        ?:  ?=  [%safe %know 1]  tes
          =.  this  shis
          =^  res  this  $(for.bloc +>+.for.bloc)
          [res this]
        ?:  ?=  [%risk %know 0]  tes
          =^  res  this  $(for.bloc +>-.for.bloc)
          [res this]
        ?:  ?=  [%risk %know 1]  tes
          =^  res  this  $(for.bloc +>+.for.bloc)
          [res this]
        ?:  ?|  ?=  [%safe %know *]  tes
                ?=  [%safe %bets *]  tes
                ?=  [%risk %know *]  tes
                ?=  [%risk %bets *]  tes
            ==
          bomb
        =^  gib this gibl
        =.  this  (inst [%br1 gib])
        =^  roo  this  $(for.bloc +>-.for.bloc)
        =.  this  (inst [%brh gib])
        =^  ral  this  $(for.bloc +>+.for.bloc)
        ?:  ?=  [%safe %flip ~]  tes
          [(gnaw roo ral) this]
        [(dare (gnaw roo ral)) this]
          ::
          [%7 * *]
        =^  news  this  $(for.bloc +<.for.bloc, mod %step)
        ?:  ?=  [%boom ~]  news
          boom
        =/  nows
          ?-  news
              ::
              [%safe *]
            sure.news
              ::
              [%risk *]
            hope.news
          ==
        =.  this  (inst [%sub ~])
        =^  res  this  $(for.bloc +>.for.bloc, sub.bloc nows)
        ?:  ?=  [%safe *]  news
          [res this]
        [(dare res) this]
          ::
          [%8 * *]
        =^  newh  this  $(for.bloc +<.for.bloc, mod %save)
        ?:  ?=  [%boom ~]  newh
          boom
        =/  nowh
          ?-  newh
              ::
              [%safe *]
            sure.newh
              ::
              [%risk *]
            hope.newh
          ==
        =.  this  (inst [%ext ~])
        =^  res  $(for.bloc +>.for.bloc, sub.bloc (knit nows sub.bloc))
        ?:  ?=  [%safe *]  newh
          [res this]
        [(dare res) this]
          ::
          [%9 @ *]
        =^  newc  this  $(for.bloc +>.for.bloc, mod %step)
        ?:  ?=([%boom ~] newc)
          bomb
        =/  nowc
          ?-  newc
            [%safe *]  sure.newc
            [%risk *]  hope.newc
          ==
        =.  this  (inst [%sub ~])
        =/  newf  (pull +<.for.bloc nowc)
        ?:  ?=  [%boom ~]  newf
          bomb
        =/  shis  this
        =.  this  (inst [%axe +<.for.bloc])
        ?:  ?=  [%safe %know *]  newf
          =.  this  shis
          =/  nabl  [nowc know.sure.newf]
          =^  res  this  (fics nabl)
          =.  this  (inst [%jmp nabl])
          ?:  ?=  [%safe *]  newc
            [res this]
          [(dare res) this]
        ?:  ?=  [%risk %know *]  newf
          =/  nabl  [nowc know.sure.newf]
          =^  res  this  (fics nabl)
          =.  this  (inst [%jmp nabl])
          [(dare res) this]
        =.  this  (inst [%lnt ~])
        [[%risk %gues ~] this]
          ::
          [%10 [@ *] *]
        =.  this  (inst [%puh 1])
        =^  wole  this  $(for.bloc +>.for.bloc, mod %save)
        ?:  ?=  [%boom ~]  wole
          bomb
        =.  this  (inst [%put 0])
        =^  pach  this  $(for.bloc +<+.for.bloc, mod %step)
        ?:  ?=  [%boom ~]  pach
          bomb
        =.  this  (inst [%reo 0])
        =.  this  (inst [%edt +<-.for.bloc])
        =.  this  (inst [%pop ~])
        [(welt axe pach wole) this]
          ::
          [%11 @ *]
        $(for.bloc +>.for.bloc)
          ::
          [%11 [* *] *]
        =/  shis  this
        =^  hnt  this  $(for.bloc +<+.for.bloc, mod %save)
        ?:  ?=  [%safe *]  hnt
          =.  this  shis
          $(for.bloc +>.for.bloc)
        $(for.bloc +>.for.bloc)
          ::
          [%12 * *]
        =.  this  (inst [%puh 1])
        :: TODO: finish 12 butt
        
      ==
        ::
        %tail
      ?+  for.bloc  bomb
          ::
          [[* *] *]
        =.  this  (inst [%puh 1])
        =^  hed  this  $(for.bloc -.for.bloc, mod %save)
        =.  this  (inst [%put 0])
        =^  tal  this  $(for.bloc +.for.bloc, mod %step)
        =.  this  (inst [%cel 0])
        =.  this  (inst [%pop ~])
        (done (cobb hed tal))
          ::
          [%0 @]
        =.  (inst [%axe +.for.bloc])
        (done (pull +.for.bloc sub.bloc))
          ::
          [%1 *]
        =.  (inst [%con +.for.bloc])
        (done (%safe %know +.for.bloc))
          ::
          [%2 *]
        =^  news  this  $(for.bloc +<.for.bloc, mod %save)
        ?:  ?=  [%boom ~]  news
          bomb
        =/  nows
          ?-  news
              ::
              [%safe *]
            sure.news
              ::
              [%risk *]
            hope.news
          ==
        =/  shis  this
        =.  this  (inst [%puh 1])
        =.  this  (inst [%put 0])
        =^  newf  this  $(for.bloc +>.for.bloc, mod %step)
        =.  this  (inst [%cel 0])
        =.  this  (inst [%pop ~])
        =.  this  (inst [%noc ~])
        ?:  ?=  [%boom ~]  newf
          bomb
        ?:  ?=  [%safe %know *]  newf
          =.  this  shis
          =.  this  (inst [%sub ~])
          =/  nabl  [nows know.sure.newf]
          =^  res  this  (fics nabl)
          =.  this  (inst [%jmp nabl])
          ?:  ?=  [%safe *]  news
            (done res)
          (done (dare res))
        ?:  ?=  [%risk %know *]  newf
          =/  nabl  [nows know.hope.newf]
          =^  res  this  (fics nabl)
          =.  this  (inst [%jmp nabl])
          (done (dare res))
        =.  this  (inst [%lnt ~])
        (done [%risk %gues ~])
          ::
          [%3 *]
        =^  non  this  $(for.bloc +.for.bloc, mod %step)
        =.  this  (inst [%clq ~])
        (done (ques:ska non))
          ::
          [%4 *]
        =^  num  this  $(for.bloc +.for.bloc, mod %step)
        =.  this  (inst [%inc ~])
        (done (pile:ska non))
          ::
          [%5 * *]
        =.  this  (inst [%puh 1])
        =^  nox  this  $(for.bloc +<.for.bloc, mod %save)
        =.  this  (inst [%put 0])
        =^  noy  this  $(for.bloc +<.for.bloc, mod %step)
        =.  this  (inst [%eqq 0])
        =.  this  (inst [%pop ~])
        (done (bopp:ska nox noy))
          ::
          [%6 * * *]
        =/  shis  this
        =^  tes  this  $(for.bloc +<.for.bloc, mod %save)
        ?:  ?=  [%boom ~]  tes
          bomb
        ?:  ?=  [%safe %know 0]
          =.  this  shis
          =^  res  this  $(for.bloc +>-.for.bloc)
          (done res)
        ?:  ?=  [%safe %know 1]
          =.  this  shis
          =^  res  this  $(for.bloc +>+.for.bloc)
          (done res)
        ?:  ?=  [%risk %know 0]
          =^  res  this  $(for.bloc +>-.for.bloc)
          (done (dare res))
        ?:  ?=  [%risk %know 1]
          =^  res  this  $(for.bloc +>+.for.bloc)
          (done (dare res))
        ?:  ?|  ?=  [%safe %know *]  tes
                ?=  [%safe %bets *]  tes
                ?=  [%risk %know *]  tes
                ?=  [%risk %bets *]  tes
            ==
          bomb
        =^  gib  this  gibl
        =.  this  (inst [%br1 gib])
        =^  roo  this  $(for.bloc +>-.for.bloc)
        =.  this  (inst [%brh gib])
        =^  ral  this  $(for.bloc +>+.for.bloc)
        ?:  ?=  [%safe %flip ~]  tes
          (done (gnaw:ska roo ral))
        (done (dare:ska (gnaw:ska roo ral))
          ::
          [%7 * *]
        =^  news  this  $(for.bloc +<.for.bloc, mod %step)
        ?:  ?=  [%boom ~]  news
          bomb
        =/  nows
          ?-  news
              ::
              [%safe *]
            sure.news
              ::
              [%risk *]
            hope.news
          ==
        =.  this  (inst [%sub ~])
        =^  res  this  $(for.bloc +>.for.bloc, sub.bloc nows)
        ?:  ?=  [%safe *]  news
          (done res)
        (done (dare:ska res))
          ::
          [%8 * *]
        =^  newh  this  $(for.bloc +<.for.bloc, mod %save)
        ?:  ?=  [%boom ~]  newh
          bomb
        =/  nowh
          ?-  newh
              ::
              [%safe *]
            sure.newh
              ::
              [%risk *]
            hope.newh
          ==
        =.  this  (inst [%ext ~])
        =^  res  $(for.bloc +>.for.bloc, sub.bloc (knit nows sub.bloc))
        ?:  ?=  [%safe *]  newh
          (done res)
        (done (dare:ska res))
          ::
          [%9 @ *]
        =^  newc  this  $(for.bloc +>.for.bloc, mod %step)
        ?:  ?=  [%boom ~]  newc
          bomb
        =/  nowc
          ?-  newc
              ::
              [%safe *]
            sure.newc
              ::
              [%risk *]
            hope.newc
          ==
        =.  this  (inst [%sub ~])
        =/  newf  (pull +<.for.bloc nowc)
        ?:  ?=  [%boom ~]  newf
          bomb
        =/  shis  this
        =.  this  (inst [%axe +<.for.bloc])
        ?:  ?=  [%safe %know *]  newf
          =.  this  shis
          =/  nabl  [nowc know.sure.newf]
          =^  res   this  (fics nabl)
          =.  this  (inst [%jmp nabl])
          ?:  ?=  [%safe *]  newc
            (done res)
          (done (dare res))
        ?:  ?=  [%risk %know *]  newf
          =/  nabl  [nowc know.sure.newf]
          =^  res   this  (fics nabl)
          =.  this  (inst [%jmp nabl])
          (done (dare res))
        =.  this  (inst [%lnt ~])
        (done [%risk %gues ~])
          ::
          [%10 [@ *] *]
        =.  this  (inst [%puh 1])
        =^  wole  this  $(for.bloc +>.for.bloc, mod %save)

    ==
  ++  done
    |=  says=boot
    ^-  [boot _this]
    :-  says
    ?:  ?=  says  [%boom ~]
      this(prog (~(put by prog) blos [~[[%bom ~]] says]), buff ~)
    this(prog (~(put by prog) blos [(flop buff) says]), buff ~)
  ++  fics
    |=  cabl=labl
    ^-  [boot _this]
    =/  vet  (~(get by prog) cabl)
    ?~  vet
      =.  this  proc
      =^  res  this  (^$ cabl)
      =.  this  crop
      [res this]
    u.vet
  ++  bomb
    ?:  ?=  mod  %tail
      (done [%boom ~])
    :-  [%boom ~]
    this(buff ~[[%bom ~]])
  --
--
