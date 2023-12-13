::    finished code table
::
::  entries describe input and output knowledge
::  and analyzed code.
::  Key to the jar is a formula.
::  Call labels are the key formula and the soot face of each entry
=|  moan=(jar * hone) :: finished code table
::    cold state
::
::  see $cool in sur/noir/hoon
=|  cole=cool :: cold state
|%
::
::    core reference
++  thus  .
::
::  trace a label, building up a set of labels it depends on
++  keep
  |=  [live=(set [sock *]) seed=[sock *]]
  =/  work=(list [=sock f=*])  ~[seed]
  =|  krow=(list [=sock f=*])
  |-  ^-  _live
  ?~  work
    ?~  krow  live
    $(work (flop krow), krow ~)
  ?:  (~(has in live) i.work)  $(work t.work)
  =/  hose  (~(get ja moan) f.i.work)
  |-  ^-  _live
  ?~  hose  ~|  %keep-wiff  !!
  ?.  (~(huge so soot.i.hose) sock.i.work)  $(hose t.hose)
  =/  next  ~(val by ices.norm.i.hose)
  ^$(live (~(put in live) i.work), krow (weld next krow), work t.work)
::
::  drop labels not in the given live set
::
::  To build the live set, repeatedly invoke +keep, threading through
::  the live set, and passing labels which will still be needed. This
::  will ensure that transitive dependencies remain in +moan.
++  drop
  |=  live=(set [sock *])
  =|  noam=(jar * hone)
  =/  moal=(list [* (list hone)])  ~(tap by moan)
  |-  ^-  _thus
  ?~  moal  thus(moan noam)
  |-  ^-  _thus
  ?~  +.i.moal  ^$(moal t.moal)
  =/  bell  [soot.i.+.i.moal -.i.moal]
  ?.  (~(has in live) bell)  $(+.i.moal t.+.i.moal)
  $(+.i.moal t.+.i.moal, noam (~(add ja noam) -.i.moal i.+.i.moal))
::
::    outer work loop
::
::  analyze nock and subject and emit %nomm.
::  Usually the sock will simply be  [%.y <subject>] for an indirect
::  call or outer runtime invocation
++  rout
  |=  [soot=sock form=*]
  ^-  _thus
  =/  moot :: in progress code table
    %+  ~(put by *(map @hail toot))  `@hail`1
    :*  soot  |
        `form  ~
        [| ~]  |  ~
    ==
  =|  mind=(map @hail hind)
  =/  work=(list @hail)  ~[`@hail`1]
  =/  mite  (~(put in *(set @hail)) `@hail`1)
  =|  kids=(jug @hail @hail)
  |^  ^-  _thus
    =>  raid
    =>  loot
    =>  espy
    =>  ruin
    ?~(work thus $)
  ::
  ::    inner core reference
  ++  this  .
  ::    lower to nomm
  ::
  ::  - translate syntactic crashes to [%not 0]
  ::  - translate [8 b c] to (nomm of) [7 [b 0 1] c]
  ::  - translate [9 b c] to (nomm of) [7 c 2 [0 1] 0 c]
  ::  - label 2 and 11 with formula axes for decoration
  ++  raid
    =/  cork  work
    |-  ^-  _this
    ?~  cork  this
    =*  hail  i.cork
    =/  firm  form:(~(got by moot) hail)
    ?>  ?=(^ firm)
    =*  form  u.firm
    =;  code
      %=  $
          moot
        %+  ~(jab by moot)  hail
        |=  =toot
        toot(norm `code)
      ::
          cork  t.cork
      ==
    |-  ^-  nomm
    ?+  form  [%not 0]  :: invalid nock crashes
        [^ *]
      [%par $(form -.form, hail (peg hail 2)) $(form +.form, hail (peg hail 3))]
    ::
        [%0 axe=@]
      [%not axe.form]
    ::
        [%1 non=*]
      [%one non.form]
    ::
        [%2 sofa=* fora=*]
      :*  %two
          ::  we treat the cell [sofa fora] as axis 6 and the
          ::  hypothetically inlined called formula as axis 7
          ::  so the hypothetical inlining looks like
          ::  [%2 [sofa fora] <called formula>]
          $(form sofa.form, hail (peg hail 12))
          $(form fora.form, hail (peg hail 13))
          hail
      ==
    ::
        [%3 coat=*] 
      [%the $(form coat.form, hail (peg hail 3))]
    ::
        [%4 tome=*]
      [%for $(form tome.form, hail (peg hail 3))]
    ::
        [%5 this=* that=*]
      :*  %ivy
          $(form this.form, hail (peg hail 6))
          $(form that.form, hail (peg hail 7))
      ==
    ::
        [%6 what=* then=* else=*]
      :*  %six
        $(form what.form, hail (peg hail 6))
        $(form then.form, hail (peg hail 14))
        $(form else.form, hail (peg hail 15))
      ==
    ::
        [%7 once=* then=*]
      :*  %eve
        $(form once.form, hail (peg hail 6))
        $(form then.form, hail (peg hail 7))
      ==
    ::
        [%8 pint=* then=*]
      $(form [%7 [pint.form [%0 1]] then.form])
    ::
        [%9 here=@ coil=*]
      $(form [%7 coil.form [%2 [%0 1] [%0 here.form]]])
    ::
        [%10 [here=@ twig=*] tree=*]
      :*  %ten
          here.form
          $(form twig.form, hail (peg hail 13))
          $(form tree.form, hail (peg hail 7))
      ==
    ::
        [%11 hint=@ then=*]
      [%sip hint.form $(form then.form, hail (peg hail 7))]
    ::
        [%11 [hint=@ vice=*] then=*]
      :*  %tip
          hint.form
          $(form vice.form, hail (peg hail 13))
          $(form then.form, hail (peg hail 7))
          hail
      ==
    ::
        [%12 rent=* walk=*]
      :*  %elf
          $(form rent.form, hail (peg hail 6))
          $(form walk.form, hail (peg hail 7))
      ==
    ==
  ::    battery masks
  ::
  ::  given the current set of known formulas,
  ::  discover a mask of which axes in each callsite subject are
  ::  actually used to fix which formulas are evaluated
  ++  espy
    =/  cork  work
    |-  ^-  _this
    ?~  cork  this
    =*  hail  i.cork
    =/  [norm=(unit nomm) rake=cape]
      [norm rake]:(~(got by moot) hail)
    ?>  ?=(^ norm)
    =*  code  u.norm
    =^  soon  moot
      |-  ^-  [cape _moot]
      ?-  -.code
          %par
        =/  [lack=cape rack=cape]  ~(rip ca rake)
        =^  lead  moot  $(code left.code, rake lack)
        =^  reed  moot  $(code rite.code, rake rack)
        [(~(uni ca lead) reed) moot]
      ::
          %not
        ?:  =(0 here.code)  [| moot]
        [(~(pat ca rake) here.code) moot]
      ::
          %one  [| moot]
          %two
        =/  [soot=sock fake=cape form=(unit) norm=(unit nomm)]
          [soot rake form norm]:(~(got by moot) rail.code)
        =/  mole=(list hone)  ?~(form ~ (~(get ja moan) u.form))
        |-  ^-  [cape _moot]
          ?^  mole
            ?:  ?&  (~(huge so soot.i.mole) soot)
                    !(~(big ca cape.root.i.mole) rake)
                ==
              [cape.soot.i.mole moot]
            $(mole t.mole)
        =.  moot
          %+  ~(jab by moot)  rail.code
          |=  =toot
          toot(rake rake)
        =?  moot  ?&(?=(^ norm) (~(big ca fake) rake))
          +:^$(hail rail.code, code u.norm, rake rake)
        =/  lake  sake:(~(got by moot) rail.code)
        =^  sake  moot  ^$(rake lake, code cost.code)
        =^  folk  moot  ^$(rake &, code corn.code)
        [(~(uni ca sake) folk) moot]
      ::
          %the
        $(code pell.code, rake |)
      ::
          %for
        $(code mall.code, rake |)
      ::
          %ivy
        =^  lake  moot  $(code this.code, rake |)
        =^  rare  moot  $(code that.code, rake |)
        [(~(uni ca lake) rare) moot]
      ::
          %six
        =^  cake  moot  $(code what.code, rake |)
        =^  lake  moot  $(code then.code)
        =^  rare  moot  $(code else.code)
        [(~(uni ca cake) (~(uni ca lake) rare)) moot]
      ::
          %eve
        =^  rare  moot  $(code then.code)
        $(code once.code, rake rare)
      ::
          %ten
        ?:  =(0 here.code)  [| moot]
        =/  [wipe=cape wine=cape]  (~(awl ca rake) here.code)
        =^  lake  moot  $(code twig.code, rake wipe)
        =^  rare  moot  $(code tree.code, rake wine)
        [(~(uni ca lake) rare) moot]
      ::
          %sip
        $(code then.code)
      ::
          %tip
        ?:  =(hint.code %slow)  [| moot]
        =?  rake  =(hint.code %fast)
          =/  kind  (~(got by mind) rail.code)
          ?>  ?=([%fast *] kind)
          ?~  tire.kind  |
          cape.bats.u.tire.kind
        =^  lake  moot  $(code vice.code, rake |)
        =^  rare  moot  $(code then.code)
        [(~(uni ca lake) rare) moot]
      ::
          %elf
        =^  lake  moot  $(code rent.code, rake |)
        =^  rare  moot  $(code walk.code, rake |)
        [(~(uni ca lake) rare) moot]
      ==
    =.  moot  
      %+  ~(jab by moot)  hail
      |=(=toot toot(sake soon))
    $(cork t.cork)
  ::    propagate subject knowledge forward
  ::
  ::  propagate knowledge from where it is known to new callsites,
  ::  possibly discovering new formulas at previously indirect
  ::  callsites.
  ++  loot
    =/  cork  work
    |-  ^-  _this
    ?~  cork  this
    =*  hail  i.cork
    =/  [norm=(unit nomm) soot=sock root=sock rake=cape sire=(unit @hail)]
    [norm soot root rake sire]:(~(got by moot) hail)
    ?>  ?=(^ norm)
    =*  code  u.norm
    =/  soda=(list (each nomm toms))  ~[[%& code] [%| %wot]]
    =/  silt=(list sock)  ~[soot]
    =|  salt=(list sock)
    =/  halt=(list @hail)  ~[hail]
    |-  ^-  _this
    ?~  soda  ^$(cork t.cork)
    ?:  ?=(%& -.i.soda)
      =*  cone  p.i.soda
      ?-  -.cone
          %par
        $(soda [[%& left.cone] [%& rite.cone] [%| %par] t.soda])
      ::
          %not
        ?:  =(0 here.cone)  $(soda t.soda, salt [[| ~] salt])
        ?>  ?=(^ silt)
        =/  sand  (~(pull so i.silt) here.cone)
        ?~  sand  $(soda t.soda, salt [[| ~] salt])
        $(soda t.soda, salt [u.sand salt])
      ::
          %one
        $(soda t.soda, salt [[& moan.cone] salt])
      ::
          %two
        $(soda [[%& cost.cone] [%& corn.cone] [%| %two rail.cone] t.soda])
      ::
          %the
        $(soda [[%& pell.cone] [%| %the] t.soda])
      ::
          %for
        $(soda [[%& mall.cone] [%| %for] t.soda])
      ::
          %ivy
        $(soda [[%& this.cone] [%& that.cone] [%| %ivy] t.soda])
      ::
          %six
        $(soda [[%& what.cone] [%& then.cone] [%& else.cone] [%| %six] t.soda])
      ::
          %eve
        $(soda [[%& once.cone] [%| %eve] [%& then.cone] [%| %vee] t.soda])
      ::
          %ten
        ?:  =(0 here.cone)  $(soda t.soda, salt [[| ~] salt])
        $(soda [[%& twig.cone] [%& tree.cone] [%| %ten here.cone] t.soda])
      ::
          %sip
        $(soda [[%& then.cone] t.soda])
      ::
          %tip
        ?:  =(hint.cone %slow)  :: %slow hint handling: no evaluation, just dynamic calls
          =/  pots=(list nomm)  ~[vice.cone then.cone]
          |-  ^-  _this  :: make sure we have moot entries for the dynamic calls
          ?^  pots
            ?-  -.i.pots
                %par  $(pots [left.i.pots rite.i.pots t.pots])
                %not  $(pots t.pots)
                %one  $(pots t.pots)
                %two
              =?  moot  ?!((~(has by moot) rail.i.pots))
                %+  ~(put by moot)  rail.i.pots
                :*  [| ~]  |
                    ~  ~
                    [| ~]  rake
                    `hail
                ==
              $(pots [cost.i.pots corn.i.pots t.pots])
            ::
                %the  $(pots [pell.i.pots t.pots])
                %for  $(pots [mall.i.pots t.pots])
                %ivy  $(pots [this.i.pots that.i.pots t.pots])
                %six  $(pots [what.i.pots then.i.pots else.i.pots t.pots])
                %eve  $(pots [once.i.pots then.i.pots t.pots])
                %ten  $(pots [twig.i.pots tree.i.pots t.pots])
                %sip  $(pots [then.i.pots t.pots])
                %tip  $(pots [vice.i.pots then.i.pots t.pots])
                %elf  $(pots [rent.i.pots walk.i.pots t.pots])
            ==
          ^$(soda t.soda, salt [[| ~] salt])
        $(soda [[%& vice.cone] [%& then.cone] [%| %tip hint.cone rail.cone] t.soda])
      ::
          %elf
        $(soda [[%& rent.cone] [%& walk.cone] [%| %elf] t.soda])
      ==
    =*  kant  p.i.soda
    ?-  kant
        %par
      ?>  ?=(^ salt)
      ?>  ?=(^ t.salt)
      $(soda t.soda, salt [(~(knit so i.t.salt) i.salt) t.t.salt])
    ::
        %the
      ?>  ?=(^ salt)
      $(soda t.soda, salt [[| ~] t.salt])
    ::
        %for
      ?>  ?=(^ salt)
      $(soda t.soda, salt [[| ~] t.salt])
    ::
        %ivy
      ?>  ?=(^ salt)
      ?>  ?=(^ t.salt)
      $(soda t.soda, salt [[| ~] t.t.salt])
    ::
        %six
      ?>  ?=(^ salt)
      ?>  ?=(^ t.salt)
      ?>  ?=(^ t.t.salt)
      $(soda t.soda, salt [(~(purr so i.t.salt) i.salt) t.t.t.salt])
    ::
        %eve
      ?>  ?=(^ salt)
      $(soda t.soda, salt t.salt, silt [i.salt silt])
    ::
        %vee
      ?>  ?=(^ silt)
      $(soda t.soda, silt t.silt)
    ::
        %elf
      ?>  ?=(^ salt)
      ?>  ?=(^ t.salt)
      $(soda t.soda, salt [[| ~] t.t.salt])
    ::
        %wot
      ?>  ?=(^ halt)
      ?>  ?=(^ salt)
      ?>  ?=(^ silt)
      =.  moot
        (~(jab by moot) i.halt |=(=toot toot(root i.salt)))
      =/  rook  (~(app ca rake) root)
      =/  soap  (~(app ca rake) i.salt)
      ?:  ?&(=(~ t.soda) ?!(=(cape.rook cape.soap)) ?=(^ sire))
        :: stack is empty but we learned more to pass on to our sire
        =/  pate  (~(got by moot) u.sire)
        ?>  ?=(^ norm.pate)
        %=  $
          soda  ~[[%& u.norm.pate] [%| %wot]]
          silt  ~[soot.pate]
          salt  ~
          halt  ~[u.sire]
          root  root.pate
          sire  sire.pate
        ==
      $(soda t.soda, halt t.halt, silt t.silt)
    ::
        [%two *]
      ?>  ?=(^ salt)
      ?>  ?=(^ t.salt)
      =?  moot  ?!((~(has by moot) rail.kant))
        %+  ~(put by moot)  rail.kant
        :*  [| ~]  |
            ~  ~
            [| ~]  |
            `hail
        ==
      =/  [soot=sock sake=cape root=sock form=(unit) noir=(unit nomm) rack=cape]
        [soot sake root form norm rake]:(~(got by moot) rail.kant)
      =/  roan=(unit hone)
        ?:  =(& cape.i.salt) :: equality because a cape can be a cell
          =/  huns  (~(get ja moan) data.i.salt)
          |-  ^-  (unit hone)
          ?~  huns  ~
          ?:  ?&  (~(huge so soot.i.huns) i.t.salt)
                  !(~(big ca cape.root.i.huns) rack)
              ==
            `i.huns
          $(huns t.huns)
        ~
      ?^  roan
        =.  moot :: copy info into moot
          %+  ~(jab by moot)  rail.kant
          |=  =toot
          %=  toot
            soot  i.t.salt
            sake  cape.soot.u.roan
            root  root.u.roan
            rake  cape.root.u.roan
            form  `data.i.salt
            norm  `nomm.norm.u.roan
          ==
        $(soda t.soda, salt [root.u.roan t.t.salt])
      ?.  ?|(?!(=(cape.soot cape.i.t.salt)) ?&(=(& cape.i.salt) =(~ form)))
        $(soda t.soda, salt [root t.t.salt])
      =/  note  ?:(=(& cape.i.salt) `data.i.salt ~)
      =?  mite  ?&(?=(^ note) =(~ form))  (~(put in mite) rail.kant)
      =.  moot
        (~(jab by moot) rail.kant |=(=toot toot(soot i.t.salt, form note)))
      ?~  noir  $(soda t.soda, salt [[| ~] t.t.salt])
      ?.  (~(huge so soot) i.t.salt)  $(soda t.soda, salt [soot t.t.salt]) :: XX shouldn't that be root?
      %=  $
        soda  [[%& u.noir] [%| %wot] t.soda]
        halt  [rail.kant halt]
        salt  t.t.salt
        silt  [i.t.salt silt]
      ==
    ::
        [%ten *]
      ?>  ?=(^ salt)
      ?>  ?=(^ t.salt)
      =/  dawn  (~(darn so i.salt) here.kant i.t.salt)
      ?~  dawn  $(soda t.soda, salt [[| ~] t.t.salt])
      $(soda t.soda, salt [u.dawn t.t.salt])
    ::
        [%tip *]
      ?>  ?=(^ salt)
      ?>  ?=(^ t.salt)
      ?>  ?=(^ halt)
      ?:  =(hint.kant %slow)
        ?>  ?=(^ silt)
        $(soda t.soda, salt [[| ~] t.t.salt], silt t.silt)
      ?:  =(hint.kant %fast)
        ?.  =(& cape.i.t.salt)  ~&  %fast-miss  $(soda t.soda, salt [i.salt t.t.salt])
        =/  pest  (past data.i.t.salt)
        ?~  pest  $(soda t.soda, salt [[| ~] t.t.salt])
        =+  u.pest
        =?  mind  !(~(has by mind) rail.kant)
          (~(put by mind) rail.kant [%fast ~])
        =/  kind  (~(got by mind) rail.kant)
        ?>  ?=([%fast *] kind)
        ?^  tire.kind
          ?>  (~(huge so bats.u.tire.kind) i.salt)
          $(soda t.soda, salt [bats.u.tire.kind t.t.salt])
        =/  boas  (~(pull so i.salt) 2)
        ?~  boas  ~&  fast-fake-b+name  $(soda t.soda, salt [i.salt t.t.salt])
        =/  pork  (~(pull so i.salt) ?~(park 3 u.park))
        ?~  pork  ~&  fast-fake-p+name  $(soda t.soda, salt [i.salt t.t.salt])
        ?.  =(& cape.u.boas)  $(soda t.soda, salt [[| ~] t.t.salt])
        =/  papa=(unit [=path =sock])
          ?~  park  ?:(=(& cape.u.pork) `[~ u.pork] ~)
          =/  bart  (~(pull so u.pork) 2)
          ?~  bart  ~&  fast-fake-pb+name  ~
          ?.  =(& cape.u.bart)  ~
          ?@  data.u.bart  ~&  fast-fake-pba+name  ~
          =/  pats  ~(tap in (~(get ju batt.cole) data.u.bart))
          |-  ^-  (unit [=path =sock])
          ?^  pats
            =/  cure  ~(tap in (~(get ju core.cole) i.pats))
            |-  ^-  (unit [=path =sock])
            ?^  cure
              ?:  (~(huge so i.cure) u.pork)
                `[i.pats i.cure]
              $(cure t.cure)
            ^$(pats t.pats)
          ~&  fast-fake-np+name  ~
        ?~  papa  $(soda t.soda, salt [[| ~] t.t.salt])
        =/  kids  (~(darn so (~(knit so u.boas) [| ~])) ?~(park 3 u.park) sock.u.papa)
        ?>  ?=(^ kids)
        =/  walk  [name path.u.papa]
        =.  core.cole  (~(put ju core.cole) walk u.kids)
        ?@  data.u.boas  ~&  fast-fake-ba+name  $(soda t.soda, salt [[| ~] t.t.salt])
        =.  batt.cole  (~(put ju batt.cole) data.u.boas walk)
        =/  matt
          %-  ~(gas by *(map @ [@hail *]))
          %+  turn  (peel data.u.boas)
          |=  [axe=@ form=*]
          [axe (peg rail.kant axe) form]
        =.  mind  (~(put by mind) rail.kant [%fast `[walk u.kids matt]])
        =.  moot
          %-  ~(gas by moot)
          %+  turn  ~(val by matt)
          |=  [rail=@hail form=*]
          :-  rail
          :*  u.kids  |
              `form  ~
              [| ~]  |  `i.halt
          ==
        =.  mite
          %-  ~(gas in mite)
          %+  turn  ~(val by matt)
          |=([rail=@hail *] rail)
        $(soda t.soda, salt [u.kids t.t.salt])
      $(soda t.soda, salt [i.salt t.t.salt])
    ==
  ::    work discovery
  ::
  ::  - find newly direct call sites, and check if they are recursive
  ::  - add new non-recursive direct callsites to worklist
  ::  - finalize calls which contain no newly direct non-recursive
  ::    callsites
  ::  - add finalized callsites and their call-tree children to moan
  ::    and cole (cold state)
  ++  ruin
    =/  mile=(list @hail)  ~(tap in mite)
    =.  work  ~  :: non-recursive direct calls
    =|  slag=(set @hail) :: excluded as finalization roots
    =|  flux=(set @hail) :: possible finalization roots
    =|  loop=(map @hail @hail) :: recursive call targets
    |-  ^-  _this
    ?^  mile
      =/  mill  i.mile
      =/  [mail=(unit @hail) soot=sock form=(unit) rack=cape]
        [sire soot form rake]:(~(got by moot) mill)
      ?>  ?=(^ form)  :: shouldn't get added to mite unless we know it
      =/  mole  (~(get ja moan) u.form)
      |-  ^-  _this
      ?^  mole
        ?:  ?&  (~(huge so soot.i.mole) soot)
                !(~(big ca cape.root.i.mole) rack)
            ==
          ^$(mile t.mile)
        $(mole t.mole)
      =|  sirs=(list @hail)
      |-  ^-  _this
      ?~  mail
        ?~  sirs :: not actually a call just the entrypoint
          ^^$(mile t.mile, flux (~(put in flux) mill))
        %=  ^^$ :: an un-analyzed indirect call XX direct surely?
          mile  t.mile
          work  [i.mile work]
          slag  (~(gas in slag) [mill sirs])
        ==
      =.  kids  (~(put ju kids) u.mail mill)
      =.  mill  u.mail
      =/  [suit=sock soju=cape firm=(unit) mire=(unit @hail) ruck=cape]
        [soot sake form sire rake]:(~(got by moot) mill)
      ?>  ?=(^ firm)
      ?:  ?&  =(u.form u.firm)
              (~(huge so (~(app ca soju) suit)) soot)
              !(~(big ca ruck) rack)
          ==
        %=  ^^$  :: found a recursive direct call
          mile  t.mile
          slag  (~(gas in slag) sirs)
          flux  (~(put in flux) mill)
          loop  (~(put by loop) i.mile mill)
        ==
      $(sirs [mill sirs], mail mire)
    =.  mite  (~(dif in mite) (~(gas in *(set @hail)) work))
    ::  normalize all the callsites before we finalize any of them
    =/  done  ~(tap in (~(dif in flux) slag))
    =|  enod=(list (list @hail))
    =|  dome=(list @hail)
    |-  ^-  _this
    ?^  done
      ?:  (~(has by loop) i.done)  $(done t.done)
      =/  hood  (~(got by moot) i.done)
      =.  soot.hood  ~(norm so (~(app ca sake.hood) soot.hood))
      =.  root.hood  ~(norm so (~(app ca rake.hood) root.hood))
      =.  moot  (~(put by moot) i.done hood)
      ?>  ?=(^ form.hood)
      ?:  =/  huns  (~(get ja moan) u.form.hood)
          |-  ^-  ?
          ?^  huns
            ?:  ?&  (~(huge so soot.i.huns) soot.hood)
                    !(~(big ca cape.root.i.huns) rake.hood)
                ==
              &
            $(huns t.huns)
          |
        $(done t.done)
      =.  dome  [i.done dome]
      =/  next  ~(tap in (~(get ju kids) i.done))
      ?~  next
        $(done t.done)
      $(done t.done, enod [next enod])
    ?^  enod
      $(done i.enod, enod t.enod)
    |-  ^-  _this
    ?~  dome
      this
    =/  hood  (~(got by moot) i.dome)
    ?>  ?=(^ norm.hood)
    =/  sell  (sale u.norm.hood)
    =.  call.cole  (~(gas ju call.cole) sell)
    =.  back.cole
      (~(gas by back.cole) (turn sell |=([p=[path @] a=[sock *]] [a p])))
    ?>  ?=(^ form.hood)
    =.  moan
      %+  ~(add ja moan)  u.form.hood
      [soot.hood (cook u.norm.hood loop) root.hood]
    $(dome t.dome)
  ::  new entries for cold state
  ++  sale
    |=  norm=nomm
    ^-  (list [[path @] sock *])
    ?-  -.norm
        %par  (weld $(norm left.norm) $(norm rite.norm))
        %not  ~
        %one  ~
        %two  (weld $(norm cost.norm) $(norm corn.norm))
        %the  $(norm pell.norm)
        %for  $(norm mall.norm)
        %ivy  (weld $(norm this.norm) $(norm that.norm))
        %six
      (weld $(norm what.norm) (weld $(norm then.norm) $(norm else.norm)))
    ::
        %eve  (weld $(norm once.norm) $(norm then.norm))
        %ten  (weld $(norm twig.norm) $(norm tree.norm))
        %sip  $(norm then.norm)
        %tip
      ?.  =(%fast hint.norm)  (weld $(norm vice.norm) $(norm then.norm))
      =/  =hind  (~(got by mind) rail.norm)
      ?~  hind  (weld $(norm vice.norm) $(norm then.norm))
      ?~  tire.hind  (weld $(norm vice.norm) $(norm then.norm))
      =*  tine  u.tire.hind
      =|  kale=(list [[path @] sock *])
      =|  calm=(map @ [=cape form=*])
      =/  tack=(list @)  ~[1]
      |-  ^-  (list [[path @] sock *])
      ?^  tack
        =/  mart  (~(get by matt.tine) i.tack)
        ?^  mart
          =/  =toot  (~(got by moot) -.u.mart)
          ?>  =(bats.tine soot.toot)
          %=  $
              calm  (~(put by calm) i.tack [sake.toot +.u.mart])
              kale
            :_  kale
            :-  [cone.tine i.tack]
            [~(norm so (~(app ca sake.toot) bats.tine)) +.u.mart]
          ::
              tack  t.tack
          ==
        =/  clam  (~(get by calm) (peg i.tack 2))
        =/  cram  (~(get by calm) (peg i.tack 3))
        ?:  ?&(?=(^ clam) ?=(^ cram))
          =/  sake  (~(uni ca cape.u.clam) cape.u.cram)
          =/  form  [form.u.clam form.u.cram]
          %=  $
              calm  (~(put by calm) i.tack sake form)
              kale
            :_  kale
            [[cone.tine i.tack] ~(norm so (~(app ca sake) bats.tine)) form]
          ::
              tack  t.tack 
          ==
        $(tack [(peg 2 i.tack) (peg 3 i.tack) tack])
      (weld kale (weld ^$(norm vice.norm) ^$(norm then.norm)))
    ::
        %elf  (weld $(norm rent.norm) $(norm walk.norm))
    ==
  ::  pick out food for nomm
  ++  cook
    |=  [norm=nomm pool=(map @hail @hail)]
    ^-  food
    =|  ices=(map @hail [=sock form=*])
    =|  leap=(set [=sock form=*])
    =/  fore=(list nomm)  ~[norm]
    |-  ^-  food
    ?^  fore
      ?-  -.i.fore
          %par  $(fore [rite.i.fore left.i.fore t.fore])
          %not  $(fore t.fore)
          %one  $(fore t.fore)
          %two
        =/  roil  (~(gut by pool) rail.i.fore rail.i.fore)
        =/  foot  (~(get by moot) roil)
        ?>  ?=(^ foot)
        ~?  ?=(~ form.u.foot)  indirect+rail.i.fore
        =?  ices  ?=(^ form.u.foot)
          %+  ~(put by ices)  rail.i.fore
          [soot u.form]:u.foot
        =?  leap  ?&((~(has by pool) rail.i.fore) ?=(^ form.u.foot))
        %-  ~(put in leap)  [soot u.form]:u.foot
        $(fore [corn.i.fore cost.i.fore t.fore])
      ::
          %the  $(fore [pell.i.fore t.fore])
          %for  $(fore [mall.i.fore t.fore])
          %ivy  $(fore [this.i.fore that.i.fore t.fore])
          %six  $(fore [what.i.fore then.i.fore else.i.fore t.fore])
          %eve  $(fore [once.i.fore then.i.fore t.fore])
          %ten  $(fore [twig.i.fore tree.i.fore t.fore])
          %sip  $(fore [then.i.fore t.fore])
          %tip  $(fore [vice.i.fore then.i.fore t.fore])
          %elf  $(fore [rent.i.fore walk.i.fore t.fore])
      ==
    [norm ices leap]
  --
::
::   parse fast hint
++  past
  |=  a=*
  ^-  (unit [name=term park=(unit @) hock=(list [term @])])
  ?.  ?=([* [@ @] *] a)  ~&  [%fast-isnt a]  ~
  =/  nume  (bait -.a)
  ?~  nume  ~&  [%fast-isnt a]  ~
  =/  huck  +>.a
  =|  hock=(list [term @])
  |-  ^-  (unit [name=term park=(unit @) hock=(list [term @])])
  ?^  huck
    ?.  ?&(?=([@ @] -.huck) ((sane %ta) -<.huck))  ~&  [%fast-isnt a]  ~
    $(hock [-.huck hock], huck +.huck)
  ?.  =(~ huck)  ~&  [%fast-isnt a]  ~
  ?:  =(0 +<-.a)  `[u.nume `+<+.a (flop hock)]
  ?:  =([1 0] +<.a)  `[u.nume ~ (flop hock)]
  ~&  [%fast-isnt a]  ~
::
::   battery members
::
:: we can discover all possible formulas in a battery by observing that
:: a pair of two valid formulas is itself a valid formula. Therefore we
:: treat the whole battery as a formula, but recursively decompose it
:: along autocons. This allows us to analyze all possible batteries in a
:: formula to enter them into the cold state
++  peel
  |=  f=*
  ^-  (list [axe=@ form=*])
  =/  tack=(list [@ *])  [1 f]~
  =|  salt=(list [axe=@ form=*])
  |-  ^-  (list [axe=@ form=*])
  ?^  tack
    ?:  ?=([^ *] +.i.tack)
      $(tack [[(peg -.i.tack 2) +<.i.tack] [(peg -.i.tack 3) +>.i.tack] t.tack])
    $(tack t.tack, salt [i.tack salt])
  salt
::
::    parse $chum 
::
::  paths are composed of terms but fast hints label cores with $chum,
::  so we need to translate the cell case of $chum into a concatenated
::  term
++  bait
  |=  a=*
  ^-  (unit term)
  ?@  a  ?.(((sane %tas) a) ~ ``@tas`a)
  ?.  ?=([@ @] a)  ~
  ?.  ((sane %tas) -.a)  ~
  `(crip (scag 32 (weld (trip -.a) (a-co:co +.a))))
--
