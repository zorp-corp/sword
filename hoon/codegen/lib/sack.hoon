/-  *noir
/+  *soak
::    state
::  moot -> working state for each call
::  moan -> finalized calls
::  mite -> calls not yet analyzed
|_  moan=(jar * hone)
::  analyze a subject and formula
++  rout
  |=  [soot=sock form=*]
  ^-  _moan
  =|  moot=(map @hail hoot)
  =|  mite=(set @hail)
  =.  moot
    %+  ~(put by moot)  1
    :*  soot  |
      `form  ~
      [| ~]  |  ~
    ==
  =/  work=(list @hail)  ~[1]
  =/  cork  work
  |^  ^-  _moan  :: driver loop
    =>  raid
    =>  espy
    =>  loot  
    =>  ruin
    ?~  work  moan
    $
  ::  driver core
  ++  this  .
  ::    turn a formula into nomm
  ++  raid
    =/  cork  work
    |-  hail=@hail
    ^-  _this
    ?~  cork  this
    =/  hail  i.cork
    =/  firm  form:(~(got by moot.seam) hail)
    ?>  ?=(^ firm)
    =/  form  u.firm
    =/  code
      |-  ^-  nomm
      ?+  form  [%not 0]
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
            $(form this.form, hail (peg hail 6))]
            $(form that.form, hail (peg hail 7))]
        ==
      ::
          [%6 what=* then=* else=*]
        :*  %six
            $(form what.form, hail (peg hail 6))]
            $(form then.form, hail (peg hail 14))]
            $(form else.form, hail (peg hail 15))]
        ==
      ::
          [%7 once=* then=*]
        :*  %eve
            $(form once.form, hailflank (peg hail 6))]
            $(form then.form, hail (peg hail 7))]
        ==
      ::
          [%8 pint=* then=*]
        $(form [%7 [pint.form %0 1] then.form])
      ::
          [%9 here=@ coil=*]
        $(form [%7 coil %2 [%0 1] %0 here])
      ::
          [%10 [here=@ twig=*] tree=*]
        :*  %ten
            here
            $(form twig.form, hail (peg hail 13))
            $(form tree.form, hail (peg hail 7))
        ==
      ::
          [%11 hint=@ then=*]
        [%sip hint $(form then.form, hail (peg hail 7))]
      ::
          [%11 [hint=* vice=*] then=nomm]
        :*  %tip
            hint
            $(form vice.form, hail (peg hail 13))
            $(form then.form, hail (peg hail 7))
        ==
      ::
          [%12 rent=* walk=*]
        :*  %elf
            $(form rent.form, hail (peg hail 6))
            $(form walk.form, hail (peg hail 7))
        ==
      ==
    %=  $
        moot
      %+  ~(jab by moot)  hail
      |=  =hoot
      hoot(norm.hoot `code)
    ::
        cork  t.cork
    ==
  ::  learn/backpropagate battery masks
  ++  espy
    =/  cork  work
    |-  ^-  _this
    ?~  cork  this
    =/  hail  i.cork
    =/  [norm=nomm rake=cape]
      [norm rake]:(~(got by moot) hail)
    ?>  ?=(^ norm)
    =/  code  u.norm
    =^  soon  moot
      |-  ^-  [cape _moot]
      ?-  -.code
          %par
        =/  [lack=cape rack=cape]  ~(rip ca rake)
        =^  lead  moot  $(code left.code, rake lack)
        =^  reed  moot  $(code rite.code, rake rack)
        :_  moot
        (~(uni ca lead) reed)
      ::
          %not
        ?:  ?=(0 here.code)  [[| |] this]
        :_  moot
        (~(pat ca rake) here.code)
      ::
          %one  [[| |] moot]
          %two
        =?  moot  ?!((~(has by moot) rail.code))
          %+  ~(put by moot)  rail.code
          :*  [| ~]  |
              ~  ~
              [| ~]  rake
              `hail
          ==
        =/  [soot=sock fake=cape form=(unit) norm=(unit nomm)]
          [soot rake firm norm]:(~(got by moot) rail.code)
        =/  mole=(list hone)  ?~(firm ~ (~(get ja moan) firm))
        |-  ^-  [cape _this]
        ?^  mole
          ?:  (~(huge so soot.i.mole) soot)  [cape.soot.i.mole this]
          $(mole t.mole)
        =?  moot  ?&(?=(^ norm) (~(big ca fake) rake))
          =.  moot
            %+  ~(jab by moot)  rail.code
            |=  =hoot
            hoot(rake rake)
          moot:(espy rail.code)
        =/  lake  sake:(~(got by moot) rail.code)
        =^  sake  moot  ^$(rake lake, code cost.code)
        =^  folk  moot  ^$(rake |, code corn.code)
        :_  moot
        (~(uni ca sake) folk)
      ::
          %the
        $(code pell.code, rake ~(any ca rake))  :: XX rake should just be %|
      ::
          %for
        $(code mall.code, rake ~(any ca rake))  :: XX rake should just be %|
      ::
          %ivy
        =^  lake  moot  $(code this.code, rake ~(any ca rake))
        =^  rare  moot  $(code that.code, rake ~(any ca rake))
        :_  moot
        (~(uni ca lake) rare)
      ::
          %six
        =^  cake  moot  $(code what.code, rake |)
        =^  lake  moot  $(code then.code)
        =^  rare  moot  $(code else.code)
        :_  moot
        (~(uni ca lake) rare)
      ::
          %eve
        =^  rare=cape  moot  $(code then.code)
        $(code once.code, rake rare)
      ::
          %ten
        =/  [wipe=cape wine=cape]
          ?:  =(0 here.ten)  [| |]
          (~(awl ca rake) here.code)
        =^  lake  moot  $(code twig.code, rake wipe)
        =^  rare  moot  $(code tree.code, rake wine)
        :_  moot
        (~(uni ca lake) rare)
      ::
          %sip
        $(code then.code)
      ::
          %tip
        =^  lake  moot  $(code vice.code, rake |)
        =^  rare  moot  $(code then.code)
        :_  moot
        (~(uni ca lake) rare)
      ::
          %elf
        =^  lake  moot  $(code rent.code, rake |)
        =^  rare  moot  $(code walk.code, rake |)
        :_  moot
        :-  (~(uni ca lake) rare)
        (~(uni ca lash) rash)
      ==
    %=  $
        moot
      %+  ~(jab by moot)  hail
      |=  =hoot
      hoot(sake soon)
    ::
      cork  t.cork
    ==
  ::  propagate subject knowledge forward
  ++  loot
    =/  cork  work
    |-  ^-  _seam
    ?~  cork  this
    =/  hail  i.cork
    =/  [norm=nomm soot=sock root=sock sire=(unit @hail)]
    [norm soot root sire]:(~(got by moot.seam) hail)
    ?>  ?=(^ norm)
    =/  code  u.norm
    =|  soda  (list (each nomm toms)) ~[[%& code] [%| %wot &]]
    =/  silt=(list sock)  ~[soot]
    =|  salt  (list sock)
    =/  halt=(list @hail)  ~[hail]
    |-  ^-  [sock _seam]
    ?^  soda
      ?:  -.i.soda
        =/  code  p.i.soda
        ?-  -.code
            %par
          $(soda [[%& left.p.i.soda] [%& rite.p.i.soda] [%| %par] t.soda])
        ::
            %not
          ?:  =(0 here.code)  $(soda t.soda, salt [[| ~] salt])
          ?>  ?=(^ silt)
          =/  sand  (~(pull so i.silt) here.code)
          ?~  sand  $(soda t.soda, salt [[| ~] salt])
          $(soda t.soda, salt [u.sand salt])
        ::
            %one
          $(soda t.soda, salt [[& moan.code] salt])
        ::
            %two
          $(soda [[%& cost.code] [%& corn.code] [%| [%two rail.code] t.soda])
        ::
            %the
          $(soda [[%& pell.code] [%| %the] t.soda])
        ::
            %for
          $(soda [[%& mall.code] [%| %for] t.soda])
        ::
            %ivy
          $(soda [[%& this.code] [%& that.code] [%| %ivy] t.soda)
        ::
            %six
          $(soda [[%& what.code] [%& then.code] [%& else.code] [%| six] t.soda])
        ::
            %eve
          $(soda [[%& once.code] [%| %eve] [%& then.code] [%| %vee] t.soda])
        ::
            %ten
          ?:  =(0 here.code)  $(soda t.soda, salt [[| ~] salt])
          $(soda [[%& twig.code] [%& tree.code] [%| [%ten here.code] t.soda])
        ::
            %sip
          $(soda [[%& then.code] t.soda])
        ::
            %tip
          $(soda [[%& vice.code] [%& then.code] [%| %tip hint.code] t.soda])
        ::
            %elf
          $(soda [[%& rent.code] [%& walk.code] [%| %elf]
        ==
      ?-  p.i.soda
          %par
        ?>  ?=(^ salt)
        ?>  ?=(^ t.salt)
        $(soda t.soda, salt [(~(knit so i.t.salt) t.salt) t.t.salt])
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
        =/  rook  (~(app ca rake) root)
        =/  soap  (~(app ca rake) i.salt)
        =.  moot
          (~(jab by moot) i.halt  |=(=hoot hoot(soot rock i.salt)))
        ?:  ?&(=(~ t.soda)  ?!(=(cape.rook cape.soap)) ?=(^ sire))
          =/  pate  (~(got by moot) u.sire)
          %=  $
            soda ~[norm.pate]
            silt ~[soot.pate]
            salt ~
            halt ~[u.sire]
            root root.pate
            sire sire.pate
        $(soda t.soda, halt t.halt)
      ::
          [%two *]
        ?>  ?=(^ salt)
        ?>  ?=(^ t.salt)
        =/  roan=(unit sock)
          ?:  =(& cape.i.salt) :: equality because cape can be a cell
            =/  huns  (~(get ja moan.seam) data.i.salt)
            |-  ?~  huns  ~
            ?:  (~(huge so soot.i.huns) coot) `root.i.huns)
            $(huns t.huns)
          ~
        ?^  roan  $(soda t.soda, salt [u.roan t.t.salt])
        =/  [soot=sock sake=cape root=sock form=(unit) noir=(unit nomm)]
          [soot sake root form norm]:(~(got by moot) rail.p.i.salt)
        ?.  ?|(?!(=(cape.soot cape.p.i.t.salt)) ?&(=(& cape.p.i.salt) =(~ form)))
          $(soda t.soda, salt [root t.t.salt])
        =/  note  ?:(=(& cape.p.i.salt) `data.p.i.salt ~)
        =?  mite  ?&(?=(^ note) ?=(~ form))  (~(put in mite.seam) rail.p.i.soda)
        =.  moot
          (~(jab by moot) rail.p.i.soda |=(=hoot hoot(soot coot, form note)))
        ?~  noir  $(soda t.soda, salt [[| ~] t.t.salt])
        %=  $
          soda [[%& noir] [%| wot |] t.soda]
          halt [rail.p.i.salt halt]
          salt t.t.salt
        ==
      ::
          [%ten *]
        ?>  ?=(^ salt)
        ?>  ?=(^ t.salt)
        $(soda t.soda, salt [(~(darn so i.t.salt) i.salt) t.t.salt])
      ::
          [%tip *]
        ?>  ?=(^ salt)
        ?>  ?=(^ t.salt)
        $(soda t.soda, salt [i.salt t.t.salt]
      ==
    $(cork t.cork)
  ::  Recursion detection
  ::    when checking if a call is recursive, repeatedly check if
  ::    subject-formula pair matches sire, so long as sire can be found.
  ::    If a match is found, we have a recursion point. Add all discovered
  ::    sires to exclusion set
  ::
  ::    If a call isnt' recursive and isn't yet evaluated, add all sires
  ::    to the root to exclusion set.
  ::
  ::    If a call has a non-nil battery mask: add it to the exclusion set
  ::
  ::    Any call variables not in the exclusion set can be finalized which
  ::    places them and their callees in moan
  ::
  ++  ruin
    ^-  [(list @hail) _seam]
    =/  mile  ~(tap in mite)
    =.  work  *(list @hail) :: non-recursive direct calls
    =|  slag=(set @hail) :: excluded as finalization roots
    =|  flux=(set @hail) :: possible finalization roots
    =|  kids=(jug @hail @hail) :: immediate callees
    =|  loop=(map @hail @hail) :: recursive call targets
    |-  ^-  _this
    ?^  mile
      =/  mill  i.mile
      =/  [mail=(unit hail) soot=sock form=(unit)]
        [sire soot form]:(~(got by moot) mill)
      =/  mole  (~(get ja moan) form)
      |-  ^-  [(list @hail) _this]
      ?^  mole
        ?:  (~(huge so soot.i.mole) soot)  ^$(mile t.mile)
        $(mole t.mole)
      =|  sirs=(list @hail)
      |-  ^-  [(list @hail) _this]
      ?~  mail
        ^$(mile t.mile, work [i.mile work], slag (~(gas in slag) [mill sirs])
      =.  kids  (~(put ju kids) u.mail mill)
      =.  mill  u.mail
      =^  [suit=sock soju=cape firm=*]  mail
        [[soot sake form] sire]:(~(got by moot) mill)
      ?:  ?&(=(form firm) (~(huge so (~(app ca soju) suit)) soot))
        %=  ^$  :: found a recursive direct call
          mile  t.mile
          slag  (~(gas in slag) sirs)
          flux  (~(put in flux) mill)
          loop  (~(put by loop) i.mile mill)
        ==
      $(sirs [mill sirs])
    =.  mite  (~(dif in mite) (~(gas in *(set @hail)) work)
    =/  done  [~(tap in (~(dif in flux) slag)) ~]
    =|  enod  (list (list @hail))
    |-  ^-  [(list @hail) _this]
    ?~  done
      ?~  enod  this
      $(done i.enod, enod t.enod)
    =/  hood  (~(got by moot) i.done)
    ?^  rake.hood  $(done t.done) :: skip because cell output mask
    ?:  rake.hood  $(done t.done) :: skip because & output mask
    ?:  (~(has by loop) i.done) $(done t.done)   :: skip because recursive
    ::  this is OK because we disallow finalizing anything with battery
    ::  dependencies on its output
    =.  soot.hood  (~(app ca sake.hood) soot.hood)
    =.  moot  (~(put by moot) i.done hood)
    ?>  ?=(^ form.hood)
    ?>  ?=(^ norm.hood)
    =.  moan  
      %+  ~(put ja moan) u.form.hood
      [soot.hood (cook u.norm.hood pool) root.hood]
    =/  next  ~(tap in (~(get ju kids) i.done))
    ?~  next
      $(done t.done)
    $(done t.done, enod [next enod])
  ::  pick out food to nomm
  ++  cook
    |=  [norm=nomm pool=(map @hail @hail)]
    ^-  food
    =|  ices  (map @hail [=sock form=*])
    =/  fore=(list nomm)  ~[norm]
    |-  ^-  food
    ?~  fore  [norm ices]
    ?-  -.i.fore
        %par
      $(fore [rite.i.fore left.i.fore t.fore])
    ::
        %not
      $(fore t.fore)
    ::
        %one
      $(fore t.fore)
    ::
        %two
      =/  roil  (~(gut by pool) rail.i.norm rail.i.norm) 
      =/  =hoot  (~(get by moot) roil)
      ?~  hoot
        $(fore [corn.i.fore cost.i.fore t.fore])
      ?~  form.u.hoot
        $(fore [corn.i.fore cost.i.fore t.fore])
      =.  ices  
        %+  ~(put by ices) rail.i.norm
        [soot u.form]:hoot
      $(fore [corn.i.fore cost.i.fore t.fore])
    ::
        %the
      $(fore [pell.i.fore t.fore])
    ::
        %for
      $(fore [mall.i.fore t.fore])
    ::
        %ivy
      $(fore [this.i.fore that.i.fore t.fore])
    ::
        %six
      $(fore [what.i.fore then.i.fore else.i.fore t.fore])
    ::
        %eve
      $(fore [once.i.fore then.i.fore t.fore])
    ::
        %ten
      $(fore [twig.i.fore tree.i.fore t.fore])
    ::
        %sip
      $(fore [then.i.fore t.fore])
    ::
        %tip
      $(fore [vice.i.fore then.i.fore t.fore])
    ::
        %elf
      $(fore [rent.i.fore walk.i.fore t.fore])
    ==
  --
--
