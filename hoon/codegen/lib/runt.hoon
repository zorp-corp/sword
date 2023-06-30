/-  *noir
/+  sack
|%
++  no
  |_  moan=(jar * hone)
  ++  run
    |=  [nous=* =food]
    ^-  [(unit) _this]
    =|  salt  (list *)
    =/  book=(list *)  ~[nous]
    =/  toll=(list (each nomm toms))  ~[[%& norm] [%| %wot]]
    =/  icey=(list (map @hail [=sock form=*]))  ~[ices.food]
    |-  ^-  [* this]
    ?^  toll
      ?:  -.i.toll
        =*  code  p.i.toll
        ?-  -.code
            %par
          $(toll [[%& left.code] [%& rite.code] [%| %par] t.toll])
        ::
            %not
          ?:  =(0 here.code)  [~ this]
          ?>  ?=(^ book)
          =/  mous  i.book
          |-  ^-  [(unit) _this]
          ?:  =(1 here.code)
            ^$(toll t.toll, salt [mous salt])
          ?@  mous  ([~ this])
          ?-  (cap here.code)
            %2  $(here.code (mas here.code), mous -.mous)
            %3  $(here.code (mas here.code), mous +.mous)
          ==
        ::
            %one
          $(toll t.toll, salt [moan.code salt])
        ::
            %two
          $(toll [[%& cost.code] [%& corn.code] [%| %two hail.code] t.toll]) 
        ::
            %the
          $(toll [[%& pell.code] [%| %the] t.toll])
        ::
            %for
          $(toll [[%& mall.code] [%| %for] t.toll])
        ::
            %ivy
          $(toll [[%& this.code] [%& that.code] [%| %ivy] t.toll])
        ::
            %six
          $(toll [[%& what.code] [%| %six] [%& then.code] [%& else.code] t.toll])
        ::
            %eve
          $(toll [[%& once.code] [%| %eve] [%& then.code] [%| %vee] t.toll])
        ::
            %ten
          ?:  =(0 here.code
          $(toll [[%& twig.code] [%& tree.code] [%| %ten here.code] t.toll])
        ::
            %sip
          $(toll [[%& then.code] t.toll])
        ::
           %tip
          $(toll [[%& vice.code] [%| %tip] [%& then.code] t.toll])
        ::
           %elf
          $(toll [[%& rent.code] [%& walk.code] [%| elf] t.toll])
        ==
      =*  rest  p.i.toll
      ?-  rest
          %par
        ?>  ?=(^ salt)
        ?>  ?=(^ t.salt)
        $(toll t.toll, salt [[i.t.salt i.salt] t.salt])
      ::
          %wot
        ?>  ?=(^ icey)
        ?>  ?=(^ book)
        $(toll t.toll, icey t.icey, book t.book)
          %the
        ?>  ?=(^ salt)
        $(toll t.toll, salt [?=(^ i.salt) t.salt])
      ::
          %for
        ?>  ?=(^ salt)
        ?^  i.salt  [~ this]
        $(toll t.toll, salt [.+(i.salt) t.salt])
      ::
          %ivy
        ?>  ?=(^ salt)
        ?>  ?=(^ t.salt)
        $(toll t.toll, salt [=(i.t.salt i.salt) t.salt])
      ::
          %six
        ?>  ?=(^ salt)
        ?>  ?=(^ t.toll)
        ?>  ?=(^ t.t.toll)
        ?:  i.salt
          $(salt t.salt, toll [i.t.toll t.t.t.toll])
        $(salt t.salt, toll [i.t.t.toll, t.t.t.toll])
      ::
          %eve
        ?>  ?=(^ salt)
        $(toll t.toll, salt t.salt, book [i.salt book])
      ::
          %vee
        ?>  ?=(^ book)
        $(toll t.toll, book t.book)
      ::
          %elf
        [~ this]
      ::
          [%two *]
        ?>  ?=(^ icey)
        ?>  ?=(^ salt)
        ?>  ?=(^ t.salt)
        =/  iced  (~(get by t.icey) hail.rest)
        ?^  iced
          =/  huns  (~(get ja moan) form.u.iced)
          |-  ^-  [(unit) _this]
          ?>  ?=(^ huns)
          ?.  (~(huge so soot.i.huns) sock.u.iced)  $(huns t.huns)
          %=  $
            toll  [[%& nomm.norm.i.huns] [%| %wot] t.toll]
            book  [i.t.salt book]
            icey  [ices.norm.i.huns icey]
            salt  t.t.salt
          ==
        =.  moan  (~(rout sack moan) [%.y i.t.salt] i.salt)
        =/  huns  (~(get ja moan) i.salt)
        |-  ^-  [(unit) _this]
        ?>  ?=(^ huns)
        ?.  (~(huge so soot.i.huns) sock.u.iced)  $(huns t.huns)
        %=  $
            toll  [[%& nomm.norm.i.huns] [%| %wot] t.toll]
            book  [i.t.salt book]
            icey  [ices.norm.i.huns icey]
            salt  t.t.salt
        ==
      ::
          [%ten *]
        ?>  ?=(^ salt)
        ?>  ?=(^ t.salt)
        =*  tree  i.salt
        =|  tack  (list  [p=$?(%2 %3) q=*])
        |-  ^-  [(unit) _this]
        ?.  =(1 here.rest)
          ?@  tree  [~ this]
          ?-  (cap here.rest)
            %2  $(here.rest (mas here.rest), tack [%2 +.tree], tree -.tree)
            %3  $(here.rest (mas here.rest), tack [%3 -.tree], tree +.tree)
          ==
        =*  twig  i.t.salt
        |-  ^-  [(unit) _this]
        ?^  tack
          ?-  p.i.tack
            %2  $(tack t.tack, twig [twig q.i.tack])
            %3  $(tack t.tack, twig [q.i.tack twig])
          ==
        $(toll t.toll, salt [twig t.t.salt])
      ::
          [%tip *]
        $(toll t.toll, salt t.salt) :: XX for now we just ignore hints
      ==
    ?>  ?=(^ salt)
    ?>  =(~ t.salt)
    [i.salt this]
  ++  this  .
  --
--
