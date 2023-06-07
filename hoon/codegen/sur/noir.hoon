/-  *sock
|%
::    mask
::
::  describes which axes of a noun are known
::  but does not include the noun
+$  cape  $@(? [cape cape])
::      sock
::    describes knowledge of a noun
+$  sock  [=cape data=*]
::    call table entry
::
::  soot: subject knowledge
::  sake: subject battery mask
::  form: formula if known
::  root: result knowledge
::  rake: result battery mask
::  sire: @hail for call to caller, if there is one
+$  hoot
  $:  soot=sock  sake=cape
      form=(unit *)  norm=(unit nomm)
      root=sock rake=cape
      sire=(unit @hail)
  ==
::    Nomm (Nock--)
::
::  9 is rewritten to 7+2
::  8 is rewritten to 7+autocons+0
+$  nomm
  $~  [%not 0]
  $%  [%par left=nomm rite=nomm]            :: autocons
      [%not here=@]                         :: Nock 0
      [%one moan=*]                         :: Nock 1
      [%two cost=nomm corn=nomm rail=@hail] :: Nock 2 - done
      [%the pell=nomm]                      :: Nock 3
      [%for mall=nomm]                      :: Nock 4
      [%ivy this=nomm that=nomm]            :: Nock 5
      [%six what=nomm then=nomm else=nomm]  :: Nock 6
      [%eve once=nomm then=nomm]            :: Nock 7
      [%ten here=@ twig=nomm tree=nomm]     :: Nock 10
      [%sip hint=@ then=nomm]               :: Nock 11 (static)
      [%tip hint=@ vice=nomm then=nomm]     :: Nock 11 (dynamic)
      [%elf rent=nomm walk=nomm]            :: "Nock 12"
  ==
--
