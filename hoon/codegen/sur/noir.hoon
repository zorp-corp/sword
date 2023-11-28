|%
::    in-progress call table entry
::
::  soot: subject knowledge
::  sake: subject battery mask
::  form: formula if known
::  root: result knowledge
::  rake: result battery mask
::  sire: @hail for call to caller, if there is one
+$  toot
  $:  soot=sock  sake=cape
      form=(unit *)  norm=(unit nomm)
      root=sock  rake=cape
      sire=(unit @hail)
  ==
::    cold state
::
::   core: nested batteries by path
::   batt: paths by outer batteries
::   call: arms to exact call label
::   back: path/axis labels by bell
+$  cool
  $:  core=(jug path sock)         
      batt=(jug ^ path)            
      call=(jug [path @] [sock *]) 
      back=(map [sock *] [path @]) 
  ==
::    hint table entry
::
::  stored information about a hint
+$  hind
  $@  ~
  [%fast tire=(unit [cone=path bats=sock matt=(map @ [@hail *])])]
::    call table entry
::
::  soot: known subject for the call
::  norm: nomm and decoration (see $food)
::  root: known output of the call
+$  hone  [soot=sock norm=food root=sock]
::    Nomm (Nock--)
::
::  9 is rewritten to 7+2 [9 b c] -> [7 c 2 [0 1] 0 c]
::  8 is rewritten to 7+autocons+0
+$  nomm
  $%  [%par left=nomm rite=nomm]                   :: autocons
      [%one moan=*]                                :: Nock 1
      [%two cost=nomm corn=nomm rail=@hail]        :: Nock 2 - done
      [%the pell=nomm]                             :: Nock 3
      [%for mall=nomm]                             :: Nock 4
      [%ivy this=nomm that=nomm]                   :: Nock 5
      [%six what=nomm then=nomm else=nomm]         :: Nock 6
      [%eve once=nomm then=nomm]                   :: Nock 7
      [%ten here=@ twig=nomm tree=nomm]            :: Nock 10
      [%sip hint=@ then=nomm]                      :: Nock 11 (static)
      [%tip hint=@ vice=nomm then=nomm rail=@hail] :: Nock 11 (dynamic)
      [%elf rent=nomm walk=nomm]                   :: "Nock 12"
      [%not here=@]                                :: Nock 0
  ==
::    Stack computation marker
::
::  used to describe remaining work in tail-recursive work-stack
::  algorithms over $nomm
+$  toms
  $@  $?(%par %wot %the %for %ivy %six %eve %vee %elf)
  $%  [%two rail=@hail]
      [%ten here=@]
      [%tip hint=@ rail=@hail]
  ==
::    call site data
::
::  nomm: lowered nock for called formula
::  ices: labels for direct calls
::  loop: set of direct calls which are recursive
+$  food
  [=nomm ices=(map @hail [=sock form=*]) loop=(set [=sock form=*])]
--
