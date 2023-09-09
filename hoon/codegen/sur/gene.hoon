/-  noir
|%
+|  %lin
::  external label
+$  bell  [text=sock:noir form=*]
::  internal label
+$  bile  [%bile axe=@ tis=@ thus=@tas bell]
::  ssa shape of a noun
+$  need
  $%  [%this sass=@stir]
      [%both left=need rite=need]
      [%none ~]
  ==
+$  next  $>(%next goal)
::  destination
+$  goal
  $%  [%pick zero=bile once=bile]
      [%done ~]
      [%next what=need then=bile]
  ==
::  instructions in a block
+$  pole
  $%  [%imm * @stir]            :: Write a noun to an SSA value
      [%mov @stir @stir]        :: Copy an SSA value
      [%phi (list @stir) @stir] :: Choose whichever SSA value is defined
      [%inc @stir @stir]        :: Define second SSA register as increment of first
      [%con @stir @stir @stir]  :: Construct a cell, first SSA head, second SSA tail, third SSA result
      [%hed @stir @stir]        :: Take the head of first SSA and place in second.
                                ::  Undefined if first SSA not a cell
      [%tal @stir @stir]        :: Take tail head of first SSA and place in second.
                                ::  Undefined if first SSA not a cell
      [%men @stir]              :: Push onto the mean stack
      [%man @stir]              :: Pop from the mean stack
      [%hit @stir]              :: Profiling hit counter
      [%slg @stir]              :: Debugging print
      [%mew @stir @stir @stir @stir]  :: Cache write - cache key - subject - formula - result
      [%tim ~]                  :: Start timer
      [%tom ~]                  :: Stop timer 
      [%mem ~]                  :: Print memory usage
  ==
+$  pool
  $%  [%hed @stir]
      [%tal @stir]
  ==
::  instructions ending a block
+$  site
  $%  [%clq @stir bile bile]                    :: Branch left if the SSA value is a cell, right otherwise
      [%eqq @stir @stir bile bile]              :: Branch left if SSA registers are equal, right otherwise
      [%brn @stir bile bile bile]               :: Branch 1st - not loobean, 2nd - 0, 3rd - 1
      [%hop bile]                               :: Go to bile unconditionally (local direct jump)
      [%lnk @stir @stir @stir bile]             :: Call formula in first SSA register with subject in second,
                                                ::   result in third, return to bile
      [%cal bell @stir (list @stir) @stir bile] :: Call arm given by bell, 
                                                ::   subject/formula pair in register
                                                ::   subject in SSA register list,
                                                ::   result to register, return to bile
      [%lnt @stir @stir]                        :: Jump to formula in first SSA register with subject in second
      [%jmp bell @stir (list @stir)]            :: Jump to the code at the label in tail position,
                                                ::   subject/formula pair in SSA register,
                                                ::   subject in register list
      [%spy @stir @stir @stir bile]             :: Scry with the ref/path pair in the first 2 SSA registers
                                                ::   define the third as the result
      [%mer @stir @stir @stir bile bile]        :: Cache read: key - subject - formula - hit - miss
      [%don @stir]                              :: Finish the procedure, returning the value in the SSA
      [%pun ~]                                  :: Punt to tree-walking nock, with a saved mean stack, subject, and formula
      [%bom ~]                                  :: Crash immediately without punting
  ==
::  basic block
+$  blob  [body=(list pole) bend=site]
::  compilation unit
+$  pile
  $:  long=bile  :: starting label for direct calls
      want=need  :: input registers for direct calls
      wish=bile  :: starting label for indirect calls
      sire=@stir :: input register for indirect calls
      will=(map bile blob)
      sans=@stir ::  next SSA register
  ==
::  code table
+$  hill  (map bell pile)
--
