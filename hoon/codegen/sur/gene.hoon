/-  noir
|%
+|  %lin
::  external label
+$  bell  [text=sock:noir form=*]
::  internal label
+$  bile  [%bile axe=@ tis=@ thus=@tas bell]
::  ssa shape of a noun
+$  need
  $%  [%this sass=@uvre]
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
  $%  [%imm * @uvre]            :: Write a noun to an SSA value
      [%mov @uvre @uvre]        :: Copy an SSA value
      [%phi (list @uvre) @uvre] :: Choose whichever SSA value is defined
      [%inc @uvre @uvre]        :: Define second SSA register as increment of first
      [%con @uvre @uvre @uvre]  :: Construct a cell, first SSA head, second SSA tail, third SSA result
      [%hed @uvre @uvre]        :: Take the head of first SSA and place in second.
                                ::  Undefined if first SSA not a cell
      [%tal @uvre @uvre]        :: Take tail head of first SSA and place in second.
                                ::  Undefined if first SSA not a cell
      [%men @uvre]              :: Push onto the mean stack
      [%man ~]                  :: Pop from the mean stack
      [%hit @uvre]              :: Profiling hit counter
      [%slg @uvre]              :: Debugging print
      [%mew @uvre @uvre @uvre @uvre]  :: Cache write - cache key - subject - formula - result
      [%tim ~]                  :: Start timer
      [%tom ~]                  :: Stop timer 
      [%mem ~]                  :: Print memory usage
  ==
+$  pool
  $%  [%hed @uvre]
      [%tal @uvre]
  ==
::  instructions ending a block
+$  site
  $%  [%clq @uvre bile bile]                    :: Branch left if the SSA value is a cell, right otherwise
      [%eqq @uvre @uvre bile bile]              :: Branch left if SSA registers are equal, right otherwise
      [%brn @uvre bile bile bile]               :: Branch 1st - not loobean, 2nd - 0, 3rd - 1
      [%hop bile]                               :: Go to bile unconditionally (local direct jump)
      [%lnk @uvre @uvre @uvre bile]             :: Call formula in first SSA register with subject in second,
                                                ::   result in third, return to bile
      [%cal bell @uvre (list @uvre) @uvre bile] :: Call arm given by bell, 
                                                ::   subject/formula pair in register
                                                ::   subject in SSA register list,
                                                ::   result to register, return to bile
      [%lnt @uvre @uvre]                        :: Jump to formula in first SSA register with subject in second
      [%jmp bell @uvre (list @uvre)]            :: Jump to the code at the label in tail position,
                                                ::   subject/formula pair in SSA register,
                                                ::   subject in register list
      [%spy @uvre @uvre @uvre bile]             :: Scry with the ref/path pair in the first 2 SSA registers
                                                ::   define the third as the result
      [%mer @uvre @uvre @uvre @uvre bile bile]  :: Cache read: key - subject - formula - hit - miss
      [%don @uvre]                              :: Finish the procedure, returning the value in the SSA
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
      sire=@uvre :: input register for indirect calls
      will=(map bile blob)
      sans=@uvre ::  next SSA register
  ==
::  code table
+$  hill  (map bell pile)
--
