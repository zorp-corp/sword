/-  *sock
|%
+|  %lin
::  external label
+$  bell  [text=sock form=*]
::  internal label
+$  bile  [%bile axe=@ thus=@tas =bell]
::  ssa shape of a noun
+$  need
  $%  [%this sass=@stir code=?]
      [%both left=need rite=need]
      [%none ~]
  ==
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
      [%inc @stir @stir]        :: Define second SSA register as increment of first
      [%unc @stir @stir]        :: Define a second SSA register as increment of first, without checking atomicity
      [%con @stir @stir @stir]  :: Construct a cell, first SSA head, second SSA tail, third SSA result
      [%hed @stir @stir]        :: Take the head of first SSA and place in second.
                                ::  Crash if first SSA not a cell
      [%hud @stir @stir]        :: Take the head of the first SSA, known to be a cell
      [%tal @stir @stir]        :: Take tail head of first SSA and place in second.
                                ::  Crash if first SSA not a cell
      [%tul @stir @stir]        :: Take the tail of the first SSA, known to be a cell
  ==
::  instructions ending a block
+$  site
  $%  [%clq @stir bile bile]              :: Branch left if the SSA value is a cell, right otherwise
      [%eqq @stir @stir bile bile]        :: Branch left if SSA registers are equal, right otherwise
      [%brn @stir bile bile]              :: Branch left if SSA register is 0, right if 1
      [%hop bile]                         :: Go to bile unconditionally (local direct jump)
      [%lnk @stir @stir @stir bile]       :: Call formula in first SSA register with subject in second,
                                          ::   result in third, return to bile
      [%cal bell (list @stir) @stir bile] :: Call arm given by bell, subject in first SSA register,
                                          ::   result in second, return to bile
      [%lnt @stir @stir]                  :: Jump to formula in first SSA register with subject in second
      [%jmp bell (list @stir)]            :: Jump to the code at the label in tail position,
                                          ::   with the subject in the SSA register
      [%spy @stir @stir @stir bile]       :: Scry with the ref/path pair in the first 2 SSA registers
                                          ::   define the third as the result
      [%hin @stir bile]                   :: Push onto the hint stack and continue
      [%hun bile]                         :: Pop from the hint stack. Note that this is implicit in %don 
      [%don @stir]                        :: Finish the procedure, returning the value in the SSA
      [%bom ~]                            :: Crash
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
