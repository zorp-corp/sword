/-  *sock
|%
::  external label
+$  bell  [text=sock form=*]
::  internal label
+$  bile  [%bile deep=@ hall=@ thus=@tas]
::  ssa shape of a noun
+$  need
  $%  [%this sass=@ code=?]
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
  $%  [%imm * @]    :: Write a noun to an SSA value
      [%mov @ @]    :: Copy an SSA value
      [%inc @ @]    :: Define second SSA register as increment of first
      [%unc @ @]    :: Define a second SSA register as increment of first, without checking atomicity
      [%con @ @ @]  :: Construct a cell, first SSA head, second SSA tail, third SSA result
      [%hed @ @]    :: Take the head of first SSA and place in second.
                    ::  Crash if first SSA not a cell
      [%hud @ @]    :: Take the head of the first SSA, known to be a cell
      [%tal @ @]    :: Take tail head of first SSA and place in second.
                    ::  Crash if first SSA not a cell
      [%tul @ @]    :: Take the tail of the first SSA, known to be a cell
  ==
::  instructions ending a block
+$  site
  $%  [%clq @ bile bile]          :: Branch left if the SSA value is a cell, right otherwise
      [%eqq @ @ bile bile]        :: Branch left if SSA registers are equal, right otherwise
      [%brn @ bile bile]          :: Branch left if SSA register is 0, right if 1
      [%hop bile]                 :: Go to bile unconditionally (local direct jump)
      [%lnk @ @ @ bile]           :: Call formula in first SSA register with subject in second,
                                  ::   result in third, return to bile
      [%cal bell (list @) @ bile] :: Call arm given by bell, subject in first SSA register,
                                  ::   result in second, return to bile
      [%lnt @ @]                  :: Jump to formula in first SSA register with subject in second
      [%jmp bell (list @)]        :: Jump to the code at the label in tail position,
                                  ::   with the subject in the SSA register
      [%spy @ @ @ bile]           :: Scry with the ref/path pair in the first 2 SSA registers
                                  ::   define the third as the result
      [%hin @ bile]               :: Push onto the hint stack and continue
      [%hun bile]                 :: Pop from the hint stack. Note that this is implicit in %don 
      [%don @]                    :: Finish the procedure, returning the value in the SSA
      [%bom ~]                    :: Crash
  ==
::  basic block
+$  blob  [body=(list pole) bend=site]
::  compilation unit
+$  pile
  $:  long=bile  :: starting label for direct calls
      want=need  :: input registers for direct calls
      wish=bile  :: starting label for indirect calls
      sire=@     :: input register for indirect calls
      will=(map bile blob)
  ==
::  code table
+$  hill  (map bell pile)
--
