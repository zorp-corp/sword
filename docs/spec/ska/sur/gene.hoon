/-  *sock
|%
+|  %comm
+$  labl  [sub=sock for=*]
+|  %lock
+$  mode
  $?  %save  :: Must not clobber subject, non-tail
      %step  :: May clobber subject, non-tail
      %butt  :: Tail, but do not write to table when done
      %tail  :: Tail, write to table when done
  ==
+$  gabl  @
+$  lick
  $%  [%con *]    :: constant
      [%axe @]    :: axis
      [%cel @]    :: Cell [slot result]
      [%clq ~]    :: Test if result is cell
      [%inc ~]    :: Increment result
      [%eqq @]    :: Test if slot is equal to result
      [%br1 gabl]    :: Jump to generated label if result is 1
      [%bru gabl]    :: Jump to generated label unconditionally
      [%brh gabl]    :: Generated label here
      [%sub ~]    :: Set subject to result
      [%ext ~]    :: Cons result onto subject
      [%dxt ~]    :: Set subject to tail of subject (restore after ext)
      [%noc ~]    :: Split cell in result register to subject and result
      [%lnk ~]    :: Call code, save PC in slot 0
      [%cal labl] :: Call label, save PC in slot 0
      [%lnt ~]    :: Tail-call code
      [%jmp labl] :: Jump to label
      [%edt @]    :: Edit result into axis in subject
      [%spy ~]    :: Slam peek gate
      [%puh @]    :: Push frame with given number of slots
      [%pop ~]    :: Pop frame
      [%put @]    :: Save result register to slot
      [%get @]    :: Restore result register from slot
      [%sav @]    :: Save subject register to slot
      [%reo @]    :: Restore subject register from slot
      [%don ~]    :: Return from procedure
      [%bom ~]    :: Crash
  ==
+$  lock  (list lick)
+$  link  [does=lock says=boot]
+$  tabl  (map labl link)
+|  %dege
+$  dabl  [sub=sock for=* ax=@ gen=@]
+$  cost  :: control destination (jump dest)
  $%  [%dab wher=dabl]            :: go here unconditionally
      [%bab troo=dabl fals=dabl]  :: branch on result
      [%ret ~]                    :: tail position
  ==
+$  dast  $~(4 @)      :: data destination (value dest)
+$  dinn
  $%  [%imm * @]       :: Write a noun to an axis
      [%mov @ @]       :: Copy an axis to another axis (destination must not nest under source!)
      [%clq dabl dabl] :: Branch left if axis 4 is a cell, right otherwise
      [%inc @]         :: Increment the atom at the axis and write it back to the axis
      [%eqq dabl dabl] :: Branch left if axes 8 and 9 are structurally equal, right otherwise
      [%brn dabl dabl] :: Branch left if axis 4 is atom 0, right if atom 1, crash otherwise
      [%hop dabl]      :: Go to dabl unconditionally (local direct jump)
      [%her dabl]      :: Label explicitly in code, as branch or jump target
      [%lnk ~]         :: Push a frame with a return pointer, eval the code at the axis 4
                       ::   Places result at axis 4
      [%cal labl]      :: Push a frame with a return pointer, call the code at labl
                       ::   Places result at axis 4
      [%lnt ~]         :: Eval the code at axis 4 in tail position
      [%jmp labl]      :: Jump to the code at the label in tail position
      [%spy ~]         :: Scry with the ref/path pair at axis 4, write back to axis 4
      [%sft ~]         :: Moral equivalent of `mov 2 5; imm 0 4;` without the anti-nesting constraint 
                       ::  Shifts the result into scratch and sets axis 4
                       ::  to 0.
      [%ust ~]         :: Equivalent to `mov 5 2`
                       ::   Undoes sft: moves head of scratch back to
                       ::   result and sets scratch to tail of scratch
      [%don ~]         :: Finish the procedure, returning the value at axis 4
      [%bom ~]         :: Crash
    ==
+$  linn  (list dinn)
+$  tine  [does=linn says=boot]
+$  tinn  (map labl tine)
--
