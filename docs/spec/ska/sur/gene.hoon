/-  *sock
|%
+$  labl  [sub=sock for=*]
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
