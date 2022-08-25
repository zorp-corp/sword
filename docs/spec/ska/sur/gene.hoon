/-  *sock
|%
+$  mode
  $?  %save  :: Must not clobber subject, non-tail
      %step  :: May clobber subject, non-tail
      %butt  :: Tail, but do not write to table when done
      %tail  :: Tail, write to table when done
  ==
+$  labl  [sub=sock for=*]
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
      [%brh gabl]    :: Generated label
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
--
