/-  *sock
|%
+|  %ska
+$  barn  [sub=sock for=*]
+$  nomm  :: SKA-analyzed nock
  $%  [%par nomm nomm] 
      [%zer @ ?]  ::  safety-tagged lookup
      [%one *]
      [%two nomm nomm sock (unit *) ?]  ::  subject knowledge and known formula, safety-tag on metaformula
      [%thr nomm]
      [%fou nomm ?]  :: safety-tagged increment
      [%fiv nomm nomm]
      [%six nomm nomm nomm]
      [%sev nomm nomm]
      [%eig nomm nomm]
      [%nin @ nomm sock (unit *) ?]  :: subject knowledge and known formula
      [%ten [@ nomm] nomm ?]  ::  safety-tagged edit
      [%els @ nomm]
      [%eld [@ nomm] nomm ?]  :: safety-tagged hint formula
      [%twe nomm nomm]
  ==
+$  farm  [yard=(map barn [does=nomm says=boot]) wood=(list barn)]
+|  %lin
+$  berm  [sub=sock for=* ax=@ gen=@] :: local label
+$  plow :: noun<->ssa map
  $%  [%fork left=plow rite=plow safe=?] :: cons of two mappings
      [%tine @]                          :: use this SSA value at this axis
      [%disc ~]                          :: no uses here or below
  ==
+$  line  :: destination
  $%  [%moat wher=berm what=plow]  :: place result in SSA values specified by what, go wher
      [%rift troo=berm fals=berm]  :: branch on result
      [%pond ~]                    :: tail position, return result in a register
  ==
+$  bran :: instructions in a block
  $%  [%imm * @]                  :: Write a noun to an SSA value
      [%mov @ @]                  :: Copy an SSA value
      [%inc @ @]                  :: Define second SSA register as increment of first
      [%con @ @ @]                :: Construct a cell, first SSA head, second SSA tail, third SSA result
      [%hed @ @]                  :: Take the head of first SSA and place in second.
                                  ::  Crash if first SSA not a cell
      [%hud @ @]                  :: Take the head of the first SSA, known to be a cell
      [%tal @ @]                  :: Take tail head of first SSA and place in second.
                                  ::  Crash if first SSA not a cell
      [%tul @ @]                  :: Take the tail of the first SSA, known to be a cell
  ==
+$  germ :: instructions ending a block
  $%  [%clq @ berm berm]          :: Branch left if the SSA value is a cell, right otherwise
      [%eqq @ @ berm berm]        :: Branch left if SSA registers are equal, right otherwise
      [%brn @ berm berm]          :: Branch left if SSA register is 0, right if 1
      [%hop berm]                 :: Go to berm unconditionally (local direct jump)
      [%lnk @ @ @ berm]           :: Call formula in first SSA register with subject in second,
                                  ::   result in third, return to berm
      [%cal barn (list @) @ berm] :: Call arm given by barn, subject in first SSA register,
                                  ::   result in second, return to berm
      [%lnt @ @]                  :: Jump to formula in first SSA register with subject in second
      [%jmp barn (list @)]        :: Jump to the code at the label in tail position,
                                  ::    with the subject in the SSA register
      [%spy @ @ @ berm]           :: Scry with the ref/path pair in the first 2 SSA registers
                                  ::   define the third as the result
      [%hnt @ berm]               :: Treat the result in the SSA register as a hint and continue to the given label

      [%don @]                    :: Finish the procedure, returning the value at axis 4
      [%bom ~]                    :: Crash
  ==
+$  lock  [body=(list bran) bend=germ]  :: basic block: instructions + a terminator or branch
+$  lake  (map (unit berm) lock)        :: labeled basic blocks
+$  rice  [goes=lake uses=(list @)]     :: labeled basic blocks and entry point arguments as subject axes
+$  sack  [does=rice says=boot]         :: code table entry: basic blocks + SKA result for an arm
+$  town  [land=(map barn sack) lamb=@] :: code table
--
