|%
::
::    hot state
::
::  faces (per element):
::  j - jet ID
::  p - path
::  a - battery axis
::  need - how this jet expects the subject
+$  heat
  (list [p=path a=@ j=@jet =need])
::
::   pokes
::
::  the codegen core can be updated by asking it to analyze new code
+$  gist  [%comp slow=path s=* f=*]

::  slow hint tree
+$  shed  $~  [~ ~]  [root=(unit bell) kids=(map @tas shed)]
::
::    external label
::
::  labels an arm by what is known of its subject paired with its
::  formula
+$  bell  [text=sock form=*]
::
::    internal label
::
::  labels a basic block within generated code for an arm
+$  bile  [%bile axe=@ thus=@tas bell]
::    noun shape
::
::  labels axes of an abstract noun with SSA registers, possibly
::  ignoring some axes
+$  need
  $%  [%this sass=@uvre]
      [%both sass=@uvre left=need rite=need]
      [%none ~]
  ==
::    linear control flow
::
::  a label for code for some nock, and the shape of its subject
+$  next  $>(%next goal)
::
::    destination
::
::  codegen destination
::
::  %pick: result will be used as a loobean for Nock 6
::  %done: nock is in tail position, return result
::  %next: jump to given label with result in given $need
+$  goal
  $%  [%pick sass=@uvre zero=bile once=bile]
      [%done ~]
      [%next what=need then=bile]
  ==
::
::    instructions in a block
::
::  note: the slow and mean stack instructions assume that non-tail
::  calls ($site cases %lnk %cal %caf) save the current state of the
::  mean stack, and %don restores it. This allows us to omit %man and
::  %sld popping instructions after the body of the relevant hints in
::  tail position, maintaining TCO in the presence of stack traces and
::  analysis boundary (%slow) hints. An implementation of this VM *must*
::  conform to this behavior.
::
::  faces:
::  n - noun
::  d - destination
::  f - formula
::  h - head
::  k - key
::  l - label
::  r - result
::  s - source
::  t - tail
::  u - subject
::
::  cases:
::  %imm - write immediate n to d
::  %mov - copy s to d
::  %phi - select source based on last %hip, copy to d
::  %inc - increment s and write to d?
::  %con - cons h and t into d
::  %cop - poison s if s is an atom
::  %lop - poison s if s is not a loobean
::  %coc - crash immediately if s is an atom
::  %hed - write head of s to d. Poison s if s is an atom
::  %tal - write tail of s to d. Poison s if s is an atom
::  %hci - write head of s to d. Crash if s is an atom
::  %tci - write tail of s to d. Crash if s is an atom.
::  %men - Push s onto the mean stack.
::  %man - Pop the mean stack
::  %slo - Push s onto the slow stack.
::  %sld - Pop from the slow stack
::  %hit - Increment a profiling hit counter labeled with the noun in s
::  %slg - Print out s for debugging
::  %mew - Write r to the memo cache at the triple [k u f]
::  %tim - Push a timer onto the timer stack and start it
::  %tom - Pop a timer from the timer stack, stop it, and print elapsed
::  %mem - Print memory usage
::  %pol - If s is poisoned, poison d
::  %poi - Poison d
::  %ibp - If any register in s is poisoned, crash.
+$  pole
  $%  [%imm n=* d=@uvre]
      [%mov s=@uvre d=@uvre]
      [%inc s=@uvre d=@uvre]
      [%con h=@uvre t=@uvre d=@uvre]
      [%cop s=@uvre]
      [%lop s=@uvre]
      [%coc s=@uvre]
      [%hed s=@uvre d=@uvre]
      [%hci s=@uvre d=@uvre]
      [%tal s=@uvre d=@uvre]
      [%tci s=@uvre d=@uvre]
      [%men l=@ta s=@uvre]
      [%man ~]
      [%slo s=@uvre]
      [%sld ~]
      [%hit s=@uvre]
      [%slg s=@uvre]
      [%mew k=@uvre u=@uvre f=@uvre r=@uvre]
      [%tim ~]
      [%tom ~]
      [%mem ~]
      [%pol p=@uvre q=@uvre]
      [%poi p=@uvre]
      [%ipb p=(list @uvre)]
  ==
::
::    origin description
::
::  %hed - register is head of given register
::  %tal - register is tail of given register
+$  pool
  $%  [%hed s=@uvre]
      [%tal s=@uvre]
  ==
::
::    instructions ending a block
::
::  faces:
::  a - target arm
::  b - poisons
::  c - come-from block
::  d - destination
::  e - scry ref
::  f - formula
::  i - in cache
::  k - key
::  l - left source
::  m - cache miss
::  n - fast label and axis into battery
::  o - "one" / false case
::  p - scry path
::  r - right source
::  s - source
::  t - target block
::  u - subject
::  v - subject but registerized
::  z - "zero" / true case
::
::  cases:
::  %clq - if s is a cell goto z else goto o
::  %eqq - if l and r equal goto z else goto o
::  %brn - if s is 0 goto z, if 1 goto o, else crash
::  %hop - unconditionally go to t
::  %hip - set comefrom label to c and goto t
::  %lnk - evaluate f against u and put the result in d, then goto t
::  %cal - call the arm a with subject in registers v, poisons in b,
::         result in d, and then goto t
::  %caf - like call but with fast label
::  %lnt - evaluate f against u in tail position
::  %jmp - call the arm a with subject in registers u, poisons in b, in
::         tail position
::  %jmf - like jmp but with fast label
::  %spy - scry with ref in e and path in p, put result in d, goto t
::  %mer - check if triple [k u f] is in cache, put result in d if so
::         and goto i, else goto m
::  %don - return value in s from current arm
::  %bom - crash
+$  site
  $%  [%clq s=@uvre z=bile o=bile]
      [%eqq l=@uvre r=@uvre z=bile o=bile]
      [%brn s=@uvre z=bile o=bile]
      [%hop t=bile]
      [%hip c=bile t=bile]
      [%lnk u=@uvre f=@uvre d=@uvre t=bile]
      [%cal a=bell b=(list @uvre) v=(list @uvre) d=@uvre t=bile]
      [%caf a=bell b=(list @uvre) v=(list @uvre) d=@uvre t=bile u=@uvre n=[path @]]
      [%lnt u=@uvre f=@uvre]
      [%jmp a=bell b=(list @uvre) v=(list @uvre)]
      [%jmf a=bell b=(list @uvre) v=(list @uvre) u=@uvre n=[path @]]
      [%spy e=@uvre p=@uvre d=@uvre t=bile]
      [%mer k=@uvre u=@uvre f=@uvre d=@uvre i=bile m=bile]
      [%don s=@uvre]
      [%bom ~]
  ==
::    basic block
::
::  map of phi-arguments, each of which initializes an @uvre from
::  another @uvre selected by which label we came from (see %hip control
::  flow instruction)
::  zero or more dataflow instructions executed in order, followed by a
::  single control-flow instruction
+$  blob  [biff=(map @uvre (map bile @uvre)) body=(list pole) bend=site]
::
::    compilation unit
::
::  basic blocks and entry information for an arm
::
::  long: starting label for direct calls    axis 2
::  want: input registers for direct calls   axis 6
::  bait: input poison registers NLR         axis 14
::  walt: input starting registers LR        axis 30
::  wish: starting label for indirect calls  axis 62
::  sire: input register for indirect calls  axis 126
::  will: code table for arm                 axis 254
::  sans: next SSA register                  axis 255
+$  pile
  $:  long=bile
      want=need
      bait=(list @uvre)
      walt=(list @uvre)
      wish=bile
      sire=@uvre
      will=(map bile blob)
      sans=@uvre
  ==
::
::    code table
::
::  code entry information for arms
+$  hill  (map bell pile)
--
