/+  skan
|.
=>  $:skan
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
  $%  [%pick sass=@uvre zero=@uwoo once=@uwoo]
      [%done ~]
      [%next what=need then=@uwoo]
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
::  %coc - crash immediately if s is an atom
::  %hed - write head of s to d. Poison s if s is an atom
::  %tal - write tail of s to d. Poison s if s is an atom
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
::  %poi - Poison d
::  %ibp - If any register in s is poisoned, crash.
+$  pole
  $%  [%imm n=* d=@uvre]
      [%mov s=@uvre d=@uvre]
      [%inc s=@uvre d=@uvre]
      [%con h=@uvre t=@uvre d=@uvre]
      [%hed s=@uvre d=@uvre]
      [%tal s=@uvre d=@uvre]
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
::  w - walt
::  x - sans
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
  $%  [%clq s=@uvre z=@uwoo o=@uwoo]
      [%eqq l=@uvre r=@uvre z=@uwoo o=@uwoo]
      [%brn s=@uvre z=@uwoo o=@uwoo]
      [%hop t=@uwoo]
      [%hip c=@uwoo t=@uwoo]
      [%lnk u=@uvre f=@uvre d=@uvre t=@uwoo]
      [%cal a=@uwoo v=(list @uvre) w=(list @uvre) x=@ud d=@uvre t=@uwoo]
      [%caf a=@uwoo v=(list @uvre) w=(list @uvre) x=@ud d=@uvre t=@uwoo u=@uvre n=[path @]]
      [%lnt u=@uvre f=@uvre]
      [%jmp a=@uwoo v=(list @uvre) w=(list @uvre) x=@ud]
      [%jmf a=@uwoo v=(list @uvre) w=(list @uvre) x=@ud u=@uvre n=[path @]]
      [%spy e=@uvre p=@uvre d=@uvre t=@uwoo]
      [%mer k=@uvre u=@uvre f=@uvre d=@uvre i=@uwoo m=@uwoo]
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
+$  blob  [biff=(map @uvre (map @uwoo @uvre)) body=(list pole) bend=site]
::
::    compilation unit
::
::  basic blocks and entry information for an arm
::
::  want: input registers for direct calls
::  walt: input starting registers LR
::  sire: input register for indirect calls
::  sans: next SSA register
+$  pile
  $:  want=need
      walt=(list @uvre)
      sire=@uvre
      sans=@uvre
  ==
::    code table state
::
::  Code table
::
::  hill: basic block table
::  peal: direct entry points
::  gong: indirect entry points
::  gist: register information
::  next: next free register
::  free: free label spaces
+$  fuji
  $:  hill=(map @uwoo blob)
      peal=(map bell @uwoo)
      gong=(map bell @uwoo)
      gist=(map bell pile)
      next=@uwoo
      free=(list @uwoo)
  ==
--
