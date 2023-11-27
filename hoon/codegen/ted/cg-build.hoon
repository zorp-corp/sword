/-  spider
/+  make, strand, strandio
!.
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  bec=beak  bind:m  get-beak:strandio
;<  now=@da  bind:m  get-time:strandio
=/  cg=path  /(scot %p p.bec)/[q.bec]/(scot %da now)
~&  "cg desk: {<cg>}"
(pure:m $:+>:(make-codegen-core:make cg))
