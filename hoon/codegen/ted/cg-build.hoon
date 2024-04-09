/-  spider
/+  make, strand, strandio
!:
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  bec=beak  bind:m  get-beak:strandio
;<  now=@da  bind:m  get-time:strandio
=/  cg=path  /(scot %p p.bec)/[q.bec]/(scot %da now)
~&  "cg desk: {<cg>}"
~!  cg
~!  make-codegen-trap:make
(pure:m (make-codegen-vase:make cg))
