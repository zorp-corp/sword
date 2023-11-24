/+  make
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] *]
~&  "bec: {<bec>}"
:-  %jam
=/  sys=path
  /(scot %p p.bec)/base/(scot %da now)/sys
=/  cg=path
  /(scot %p p.bec)/sandbox/(scot %da now)
~&  "cg: {<cg>}"
(make-codegen-core:make sys cg)
