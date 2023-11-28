:: XX this needs to actually get the desk from the beak once we can get
:: the correct desk
/+  make
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] *]
:-  %jam
=/  cg=path
  /(scot %p p.bec)/[q.bec]/(scot %da now)
~&  "cg desk path: {<cg>}"
(make-codegen-core:make cg)
