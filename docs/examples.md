# Examples

## reapsum

This example implements a sum function using `++reap`, which accepts `a=@` and `b=*`
and allocates a list of length `a` with each slot filled with `b`:

```hoon
++  reap                                                ::  replicate
  ~/  %reap
  |*  [a=@ b=*]
  |-  ^-  (list _b)
  ?~  a  ~
  [b $(a (dec a))]
::
```

We will illustrate how New Mars' 2stackz memory model operates a trivial program that 
calls `++reap`.