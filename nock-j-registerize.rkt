#lang racket

(require rackunit)

;; This pass builds on the mutable state (the continuation stack) introduced in (i)
;; and adds mutable registers, which are updated by the `set-register` function.
;;
;; Procedures no longer take language-native arguments, but have an explicit convention
;; for the globally-defined registers in which they expect their arguments.

(define stack '())
(define (push-k k)
 (set! stack (cons k stack))) 

(define ra 0)
(define rb 0)
(define rc 0)
(define rd 0)
(define re 0)

(define (set-register register x)
 (match register
  ('ra (set! ra x))
  ('rb (set! rb x))
  ('rc (set! rc x))
  ('rd (set! rd x))
  ('re (set! re x))))

; interface with non-CPS, non-registerized calling convention
(define (nock-noun subject formula gates err-k trace)
 (begin
  (push-k empty-k)
  (set-register 'ra subject)
  (set-register 'rb formula)
  (set-register 'rc gates)
  (set-register 'rd err-k)
  (set-register 're trace)
  (nock-noun-cps)))

; ra - subject
; rb - formula
; rc - gate stack
; rd - err continuation
; re - err trace
(define (nock-noun-cps)
 (match rb
  ([cons (cons (var b) (var c)) (var d)]
   (begin
    (push-k (nock-cons-k-1 ra d rc rd re))
    ; ra already set
    (set-register 'rb (cons b c))
    ; rc already set
    ; rd already set
    ; re already set
    (nock-noun-cps)))
  ([cons 0 (var b)]
   (begin
    ; ra already set
    (set-register 'rb b)
    (set-register 'rc rd)
    (set-register 'rd re)
    (nock-tree-find)))
  ([cons 1 (var b)]
   (begin
    (set-register 'ra b)
    (apply-k)))
  ([cons 2 (cons (var b) (var c))]
   (begin
    (push-k (nock-2-k-1 ra c rc rd re))
    ; ra already set
    (set-register 'rb b)
    ; rc already set
    ; rd already set
    ; re already set
    (nock-noun-cps)))
  ([cons 3 (var b)]
   (begin
    (push-k nock-3-k)
    ; ra already set
    (set-register 'rb b)
    ; rc already set
    ; rd already set
    ; re already set
    (nock-noun-cps)))
  ([cons 4 (var b)]
   (begin
    (push-k nock-4-k)
    ; ra already set
    (set-register 'rb b)
    ; rc already set
    ; rd already set
    ; re already set
    (nock-noun-cps)))
  ([cons 5 (cons (var b) (var c))]
   (begin
    (push-k (nock-5-k-1 ra c rc rd re))
    ; ra already set
    (set-register 'rb b)
    ; rc already set
    ; rd already set
    ; re already set
    (nock-noun-cps)))
  ([cons 6 (cons (var b) (cons (var c) (var d)))]
   (begin
    (push-k (nock-6-k ra c d rc rd re))
    ; ra already set
    (set-register 'rb b)
    ; rc already set
    ; rd already set
    ; re already set
    (nock-noun-cps)))
  ([cons 7 (cons (var b) (var c))]
   (begin
    (push-k (nock-7-k c rc rd re))
    ; ra already set
    (set-register 'rb b)
    ; rc already set
    ; rd already set
    ; re already set
    (nock-noun-cps)))
  ([cons 8 (cons (var b) (var c))]
   (begin
    (push-k (nock-8-k ra c rc rd re))
    ; ra already set
    (set-register 'rb b)
    ; rc already set
    ; rd already set
    ; re already set
    (nock-noun-cps)))
  ([cons 9 (cons (var b) (var c))]
   (begin
    (push-k (nock-9-k-1 b rc rd re))
    ; ra already set
    (set-register 'rb c)
    ; rc already set
    ; rd already set
    ; re already set
    (nock-noun-cps)))
  ([cons 10 (cons (cons (var b) (var c)) (var d))]
   (begin
    (push-k (nock-10-k-1 ra d b rc rd re))
    ; ra already set
    (set-register 'rb c)
    ; rc already set
    ; rd already set
    ; re already set
    (nock-noun-cps)))
  ([cons 11 (cons (cons (var b) (var c)) (var d))]
   (begin
    (push-k (nock-11-k ra b d rc rd re))
    ; ra already set
    (set-register 'rb c)
    ; rc already set
    ; rd already set
    ; re already set
    (nock-noun-cps)))
  ([cons 11 (cons (var b) (var c))]
   (begin
    ; ra already set
    (set-register 'rb c)
    ; rc already set
    ; rd already set
    ; re already set
    (nock-noun-cps)))
  ([cons 12 (cons (var ref) (var path))]
   (begin
    (push-k (nock-12-k-1 ra path rc rd re))
    ; ra already set
    (set-register 'rb ref)
    ; rc already set
    ; rd already set
    ; re already set
    (nock-noun-cps)))))

; ra - address to reverse
(define (reverse-address)
 (begin
  ; ra already set
  (set-register 'rb 1)
  (reverse-address-acc)))

; ra - address to reverse
; rb - accumulator for reversed address
(define (reverse-address-acc)
 (if (= ra 1)
  (begin
   (set-register 'ra rb)
   (apply-k))
  (begin
   (set-register 'rb (bitwise-ior (arithmetic-shift rb 1) (bitwise-and ra 1)))
   (set-register 'ra (arithmetic-shift ra -1))
   (reverse-address-acc))))

; ra - tree to find subtree of
; rb - reversed address to find
(define (nock-tree-find-reversed)
 (if (= rb 1)
  ; ra already set
  (apply-k)
  (if (even? rb)
   (begin
    (set-register 'ra (car ra))
    (set-register 'rb (arithmetic-shift rb -1))
    (nock-tree-find-reversed))
   (begin
    (set-register 'ra (cdr ra))
    (set-register 'rb (arithmetic-shift rb -1))
    (nock-tree-find-reversed)))))

; ra - tree to find subtree of
; rb - address of subtree
; rc - err continuation
; rd - err trace
(define (nock-tree-find)
 (if (= rb 0)
  (begin
   (set-register 'ra rc)
   (set-register 'rb (cons 2 rd))
   (apply-err-k))
  (begin
   (push-k (nock-tree-find-k ra))
   (set-register 'ra rb)
   (reverse-address))))

; ra - subtree to place at address
; rb - reversed address
; rc - tree to edit
(define (nock-tree-edit-reversed)
 (if (= rb 1)
  ; ra already set
  (apply-k)
  (if (even? rb)
   (begin
    (push-k (nock-tree-edit-car-k rc))
    ; ra already set
    (set-register 'rb (arithmetic-shift rb -1))
    (set-register 'rc (car rc))
    (nock-tree-edit-reversed))
   (begin
    (push-k (nock-tree-edit-cdr-k rc))
    ; ra already set
    (set-register 'rb (arithmetic-shift rb -1))
    (set-register 'rc (cdr rc))
    (nock-tree-edit-reversed)))))

; # operator in nock spec: tree editing
; ra - subtree to place at address
; rb - address
; rc - tree to edit
; rd - err continuation
; re - err trace
(define (nock-tree-edit)
 (if (= rb 0)
  (begin
   (set-register 'ra rd)
   (set-register 'rb (cons 2 re))
   (apply-err-k))
  (begin
   (push-k (nock-tree-edit-k ra rc))
   (set-register 'ra rb)
   (reverse-address))))

(define empty-k (list 'empty-k))
(define (nock-cons-k-1 subject d gates err-k trace) (list 'nock-cons-k-2 subject d gates err-k trace))
(define (nock-cons-k-2 u) (list 'nock-cons-k-2 u))
(define (nock-2-k-1 subject c gates err-k trace) (list 'nock-2-k-1 subject c gates err-k trace)) 
(define (nock-2-k-2 u gates err-k trace) (list 'nock-2-k-2 u gates err-k trace))
(define nock-3-k (list 'nock-3-k))
(define nock-4-k (list 'nock-4-k))
(define (nock-5-k-1 subject c gates err-k trace) (list 'nock-5-k-1 subject c gates err-k trace))
(define (nock-5-k-2 u) (list 'nock-5-k-2 u))
(define (nock-6-k subject c d gates err-k trace) (list 'nock-6-k subject c d gates err-k trace))
(define (nock-7-k c gates err-k trace) (list 'nock-7-k c gates err-k trace))
(define (nock-8-k subject c gates err-k trace) (list 'nock-8-k subject c gates err-k trace))
(define (nock-9-k-1 b gates err-k trace) (list 'nock-9-k-1 b gates err-k trace))
(define (nock-9-k-2 u gates err-k trace) (list 'nock-9-k-2 u gates err-k trace))
(define (nock-10-k-1 subject d b gates err-k trace) (list 'nock-10-k-1 subject d b gates err-k trace))
(define (nock-10-k-2 u b err-k trace) (list 'nock-10-k-2 u b err-k trace))
(define (nock-11-k subject b d gates err-k trace) (list 'nock-11-k subject b d gates err-k trace))
(define (nock-12-k-1 subject path gates err-k trace) (list 'nock-12-k-1 subject path gates err-k trace))
(define (nock-12-k-2 gates err-k trace u) (list 'nock-12-k-2 gates err-k trace u))
(define (nock-12-k-3 u v outer-err-k outer-trace) (list 'nock-12-k u v outer-err-k outer-trace))
(define (nock-tree-find-k tree) (list 'nock-tree-find-k tree))
(define (nock-tree-edit-car-k tree) (list 'nock-tree-edit-car-k tree))
(define (nock-tree-edit-cdr-k tree) (list 'nock-tree-edit-cdr-k tree))
(define (nock-tree-edit-k subtree tree) (list 'nock-tree-edit-k subtree tree))

; apply the continuation from the top of the stack
; ra - result
(define (apply-k)
 (let
  [(k (car stack))]
  (begin
   (set! stack (cdr stack))
   (match k       
    ([list 'empty-k] ra)
    ([list 'nock-cons-k-1 (var subject) (var d) (var gates) (var err-k) (var trace)]
     (begin
      (push-k (nock-cons-k-2 ra))
      (set-register 'ra subject)
      (set-register 'rb d)
      (set-register 'rc gates)
      (set-register 'rd err-k)
      (set-register 're trace)
      (nock-noun-cps subject d gates err-k trace)))
    ([list 'nock-cons-k-2 (var u) (var k^)]
     (begin
      (set-register 'ra (cons u ra))
      (apply-k)))
    ([list 'nock-2-k-1 (var subject) (var c) (var gates) (var err-k) (var trace)]
     (begin
      (push-k (nock-2-k-2 ra gates err-k trace))
      (set-register 'ra subject)
      (set-register 'rb c)
      (set-register 'rc gates)
      (set-register 'rd err-k)
      (set-register 're trace)
      (nock-noun-cps)))
    ([list 'nock-2-k-2 (var u) (var gates) (var err-k) (var trace)]
     (begin
      (set-register 'rb ra)
      (set-register 'ra u)
      (set-register 'rc gates)
      (set-register 'rd err-k)
      (set-register 're trace)
      (nock-noun-cps)))
    ([list 'nock-3-k]
     (if (pair? ra)
      (begin
       (set-register 'ra 0)
       (apply-k))
      (begin
       (set-register 'ra 1)
       (apply-k))))
    ([list 'nock-4-k]
     (begin
      (set-register 'ra (+ 1 ra))
      (apply-k)))
    ([list 'nock-5-k-1 (var subject) (var c) (var gates) (var err-k) (var trace)]
     (begin
      (push-k (nock-5-k-2 ra))
      (set-register 'ra subject)
      (set-register 'rb c)
      (set-register 'rc gates)
      (set-register 'rd err-k)
      (set-register 're trace)
      (nock-noun-cps)))
    ([list 'nock-5-k-2 (var u)]
     (if (eqv? u ra)
      (begin
       (set-register 'ra 0)
       (apply-k))
      (begin
       (set-register 'ra 1)
       (apply-k))))
    ([list 'nock-6-k (var subject) (var c) (var d) (var gates) (var err-k) (var trace)]
     (if (= 0 ra)
      (begin
       (set-register 'ra subject)
       (set-register 'rb c)
       (set-register 'rc gates)
       (set-register 'rd err-k)
       (set-register 're trace)
       (nock-noun-cps))
       (if (= 1 ra)
        (begin
         (set-register 'ra subject)
         (set-register 'rb d)
         (set-register 'rc gates)
         (set-register 'rd err-k)
         (set-register 're trace)
         (nock-noun-cps))
        (begin
         (set-register 'ra err-k)
         (set-register 'rb (cons 2 trace))
         (apply-err-k)))))
    ([list 'nock-7-k (var c) (var gates) (var err-k) (var trace)]
     (begin
      ; ra already set
      (set-register 'rb c)
      (set-register 'rc gates)
      (set-register 'rd err-k)
      (set-register 're trace)
      (nock-noun-cps)))
    ([list 'nock-8-k (var subject) (var c) (var gates) (var err-k) (var trace)]
     (begin
      (set-register 'ra (cons ra subject))
      (set-register 'rb c)
      (set-register 'rc gates)
      (set-register 'rd err-k)
      (set-register 're trace)
      (nock-noun-cps)))
    ([list 'nock-9-k-1 (var b) (var gates) (var err-k) (var trace)]
     (begin
      (push-k (nock-9-k-2 ra gates err-k trace))
      ; ra already set
      (set-register 'rb b)
      (set-register 'rc err-k)
      (set-register 'rd trace)
      (nock-tree-find)))
    ([list 'nock-9-k-2 (var u) (var gates) (var err-k) (var trace)]
     (begin
      (set-register 'rb ra)
      (set-register 'ra u)
      (set-register 'rc gates)
      (set-register 'rd err-k)
      (set-register 're trace)
      (nock-noun-cps)))
    ([list 'nock-10-k-1 (var subject) (var d) (var b) (var gates) (var err-k) (var trace)]
     (begin
      (push-k (nock-10-k-2 ra b err-k trace))
      (set-register 'ra subject)
      (set-register 'rb d)
      (set-register 'rc gates)
      (set-register 'rd err-k)
      (set-register 're trace)
      (nock-noun-cps)))
    ([list 'nock-10-k-2 (var u) (var b) (var err-k) (var trace)]
     (begin
      (set-register 'rc ra)
      (set-register 'ra u)
      (set-register 'rb b)
      (set-register 'rd err-k)
      (set-register 're trace)
      (nock-tree-edit)))
    ([list 'nock-11-k (var subject) (var b) (var d) (var gates) (var err-k) (var trace)]
     (if (member b (list (tas "hunk") (tas "hand") (tas "lose") (tas "mean") (tas "spot")))
      (begin
       (set-register 'ra subject)
       (set-register 'rb d)
       (set-register 'rc gates)
       (set-register 'rd err-k)
       (set-register 're (cons (cons b ra) trace))
       (nock-noun-cps))
      (begin
       (set-register 'ra subject)
       (set-register 'rb d)
       (set-register 'rc gates)
       (set-register 'rd err-k)
       (set-register 're trace)
       (nock-noun-cps))))
    ([list 'nock-12-k-1 (var subject) (var path) (var gates) (var err-k) (var trace)]
     (begin
      (push-k (nock-12-k-2 gates err-k trace ra))
      (set-register 'ra subject)
      (set-register 'rb path)
      (set-register 'rc gates)
      (set-register 'rd err-k)
      (set-register 're trace)
      (nock-noun-cps)))
    ([list 'nock-12-k-2 (var gates) (var err-k) (var trace) (var u)]
      (let*
       [(gate (car (car gates)))
        (outer-err-k err-k)
        (err-k (car (cdr (car gates))))
        (outer-trace trace)
        (trace (cdr (cdr (car gates))))
        (gates (cdr gates))
        (core (cons (car gate) (cons (cons u ra) (cdr (cdr gate)))))]
       (begin
        (push-k (nock-12-k-3 u ra outer-err-k outer-trace))
        (set-register 'ra core)
        (set-register 'rb (car core))
        (set-register 'rc gates)
        (set-register 'rd err-k)
        (set-register 're trace)
        (nock-noun-cps))))
    ([list 'nock-12-k-3 (var u) (var v) (var outer-err-k) (var outer-trace)]
     (if (equal? 0 ra)
      ; ~
      (begin
       (set-register 'ra outer-err-k)
       (set-register 'rb (cons 1 (cdr v)))
       (apply-err-k))
      (if (equal? 0 (car ra))
       (begin
        (set-register 'ra outer-err-k)
        (set-register 'rb (cons 2 (cons (cons (tas "hunk") (cons u v)) outer-trace)))
        (apply-err-k))
       (begin
        (set-register 'ra (cdr (cdr ra)))
        (apply-k)))))
    ([list 'nock-tree-edit-car-k (var tree)]
     (begin
      (set-register 'ra (cons ra (cdr tree)))
      (apply-k)))
    ([list 'nock-tree-edit-cdr-k (var tree)]
     (begin
      (set-register 'ra (cons (car tree) ra))
      (apply-k)))
    ([list 'nock-tree-edit-k (var subtree) (var tree)]
     (begin
      (set-register 'rb ra)
      (set-register 'ra subtree)
      (set-register 'rc tree)
      (nock-tree-edit-reversed)))
    ([list 'nock-tree-find-k (var tree)]
     (begin
      (set-register 'rb ra)
      (set-register 'ra tree)
      (nock-tree-find-reversed)))
    ((var k^) #:when (procedure? k^) (k^ ra))))))

; ra - err continuation
; rb - err trace
(define (apply-err-k) (ra))

;; macro for %tas literals:
;; converts input string into a numeric literal of that string represented as a %tas, i.e. an
;; atom with the ascii bytes of the string in sequence (first->LSB, last->MSB)
(define-syntax (tas str)
 (quasisyntax
  (unsyntax
   (foldr
    (lambda (char atom) (bitwise-ior (bitwise-and #xFF (char->integer char)) (arithmetic-shift atom 8)))
    0
    (string->list (car (cdr (syntax->datum str))))))))

(define nock-here 1)
(define (nock-car address) (* address 2))
(define (nock-cdr address) (+ 1 (* address 2)))
(define (get-0 x) (cons 0 x))
(define (literal-1 x) (cons 1 x))
(define (eval-2 x y) (cons 2 (cons x y)))
(define (cell?-3 x) (cons 3 x))
(define (inc-4 x) (cons 4 x))
(define (=-5 x y) (cons 5 (cons x y)))
(define (if-6 x y z) (cons 6 (cons x (cons y z))))
(define (compose-7 x y) (cons 7 (cons x y)))
(define (declare-8 x y) (cons 8 (cons x y)))
(define (call-9 x y) (cons 9 (cons x y)))
(define (update-10 x y z) (cons 10 (cons (cons x y) z)))
(define (hint-11 x y) (cons 11 (cons x y)))
(define lootru 0)
(define loofal 1)

(define test-tree (cons (cons 4 5) 3))
(define decrement-4-core
  (cons
   (if-6 (=-5 (get-0 (nock-car (nock-cdr nock-here))) (inc-4 (get-0 (nock-cdr (nock-cdr nock-here)))))
    (get-0 (nock-cdr (nock-cdr nock-here)))
    (call-9 (nock-car nock-here) (update-10 (nock-cdr (nock-cdr nock-here)) (inc-4 (get-0 (nock-cdr (nock-cdr nock-here)))) (get-0 nock-here))))
   (cons 4 0)))

(define (nock-test subject formula) (nock-noun subject formula '() test-err-k '()))

; rb - err trace
(define (test-err-k)
 (printf "Error: ~v" ra)
 (error 'nock-err))

(check-equal? (nock-test test-tree (get-0 nock-here) ) test-tree "tree address 1")
(check-equal? (nock-test test-tree (get-0 (nock-car nock-here))) (car test-tree) "tree address 2")
(check-equal? (nock-test test-tree (get-0 (nock-cdr nock-here))) (cdr test-tree) "tree address 3")
(check-equal? (nock-test test-tree (get-0 (nock-car (nock-car nock-here)))) (car (car test-tree)) "tree address 4")
(check-equal? (nock-test test-tree (get-0 (nock-cdr (nock-car nock-here)))) (cdr (car test-tree)) "tree address 5")
(check-equal? (nock-test 0 (literal-1 test-tree)) test-tree "literal")
(check-equal? (nock-test 0 (eval-2 (literal-1 test-tree) (literal-1 (get-0 2)))) (car test-tree) "eval")
(check-equal? (nock-test test-tree (cell?-3 (get-0 1))) lootru "test cell true")
(check-equal? (nock-test test-tree (cell?-3 (get-0 3))) loofal "test cell false")
(check-equal? (nock-test 0 (inc-4 (literal-1 0))) 1 "increment")
(check-equal? (nock-test test-tree (=-5 (literal-1 test-tree) (get-0 1))) lootru "test equals true")
(check-equal? (nock-test test-tree (=-5 (literal-1 test-tree) (get-0 2))) loofal "test equals false")
(check-equal? (nock-test test-tree (if-6 (literal-1 lootru) (literal-1 5) (get-0 100))) 5 "test if tru")
(check-equal? (nock-test test-tree (if-6 (literal-1 loofal) (get-0 100) (literal-1 5))) 5 "test if false")
(check-equal? (nock-test 0 (compose-7 (literal-1 test-tree) (get-0 2))) (car test-tree) "test compose")
(check-equal? (nock-test 0 (declare-8 (literal-1 test-tree) (get-0 2))) test-tree "test declare")
(check-equal? (nock-test 0 (call-9 (nock-car nock-here) (literal-1 decrement-4-core))) 3 "test call")
(check-equal? (nock-test 0 (update-10 (nock-cdr nock-here) (literal-1 (cons 6 7)) (literal-1 test-tree))) (cons (cons 4 5) (cons 6 7)) "test update")
(check-equal? (nock-test 0 (call-9 (nock-car nock-here) (update-10 (nock-car (nock-cdr nock-here)) (literal-1 8) (literal-1 decrement-4-core)))) 7 "test slam i.e. update sample and call")
; test 11 static and dynamic
