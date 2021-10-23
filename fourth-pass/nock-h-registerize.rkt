#lang racket

(require rackunit)

; registers
(define ra 0)
(define rb 0)
(define rc 0)
(define rd 0)
(define re 0)

; special, will be a stack
(define k 0)


; note: we are not registerising continuation arguments at this time.
; - apply functions will be unified (since arity 1)
; - abstract interpretation to bytecode

(define (nock-noun subject formula gates err-k trace)
 (begin
  (set! ra subject)
  (set! rb formula)
  (set! rc gates)
  (set! rd err-k)
  (set! re trace)
  (set! k empty-k)
  (nock-noun-cps)))

(define (nock-noun-cps)
 (let*
  [(subject ra)
   (formula rb)
   (gates rc)
   (err-k rd)
   (trace re)
   (recur-on-noun (recur-on-noun-closure gates err-k trace))
   (recur-on-noun-with-hint (recur-on-noun-with-hint-closure gates err-k trace))
   (recur-on-scry-gate (recur-on-scry-gate-closure gates))]
  (match formula
   ([cons (cons (var b) (var c)) (var d)]
    (begin
     (set! k (nock-cons-k-1 recur-on-closure subject d k))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rb (cons b c))
     (apply-3)
   ([cons 0 (var b)]
    (begin
     (set! ra subject)
     (set! rb b)
     (set! rc err-k)
     (set! rd trace)
     (nock-tree-find-cps)))
   ([cons 1 (var b)]
    (begin
     (set! ra b)
     (apply-k)))
   ([cons 2 (cons (var b) (var c))]
    (begin
     (set! k (nock-2-k-1 recur-on-noun subject c k))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc b)
     (apply-3)))
   ([cons 3 (var b)]
    (begin
     (set! (nock-3-k k))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc b)
     (apply-3)))
   ([cons 4 (var b)]
    (begin
     (set! k (nock-4-k k))
     (set! ra recur-on-noun)
     (set! rb subject)
     (apply-3)))
   ([cons 5 (cons (var b) (var c))]
    (begin
     (set! k (nock-5-k-1 recur-on-noun subject c k))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc b)
     (apply-3)))
   ([cons 6 (cons (var b) (cons (var c) (var d)))]
    (begin
     (set! k (nock-6-k recur-on-noun subject c d err-k trace k))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc b)
     (apply-3)))
   ([cons 7 (cons (var b) (var c))]
    (begin
     (set! k (nock-7-k recur-on-noun c k))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc b)
     (apply-3)))
   ([cons 8 (cons (var b) (var c))]
    (begin
     (set! k (nock-8-k recur-on-noun subject c k))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc b)
     (apply-3)))
   ([cons 9 (cons (var b) (var c))]
    (begin
     (set! k (nock-9-k-1 recur-on-noun b err-k trace k))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc c)
     (apply-3)))
   ([cons 10 (cons (cons (var b) (var c)) (var d))]
    (begin
     (set! k (nock-10-k-1 recur-on-noun subject d b err-k trace k))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc c)
     (apply-3)))
   ([cons 11 (cons (cons (var b) (var c)) (var d))]
    (begin
     (set! k (nock-11-k recur-on-noun-with-hint recur-on-noun subject d b k))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc c)
     (apply-3)))
   ([cons 11 (cons (var b) (var c))]
    (begin
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc c)
     (apply-3)))
   ([cons 12 (cons (var ref) (var path))]
    (begin
     (set! k (nock-12-k-1 recur-on-noun subject path recur-on-scry-gate err-k trace k))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc ref)
     (apply-3)))

(define nock-tree-find-cps
  (let
   [(tree ra)
    (address rb)
    (err-k rc)
    (trace rd)]
    (if (= address 0)
     (begin
      (set! ra (cons 2 trace))
      (err-k))
     (if (= address 1)
      (begin
       (set! ra tree)
       (apply-k))
      (if (even? address)
       (begin
        (set! k (nock-tree-find-k-car k))
        (set! ra tree)
        (set! rb (quotient address 2))
        (set! rc err-k)
        (set! rd trace)
        (nock-tree-find-cps))
       (begin
        (set! k (nock-tree-find-k-cdr k))
        (set! ra tree)
        (set! rb (quotient address 2))
        (set! rc err-k)
        (set! rd trace)
        (nock-tree-find-cps)))))))

; # operator in nock spec: tree editing
(define (nock-tree-edit-cps)
 (let
  [(subtree ra)
   (address rb)
   (tree rc)
   (err-k rd)
   (trace re)]
  (if (= address 0)
   (begin
    (set! ra (cons 2 trace))
    (err-k))
   (begin
    (set! k (nock-tree-edit-k subtree tree k))
    (set! ra address)
    (reverse-address-cps)))))

; Transform a nock address into a bitwise reversed address and a depth
; Note that the MSB of the address is always 1, and is *not* a head/tail bit.
; So we discard that one and do not count it in the depth.
; Editing on our representation is then a matter of
; editing the car or cdr of the tree based on whether the LSB is 1 or 0,
; shifting, decrementing the depth, and going around again *until the depth is 0*
;
; note that with several car bits at the bottom of the path (LSB in address, MSB reversed)
; the reversed address will be 0 for several iteraitons at the end, thus we test the depth
; and not the reversed address
(define (reverse-address-cps)
 (let
  [(address ra)]
  (begin
   (set! ra address)
   (set! rb 0)
   (set! rc 0)
   (reverse-address-acc-cps))))

(define (reverse-address-acc-cps)
 (let
  [(address ra)
   (reversed rb)
   (depth rc)]
  (if (= address 0)
   ; The most-significant bit in the address is a marker for the depth of
   ; the address, not a head/tail flag. We are instead storing the depth separately
   ; in the reversed representation, so we discard it.
   (begin
    (set! ra (cons (arithmetic-shift reversed -1) (- depth 1)))
    (apply-k))
   (let*
    [(top-bit (bitwise-and address 1))
     (reversed (bitwise-ior (arithmetic-shift reversed 1) top-bit))
     (address (arithmetic-shift address -1))
     (depth (+ depth 1))]
    (begin
     (set! ra address)
     (set! rb reversed)
     (set! rc depth)
     (reverse-address-acc-cps))))))

(define (nock-tree-edit-reversed-cps)
 (let*
  [(subtree ra)
   (reversed-depth rb)
   (tree rc)]
   (reversed (car reversed-depth))
   (depth (cdr reversed-depth))
   (reversed-depth (cons (arithmetic-shift reversed -1) (- depth 1)))]
  (if (= depth 0)
   (begin
    (set! ra subtree)
    (apply-k))
   (if (even? reversed)
    (begin
     (set! k (nock-tree-edit-k-car tree k))
     (set! ra subtree)
     (set! rb reversed-depth)
     (set! rc (car tree))
     (nock-tree-edit-reversed-cps))
    (begin
     (set! k (nock-tree-edit-k-cdr tree k))
     (set! ra subtree)
     (set! rb reversed-depth)
     (nock-tree-edit-reversed-cps))))))

(define (recur-on-noun-closure gates err-k trace) (list 'recur-on-noun gates err-k trace))
(define (recur-on-noun-with-hint-closure gates err-k trace) (list 'recur-on-noun-with-hint gates err-k trace))
(define (recur-on-scry-gate-closure gates) (list 'recur-on-scry-gate gates))

(define empty-k (list 'empty-k))

(define (nock-cons-k-1 recur-on-noun subject d k) (list 'nock-cons-k-1 recur-on-noun subject d k))
(define (nock-cons-k-2 u k) (list 'nock-cons-k-2 u k))
(define (nock-2-k-1 recur-on-noun subject c k) (list 'nock-2-k-1 recur-on-noun subject c k))
(define (nock-2-k-2 recur-on-noun u k) (list 'nock-2-k-2 recur-on-noun u k))
(define (nock-3-k k) (list 'nock-3-k k))
(define (nock-4-k k) (list 'nock-4-k k))
(define (nock-5-k-1 recur-on-noun subject c k) (list 'nock-5-k-1 recur-on-noun subject c k))
(define (nock-5-k-2 u k) (list 'nock-5-k-2 u k))
(define (nock-6-k recur-on-noun subject c d err-k trace k) (list 'nock-6-k recur-on-noun subject c d err-k trace k))
(define (nock-7-k recur-on-noun c k) (list 'nock-7-k recur-on-noun c k))
(define (nock-8-k recur-on-noun subject c k) (list 'nock-8-k recur-on-noun subject c k))
(define (nock-9-k-1 recur-on-noun b err-k trace k) (list 'nock-9-k-1 recur-on-noun b err-k trace k))
(define (nock-9-k-2 recur-on-noun u k) (list 'nock-9-k-2 recur-on-noun u k))
(define (nock-10-k-1 recur-on-noun subject d b err-k trace k) (list 'nock-10-k-1 recur-on-noun subject d b err-k trace k))
(define (nock-10-k-2 u b err-k trace k) (list 'nock-10-k-2 u b err-k trace k))
(define (nock-11-k recur-on-noun-with-hint recur-on-noun subject d b k) (list 'nock-11-k recur-on-noun-with-hint recur-on-noun subject d b k))
(define (nock-12-k-1 recur-on-noun subject path recur-on-scry-gate err-k trace k) (list 'nock-12-k-2 recur-on-noun subject path recur-on-scry-gate err-k trace k))
(define (nock-12-k-2 recur-on-scry-gate u err-k trace k) (list 'nock-12-k-2 recur-on-scry-gate u err-k trace k))
(define (nock-12-k-3 u v^ err-k trace k) (list 'nock-12-k-3 u v^ err-k trace k))
(define (nock-tree-edit-k subtree tree k) (list 'nock-tree-edit-k subtree tree k))
(define (nock-tree-find-k-car k) (list 'nock-tree-find-k-car k))
(define (nock-tree-find-k-cdr k) (list 'nock-tree-find-k-cdr k))
(define (nock-tree-edit-k-car tree k) (list 'nock-tree-edit-k-car tree k))
(define (nock-tree-edit-k-cdr tree k) (list 'nock-tree-edit-k-cdr tree k))

(define (apply-k)
 (let
  [(v ra)]
  (match k
   ([list 'empty-k] v)
   ([list 'nock-cons-k-1 (var recur-on-noun) (var subject) (var d) (var k^)]
    (begin
     (set! k (nock-cons-k-2 v k^))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc d)
     (apply-3)))
   ([list 'nock-cons-k-2 (var u) (var k^)]
    (begin
     (set! k k^)
     (set! ra (cons u v))
     (apply-k)))
   ([list 'nock-2-k-1 (var recur-on-noun) (var subject) (var c) (var k^)]
    (begin
     (set! k (nock-2-k-2 recur-on-noun subject c k^))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc c)
     (apply-3)))
   ([list 'nock-2-k-2 (var recur-on-noun) (var u) (var k^)]
    (begin
     (set! k k^)
     (set! ra recur-on-noun)
     (set! rb u)
     (set! rc v)
     (apply-3)))
   ([list 'nock-3-k (var k^)]
    (if (pair? v)
     (begin
      (set! k k^)
      (set! ra 0)
      (apply-k))
     (begin
      (set! k k^)
      (set! ra 1)
      (apply-k))))
   ([list 'nock-4-k (var k^)]
    (begin
     (set! k k^)
     (set! ra (+ 1 v))
     (apply-k)))
   ([list 'nock-5-k-1 (var recur-on-noun) (var subject) (var c) (var k^)]
    (begin
     (set! k (nock-5-k-2 v k^))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc c)
     (apply-3)))
   ([list 'nock-5-k-2 (var u) (var k^)]
    (if (eqv? u v)
     (begin
      (set! k k^)
      (set! ra 0))
     (begin
      (set! k k^)
      (set! ra 1))))
   ([list 'nock-6-k (var recur-on-noun) (var subject) (var c) (var d) (var err-k) (var trace) (var k^)]
    (if (= 0 v)
     (begin
      (set! k k^) 
      (set! ra recur-on-noun)
      (set! rb subject)
      (set! rc c)
      (apply-3))
     (if (= 1 v)
      (begin
       (set! k k^)
       (set! ra recur-on-noun)
       (set! rb subject)
       (set! rc d)
       (apply-3))
      (begin
       (set! ra (cons 2 trace))
       (err-k)))))
   ([list 'nock-7-k (var recur-on-noun) (var c) (var k^)]
    (begin
     (set! k k^)
     (set! ra recur-on-noun)
     (set! rb c)
     (apply-3)))
   ([list 'nock-8-k (var recur-on-noun) (var subject) (var c) (var k^)]
    (begin
     (set! k k^)
     (set! ra recur-on-noun)
     (set! rb (cons v subject))
     (set! rc c)
     (apply-3)))
   ([list 'nock-9-k-1 (var recur-on-noun) (var b) (var err-k) (var trace) (var k^)]
    (begin
     (set! k (nock-9-k-2 recur-on-noun v k^))
     (set! ra v)
     (set! rb b)
     (set! rc err-k)
     (set! rd trace)
     (nock-tree-find-cps)))
   ([list 'nock-9-k-2 (var recur-on-noun) (var u) (var k^)]
    (begin
     (set! k k^)
     (set! ra recur-on-noun)
     (set! rb u)
     (set! rc v)
     (apply-3)))
   ([list 'nock-10-k-1 (var recur-on-noun) (var subject) (var d) (var b) (var err-k) (var trace) (var k^)]
    (begin
     (set! k (nock-10-k-2 v b err-k trace k^))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc d)
     (apply-3)))
   ([list 'nock-10-k-2 (var u) (var b) (var err-k) (var trace) (var k^)]
    (begin
     (set! k k^)
     (set! ra u)
     (set! rb b)
     (set! rc v)
     (set! rd err-k)
     (set! re trace)
     (nock-tree-edit-cps)))
   ([list 'nock-11-k (var recur-on-noun-with-hint) (var recur-on-noun) (var subject) (var d) (var b) (var k^)]
     (if (member b (list (tas "hunk") (tas "hand") (tas "lose") (tas "mean") (tas "spot")))
      (begin
       (set! k k^)
       (set! ra recur-on-noun-with-hint)
       (set! rb subject)
       (set! rc d)
       (set! rd (cons b v))
       (apply-4))
      (begin
       (set! k k^)
       (set! ra recur-on-noun)
       (set! rb subject)
       (set! rc d)
       (apply-3))))
   ([list 'nock-12-k-1 (var recur-on-noun) (var subject) (var path) (var recur-on-scry-gate) (var err-k) (var trace) (var k^)]
    (begin
     (set! k (nock-12-k-2 recur-on-scry-gate v err-k trace k^))
     (set! ra recur-on-noun)
     (set! rb subject)
     (set! rc path)
     (apply-3)))
   ([list 'nock-12-k-2 (var recur-on-scry-gate) (var u) (var err-k) (var trace) (var k^)]
    (begin
     (set! k (nock-12-k-3 u v err-k trace k^))
     (set! ra recur-on-scry-gate)
     (set! rb u)
     (set! rc v)
     (apply-3)))
   ([list 'nock-12-k-3 (var u) (var v^) (var err-k) (var trace) (var k^)]
    (if (equal? 0 v)
     ; ~
     (begin
      (set! ra (cons 1 v))
      (err-k))
     (if (equal? 0 (cdr v))
      ; [~ ~]
      (begin
       (set! ra (cons 2 (cons (cons (tas "hunk") (cons u v^)) trace)))
       (err-k))
      (begin
       (set! k k^)
       (set! ra (cdr (cdr v)))))))
   ([list 'nock-tree-find-k-car (var k^)]
    (begin
     (set! k k^)
     (set! ra (car v))
     (apply-k)))
   ([list 'nock-tree-find-k-cdr (var k^)] (apply-k k (cdr v)))
   ([list 'nock-tree-edit-k (var subtree) (var tree) (var k)]
    (nock-tree-edit-reversed-cps subtree v tree k))
   ([list 'nock-tree-edit-k-car (var tree) (var k)] (apply-k k (cons v (cdr tree))))
   ([list 'nock-tree-edit-k-cdr (var tree) (var k)] (apply-k k (cons (car tree) v)))
   ([var k^] #:when (procedure? k^) (k^ v))))

(define (apply-3 k^ u v w) 
  (match k^
   ([list 'recur-on-noun (var gates) (var err-k) (var trace)]
    (nock-noun-cps u v gates err-k trace w))
   ([list 'recur-on-scry-gate (var gates)]
    (let*
     [(gate (car (car gates)))
      (err-k (car (cdr (car gates))))
      (trace (cdr (cdr (car gates))))
      (gates (cdr gates))
      (core (cons (car gate) (cons (cons u v) (cdr (cdr gate)))))]
     (nock-noun-cps core (car core) gates err-k trace w)))
   ([var k^] #:when (procedure? k^) (k^ u v w))))

(define (apply-4 k^ u v w x)
 (match k^
  ([list 'recur-on-noun-with-hint (var gates) (var err-k) (var trace)]
   (nock-noun-cps u v gates err-k (cons w trace) x)) 
  ([var k^] #:when (procedure? k^) (k^ u v w x))))

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

(define (test-err-k err)
 (printf "Error: ~v" err)
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
