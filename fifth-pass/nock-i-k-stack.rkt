#lang racket

(require rackunit)

;; This interpreter converts the implicit stack of continuations from (j)
;; (represented by every continuation closure other than empty-k receiving
;; the current continuation as its last argument) into an explicit stack.
;;
;; It removes the continuation variable from all continuation closures
;; and adds a `push-k` operation to push a continuation closure onto the stack.
;;
;; The apply-k function now functions explicitly as a stack-popping operation.

(define stack '())
(define (push-k k)
 (set! stack (cons k stack))) 

(define (nock-noun subject formula gates err-k trace)
 (begin
  (push-k empty-k)
  (nock-noun-cps subject formula gates err-k trace)))

(define (nock-noun-cps subject formula gates err-k trace)
 (match formula
  ([cons (cons (var b) (var c)) (var d)]
   (begin
    (push-k (nock-cons-k-1 subject d gates err-k trace))
    (nock-noun-cps subject (cons b c) gates err-k trace)))
  ([cons 0 (var b)]
   (nock-tree-find subject b err-k trace))
  ([cons 1 (var b)]
   (apply-k b))
  ([cons 2 (cons (var b) (var c))]
   (begin
    (push-k (nock-2-k-1 subject c gates err-k trace))
    (nock-noun-cps subject b gates err-k trace)))
  ([cons 3 (var b)]
   (begin
    (push-k nock-3-k)
    (nock-noun-cps subject b gates err-k trace)))
  ([cons 4 (var b)]
   (begin
    (push-k nock-4-k)
    (nock-noun-cps subject b gates err-k trace)))
  ([cons 5 (cons (var b) (var c))]
   (begin
    (push-k (nock-5-k-1 subject c gates err-k trace))
    (nock-noun-cps subject b gates err-k trace)))
  ([cons 6 (cons (var b) (cons (var c) (var d)))]
   (begin
    (push-k (nock-6-k subject c d gates err-k trace))
    (nock-noun-cps subject b gates err-k trace)))
  ([cons 7 (cons (var b) (var c))]
   (begin
    (push-k (nock-7-k c gates err-k trace))
    (nock-noun-cps subject b gates err-k trace)))
  ([cons 8 (cons (var b) (var c))]
   (begin
    (push-k (nock-8-k subject c gates err-k trace))
    (nock-noun-cps subject b gates err-k trace)))
  ([cons 9 (cons (var b) (var c))]
   (begin
    (push-k (nock-9-k-1 b gates err-k trace))
    (nock-noun-cps subject c gates err-k trace)))
  ([cons 10 (cons (cons (var b) (var c)) (var d))]
   (begin
    (push-k (nock-10-k-1 subject d b gates err-k trace))
    (nock-noun-cps subject c gates err-k trace)))
  ([cons 11 (cons (cons (var b) (var c)) (var d))]
   (begin
    (push-k (nock-11-k subject b d gates err-k trace))
    (nock-noun-cps subject c gates err-k trace)))
  ([cons 11 (cons (var b) (var c))]
   (nock-noun-cps subject c gates err-k trace))
  ([cons 12 (cons (var ref) (var path))]
   (begin
    (push-k (nock-12-k-1 subject path gates err-k trace))
    (nock-noun-cps subject ref gates err-k trace)))))

(define (reverse-address address) (reverse-address-acc address 1))

(define (reverse-address-acc address reversed)
 (if (= address 1)
  (apply-k reversed)
  (reverse-address-acc (arithmetic-shift address -1) (bitwise-ior (arithmetic-shift reversed 1) (bitwise-and address 1)))))

(define (nock-tree-find-reversed tree reversed)
 (if (= reversed 1)
  (apply-k tree)
  (if (even? reversed)
   (nock-tree-find-reversed (car tree) (arithmetic-shift reversed -1))
   (nock-tree-find-reversed (cdr tree) (arithmetic-shift reversed -1)))))

(define (nock-tree-find tree address err-k trace)
 (if (= address 0)
  (apply-err-k err-k (cons 2 trace))
  (begin
   (push-k (nock-tree-find-k tree))
   (reverse-address address))))

(define (nock-tree-edit-reversed subtree reversed tree)
 (if (= reversed 1)
  (apply-k subtree)
  (if (even? reversed)
   (begin
    (push-k (nock-tree-edit-car-k tree))
    (nock-tree-edit-reversed subtree (arithmetic-shift reversed -1) (car tree)))
   (begin
    (push-k (nock-tree-edit-cdr-k tree))
    (nock-tree-edit-reversed subtree (arithmetic-shift reversed -1) (cdr tree))))))

; # operator in nock spec: tree editing
(define (nock-tree-edit subtree address tree err-k trace)
 (if (= address 0)
  (apply-err-k err-k (cons 2 trace))
  (begin
   (push-k (nock-tree-edit-k subtree tree))
   (reverse-address address))))

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

(define (apply-k x)
 (let
  [(k (car stack))]
  (begin
   (set! stack (cdr stack))
   (match k       
    ([list 'empty-k] x)
    ([list 'nock-cons-k-1 (var subject) (var d) (var gates) (var err-k) (var trace)]
     (begin
      (push-k (nock-cons-k-2 x))
      (nock-noun-cps subject d gates err-k trace)))
    ([list 'nock-cons-k-2 (var u) (var k^)]
     (apply-k (cons u x)))
    ([list 'nock-2-k-1 (var subject) (var c) (var gates) (var err-k) (var trace)]
     (begin
      (push-k (nock-2-k-2 x gates err-k trace))
      (nock-noun-cps subject c gates err-k trace)))
    ([list 'nock-2-k-2 (var u) (var gates) (var err-k) (var trace)]
     (nock-noun-cps u x gates err-k trace))
    ([list 'nock-3-k]
     (if (pair? x) (apply-k 0) (apply-k 1)))
    ([list 'nock-4-k]
     (apply-k (+ 1 x)))
    ([list 'nock-5-k-1 (var subject) (var c) (var gates) (var err-k) (var trace)]
     (begin
      (push-k (nock-5-k-2 x))
      (nock-noun-cps subject c gates err-k trace)))
    ([list 'nock-5-k-2 (var u)]
     (if (eqv? u x) (apply-k 0) (apply-k 1)))
    ([list 'nock-6-k (var subject) (var c) (var d) (var gates) (var err-k) (var trace)]
     (if (= 0 x)
      (nock-noun-cps subject c gates err-k trace)
       (if (= 1 x)
        (nock-noun-cps subject d gates err-k trace)
        (apply-err-k err-k (cons 2 trace)))))
    ([list 'nock-7-k (var c) (var gates) (var err-k) (var trace)]
     (nock-noun-cps x c gates err-k trace))
    ([list 'nock-8-k (var subject) (var c) (var gates) (var err-k) (var trace)]
     (nock-noun-cps (cons x subject) c gates err-k trace))
    ([list 'nock-9-k-1 (var b) (var gates) (var err-k) (var trace)]
     (begin
      (push-k (nock-9-k-2 x gates err-k trace))
      (nock-tree-find x b err-k trace)))
    ([list 'nock-9-k-2 (var u) (var gates) (var err-k) (var trace)]
     (nock-noun-cps u x gates err-k trace))
    ([list 'nock-10-k-1 (var subject) (var d) (var b) (var gates) (var err-k) (var trace)]
     (begin
      (push-k (nock-10-k-2 x b err-k trace))
      (nock-noun-cps subject d gates err-k trace)))
    ([list 'nock-10-k-2 (var u) (var b) (var err-k) (var trace)]
     (nock-tree-edit u b x err-k trace))
    ([list 'nock-11-k (var subject) (var b) (var d) (var gates) (var err-k) (var trace)]
     (if (member b (list (tas "hunk") (tas "hand") (tas "lose") (tas "mean") (tas "spot")))
      (nock-noun-cps subject d gates err-k (cons (cons b x) trace))
      (nock-noun-cps subject d gates err-k trace)))
    ([list 'nock-12-k-1 (var subject) (var path) (var gates) (var err-k) (var trace)]
     (begin
      (push-k (nock-12-k-2 gates err-k trace x))
      (nock-noun-cps subject path gates err-k trace)))
    ([list 'nock-12-k-2 (var gates) (var err-k) (var trace) (var u)]
      (let*
       [(gate (car (car gates)))
        (outer-err-k err-k)
        (err-k (car (cdr (car gates))))
        (outer-trace trace)
        (trace (cdr (cdr (car gates))))
        (gates (cdr gates))
        (core (cons (car gate) (cons (cons u x) (cdr (cdr gate)))))]
       (begin
        (push-k (nock-12-k-3 u x outer-err-k outer-trace))
        (nock-noun-cps core (car core) gates err-k trace))))
    ([list 'nock-12-k-3 (var u) (var v) (var outer-err-k) (var outer-trace)]
     (if (equal? 0 x)
      ; ~
      (outer-err-k (cons 1 (cdr v)))
      (if (equal? 0 (car x))
       (outer-err-k (cons 2 (cons (cons (tas "hunk") (cons u v)) outer-trace)))
       (apply-k (cdr (cdr x))))))
    ([list 'nock-tree-edit-car-k (var tree)]
     (apply-k (cons x (cdr tree))))
    ([list 'nock-tree-edit-cdr-k (var tree)]
     (apply-k (cons (car tree) x)))
    ([list 'nock-tree-edit-k (var subtree) (var tree)]
     (nock-tree-edit-reversed subtree x tree))
    ([list 'nock-tree-find-k (var tree)]
     (nock-tree-find-reversed tree x))
    ((var k^) #:when (procedure? k^) (k^ x))))))

(define (apply-err-k err-k err) (err-k err))

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
