#lang racket

(require rackunit)

;; This pass optimizes the nock-tree-edit (and nock-tree-find) functions by reversing the atom
;; passed as an address

(define (nock-noun subject formula gates err-k trace)
 (match formula
  ([cons (cons (var b) (var c)) (var d)]
   (cons (nock-noun subject (cons b c) gates err-k trace) (nock-noun subject d gates err-k trace)))
  ([cons 0 (var b)]
   (nock-tree-find subject b err-k trace))
  ([cons 1 (var b)]
   b)
  ([cons 2 (cons (var b) (var c))]
   (nock-noun (nock-noun subject b gates err-k trace) (nock-noun subject c gates err-k trace) gates err-k trace))
  ([cons 3 (var b)]
   (if (pair? (nock-noun subject b gates err-k trace)) 0 1))
  ([cons 4 (var b)]
   (+ 1 (nock-noun subject b gates err-k trace)))
  ([cons 5 (cons (var b) (var c))]
   (if (eqv? (nock-noun subject b gates err-k trace) (nock-noun subject c gates err-k trace)) 0 1))
  ([cons 6 (cons (var b) (cons (var c) (var d)))]
   (let
    [(test (nock-noun subject b gates err-k trace))]
   (if (= 0 test)
    (nock-noun subject c gates err-k trace)
    (if (= 1 test)
     (nock-noun subject d gates err-k trace)
     (err-k (cons 2 trace))))))
  ([cons 7 (cons (var b) (var c))]
   (nock-noun (nock-noun subject b gates err-k trace) c gates err-k trace))
  ([cons 8 (cons (var b) (var c))]
   (nock-noun (cons (nock-noun subject b gates err-k trace) subject) c gates err-k trace))
  ([cons 9 (cons (var b) (var c))]
   (let
    ([core (nock-noun subject c gates err-k trace)])
    (nock-noun core (nock-tree-find core b err-k trace) gates err-k trace)))
  ([cons 10 (cons (cons (var b) (var c)) (var d))]
   (nock-tree-edit (nock-noun subject c gates err-k trace) b (nock-noun subject d gates err-k trace) err-k trace))
  ([cons 11 (cons (cons (var b) (var c)) (var d))]
   (let
    [(clue (nock-noun subject c))]
     (if (member b (list (tas "hunk") (tas "hand") (tas "lose") (tas "mean") (tas "spot")))
      (nock-noun subject d gates err-k (cons (cons b clue) trace))
      (nock-noun subject d gates err-k trace))))
  ([cons 11 (cons (var b) (var c))]
   (nock-noun subject c gates err-k trace))
  ([cons 12 (cons (var ref) (var path))]
   (let*
    [(ref (nock-noun subject ref gates err-k trace))
     (path (nock-noun subject path gates err-k trace))
     (gate (car (car gates)))
     (outer-err-k err-k)
     (err-k (car (cdr (car gates))))
     (outer-trace trace)
     (trace (cdr (cdr (car gates))))
     (gates (cdr gates))
     (core (cons (car gate) (cons (cons ref path) (cdr (cdr gate)))))
     (result (nock-noun core (car core) gates err-k trace))]
    (if (equal? 0 result)
     ; ~
     (outer-err-k (cons 1 (cdr path)))
     (if (equal? 0 (car result))
      (outer-err-k (cons 2 (cons (cons (tas "hunk") (cons ref path)) outer-trace))) 
      (cdr (cdr result))))))))

(define (reverse-address address) (reverse-address-acc address 1))

(define (reverse-address-acc address reversed)
 (if (= address 1)
  reversed
  (reverse-address-acc (arithmetic-shift address -1) (bitwise-ior (arithmetic-shift reversed 1) (bitwise-and address 1)))))

(define (nock-tree-find-reversed tree reversed)
 (if (= reversed 1)
  tree
  (if (even? reversed)
   (nock-tree-find-reversed (car tree) (arithmetic-shift reversed -1))
   (nock-tree-find-reversed (cdr tree) (arithmetic-shift reversed -1)))))

(define (nock-tree-find tree address err-k trace)
 (if (= address 0)
  (err-k (cons 2 trace))
  (nock-tree-find-reversed tree (reverse-address address))))

(define (nock-tree-edit-reversed subtree reversed tree)
 (if (= reversed 1)
  subtree
  (if (even? reversed)
   (cons (nock-tree-edit-reversed subtree (arithmetic-shift reversed -1) (car tree)) (cdr tree))
   (cons (car tree) (nock-tree-edit-reversed subtree (arithmetic-shift reversed -1) (cdr tree))))))

; # operator in nock spec: tree editing
(define (nock-tree-edit subtree address tree err-k trace)
 (if (= address 0)
  (err-k (cons 2 trace))
  (nock-tree-edit-reversed subtree (reverse-address address) tree)))

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
