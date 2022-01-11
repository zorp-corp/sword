#lang racket

(require rackunit)

;; This interpreter builds on (b) by adding an explicit exception-handling mechanism
;; in the form of an error continuation and a trace.
;;
;; Traces are updated by specific static hints for nock 11 paired with specific dynamic hints.

(define (nock-noun subject formula gates err-k trace)
 (let*
  [(recur-on-noun (lambda (subject formula)
    (nock-noun subject formula gates err-k trace)))
   (recur-on-noun-with-hint (lambda (subject formula hint)
    (nock-noun subject formula gates err-k (cons hint trace))))
   (recur-on-scry-gate (lambda (ref path)
    (let*
     [(gate (car (car gates)))
      (err-k (car (cdr (car gates))))
      (trace (cdr (cdr (car gates))))
      (gates (cdr gates))
      (core (cons (car gate) (cons (cons ref path) (cdr (cdr gate)))))]
     (nock-noun core (car core) gates err-k trace))))]
  (match formula
   ([cons (cons (var b) (var c)) (var d)]
    (cons (recur-on-noun subject (cons b c)) (recur-on-noun subject d)))
   ([cons 0 (var b)]
    (nock-tree-find subject b err-k trace))
   ([cons 1 (var b)]
    b)
   ([cons 2 (cons (var b) (var c))]
    (recur-on-noun (recur-on-noun subject b) (recur-on-noun subject c)))
   ([cons 3 (var b)]
    (if (pair? (recur-on-noun subject b)) 0 1))
   ([cons 4 (var b)]
    (+ 1 (recur-on-noun subject b)))
   ([cons 5 (cons (var b) (var c))]
    (if (eqv? (recur-on-noun subject b) (recur-on-noun subject c)) 0 1))
   ([cons 6 (cons (var b) (cons (var c) (var d)))]
    (let
     [(test (recur-on-noun subject b))]
    (if (= 0 test)
     (recur-on-noun subject c)
     (if (= 1 test)
      (recur-on-noun subject d)
      (err-k (cons 2 trace))))))
   ([cons 7 (cons (var b) (var c))]
    (recur-on-noun (recur-on-noun subject b) c))
   ([cons 8 (cons (var b) (var c))]
    (recur-on-noun (cons (recur-on-noun subject b) subject) c))
   ([cons 9 (cons (var b) (var c))]
    (let
     ([core (recur-on-noun subject c)])
     (recur-on-noun core (nock-tree-find core b err-k trace))))
   ([cons 10 (cons (cons (var b) (var c)) (var d))]
    (nock-tree-edit (recur-on-noun subject c) b (recur-on-noun subject d) err-k trace))
   ([cons 11 (cons (cons (var b) (var c)) (var d))]
    (let
     [(clue (recur-on-noun subject c))]
      (if (member b (list (tas "hunk") (tas "hand") (tas "lose") (tas "mean") (tas "spot")))
       (recur-on-noun-with-hint subject d (cons b clue))
       (recur-on-noun subject d))))
   ([cons 11 (cons (var b) (var c))]
    (recur-on-noun subject c))
   ([cons 12 (cons (var ref) (var path))]
    (let
     [(result (recur-on-scry-gate (recur-on-noun subject ref) (recur-on-noun subject path)))]
     (if (equal? 0 result)
      ; ~
      (err-k (cons 1 (cdr path)))
      (if (equal? 0 (car result))
       (err-k (cons 2 (cons (cons (tas "hunk") (cons ref path)) trace))) 
       (cdr (cdr result)))))))))

(define nock-tree-find
  (lambda (tree address err-k trace)
    (if (= address 0)
     (err-k (cons 2 trace))
     (if (= address 1) tree
      (if (even? address)
       (car (nock-tree-find tree (quotient address 2) err-k trace))
       (cdr (nock-tree-find tree (quotient address 2) err-k trace)))))))

; # operator in nock spec: tree editing
(define nock-tree-edit
  (lambda (subtree address tree err-k trace)
    (if (= address 0)
     (err-k (cons 2 trace))
     (if (= address 1) subtree
      (if (even? address)
       (nock-tree-edit (cons subtree (nock-tree-find tree (+ address 1) err-k trace)) (quotient address 2) tree err-k trace)
       (nock-tree-edit (cons (nock-tree-find tree (- address 1) err-k trace) subtree) (quotient address 2) tree err-k trace))))))

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
