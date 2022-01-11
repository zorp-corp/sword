#lang racket

(require rackunit)

;; This is a naive, direct, structurally recursive interpretation of Nock according
;; to the nock specification, with cells represented as cons cells and atoms represented
;; as Racket natural numbers.

(define (nock-noun subject formula)
 (match formula
  ([cons (cons (var b) (var c)) (var d)]
   (cons (nock-noun subject (cons b c)) (nock-noun subject d)))
  ([cons 0 (var b)]
   (nock-tree-find subject b))
  ([cons 1 (var b)]
   b)
  ([cons 2 (cons (var b) (var c))]
   (nock-noun (nock-noun subject b) (nock-noun subject c)))
  ([cons 3 (var b)]
   (if (pair? (nock-noun subject b)) 0 1))
  ([cons 4 (var b)]
   (+ 1 (nock-noun subject b)))
  ([cons 5 (cons (var b) (var c))]
   (if (eqv? (nock-noun subject b) (nock-noun subject c)) 0 1))
  ([cons 6 (cons (var b) (cons (var c) (var d)))]
   (if (= 0 (nock-noun subject b))
    (nock-noun subject c)
    (nock-noun subject d)))
  ([cons 7 (cons (var b) (var c))]
   (nock-noun (nock-noun subject b) c))
  ([cons 8 (cons (var b) (var c))]
   (nock-noun (cons (nock-noun subject b) subject) c))
  ([cons 9 (cons (var b) (var c))]
   (let
    ([core (nock-noun subject c)])
    (nock-noun core (nock-tree-find core b))))
  ([cons 10 (cons (cons (var b) (var c)) (var d))]
   (nock-tree-edit (nock-noun subject c) b (nock-noun subject d)))
  ([cons 11 (cons (cons (var b) (var c)) (var d))]
   (let
    ([_ (nock-noun subject c)])
     (nock-noun subject d)))
  ([cons 11 (cons (var b) (var c))]
   (nock-noun subject c))))

(define nock-tree-find
  (lambda (tree address)
    (if (= address 1) tree
     (if (even? address)
      (car (nock-tree-find tree (quotient address 2)))
      (cdr (nock-tree-find tree (quotient address 2)))))))

; # operator in nock spec: tree editing
(define nock-tree-edit
  (lambda (subtree address tree)
    (if (= address 1) subtree
     (if (even? address)
      (nock-tree-edit (cons subtree (nock-tree-find tree (+ address 1))) (quotient address 2) tree)
      (nock-tree-edit (cons (nock-tree-find tree (- address 1)) subtree) (quotient address 2) tree)))))

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

(check-equal? (nock-noun test-tree (get-0 nock-here)) test-tree "tree address 1")
(check-equal? (nock-noun test-tree (get-0 (nock-car nock-here))) (car test-tree) "tree address 2")
(check-equal? (nock-noun test-tree (get-0 (nock-cdr nock-here))) (cdr test-tree) "tree address 3")
(check-equal? (nock-noun test-tree (get-0 (nock-car (nock-car nock-here)))) (car (car test-tree)) "tree address 4")
(check-equal? (nock-noun test-tree (get-0 (nock-cdr (nock-car nock-here)))) (cdr (car test-tree)) "tree address 5")
(check-equal? (nock-noun 0 (literal-1 test-tree)) test-tree "literal")
(check-equal? (nock-noun 0 (eval-2 (literal-1 test-tree) (literal-1 (get-0 2)))) (car test-tree) "eval")
(check-equal? (nock-noun test-tree (cell?-3 (get-0 1))) lootru "test cell true")
(check-equal? (nock-noun test-tree (cell?-3 (get-0 3))) loofal "test cell false")
(check-equal? (nock-noun 0 (inc-4 (literal-1 0))) 1 "increment")
(check-equal? (nock-noun test-tree (=-5 (literal-1 test-tree) (get-0 1))) lootru "test equals true")
(check-equal? (nock-noun test-tree (=-5 (literal-1 test-tree) (get-0 2))) loofal "test equals false")
(check-equal? (nock-noun test-tree (if-6 (literal-1 lootru) (literal-1 5) (get-0 100))) 5 "test if tru")
(check-equal? (nock-noun test-tree (if-6 (literal-1 loofal) (get-0 100) (literal-1 5))) 5 "test if false")
(check-equal? (nock-noun 0 (compose-7 (literal-1 test-tree) (get-0 2))) (car test-tree) "test compose")
(check-equal? (nock-noun 0 (declare-8 (literal-1 test-tree) (get-0 2))) test-tree "test declare")
(check-equal? (nock-noun 0 (call-9 (nock-car nock-here) (literal-1 decrement-4-core))) 3 "test call")
(check-equal? (nock-noun 0 (update-10 (nock-cdr nock-here) (literal-1 (cons 6 7)) (literal-1 test-tree))) (cons (cons 4 5) (cons 6 7)) "test update")
(check-equal? (nock-noun 0 (call-9 (nock-car nock-here) (update-10 (nock-car (nock-cdr nock-here)) (literal-1 8) (literal-1 decrement-4-core)))) 7 "test slam i.e. update sample and call")
; test 11 static and dynamic
