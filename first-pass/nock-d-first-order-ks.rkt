#lang racket
(define (nock-eval nock k)
    (match nock
      ; Cell at head of formula, evaluate both formulae and return as cell
      ([cons (var a) (cons (cons (var b) (var c)) (var d))]
       (nock-eval (cons a (cons b c)) (nock-cell-1 a d k)))
      ; literal tree index into subject
      ([cons (var a) (cons 0 b)] (nock-tree-find a b k))
      ; literal
      ([cons _ (cons 1 b)] (apply-k k b))
      ; evaluate b with subject a
      ; evaluate c with subject a
      ; evaluate result of c with result of b as subject
      ([cons (var a) (cons 2 (cons (var b) (var c)))]
        (nock-eval (cons a b) (nock-2-1 a c k)))
      ; evaluate b with a as subject, return 0 if cell and 1 if atom
      ([cons (var a) (cons 3 (var b))]
       (nock-eval (cons a b) (nock-3-1 k)))
      ; evaluate b with a as subject, return atom-value of result incremented by 1
      ([cons (var a) (cons 4 (var b))]
       (nock-eval (cons a b) (nock-4-1 k)))
      ; Test for structural equality between 2 nouns
      ([cons (var a) (cons 5 (cons (var b) (var c)))]
       (nock-eval (cons a b) (nock-5-1 a c k)))
      ; Here we deviate from slavishly following the spec to a symbolically-evaluated reduction of the spec
      ; Evaluate b with subject a. If 0 then evaluate c with subject a, else evaluate d with subject a
      ;
      ; Note that in the CPS transformation we do *not* evaluate the branches first and then
      ; test for which result to feed the continuation.
      ([cons (var a) (cons 6 (cons (var b) (cons (var c) (var d))))]
       (nock-eval (cons a b) (nock-6-1 a c d k)))
      ; Evaluate b with subject a, use result as subject and evaluate c
      ([cons (var a) (cons 7 (cons (var b) (var c)))]
       (nock-eval (cons a b)) (nock-7-1 c k))
      ; Evaluate b with subject a, add result to subject a and evaluate c
      ([cons (var a) (cons 8 (cons (var b) (var c)))]
       (nock-eval (cons a b) (nock-8-1 a c k)))
      ; Evaluate c with subject a to get a core, look up address b in the core and use it as a formula with the core
      ; as the subject
      ([cons (var a) (cons 9 (cons (var b) (var c)))]
       (nock-eval (cons a c) (nock-9-1 b k)))
      ; Replace the address specified by b in the result of c with subject a with the result of d with subject a
      ([cons (var a) (cons 10 (cons (cons (var b) (var c)) (var d)))]
       (nock-eval (cons a c) (nock-10-1 a b d k)))
      ; Cell hint, compute then discard
      ([cons (var a) (cons 11 (cons (cons (var b) (var c)) (var d)))]
       (nock-eval (cons b c) (nock-11-1 a d k)))  
      ; Atom hint, discard
      ([cons (var a) (cons 11 (cons _ (var c)))]
       (nock-eval a c k))
      ; Atom, return
      ([var a] (apply-k k a))))

; continuation definitions for the cell-formula case
(define (nock-cell-1 a d k) (list 'nock-cell-1 a d k))

(define (nock-cell-2 abc k) (list 'nock-cell-2 abc k))

; continuation definitions for nock 2
(define (nock-2-1 a c k) (list 'nock-2-1 a c k))

(define (nock-2-2 ab k) (list 'nock-2-2 ab k))

; continuation definition for nock 3
(define (nock-3-1 k) (list 'nock-3-1 k))

; continuation definition for nock 4
(define (nock-4-1 k) (list 'nock-4-1 k))

; continuation definitions for nock 5
(define (nock-5-1 a c k) (list 'nock-5-1 a c k))

(define (nock-5-2 ab k)
  (lambda (ac) (if (= ab ac) (apply-k k 0) (apply-k k 1))))

; continuation definitions for nock 6
(define (nock-6-1 a c d k) (list 'nock-6-1 a c d k))

; continuation definitions for nock 7
(define (nock-7-1 c k) (list 'nock-7-1 c k))

; continuation definitions for nock 8
(define (nock-8-1 a c k) (list 'nock-8-1 a c k))

; continuation definitions for nock 9
(define (nock-9-1 b k) (list 'nock-9-1 b k))

(define (nock-9-2 core k) (list 'nock-9-2 core k))

; continuation definitions for nock 10
(define (nock-10-1 a b d k) '(nock-10-1 a b d k))

(define (nock-10-2 ac b k) '(nock-11-1 ac b k))

; continuation definitions for nock 11
(define (nock-11-1 a d k)
  (lambda (_) (nock-eval (cons a d) k)))

; / operator in nock spec: tree addressing
(define (nock-tree-find tree address k)
    (if (= address 1) (apply-k k tree)
        (nock-tree-find (if (even? address) (car tree) (cdr tree)) (quotient address 2) k)))

; # operator in nock spec: tree editing
(define (nock-tree-edit subtree address tree k)
    (if (= address 1) (apply-k k subtree)
      (if (even? address)
          (nock-tree-edit subtree (quotient address 2) (car tree) (nock-tree-edit-even tree k))
          (nock-tree-edit subtree (quotient address 2) (cdr tree) (nock-tree-edit-odd tree k)))))

(define (nock-tree-edit-even tree k) (list 'nock-tree-edit-even tree k))
(define (nock-tree-edit-odd tree k) (list 'nock-tree-edit-odd tree k))

(define (apply-k k^ v)
 (if (procedure? k^) (k^ v)
  (match k^
   ([list 'nock-cell-1 (var a) (var d) (var k)] (nock-eval (cons a d) (nock-cell-2 v k)))
   ([list 'nock-cell-2 (var abc) (var k)] (apply-k k (cons abc v)))
   ([list 'nock-2-1 (var a) (var c) (var k)] (nock-eval (cons a c) (nock-2-2 v k)))
   ([list 'nock-2-2 (var ab) (var k)] (nock-eval (cons ab v) k))
   ([list 'nock-3-1 (var k)] (if (pair? v) (apply-k k 0) (apply-k k 1)))
   ([list 'nock-4-1 (var k)] (apply-k k (+ 1 v)))
   ([list 'nock-5-1 (var a) (var c) (var k)] (nock-eval (cons a c) (nock-5-2 v k)))
   ([list 'nock-5-2 (var ab) (var k)] (if (= ab v) (apply-k k 0) (apply-k k 1)))
   ([list 'nock-6-1 (var a) (var c) (var d) (var k)] (if (= v 0) (nock-eval (cons a c) k) (nock-eval (cons a d) k)))
   ([list 'nock-7-1 (var c) (var k)] (nock-eval (cons v c) k))
   ([list 'nock-8-1 (var a) (var c) (var k)] (nock-eval (cons (cons v a) c) k))
   ([list 'nock-9-1 (var b) (var k)] (nock-tree-find v b (nock-9-2 v k)))
   ([list 'nock-9-2 (var core) (var k)] (nock-eval (cons core v) k))
   ([list 'nock-10-1 (var a) (var b) (var d) (var k)] (nock-eval (cons a d) (nock-10-2 v b k)))
   ([list 'nock-10-2 (var ac) (var b) (var k)] (nock-tree-edit ac b v k))
   ([list 'nock-11-1 (var a) (var d) (var k)] (nock-eval (cons a d) k))
   ([list 'nock-tree-edit-even (var tree) (var k)] (k (cons v (cdr tree))))
   ([list 'nock-tree-edit-odd (var tree) (var k)] (k (cons (car tree) v))))))
   
    
;;;;; Examples
(define tree23 (cons 2 3))
(define (get x) (cons 0 x))
(define (const x) (cons 1 x))
(define (call x y) (cons 9 (cons x y)))
(define (declare x y) (cons 8 (cons x y)))

(nock-eval (cons tree23 (get 1)) identity)
(nock-eval (cons 4 (declare (const tree23) (get 4))) identity)
(nock-eval (cons (declare (const tree23) (get 4)) (call 1 (get 1))) identity)
