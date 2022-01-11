#lang racket
; registers
(define nock 0)
(define tree 0)
(define address 0)
(define subtree 0)
(define k 0)
(define k^ 0)
(define v 0)

(define (nock-eval)
    (match nock
      ; Cell at head of formula, evaluate both formulae and return as cell
      ([cons (var a) (cons (cons (var b) (var c)) (var d))]
       (begin
        (set! nock (cons a (cons b c)))
        (set! k (nock-cell-1 a d k))
        (nock-eval)))
      ; literal tree index into subject
      ([cons (var a) (cons 0 b)]
       (begin
        (set! tree a)
        (set! address b)
        (nock-tree-find)))
      ; literal
      ([cons _ (cons 1 b)]
       (begin
        (set! k^ k)
        (set! v b)
        (apply-k)))
      ; evaluate b with subject a
      ; evaluate c with subject a
      ; evaluate result of c with result of b as subject
      ([cons (var a) (cons 2 (cons (var b) (var c)))]
       (begin
        (set! nock (cons a b))
        (set! k (nock-2-1 a c k))
        (nock-eval)))
      ; evaluate b with a as subject, return 0 if cell and 1 if atom
      ([cons (var a) (cons 3 (var b))]
       (begin
        (set! nock (cons a b))
        (set! k (nock-3-1 k))
        (nock-eval)))
      ; evaluate b with a as subject, return atom-value of result incremented by 1
      ([cons (var a) (cons 4 (var b))]
       (begin
        (set! nock (cons a b))
        (set! k (nock-4-1 k))
        (nock-eval)))
      ; Test for structural equality between 2 nouns
      ([cons (var a) (cons 5 (cons (var b) (var c)))]
       (begin
        (set! nock (cons a b))
        (set! k (nock-5-1 a c k))
        (nock-eval)))
      ; Here we deviate from slavishly following the spec to a symbolically-evaluated reduction of the spec
      ; Evaluate b with subject a. If 0 then evaluate c with subject a, else evaluate d with subject a
      ;
      ; Note that in the CPS transformation we do *not* evaluate the branches first and then
      ; test for which result to feed the continuation.
      ([cons (var a) (cons 6 (cons (var b) (cons (var c) (var d))))]
       (begin
        (set! nock (cons a b))
        (set! k (nock-6-1 a c d k))
        (nock-eval)))
      ; Evaluate b with subject a, use result as subject and evaluate c
      ([cons (var a) (cons 7 (cons (var b) (var c)))]
       (begin
        (set! nock (cons a b))
        (set! k (nock-7-1 c k))
        (nock-eval)))
      ; Evaluate b with subject a, add result to subject a and evaluate c
      ([cons (var a) (cons 8 (cons (var b) (var c)))]
       (begin
        (set! nock (cons a b))
        (set! k (nock-8-1 a c k))
        (nock-eval)))
      ; Evaluate c with subject a to get a core, look up address b in the core and use it as a formula with the core
      ; as the subject
      ([cons (var a) (cons 9 (cons (var b) (var c)))]
       (begin
        (set! nock (cons a c))
        (set! k (nock-9-1 b k))
        (nock-eval)))
      ; Replace the address specified by b in the result of c with subject a with the result of d with subject a
      ([cons (var a) (cons 10 (cons (cons (var b) (var c)) (var d)))]
       (begin
        (set! nock (cons a c))
        (set! k (nock-10-1 a b d k))
        (nock-eval)))
      ; Cell hint, compute then discard
      ([cons (var a) (cons 11 (cons (cons (var b) (var c)) (var d)))]
       (begin
        (set! nock (cons a c))
        (set! k (nock-10-1 a d k))
        (nock-eval)))  
      ; Atom hint, discard
      ([cons (var a) (cons 11 (cons _ (var c)))]
       (begin
        (set! nock (cons a c))
        (set! k k))
        (nock-eval))
      ; Atom, return
      ([var a]
       (begin
        (set! v a)
        (set! k^ k)
        (apply-k)))))

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

(define (nock-5-2 ab k) (list 'nock-5-2 ab k))

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
; address: the atom address into the tree
; tree: the tree to address
(define (nock-tree-find)
 (if (= address 1)
  (begin
   (set! v tree)
   (set! k k)
   (apply-k))
  (begin
   (if (even? address)
    (set! tree (car tree))
    (set! tree (cdr tree)))
   (set! address (quotient address 2))
   (set! k k)
   (nock-tree-find))))

; # operator in nock spec: tree editing
; subtree: tree to insert
; address: address of subtree to replace
; tree: tree to edit
(define (nock-tree-edit subtree address tree k)
 (if (= address 1)
  (begin
   (set! k k)
   (set! v subtree)
   (apply-k))
  (begin
   (set! subtree subtree)
   (set! address (quotient address 2))
   (if (even? address)
    (begin
     (set! k (nock-tree-edit-even tree k))
     (set! tree (car tree)))
    (begin
     (set! k (nock-tree-edit-odd tree k))
     (set! tree (cdr tree))))
   (nock-tree-edit))))

(define (nock-tree-edit-even tree k) (list 'nock-tree-edit-even tree k))
(define (nock-tree-edit-odd tree k) (list 'nock-tree-edit-odd tree k))

(define (apply-k k^ v)
 (if (procedure? k^) (k^ v)
  (match k^
   ([list 'nock-cell-1 (var a) (var d) (var k)]
    (begin
     (set! nock (cons a d))
     (set! k (nock-cell-2 v k))
     (nock-eval)))
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
