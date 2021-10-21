#lang racket
(define nock-eval
  (lambda (nock)
    (match nock
      ; Cell at head of formula, evaluate both formulae and return as cell
      ([cons (var a) (cons (cons (var b) (var c)) (var d))]
       (cons
        (nock-eval (cons a (cons b c)))
        (nock-eval (cons a (cons d)))))
      ; literal tree index into subject
      ([cons (var a) (cons 0 b)] (nock-tree-find a b))
      ; literal
      ([cons _ (cons 1 b)] b)
      ; evaluate b with subject a
      ; evaluate c with subject a
      ; evaluate result of c with result of b as subject
      ([cons (var a) (cons 2 (cons (var b) (var c)))] (nock-eval (cons (nock-eval (cons a b)) (nock-eval (cons a c)))))
      ; evaluate b with a as subject, return
      ([cons (var a) (cons 3 (var b))] (if (pair? (nock-eval (cons a b))) 0 1))
      ([cons (var a) (cons 4 (var b))] (+ 1 (nock-eval (cons a b))))
      ([cons (var a) (cons 5 (cons (var b) (var c)))] (if (eqv? (nock-eval (cons a b) (cons a c))) 0 1))
      ; Here we deviate from slavishly following the spec to a symbolically-evaluated reduction of the spec
      ; Evaluate b with subject a. If 0 then evaluate c with subject a, else evaluate d with subject a
      ([cons (var a) (cons 6 (cons (var b) (cons (var c) (var d))))]
       (let ([bval (nock-eval (cons a b))])
         (if (= bval 0)
             (nock-eval (cons a c))
             (nock-eval (cons a d)))))
      ; Evaluate b with subject a, use result as subject and evaluate c
      ([cons (var a) (cons 7 (cons (var b) (var c)))] (nock-eval (cons (nock-eval (cons a b)) c)))
      ; Evaluate b with subject a, add result to subject a and evaluate c
      ([cons (var a) (cons 8 (cons (var b) (var c)))] (nock-eval (cons (cons (nock-eval (cons a b)) a) c)))
      ; Evaluate c with subject a to get a core, look up address b in the core and use it as a formula with the core
      ; as the subject
      ([cons (var a) (cons 9 (cons (var b) (var c)))]
       (let ([core (nock-eval (cons a c))])
         (nock-eval (cons core (nock-tree-find core b)))))
      ; Replace the address specified by b in the result of c with subject a with the result of d with subject a
      ([cons (var a) (cons 10 (cons (cons (var b) (var c)) (var d)))]
       (nock-tree-edit (nock-eval a c) b (nock-eval a d)))
      ; Cell hint, compute then discard
      ([cons (var a) (cons 11 (cons (cons (var b) (var c)) (var d)))]
       ((const (nock-eval (cons b c))) (nock-eval (cons a d))))
      ; Atom hint, discard
      ([cons (var a) (cons 11 (cons _ (var c)))]
       (nock-eval a c))
      ; Atom, return
      ([var a] a))))

; / operator in nock spec: tree addressing
(define nock-tree-find
  (lambda (tree address)
    (if (= address 1) tree
        (nock-tree-find (if (even? address) (car tree) (cdr tree)) (quotient address 2)))))

; # operator in nock spec: tree editing
(define nock-tree-edit
  (lambda (subtree address tree)
    (if (= address 1) subtree
      (if (even? address)
          (cons (nock-tree-edit subtree (quotient address 2) (car tree)) (cdr tree))
          (cons (car tree) (nock-tree-edit subtree (quotient address 2) (cdr tree)))))))

(nock-eval (cons (cons 2 3) (cons 0 3)))
