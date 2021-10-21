#lang racket

(require rackunit)

(define (nock cell)
  (cdr (mink-cell (car cell) (cdr cell) '() '())))

(define (nock-noun subject formula)
  (cdr (mink-cell subject formula '() '())))

(define (mink-cell subject formula trace gates)
 (let
  [(recur-on-noun (lambda (subject formula) (mink-cell subject formula trace gates)))
   (recur-on-noun-with-hint (lambda (subject formula hint) (mink-cell subject formula (cons hint trace) gates)))
   (recur-on-top-gate (lambda (ref path)
    (let*
     [(gate (car gates))
      (gates (cdr gates))
      (cor (cons (car gate) (cons (cons ref path) (cdr (cdr gate)))))]
     (mink-cell cor (car gate) trace gates))))]
  (match formula
   ;; A cell of two formulae computes both against the same subject
   ;; and returns their result as a cell
   ([cons (cons (var b) (var c)) (var d)]
    (let
     [(head (recur-on-noun subject (cons b c)))]
     (if (not (= 0 (car head)))
      head
      (let
       [(tail (recur-on-noun subject d))]
       (if (not (= 0 (car tail)))
        tail
        (cons 0 (car head) (car tail)))))))
   ;; Get an axis from the subject
   ([cons 0 (var axis)]
    (let
     [(part (frag axis subject))]
     (if (equal? 0 part)
      (cons 2 trace)
      (cons 0 (cdr part)))))
   ;; Return a constant noun
   ([cons 1 (var constant)]
    (cons 0 constant))
   ;; Compute both arguments as formulae against the subject,
   ;; then treat the first result as the subject and the second
   ;; as the formula and compute again
   ([cons 2 (cons (var subject^) (var formula))]
    (let
     [(subject^ (recur-on-noun subject subject^))]
     (if (not (= 0 (car subject^)))
      subject^
      (let
       [(formula (recur-on-noun subject formula))]
       (if (not (= 0 (car formula)))
        formula
        (recur-on-noun subject^ formula))))))
   ;; 0 result if argument computes to cell, 1 if atom
   ([cons 3 (var argument)]
    (let
     [(argument (recur-on-noun subject argument))]
     (if (not (= 0 (car argument)))
      argument
      (if (pair? (cdr argument)) 0 1)))) 
   ;; Compute argument which must compute to atom, then increment that atom
   ([cons 4 (var argument)]
    (let
     [(argument (recur-on-noun subject argument))]
     (if (not (= 0 (car argument)))
      argument
      (+ 1 (cdr argument))))) 
   ;; Test equality of two nouns, 0 if equal, 1 if not
   ([cons 5 (cons (var a) (var b))]
    (let
     [(a (recur-on-noun subject a))]
     (if (not (= 0 (car a)))
      a
      (let
       [(b (recur-on-noun subject b))]
       (if (not = 0 (car b))
        b
        (if (equal? (cdr a) (cdr b)) 0 1))))))
   ;; If then else: evaluate test argument, which must return 0 or 1.
   ;; If 0 then evaluate 'yes' argument
   ;; If 1 then evaluate 'no' argument
   ([cons 6 (cons (var test) (cons (var yes) (var no)))]
    (let
     [(result (recur-on-noun subject test))]
     (if (not (= 0 (car result)))
      result
      (match (cdr result)
       (0 (recur-on-noun subject yes))
       (1 (recur-on-noun subject no))
       (_ (cons 2 trace))))))
   ;; Compute a subject, then evaluate next against the subject
   ;; cf => in Hoon
   ([cons 7 (cons (var subject^) (var next))]
    (let
     [(subject (recur-on-noun subject subject^))]
     (if (not (= 0 (car subject)))
      subject
      (recur-on-noun subject next))))
   ;; Compute a value, then cons it onto the subject
   ;; cf =+ in Hoon
   ([cons 8 (cons (var head) (var next))]
    (let
     [(head (recur-on-noun subject head))]
     (if (not (= 0 (car head)))
      head
      (recur-on-noun (cons head subject) next))))
   ;; Compute a core, then call an arm of the core with the core as the subject
   ([cons 9 (cons (var axis) (var core))]
    (let
     [(core (recur-on-noun subject core))]
     (if (not (= 0 (car core)))
      core
      (let
       [(arm (frag axis (cdr core)))]
       (if (equal? 0 arm)
        (cons 2 trace)
        (recur-on-noun (cdr core) (cdr arm)))))))
   ;; Evaluate a noun 'target', then replace the subnoun at the given axis
   ;; with the noun 'value'
   ([cons 10 (cons (cons (var axis) (var value)) (var target))]
    (if (= 0 axis)
     (cons 2 trace)
     (let
      [(target (recur-on-noun subject target))]
      (if (not (= 0 (car target)))
       target
       (let
        [(value (recur-on-noun subject value))]
        (if (not (= 0 (car value)))
         value
         (let
          [(mutant (edit axis (cdr target) (cdr value)))]
          (if (= 0 mutant)
           (cons 2 trace)
           (cons 0 (cdr mutant))))))))))
   ;; Dynamic hint: compute and a hint, fail if hint computation fails
   ;; If hint succeeds discard result and compute 'next'
   ([cons 11 (cons (cons (var tag) (var clue)) (var next))]
    (let
     [(clue (recur-on-noun subject clue gates))]
     (if (not (= 0 (car clue)))
      clue
      (if (member tag (list (tas "hunk") (tas "hand") (tas "lose") (tas "mean") (tas "spot")))
       (recur-on-noun-with-hint subject next (cons tag (cdr clue)))
       (recur-on-noun subject next)))))
   ;; Static hint, discard hint and compute 'next'
   ([cons 11 (cons (var tag) (var next))]
    (recur-on-noun subject next))
   ;; Call a gate in the sky
   ;; 
   ;; Compute 'ref' and 'path',
   ;; then pop the top gate from the scry gate stack and
   ;; slam it with [ref path]
   ([cons 12 (cons (var ref) (var path))]
    (let
     [(ref (recur-on-noun subject ref))]
     (if (not (= 0 (car ref)))
      ref
      (let
       [(path (recur-on-noun subject path))]
       (if (not (= 0 (car path)))
        path
        (let
         [(result (recur-on-top-gate (cdr ref) (cdr path)))]
         (if (not (= 0 (car result)))
          result
          (if (equal? 0 (cdr result))
           (cons 1 (cdr path))
           (if (equal? 0 (cdr (cdr result)))
            (cons 2 (cons (cons (tas "hunk") (cons (cdr ref) (cdr path))) trace))
            (cdr (cdr (cdr result)))))))))))))))

(define (frag axis noun)
 (if (= 0 axis)
  0
  (cons 0 (if (= 1 axis)
   noun
   (if (even? axis)
    (car (cdr (frag axis (quotient axis 2))))
    (cdr (cdr (frag axis (quotient axis 2)))))))))

(define (mas axis)
  (quotient axis 2))

(define (cap axis)
  (if (= axis 2)
   2
   (if (= axis 3)
    3
    (cap (mas axis)))))

(define (edit axis target value)
  (if (= 1 axis)
   (cons 0 value)
   (if (not (pair? target))
    0
    (let*
     [(pick (cap axis))
      (mutant (edit (mas axis) (if (= 2 pick) (car target) (cdr target)) value))]
     (if (= mutant 0)
      0
      (if (= 2 pick)
       (cons 0 (cons (cdr mutant) (cdr target)))
       (cons 0 (cons (car target) (cdr mutant)))))))))

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
