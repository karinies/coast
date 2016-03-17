#lang racket/base

;; Copyright 2011 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(require
 racket/vector
 racket/function
 rackunit
 rackunit/text-ui
 racket/pretty
 "../compile/compile.rkt"
 "../baseline.rkt"
 "../persistent/environ.rkt"
 "../persistent/hash.rkt"
 "../persistent/set.rkt"
 (only-in "../generate/baseline.rkt" motile/call motile/decompile)
 (only-in "../../serialize.rkt" motile/serialize motile/deserialize))

(provide
 compile/start
 compile/tests/all
 ; Test suites.
 constants
 baseline
 lambda-exprs
 let-exprs
 letrec-exprs
 let*-exprs
 named-let-exprs
 define-exprs
 begin-exprs
 when-exprs
 unless-exprs
 cond-exprs
 case-exprs
 do-exprs
 and-exprs
 or-exprs
 apply-exprs
 sort-tests
 quasiquote-exprs
 for-each-exprs
 map-exprs
 box-tests
 call/cc-exprs
 closure-tests
 binding-environment-tests
 tuple-tests
 vector-tests
 set-tests
 hash-tests
 ;; regression tests
 nested-define-tests
 frame-bug-tests)
 

(define compile/start (make-parameter (lambda (e) (motile/call (motile/compile e) ENVIRON/TEST))))

;(define (compile/start e)
;  (motile/call (motile/compile e) ENVIRON/TEST))

; -----------------------------------

(define-test-suite constants
  (test-case "Zero" (check-equal? 0 ((compile/start) 0)))
  (test-case "One" (check-equal? 1 ((compile/start) 1)))
  (test-case "Two" (check-equal? 2 ((compile/start) 2)))
  (test-case "True" (check-equal? #t ((compile/start) #t)))
  (test-case "False" (check-equal? #f ((compile/start) #f)))
  (test-case "Nil" (check-equal? '() ((compile/start) null)))
  (test-case "String" (check-equal? "Foobar" ((compile/start) "Foobar")))
  (test-case "Symbol" (check-equal? 'redondo ((compile/start) ''redondo)))
  (test-case "List" (check-equal? '(1 2 3) ((compile/start) ''(1 2 3))))
  (test-case "Vector" (check-equal? #(1 2 3) ((compile/start) '#(1 2 3)))))

; -----------------------------------

(define-test-suite baseline
  (test-case "cons" (check-equal? (cons 29 82) ((compile/start) '((lambda () (cons 29 82))))))
  (test-case "car" (check-equal? 29 ((compile/start) '((lambda () (car (cons 29 82)))))))
  (test-case "cdr" (check-equal? 82 ((compile/start) '((lambda () (cdr (cons 29 82)))))))
  (test-case "null?"
             (check-true ((compile/start) '((lambda () (null? null)))))
             (check-false ((compile/start) '((lambda () (null? '(1 2 3)))))))
  (test-case "not"
             (check-true ((compile/start) '(not #f)))
             (check-false ((compile/start) '(not #t)))
             (check-false ((compile/start) '(not 1))))
  (test-case "<"
             (check-true ((compile/start) '(< 8 31)))
             (check-false ((compile/start) '(< -1 -7))))
  (test-case "="
             (check-true ((compile/start) '(= 8 8.0))
                         ((compile/start) '(= -1 -7))))
  (test-case "add1" (check-equal? 93052 ((compile/start) '(add1 93051))))
  (test-case "sub1" (check-equal? 93050 ((compile/start) '(sub1 93051))))
  (test-case "+"
             (check-equal? 93051 ((compile/start) '(+ 93051)))
             (check-equal? 835 ((compile/start) '(+ 812 23)))
             (check-equal? 825 ((compile/start) '(+ 812 23 -10)))
             (check-equal? 842 ((compile/start) '(+ 812 23 -10 17))))
  (test-case "*"
             (check-equal? 93051 ((compile/start) '(* 93051)))
             (check-equal? 18676 ((compile/start) '(* 812 23)))
             (check-equal? -186760 ((compile/start) '(* 812 23 -10)))
             (check-equal? -3174920 ((compile/start) '(* 812 23 -10 17))))
  (test-case "format"
             (check-equal? "99 hello (1 2 3) |a weird name|"
                           ((compile/start) '(format "~a ~a ~a ~s" 99 "hello" '(1 2 3) '|a weird name|)))))

; -----------------------------------

(define-test-suite lambda-exprs
  (test-case
   "Trivial zero-argument lambda expression"
   (check-equal? 1 ((compile/start) '((lambda () 1)))))
  (test-case
   "Identity function"
   (check-equal? 17 ((compile/start) '((lambda (x) x) 17))))
  (test-case
   "Single argument invoking a primitive in global namespace"
   (check-equal? 100 ((compile/start) '((lambda (x) (1+ x)) 99))))
  (test-case
   "Two arguments"
   (check-equal? 132 ((compile/start) '((lambda (x y) (+ x y)) 99 33))))
  (test-case
   "Three arguments"
   (check-equal? 6 ((compile/start) '((lambda (x y z) (+ x y z)) 1 2 3))))
  (test-case
   "Four arguments"
   (check-equal? 10 ((compile/start) '((lambda (w x y z) (+ w x y z)) 1 2 3 4))))
  (test-case
   "Five arguments"
   (check-equal? 15 ((compile/start) '((lambda (v w x y z) (+ v w x y z)) 1 2 3 4 5)))) 
  (test-case
   "in-order evaluation of expressions in lambda body"
   (check-equal? 60  ((compile/start) '((lambda (x y z)
                                        (display (format "~a\n" x))
                                        (display (format "~a\n" y))
                                        (display (format "~a\n" z))
                                        (+ x y z))
                                      10 20 30))))
  (test-case
   "Rest argument"
   (check-equal? '(3 17 22 87) ((compile/start) '((lambda rest rest) 3 17 22 87))))
  (test-case
   "One positional argument and rest argument"
   (check-equal? '(3 17 22 87) ((compile/start) '((lambda (x . rest) (cons x rest)) 3 17 22 87))))
  (test-case
   "Two positional arguments and rest argument"
   (check-equal? '(20 22 87) ((compile/start) '((lambda (x y . rest) (cons (+ x y) rest)) 3 17 22 87))))
  (test-case
   "Three positional arguments and rest argument"
   (check-equal? '(42 87 127) ((compile/start) '((lambda (x y z . rest)
                                                 (cons (+ x y z) rest)) 3 17 22 87 127))))
  (test-case
   "Four positional arguments and rest argument"
   (check-equal? '(129 127 999) ((compile/start) '((lambda (a b c d . rest)
                                                   (cons (+ a b c d) rest)) 3 17 22 87 127 999))))
  (test-case
   "One closed variable and zero parameters"
   (check-equal? 34 ((compile/start) '(((lambda (a) (lambda () (add1 a))) 33)))))
  (test-case
   "One closed variable and one parameter"
   (check-equal? 44 ((compile/start) '((lambda (a) ((lambda (b) (+ a b)) 33)) 11))))
  (test-case
   "Two closed variables and two parameters"
   (check-equal? 420 ((compile/start) '((lambda (u v)
                                        ((lambda (w x) (+ u v x)) 300 400)) 19 1))))
  
  (test-case
   "Deeply nested lexical scope" 
   (check-equal? '(1 2 3 4 5 6)
                 ((compile/start) '((lambda (a)
                                    ((lambda (b)
                                       ((lambda (c)
                                          ((lambda (d)
                                             ((lambda (e)
                                                ((lambda (f) (list a b c d e f))
                                                 6))
                                              5))
                                           4))
                                        3))
                                     2))
                                  1))))
  (test-case
   "inner lexical scope shadows outer lexical scope"
   (check-equal? 107 ((compile/start) '((lambda (a b)
                                        ((lambda (b)
                                           ((lambda (a) (+ a b)) 85))
                                         22))
                                      5 10))))
  (test-case
   "Combined rest arguments and closed variables"
   (check-equal? '(33 alpha beta gamma)
                 ((compile/start) '((lambda rest
                                    ((lambda (a) (cons a rest)) 33)) 'alpha 'beta 'gamma)))
   (check-equal? '(111 213 300 400 500)
                 ((compile/start) '((lambda (a b)
                                    ((lambda (x y . rest) (list* (+ a x) (+ b y) rest))
                                     100 200 300 400 500)) 11 13))))
  
  (test-case
   "Lambda is not a reserved symbol"
   (check-equal? '(7 8 9 10) ((compile/start) '((lambda lambda lambda) 7 8 9 10)))))

; ---------------------------

(define-test-suite let-exprs
  (test-case
   "Simple let case"
   (check-equal? '(1 2 3) ((compile/start) '(let ((a 1) (b 2) (c 3)) (list a b c)))))
  (test-case
   "lexical scope established by a let is seen only by the expressions in the body of the let"
   (check-equal? '((foo) bar) ((compile/start) '(let ((cons (lambda (x y)
                                                            (cons (cons x '()) (cons y '()))))) ; Rebinding of cons in body of let.
                                                (cons 'foo 'bar)))))
  (test-case
   "Another simple let case"
   (check-equal? ((compile/start) '(let ((a 1) (b 2) (c 3) (d "silly") (e #(8 9 10)) (f (list 'x 'y)))
                                   (list a b c d e f)))
                 '(1 2 3 "silly" #(8 9 10) (x y))))
  (test-case
   "Let with no definitions"
   (check-equal? "gamma" ((compile/start) '(let () (display "alpha\n") (display "beta\n") "gamma")))))

; ---------------------------

(define-test-suite letrec-exprs
  (test-case
   "Simple letrec case with two definitions"
   (check-equal? 24
                 ((compile/start) '(letrec ((a 11) 
                                          (b (lambda () (+ a 13))))
                                   (b)))))
  (test-case
   "Letrec with one function definition"
   (check-equal? 120
                 ((compile/start) '(letrec ((f (lambda (n) (if (= n 1) 1 (* n (f (sub1 n))))))) ; Factorial.
                                   (f 5)))))
  (test-case
   "Recursive definition of two functions"
   (check-equal? '(#t #f #t #f)
                 ((compile/start) '(letrec ; Mutually recursive functions.
                                     ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                                      (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
                                   (list (even? 12) (even? 3) (odd? 17) (odd? 8))))))
  (test-case
   "Mutually recursive function definitions with multiple (> 1) closed variables per function"
   (check-equal? '(#t #f #t #f)
                 ((compile/start) '(letrec
                                     ((even? (lambda (n) (if (= n zero) #t (odd? (- n one)))))
                                      (odd? (lambda (n) (if (= n zero) #f (even? (- n one)))))
                                      (zero 0)
                                      (one 1))
                                   (list (even? 12) (even? 3) (odd? 17) (odd? 8))))))
  (test-case
   "Recursive definition of three functions"
   (check-equal? '(#t #f #t #f 120)
                 ((compile/start) '(letrec
                                     ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                                      (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                                      (factorial (lambda (n) (if (= n 1) 1 (* n (factorial (sub1 n)))))))
                                   (list (even? 12) (even? 3) (odd? 17) (odd? 8) (factorial 5))))))
  (test-case
   "Recursive definition of five bindings"
   (check-equal? '(#t #f #t #f 120 24) 
                 ((compile/start) '(letrec ; Mutually recursive functions.
                                     ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                                      (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                                      (factorial (lambda (n) (if (= n 1) 1 (* n (factorial (sub1 n))))))
                                      (a 11)
                                      (b (lambda () (+ a 13))))
                                   (list (even? 12) (even? 3) (odd? a) (odd? 8) (factorial 5) (b))))))
  (test-case
   "Self-recursive definition"
   (check-true ((compile/start) '(letrec ((foo (lambda (x) (eq? x foo))))
                                 (foo foo)))))
  (test-case
   "Letrec with define in the body"
   (check-equal? '(#t #f #t #f 120 24)
                 ((compile/start)
                  '(letrec ; Mutually recursive functions.
                       ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                        (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                        (factorial (lambda (n) (if (= n 1) 1 (* n (factorial (sub1 n)))))))
                     ; Definitions in the letrec body.
                     (define a 11)
                     (define b (lambda () (+ a 13)))
                     (list (even? 12) (even? 3) (odd? a) (odd? 8) (factorial 5) (b))))))
  (test-case 
   "Takeuchi functions"
   (check-equal? 7 ((compile/start) '(letrec ((tak
                                             (lambda (x y z)
                                               (if (not (< y x))
                                                   z
                                                   (tak (tak (- x 1) y z)
                                                        (tak (- y 1) z x)
                                                        (tak (- z 1) x y))))))
                                     (tak 18 12 6))))))

; -------------------------

(define-test-suite letrec*-exprs
  (test-case
   "Trivial letrec* with one definition"
   (check-equal?
    ((compile/start)
     '(letrec* ((a 11)) (add1 a)))
    12))
  
  (test-case
   "Simple letrec* with two definitions"
   (check-equal?
    ((compile/start)
     '(letrec* ((a 11) 
                (b (+ a 13)))
        (* a b)))

    264))
  
  (test-case
   "Translation of empty let with defines into letrec*"
   (check-equal?
    ((compile/start)
      '(let ()
         (define x 1)
         (define y (+ x 7)) ; Note that y depends on x.
         (+ x y)))
    
    9)) ; Expected.
  
    (test-case
   "Translation of empty let* with defines into letrec*"
   (check-equal?
    ((compile/start)
      '(let* ()
         (define x 1)
         (define y (+ x 7)) ; Note that y depends on x.
         (+ x y)))
    
    9)) ; Expected.

  (test-case
   "Letrec* with one function definition"
   (check-equal?
    ((compile/start)
     '(letrec* ((f (lambda (n) (if (= n 1) 1 (* n (f (sub1 n))))))) ; Factorial.
               (f 5)))
    120))

  (test-case
   "Recursive definition of two functions"
   (check-equal? 
    ((compile/start)
     '(letrec* ; Mutually recursive functions.
       ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
        (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
       (list (even? 12) (even? 3) (odd? 17) (odd? 8))))

    '(#t #f #t #f))) ; Expected.

  (test-case
   "Mutually recursive function definitions each using a letrec* defined constant"
   (check-equal? 
    ((compile/start)
     '(letrec*
          ((even? (lambda (n) (if (= n zero) #t (odd? (- n one)))))
           (odd? (lambda (n) (if (= n zero) #f (even? (- n one)))))
           (zero 0)
           (one 1))
        (list (even? 12) (even? 3) (odd? 17) (odd? 8))))

    '(#t #f #t #f))) ; Expected.

  (test-case
   "Recursive definition of three functions"
   (check-equal? 
    ((compile/start)
     '(letrec*
       ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
        (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
        (factorial (lambda (n) (if (= n 1) 1 (* n (factorial (sub1 n)))))))
       (list (even? 12) (even? 3) (odd? 17) (odd? 8) (factorial 5))))

    '(#t #f #t #f 120))) ; Expected.

  (test-case
   "Recursive definition of three functions with inline calls in later bindings"
   (check-equal?
    ((compile/start)
     '(letrec*
          ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
           (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
           (factorial (lambda (n) (if (= n 1) 1 (* n (factorial (sub1 n))))))
           (a (even? 12))
           (b (even? 3))
           (c (odd? 17))
           (d (odd? 8))
           (e (factorial 5)))
        (list a b c d e)))

    '(#t #f #t #f 120))) ; Expected.

  (test-case
   "Recursive definitions in three bindings + dependent definitions in two"
   (check-equal?  
    ((compile/start)
     '(letrec* ; Mutually recursive functions.
       ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
        (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
        (factorial (lambda (n) (if (= n 1) 1 (* n (factorial (sub1 n))))))
        (a (add1 (factorial 4)))
        (b (+ a 13)))
       (list (even? 12) (even? 3) (odd? a) (odd? 8) (factorial 5) b)))

    '(#t #f #t #f 120 38))) ; Expected.

  (test-case
   "Self-recursive definition"
   (check-true
    ((compile/start)
     '(letrec* ((foo (lambda (x) (eq? x foo))))
        (foo foo)))))

  (test-case
   "Letrec* with define in the body"
   (check-equal?
    ((compile/start)
     '(letrec* ; Mutually recursive functions.
          ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
           (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
           (factorial (lambda (n) (if (= n 1) 1 (* n (factorial (sub1 n)))))))
        ; Definitions in the letrec* body.
        (define a 11)
        (define b (lambda () (+ a 13)))
        (list (even? 12) (even? 3) (odd? a) (odd? 8) (factorial 5) (b))))

    '(#t #f #t #f 120 24))) ; Expected.

  (test-case
   "Letrec* binds over closed variables"
   (check-equal?
    ((compile/start)
      '(let ((x 17)
             (y 7)
             (z 103))
         (letrec*
             ((even? (lambda (n) (if (= n 0) #t (odd?  (- n 1)))))
              (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1)))))
              (f     (lambda (n) (if (= n 1) y (* n (f (sub1 n)))))) ; A misguided factorial.
              (a (lambda (n) (if (even? n) (+ n x) (+ n x y z))))
              (b (lambda (n) (> n z)))
              (c (lambda (n) (odd? (a n)))))
           (list x y z (f 5) (a 2) (a 3) (b 22) (b 905) (c 2) (c 5)))))
    
    (list 17 7 103 840 19 130 #f #t #t #f))) ; Expected.
  
  (test-case
   "Trivial letrec* in a lambda body"
   (check-equal?
    ((compile/start)
     '((lambda (f)
         (letrec* ((y f)) f))
       1))

    1)) ; Expected.

  (test-case
   "Letrec* inside let* body"
   (check-equal?
    ((compile/start)
     '(let* ([p 3] [q 5])
        (define (foo) (+ p 3)) ; (define ...) rewritten as (letrec* ...) by compiler.
        (foo)))

    6)) ; Expected
  
  (test-case
   "Letrec* inside trivial loop body #1"
   (check-equal?
    ((compile/start)
     '(let loop ([f 1])
        (define y (add1 f)) ; (define ...) rewritten as (letrec* ...) by compiler.
        f))
   
    1)) ; Expected.

  (test-case
   "Letrec* inside trivial loop body #2"
   (check-equal?
    ((compile/start)
     '(let loop ([f 1] [z 2])
        (define y (+ f z))
        (list f z y)))
   
    (list 1 2 3))) ; Expected.

  (test-case
   "Letrec* inside loop body"
   (check-equal?
    ((compile/start)
     '(let loop ([f 1] [z 2])
        (define y (+ f z))
        (if (positive? z)
            (loop (add1 f) (sub1 z))
            (list f z y))))
    
    (list 3 0 3)))

  (test-case 
   "Takeuchi functions"
   (check-equal?
    ((compile/start) 
     '(letrec* ((tak
                 (lambda (x y z)
                   (if (not (< y x))
                       z
                       (tak (tak (- x 1) y z)
                            (tak (- y 1) z x)
                            (tak (- z 1) x y))))))
        (tak 18 12 6)))

    7)) ; Expected.
  )
  
; -------------------------

(define-test-suite let*-exprs
  (test-case 
   "Simple let* case"
   (check-equal? 50 ((compile/start) '(let* ((a 1)
                                           (b (+ a 13))
                                           (c (+ a b 20)))
                                      (+ a b c)))))
  (test-case
   "Lambda expression in let*-bindings"
   (check-equal? 35 ((compile/start) '(let* ((a 1)
                                           (b (+ a 13))
                                           (c (lambda () (+ a b 20))))
                                      (c))))))

(define-test-suite cond-exprs
  (test-case "Degenerate cond" (check-eq? #f ((compile/start) '(cond))))
  
  (test-case
   "Cond with only an else clause"
   (check-eq?
    33
    ((compile/start)
     '(let ((a 10))
        (cond
          (else (+ a 23)))))))

  (test-case
   "Simple cond with three cases"
   (check-equal?
    '(-1 1 0)
    ((compile/start)
     '(let ((f (lambda (n)
                 (cond
                   ((zero? n) 0)
                   ((positive? n) 1)
                   ((negative? n) -1)))))
        (list (f -3) (f 22) (f 0))))))

  (test-case
   "Simple cond with else clause"
   (check-equal?
    '(-1 1 0)
    ((compile/start)
     '(let ((f (lambda (n)
                 (cond
                   ((zero? n) 0)
                   ((positive? n) 1)
                   (else -1)))))
        (list (f -3) (f 22) (f 0))))))

  (test-case
   "Fibonnaci function"
   (check-equal?
    '(5 13 55)
    ((compile/start)
     '(let ()
        (define (fib n)
          (cond
            ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (fib (- n 1)) (fib (- n 2))))))
        (list (fib 5) (fib 7) (fib 10))))))
  
  (test-case
   "test => procedure clause"
   (check-equal?
    '(55 233 178)
    ((compile/start)
     '(let ((f (lambda (a b)
                 (define (fib n)
                   (cond
                     ((= n 0) 0)
                     ((= n 1) 1)
                     (else (+ (fib (sub1 n)) (fib (- n 2))))))
                 (cond
                   ((> a 9)        (fib a))
                   ((> (+ a b) 11) (fib (+ a b)))
                   ((+ a b) => (lambda (n) (* 2 (fib n))))))))
        (list  (f 10 0) (f 8 5) (f 5 6)))))))

; -------------------------

(define-test-suite named-let-exprs
  (test-case
   "Named let"
   (check-equal? '((6 1 3) (-5 -2))
                 ((compile/start) '(let loop ((numbers '(3 -2 1 6 -5))
                                            (nonnegative '())
                                            (negative '()))
                                   (cond ((null? numbers)
                                          (list nonnegative negative))
                                         ((>= (car numbers) 0)
                                          (loop (cdr numbers)
                                                (cons (car numbers) nonnegative)
                                                negative))
                                         ((< (car numbers) 0)
                                          (loop (cdr numbers)
                                                nonnegative
                                                (cons (car numbers) negative)))))))))

; -------------------------

(define-test-suite define-exprs
  (test-case
   "Two defines at same level"
   (check-equal? -18 ((compile/start) '((lambda (a b)
                                        (define (a+) (add1 a))
                                        (define (b-) (sub1 b))
                                        (* (a+) (b-)))
                                      8 -1))))
  (test-case
   "Nested defines"
   (check-equal? 6 ((compile/start) '((lambda (a b)
                                      (define (a+) ; Define within lambda body.
                                        (define (b-) (sub1 b)) ; Nested definition within definition of a+
                                        (* (add1 a) (b-))) ; Body of a+
                                      (a+))
                                    2 3))))
  (test-case
   "Twice-nested defines"
   (check-equal? 5 ((compile/start) '((lambda (a b)
                                      (define (a+) ; Define within lambda body.
                                        (define (f) b) ; Nested definition within definition of a+
                                        (+ a (f))) ; Body of a+
                                      (a+))
                                    2 3)))
   (check-equal? -16 ((compile/start) '((lambda (a b)
                                        (define (a*) ; Define within lambda body.
                                          (define (b-) (sub1 b)) ; Nested definition within definition of a*
                                          (* a (b-))) ; Body of a*
                                        (a*))
                                      8 -1))))
  (test-case
   "Recursive define statement"
   (check-equal? 233 ((compile/start) '((lambda (a)
                                        (define (fib n)
                                          (if (= n 0) 0 (if (= n 1) 1 (+ (fib (sub1 n)) (fib (- n 2))))))
                                        (fib a))
                                      13))))
  (test-case
   "A let with a define statement"
   (check-equal? -18 ((compile/start) '(let ((a 8) (b -1))
                                       (define (a+) (add1 a))
                                       (define (b-) (sub1 b))
                                       (* (a+) (b-))))))
  (test-case
   "A let* with a define statement"
   (check-equal? '(8 21 12 144)
                 ((compile/start) '(let* ((a 8) (b (+ a 4)))
                                   (define (fib n)
                                     (if (= n 0) 0 (if (= n 1) 1 (+ (fib (sub1 n)) (fib (- n 2))))))
                                   (list a (fib a) b (fib b))))))
  (test-case
   "A letrec with a define statement"
   (check-equal? '(8 21 12 144)
                 ((compile/start) '(letrec ((a 8) (b (lambda () (+ a 4))))
                                   (define (fib n)
                                     (if (= n 0) 0 (if (= n 1) 1 (+ (fib (sub1 n)) (fib (- n 2))))))
                                   (list a (fib a) (b) (fib (b)))))))
  (test-case
   "A letrec with multiple define statements"
   (check-equal? -18 ((compile/start) '(letrec ((a 8) (b -1))
                                       (define (a+) (add1 a))
                                       (define (b-) (sub1 b))
                                       (* (a+) (b-)))))))

; --------------------

(define-test-suite begin-exprs
  (test-case
   "Begin: no sequence"
   (check-equal? 88 ((compile/start) '(begin 88))))

  (test-case
   "Begin: sequence"
   (check-equal? 99 ((compile/start) '(begin
                                      (display "begin: first\n")
                                      (display "begin: second\n")
                                      (display "begin: third\n")
                                      99)))))

; -------------------

(define-test-suite when-exprs
  (test-case "When true"  (check-equal? 'hello ((compile/start) '(when #t 'hello))))
  (test-case "When false" (check-equal? (void) ((compile/start) '(when #f 'wrong)))))

; ------------------

(define-test-suite unless-exprs
  (test-case "Unless false" (check-equal? 'hello ((compile/start) '(unless #f 'hello))))
  (test-case "Unless true"  (check-equal? (void) ((compile/start) '(unless #t 'wrong)))))

; ------------------

; --------------------------

(define-test-suite case-exprs
  (test-case
   "Numerical expressions"
   (check-equal? 'composite ((compile/start) '(case (* 2 3)
                                              ((2 3 5 7) 'prime)
                                              ((1 4 6 8 9) 'composite)))))
  (test-case
   "Symbol expressions"
   (check-equal? #f ((compile/start) '(case (car '(c d))
                                          ((a) 'a)
                                          ((b) 'b)))))
  (test-case
   "Else clause"
   (check-equal? 'consonant ((compile/start) '(case (car '(c d))
                                              ((a e i o u) 'vowel)
                                              ((w y) 'semivowel)
                                              (else 'consonant))))))

; -----------------------

(define-test-suite do-exprs
  (test-case 
   "With return value"
   (check-equal? #(0 1 2 3 4) ((compile/start) '(do ((vec (make-vector 5))
                                                   (i 0 (+ i 1)))
                                                ((= i 5) vec)
                                                (vector-set! vec i i)))))
  (test-case 
   "No expression following test"
   (check-equal? (void) ((compile/start) '(do ((vec (make-vector 5))
                                             (i 0 (+ i 1)))
                                          ((= i 5)) ; No expressions following <test>.
                                          (vector-set! vec i i)))))
  (test-case
   "No commands in body"
   (check-equal? 25 ((compile/start) '(let ((x '(1 3 5 7 9)))
                                      (do ((x x (cdr x))
                                           (sum 0 (+ sum (car x))))
                                        ((null? x) sum)))))))

; -------------------

(define-test-suite and-exprs
  (test-case "No clauses => #t" (check-true ((compile/start) '(and))))
  
  (test-case
   "No clauses => triggers if positive branch"
   (check-equal? "good" ((compile/start) '(if (and) "good" "bad"))))
  
  (test-case
   "One clause"
   (check-true ((compile/start) '((lambda (a b) (and (< a b)))
                                3 5))))
  (test-case
   "Only elements one and two should execute"
   (check-false ((compile/start)
                 '(let* ((element (lambda (n a b) (display (format "~a " n)) (< a b)))
                         (outcome (and (element 1 3 5) (element 2 5 3) (element 3 7 8))))
                    (newline)
                    outcome))))
                
  (test-case
   "All elements should execute"
   (check-true ((compile/start)
                '(let* ((element (lambda (n a b) (display (format "~a " n)) (< a b)))
                         (outcome (and (element 1 3 5) (element 2 2 3) (element 3 11 17))))
                   (newline)
                   outcome))))
  
  (test-case
   "#f => triggers #f branch of if"
   (check-equal? "unhappy" ((compile/start)
                            '(let* ((element (lambda (n a b) (display (format "~a " n)) (< a b)))
                                    (happy   (lambda (flag) (if flag "happy" "unhappy")))
                                    (outcome (happy (and (element 1 3 5) (element 2 5 3) (element 3 7 8)))))
                               (newline)
                               outcome)))))
                               

; ----------------------------

(define-test-suite or-exprs
  (test-case "No clauses" (check-false ((compile/start) '(or))))
  
  (test-case
   "No clauses => trigger #f branch of if"
   (check-equal?
    ((compile/start) '(if (or) "bad" "good"))
    "good"))
  
  (test-case
   "or #t => #t"
   (check-true ((compile/start) '((lambda (a b) (or (< a b))) 3 5))))
  
  (test-case
   "Only elements 1 & 2 should execute"
   (check-true
    ((compile/start)
     '(let ()
        (define (element n a b)
          (display (format "element: ~a\n" n))
          (< a b))
        (or (element 1 5 3) (element 2 3 5) (element 3 7 8))))))
  
  (test-case
   "All elements should execute"
   (check-true
    ((compile/start)
     '(let ()
        (define (element n a b)
          (display (format "element: ~a\n" n))
          (< a b))
        (or (element 1 5 3) (element 2 3 2) (element 3 11 17))))))
  
  (test-case
   "Only element 1 should execute"
   (check-equal?
    ((compile/start)
     '(let ()
        (define (element n a b)
          (display (format "element: ~a\n" n))
          (< a b))
        (define (happy flag)
          (if flag "happy" "unhappy"))
        
        (happy (or (element 1 5 3) (element 2 3 2) (element 3 7 8)))))

    "happy")))

; --------------

(define-test-suite sort-tests
  (test-case
   "String sorting"
   (check-equal?
    ((compile/start)
     '(let ()
        (define (sort-list obj pred)
          
          (define (loop l)
            (if (and (pair? l) (pair? (cdr l)))
                (split l '() '())
                l))
          
          (define (split l one two)
            (if (pair? l)
                (split (cdr l) two (cons (car l) one))
                (merge (loop one) (loop two))))
          
          (define (merge one two)
            (cond
              ((null? one) two)
              ((pred (car two) (car one))
               (cons (car two)
                     (merge (cdr two) one)))
              (else
               (cons (car one)
                     (merge (cdr one) two)))))
          
          (loop obj))
        
        (sort-list '("one" "two" "three" "four" "five" "six"
                           "seven" "eight" "nine" "ten" "eleven" "twelve")
                   string<?)))
    '("eight" "eleven" "five" "four" "nine" "one" "seven" "six" "ten" "three" "twelve" "two")))
  (test-case
   "Number sort"
   (check-equal? '(5 10 15 20 25) ((compile/start) '(sort '(25 20 5 15 10) <)))))

; --------------------

(define-test-suite quasiquote-exprs
  (test-case "Single quasiquotation"
             (check-equal? ((compile/start) '`(1 (+ 2 3) 17)) '(1 (+ 2 3) 17)))
  (test-case "Single quasiquotation with unquote"
             (check-equal? ((compile/start) '`(1 ,(+ 2 3) 17)) '(1 5 17)))
  (test-case "Single quasiquote with spliced unquote"
             (check-equal? ((compile/start) '`(1 ,@'(a b c) 17)) '(1 a b c 17)))
  (test-case "Let/quasiquote interaction"
             (check-equal? ((compile/start) '(let ((a 99)) `(1 ,a 17))) '(1 99 17)))
  (test-case "Let/quasiquote interaction with unquote"
             (check-equal? ((compile/start) '(let ((a 13)
                                                 (b 19))
                                             `(front ,(+ a b) ,(list a b (* 2 a) (* 2 b)) rear)))
                           '(front 32 (13 19 26 38) rear)))
  (test-case "Let/quasiquote with spliced unquote"
             (check-equal? ((compile/start) '(let ((a 13)
                                                 (b 19))
                                             `(front ,(+ a b) ,@(list a b (* 2 a) (* 2 b)) rear)))
                           '(front 32 13 19 26 38 rear)))
  (test-case "Nested quasiquote"
             (check-equal? ((compile/start) '`(1 `,(+ 1 ,(+ 2 3)) 4))
                           '(1 `,(+ 1 5)  4)))
  (test-case "triple-nested quasiquote and nested spliced unquote"
             (check-equal? ((compile/start)'`(1 ```,,@,,@(list (+ 1 2)) 4))
                           '(1 ```,,@,3    4))))


; --------------

(define-test-suite macro-exprs
  (test-case "Trivial constant macro"
             (check-equal? 11 ((compile/start) '(let ()
                                                (define-macro (eleven) 11)
                                                (eleven)))))
  (test-case "Repeated application of a macro"
             (check-equal? 22 ((compile/start) '(let ()
                                                (define-macro (eleven) 11)
                                                (+ (eleven) (eleven))))))
  (test-case "Repeated nested application of macros"
             (check-equal? 22 ((compile/start) '(let ()
                                                (define-macro (eleven) '(ten+1))
                                                (define-macro (ten+1)  '(add1 10))
                                                (+ (eleven) (eleven))))))
  (test-case "Lexical scoping of macros"
             (check-equal? 24 ((compile/start) '(let ()
                                                (define-macro (eleven) '(ten+1))
                                                (define-macro (ten+1)  '(add1 10))
                                                (let ()
                                                  (define-macro (ten+1) '(+ 10 2))
                                                  (+ (eleven) (eleven)))))))
  (test-case
   "Macros containing local functions"
   (check-equal?
    21
    ((compile/start)
     '(let ()
        (define-macro (<let> bindings . body)
          ; Local function inside macro definition body.
          (define (unzip bindings variables values)
            (if (null? bindings)
                (cons variables values)
                (let ((binding (car bindings)))
                  (unzip (cdr bindings)
                         (cons (car binding) variables)
                         (cons (cadr binding) values)))))
          
          (let* ((unzipping (unzip bindings null null))
                 (variables (car unzipping))
                 (values    (cdr unzipping)))
            `((lambda ,variables ,@body) ,@values)))
        
        (<let> ((a 3) (b 7)) (* a b)))))))

; ------------------------

(define-test-suite map-exprs
  (test-case
   "Map with 1 list"
   (check-equal?
    '(8 13 18 23 28)
    ((compile/start)
     '(let ((f (lambda (n) (+ n 3))))
        (map f '(5 10 15 20 25))))))

  (test-case
   "Map with 2 lists"
   (check-equal?
    '(8 14 20 26 32)
    ((compile/start)
     '(let ((f (lambda (a b) (+ a b 3))))
        (map f '(5 10 15 20 25) '(0 1 2 3 4))))))

  (test-case
   "Map with 3 lists"
   (check-equal?
    '(7 15 23 31 39)
    ((compile/start)
     '(let ()
        (map (lambda (a b c) (+ a b c)) '(5 10 15 20 25) '(0 1 2 3 4) '(2 4 6 8 10)))))))
; -------------------

(define-test-suite for-each-exprs
  (test-case
   "For-each with 1 list"
   (check-equal?
    #(1 4 7 10 13)
    ((compile/start)
     '(letrec ((f (lambda (i) (vector-set! outcome i (add1 (* i 3)))))
               (outcome (make-vector 5 0)))
        (for-each f '(0 1 2 3 4))
        outcome))))
                     
  (test-case
   "For-each with 2 lists"
   (check-equal?
    #(11 21 31 41 51)
    ((compile/start)
     '(letrec ((f (lambda (i j) (vector-set! outcome i j)))
               (outcome (make-vector 5 0)))
        (for-each f '(0 1 2 3 4) '(11 21 31 41 51))
        outcome))))

  (test-case
   "For-each with 3 lists"
   (check-equal?
    #(21 36 51 66 81)
    ((compile/start)
     '(letrec ((f (lambda (i j k) (vector-set! outcome i (+ j k))))
               (outcome (make-vector 5 0)))
        (for-each f '(0 1 2 3 4) '(11 21 31 41 51) '(10 15 20 25 30))
        outcome)))))
; -------------------

(define-test-suite apply-exprs
  (test-case
   "Apply + to list"
   (check-eq? 75 ((compile/start) '(apply + '(5 10 15 20 25)))))

  (test-case
   "Apply + to arguments and list"
   (check-eq? 75 ((compile/start) '(apply + 5 10 15 '(20 25)))))

  (test-case
   "Apply Motile lambda"
   (check-eq? 150
    ((compile/start)
     '(let ((f (lambda (a b c d e) (+ a b c d e))))
        (+ (apply f 5 10 15 '(20 25)) (apply f '(5 10 15 20 25))))))))

; -------------------

(define-test-suite call/cc-exprs
  (test-case
   "No application of the continuation"
   (check-equal? 20 ((compile/start) '(call/cc (lambda (k) (* 5 4))))))
  (test-case
   "Simple application of the continuation jumps out of the lambda expression"
   (check-equal? 4 ((compile/start) '(call/cc (lambda (k) (* 5 (k 4)))))))
  (test-case
   "Simple application reduces to the redex expectedly"
   (check-equal? 6 ((compile/start) '(+ 2 (call/cc (lambda (k) (* 5 (k 4))))))))
  (test-case 
   "Application breaks out of recursive loop"
   (check-equal? '(120 999) ((compile/start) 
                             '(let ((product
                                     (lambda (ls)
                                       (call/cc
                                        (lambda (break)
                                          (let f ([ls ls])
                                            (cond
                                              [(null? ls) 1]
                                              [(= (car ls) 0) (break 999)]
                                              [else (* (car ls) (f (cdr ls)))])))))))
                                (list
                                 (product '(1 2 3 4 5))
                                 (product '(7 3 8 0 1 9 5)))))))
  (test-case
   "Applying current continuation to a function f returns f's value"
   (check-equal? "hi" ((compile/start) '(let ([x (call/cc (lambda (k) k))])
                                        (x (lambda (ignore) "hi"))))))
  (test-case
   "Self-application of call/cc captures current continuation"
   (check-equal? 5 ((compile/start) ((call/cc call/cc) (lambda _ 5)))))
  (test-case
   "Saving continuation in box (check printed output, should be call/cc: 21 ; call/cc: 25)"
   (check-equal? (void) ((compile/start) '(let ((saved (box #f))
                                              (f (lambda (a b c) (+ a b c)))) ; Trivial function.
                                          
                                          (display
                                           (format "\tcall/cc: ~a\n"
                                                   (f 3 
                                                      (call/cc (lambda (k) (set-box! saved k) 7)) ; Continuation capture in argument of f.
                                                      11)))
                                          
                                          (let ((k (unbox saved)))
                                            (when k
                                              (set-box! saved #f)
                                              (k 11)))))))
  (test-case
   "Output should be 24 ; 28"
   (check-equal? (void)
                 ((compile/start) '(let ((saved (box #f)))
                                   (let ()
                                     (define (factorial n)
                                       (if (= n 0)
                                           (call/cc
                                            (lambda (k) (set-box! saved k) 1))
                                           (* n (factorial (sub1 n)))))
                                     
                                     (display (format "\tcall/cc/6b: ~a\n" (factorial 4))))
                                   
                                   (let ((k (unbox saved)))
                                     (when k
                                       (set-box! saved #f)
                                       (k 2)))))))
  (test-case
   "Should print and then return 1"
   (check-eq? 1 ((compile/start)
                 '(let ((saved (box #f))
                        (boxes (vector #f #f)))
                    (letrec ((x (box (call/cc (lambda (c) (set-box! saved c) 0))))
                             (y (box (call/cc (lambda (c) (set-box! saved c) 0)))))
                      (if (unbox saved)
                          (let ((k (unbox saved)))
                            (set-box! saved #f)
                            (vector-set! boxes 0 x)
                            (vector-set! boxes 1 y)
                            (set-box! x 1)
                            (set-box! y 1)
                            (k 0))
                          (begin
                            (display (format "x: ~a y: ~a\n"
                                             (eq? x (vector-ref boxes 0))
                                             (eq? y (vector-ref boxes 1))))
                            (+ (unbox x) (unbox y)))))))))
  (test-case
   "Letrec and call/cc interaction"  
   ;; This test is the brainchild of Al Petrofsky (May 20, 2001 in comp.lang.scheme).
   ;; In an implementation which evaluates the letrec initializers prior to the assignments
   ;; (as required by R5RS) it returns #t. In letrec implementations in which initializer evaluation
   ;; is intermingled with assignment it returns #f.
   (check-true ((compile/start) '(letrec ((x (call/cc list)) (y (call/cc list)))
                                 (cond ((procedure? x) (x (pair? y)))
                                       ((procedure? y) (y (pair? x))))
                                 (let ((x (car x)) (y (car y)))
                                   (and (call/cc x) (call/cc y) (call/cc x))))))))

; ----------------------

;; A few tests for the integration of persistent vectors into motile.
(define-test-suite vector-tests
  (test-case
   "List/vector"
   (check-equal? 
    ((compile/start)
     '(let ((v (list/vector vector/null '(2 4 6 8 10))))
        (vector/list v)))
    '(2 4 6 8 10)))
  
  (test-case
   "Vector/build"
   (check-equal? 
    ((compile/start)
     '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
        (vector/list v)))
    '(1 2 3 4 5 6 7)))

  (test-case
   "Vector/fold/left"
   (check-equal? 
    ((compile/start)
     '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
        (vector/fold/left v (lambda (x seed) (cons x seed)) null)))
    '(7 6 5 4 3 2 1)))

  (test-case
   "Vector/fold/right"
   (check-equal? 
    ((compile/start)
     '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
        (vector/fold/right v (lambda (x seed) (cons x seed)) null)))
    '(1 2 3 4 5 6 7)))
  
  (test-case
   "vector/persist?"
   (check-equal?
    ((compile/start)
     '(let ((v (list/vector vector/null '(2 4 6 8))))
        (list (vector/persist? v) (vector/persist? (vector/list v)))))
    '(#t #f)))
  
  (test-case
   "Vector/length"
   (check-equal? 
    ((compile/start)
     '(let* ((a (list/vector vector/null '(1 2 3)))
             (b (list/vector a           '(4 5)))
             (c (list/vector b           '(6 7 8 9))))
        (list (vector/length vector/null) (vector/length a) (vector/length b) (vector/length c))))
    '(0 3 5 9)))

  (test-case
   "Vector/null?"
   (check-equal?
    ((compile/start)
     '(let* ((a (list/vector vector/null '(1 2 3)))
             (b (list/vector a           '(4 5)))
             (c (list/vector b           '(6 7 8 9))))
        (list (vector/null? vector/null) (vector/null? a) (vector/null? b) (vector/null? c))))
   '(#t #f #f #f)))

  (test-case
   "Vector/cons"
   (check-equal? 
    ((compile/start)
     '(let* ((a (vector/cons vector/null 33))
             (b (vector/cons a 44))
             (c (vector/cons b 55)))
        (vector/list c)))
    '(33 44 55)))
  
  (test-case
   "Vector/cdr"
   (check-true
    ((compile/start)
     '(let* ((a (list/vector vector/null '(33 44)))
             (b (vector/cdr a))
             (c (vector/cdr b)))
        (vector/null? c)))))

  (test-case
   "Vector/filter"
   (check-equal?
    ((compile/start)
     '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
        (vector/list (vector/filter v (lambda (x) (odd? x))))))
    '(1 3 5 7)))
  
  (test-case
   "Vector/map"
   (check-equal? 
    ((compile/start)
     '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
        (vector/list (vector/map v (lambda (x) (* 2 x))))))
    '(2 4 6 8 10 12 14)))

  (test-case
   "Vector/ref"
   (check-equal?
    ((compile/start)
     '(let ((v (vector/build 7 (lambda (i) i))))
        (+ (vector/ref v 1) (vector/ref v 3) (vector/ref v 6))))
     10))

  (test-case
   "Vector/subvector"
   (check-equal?
    ((compile/start)
     '(let ((v (vector/build 7 (lambda (i) i))))
        (list (vector/list (vector/subvector v 3)) (vector/list (vector/subvector v 0 3)))))
     '((3 4 5 6) (0 1 2))))

  (test-case
   "Vector/update"
   (check-equal?
    ((compile/start)
     '(let* ((a (vector/build 5 (lambda (i) i)))
             (b (vector/update a 3 33))
             (c (vector/update b 4 44)))
        (list (vector/list a) (vector/list b) (vector/list c))))
    '((0 1 2 3 4) (0 1 2 33 4) (0 1 2 33 44)))))

; -----------------------------

;; Helper function used in hash=>vector/racket test case below.
(define (vector/pairs v)
  (let loop ((pairs null)
             (i 0)
             (n (vector-length v)))
    (if (< i n)
        (loop
         (cons (cons (vector-ref v i) (vector-ref v (add1 i))) pairs)
         (+ i 2)
         n)
        pairs)))

;; A suite of tests for motile persistent hash tables.
(define-test-suite hash-tests
  (test-case
   "hash=>pairs"
   (check-equal? 
    
    ((compile/start)
     '(let ((h/26
             (list/hash
              hash/eq/null
              '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                  k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                  u 21 v 22 w 23 x 24 y 25 z 26)))
            (less? (lambda (alpha beta)
                     (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
        (sort (hash=>pairs h/26) less?)))
    
    '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8) (i . 9) (j . 10)
      (k . 11) (l . 12) (m . 13) (n . 14) (o . 15) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
      (u . 21) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))

  (test-case
   "hash=>vector/racket"
   (check-equal? 
    
    (let* ((h/26
            ((compile/start)
             '(begin (list/hash
                      hash/eq/null
                      '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                          k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                          u 21 v 22 w 23 x 24 y 25 z 26)))))
           (less? (lambda (alpha beta)
                    (string<? (symbol->string (car alpha)) (symbol->string (car beta)))))
           (v (hash=>vector/racket h/26)))
      (sort (vector/pairs v) less?))
    
    '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8) (i . 9) (j . 10)
      (k . 11) (l . 12) (m . 13) (n . 14) (o . 15) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
      (u . 21) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))
  
  (test-case
   "hash (de)serialize #1"
   (check-equal?
    
    (let ((h ((compile/start)
              '(begin (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))))
          (less? (lambda (alpha beta)
                   (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
      (pretty-display (motile/serialize h))
      (pretty-display (motile/deserialize (motile/serialize h) #f))
      
      (pretty-display (hash=>pairs h))
      (pretty-display (hash=>pairs (motile/deserialize (motile/serialize h) #f)))
      
      (hash=>vector/racket (motile/deserialize (motile/serialize h) #f)))
    
    (hash=>vector/racket
     (pairs/hash
      hash/eq/null
      '((a . 1) (b . 2)  (c . 3)  (d . 4)  (e . 5)  (f . 6)  (g . 7)  (h . 8)  (i . 9)  (j . 10)
                (k . 11) (l . 12) (m . 13) (n . 14) (o . 15) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                (u . 21) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))))

  (test-case
   "hash (de)serialize #2"
   (check-equal?
    
    (let ((code ((compile/start)
                 '(let ((h (list/hash
                            hash/eq/null
                            '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                                k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                                u 21 v 22 w 23 x 24 y 25 z 26))))
                    (lambda () h))))
          (less? (lambda (alpha beta)
                   (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
      (pretty-display (motile/serialize code))
      (hash=>vector/racket (motile/call (motile/deserialize (motile/serialize code) #f) ENVIRON/TEST)))
    
    (hash=>vector/racket
     (pairs/hash
      hash/eq/null
      '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8) (i . 9) (j . 10)
                (k . 11) (l . 12) (m . 13) (n . 14) (o . 15) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                (u . 21) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))))

  (test-case
   "hash/remove"
   (check-equal?
    
    ((compile/start)
     '(let ((h/26
             (list/hash
              hash/eq/null
              '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                  k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                  u 21 v 22 w 23 x 24 y 25 z 26)))
            (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                     (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
        ; Remove the vowels.
        (let loop ((h h/26) (vowels '(a e i o u)))
          (if (null? vowels)
              (sort (hash=>pairs h) less?) ; Sorted pairs but without the vowels.
              (loop (hash/remove h (car vowels)) (cdr vowels))))))

    '((b . 2) (c . 3) (d . 4) (f . 6) (g . 7) (h . 8) (j . 10)
      (k . 11) (l . 12) (m . 13) (n . 14) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
      (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))
  
(test-case
   "hash/cons"
   (check-equal?
    
    ((compile/start)
     '(let ((h/21 ; The alphabet without the vowels.
             (pairs/hash
              hash/eq/null
              '((b . 2) (c . 3) (d . 4) (f . 6) (g . 7) (h . 8) (j . 10)
                        (k . 11) (l . 12) (m . 13) (n . 14) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                        (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))
            (less? (lambda (alpha beta)
                     (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
        ; Add the vowels.
        (let loop ((h h/21) (vowels '(a e i o u)))
          (if (null? vowels)
              (sort (hash=>pairs h) less?)
              (loop (hash/cons h (car vowels) #t) (cdr vowels))))))

    '((a . #t) (b . 2) (c . 3) (d . 4) (e . #t) (f . 6) (g . 7) (h . 8) (i . #t) (j . 10)
      (k . 11) (l . 12) (m . 13) (n . 14) (o . #t) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
      (u . #t) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))

  (test-case
   "hash/merge"
   (check-equal?
    
    ((compile/start)
     '(let ((h/21 ; The alphabet without the vowels.
             (pairs/hash
              hash/eq/null
              '((b . 2) (c . 3) (d . 4) (f . 6) (g . 7) (h . 8) (j . 10)
                        (k . 11) (l . 12) (m . 13) (n . 14) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                        (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))
            (h/vowels (hash/new hash/eq/null 'a #t 'e #t 'i #t 'o #t 'u #t))
            (less? (lambda (alpha beta)
                     (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
        (sort (hash=>pairs (hash/merge h/21 h/vowels)) less?)))

    '((a . #t) (b . 2) (c . 3) (d . 4) (e . #t) (f . 6) (g . 7) (h . 8) (i . #t) (j . 10)
      (k . 11) (l . 12) (m . 13) (n . 14) (o . #t) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
      (u . #t) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))
  
  (test-case
   "hash/keys"
   (check-equal?
    
    ((compile/start)
     '(let ((h/26 (list/hash
                   hash/eq/null
                   '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                       k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                       u 21 v 22 w 23 x 24 y 25 z 26)))
            (less? (lambda (alpha beta)
                     (string<? (symbol->string alpha) (symbol->string beta)))))
        (sort (hash/keys h/26) less?)))
    
    '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

  (test-case
   "hash/ref"
   (check-equal?
    
    ((compile/start)
     '(let ((h/26
             (list/hash
              hash/eq/null
              '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                  k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                  u 21 v 22 w 23 x 24 y 25 z 26))))
        (let loop ((alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
                   (values null))
          (if (null? alphabet)
              (reverse values)
              (loop (cdr alphabet) (cons (hash/ref h/26 (car alphabet) #f) values))))))
    
    '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26)))

  (test-case
   "hash/car and hash/cdr table deconstruction"
   (check-equal?
    
    ((compile/start)
     '(let ((h/26
             (list/hash
              hash/eq/null
              '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                  k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                  u 21 v 22 w 23 x 24 y 25 z 26)))
            (less? (lambda (alpha beta)
                     (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
        (let loop ((h h/26) (pairs null))
          (if (hash/empty? h)
              (sort pairs less?)
              (loop (hash/cdr h) (cons (hash/car h) pairs))))))
    
    '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8) (i . 9) (j . 10)
      (k . 11) (l . 12) (m . 13) (n . 14) (o . 15) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
      (u . 21) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))

    (test-case
     "hash/length"
     (check-equal?
      
      ((compile/start)
       '(let ((h/26
               (list/hash
                hash/eq/null
                '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                    k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                    u 21 v 22 w 23 x 24 y 25 z 26)))
              (h/vowels (hash/new hash/eq/null 'a #t 'e #t 'i #t 'o #t 'u #t))
              (h/1 (hash/new hash/eq/null 'foo 'bar)))
          (list (hash/length (hash/cdr h/1)) (hash/length h/1) (hash/length h/vowels) (hash/length h/26))))
      
      '(0 1 5 26)))

    (test-case
     "hash/empty?"
     (check-equal?
      
      ((compile/start)
       '(let ((h/26
               (list/hash
                hash/eq/null
                '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                    k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                    u 21 v 22 w 23 x 24 y 25 z 26)))
              (h/vowels (hash/new hash/eq/null 'a #t 'e #t 'i #t 'o #t 'u #t))
              (h/1 (hash/new hash/eq/null 'foo 'bar)))
          (list (hash/empty? (hash/cdr h/1)) (hash/empty? h/1) (hash/empty? h/vowels) (hash/empty? h/26))))
      
      '(#t #f #f #f)))

    (test-case
     "hash/contains?"
     (check-equal?
      
      ((compile/start)
       '(let ((h/vowels (hash/new hash/eq/null 'a #t 'e #t 'i #t 'o #t 'u #t)))
          (list
           (hash/contains? h/vowels 'a)
           (hash/contains? h/vowels 'e)
           (hash/contains? h/vowels 'i)
           (hash/contains? h/vowels 'o)
           (hash/contains? h/vowels 'u)
           (hash/contains? h/vowels 'z))))
      
      '(#t #t #t #t #t #f)))

    (test-case
     "hash/fold"
     (check-equal?

      ((compile/start)
       '(let ((h/26
               (list/hash
                hash/eq/null
                '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                    k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                    u 21 v 22 w 23 x 24 y 25 z 26))))
          (hash/fold
           h/26
           (lambda (_key value seed) (if (< value seed) value seed))
           9999)))
      1)) ; The least value among all key/value pairs.

    (test-case
     "hash/map"
     (check-equal?
      
      ((compile/start)
       '(let* ((h/26
                (list/hash
                 hash/eq/null
                 '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                     k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                     u 21 v 22 w 23 x 24 y 25 z 26)))
               ; Generate a hash table whose keys are the string <letter>.m where m is the ordinal number of the letters's order
               ; in the alphabet and where each value is just #t.
               (map (hash/map
                     h/26
                     (lambda (key value)
                       (cons
                        (string-append (symbol->string key) "." (number->string value))
                        #t)))))
          (sort (hash/keys map) string<?)))
      
      '("a.1" "b.2" "c.3" "d.4" "e.5" "f.6" "g.7" "h.8" "i.9" "j.10"
              "k.11" "l.12" "m.13" "n.14" "o.15" "p.16" "q.17" "r.18" "s.19" "t.20"
              "u.21" "v.22" "w.23" "x.24" "y.25" "z.26")))

  (test-case
   "hash/filter"
   (check-equal?
    
    ((compile/start)
     '(let* ((h/26
              (list/hash
               hash/eq/null
               '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                   k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                   u 21 v 22 w 23 x 24 y 25 z 26)))
             (h/odd (hash/filter h/26 (lambda (_key value) (odd? value))))
             (less? (lambda (alpha beta) (< (cdr alpha) (cdr beta)))))
        (sort (hash=>pairs h/odd) less?)))
    
    '((a . 1) (c . 3)  (e . 5)  (g . 7)  (i . 9)
              (k . 11) (m . 13) (o . 15) (q . 17) (s . 19)
              (u . 21) (w . 23) (y . 25))))

  (test-case
   "hash/partition"
   (check-equal?
    
    ((compile/start)
     '(let* ((h/26
              (list/hash
               hash/eq/null
               '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                   k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                   u 21 v 22 w 23 x 24 y 25 z 26)))
             (partition (hash/partition h/26 (lambda (key value) (odd? value))))
             (less? (lambda (alpha beta) (< (cdr alpha) (cdr beta))))
             (odd  (sort (hash=>pairs (car partition)) less?))
             (even (sort (hash=>pairs (cdr partition)) less?)))
        (list odd even)))
    
    '(((a . 1) (c . 3) (e . 5)  (g . 7) (i . 9)
               (k . 11) (m . 13) (o . 15) (q . 17) (s . 19)
               (u . 21) (w . 23) (y . 25))
      
      ((b . 2) (d . 4) (f . 6) (h . 8) (j . 10)
               (l . 12) (n . 14) (p . 16) (r . 18) (t . 20)
               (v . 22) (x . 24) (z . 26)))))

  (test-case
   "hash/for-each"
   (check-equal?
    
    ((compile/start)
     '(let ((h/26
             (list/hash
              hash/eq/null
              '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                  k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                  u 21 v 22 w 23 x 24 y 25 z 26))))
        (hash/for-each
         h/26
         (lambda (key value) (display (cons key value)) (newline)))
        (hash/length h/26)))
    
    26)))
; ------------------------

(define-test-suite set-tests
  (test-case
   "set/new and set/list"
   (check-equal?

    ((compile/start)
     '(let ((s/alphabet
             (set/new set/eq/null
                      'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm
                      'n 'o 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z))
            (less? (lambda (alpha beta)
                     (string<? (symbol->string alpha) (symbol->string beta)))))
        (sort (set/list s/alphabet) less?)))

    '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

  (test-case
   "set/remove"
   (check-equal?
    
    ((compile/start)
     '(let ((s/alphabet
             (list/set set/eq/null
                       '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
            (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                     (string<? (symbol->string alpha) (symbol->string beta)))))
        ; Remove the vowels.
        (let loop ((s s/alphabet) (vowels '(a e i o u)))
          (if (null? vowels)
              (sort (set/list s) less?) ; Sorted alphabet but without the vowels.
              (loop (set/remove s (car vowels)) (cdr vowels))))))
    
    '(b c d f g h j k l m n p q r s t v w x y z)))

  (test-case
   "set/cons"
   (check-equal?
    
    ((compile/start)
     '(let ((consonants
             (list/set set/eq/null '(b c d f g h j k l m n p q r s t v w x y z)))
            (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                     (string<? (symbol->string alpha) (symbol->string beta)))))
        ; Add the vowels.
        (let loop ((s consonants) (vowels '(a e i o u)))
          (if (null? vowels)
              (sort (set/list s) less?) ; Sorted complete alphabet.
              (loop (set/cons s (car vowels)) (cdr vowels))))))
    
    '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

  (test-case
   "set/union"
   (check-equal?

    ((compile/start)
     '(let ((consonants
             (list/set set/eq/null '(b c d f g h j k l m n p q r s t v w x y z)))
            (vowels (list/set set/eq/null '(a e i o u)))
            (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                     (string<? (symbol->string alpha) (symbol->string beta)))))
        (sort (set/list (set/union consonants vowels)) less?)))

    '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

  (test-case
   "set/intersection"
   (check-equal?

    ((compile/start)
     '(let ((alphabet
             (list/set set/eq/null
                       '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
            (other (list/set set/eq/null '(a 1 e 5 i 9 o 15 u 21)))
            (less? (lambda (alpha beta) 
                     ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                     (string<? (symbol->string alpha) (symbol->string beta)))))
        (sort (set/list (set/intersection alphabet other)) less?)))

    '(a e i o u)))

  (test-case
   "set/difference"
   (check-equal?

    ((compile/start)
     '(let ((alphabet
             (list/set set/eq/null 
                       '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
            (other (list/set set/eq/null '(a 1 e 5 i 9 o 15 u 21)))
            (less? (lambda (alpha beta)
                     ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                     (string<? (symbol->string alpha) (symbol->string beta)))))
        (sort (set/list (set/difference alphabet other)) less?)))

    '(b c d f g h j k l m n p q r s t v w x y z)))

  (test-case
   "set/contains?"
   (check-equal?

    ((compile/start)
     '(let* ((alphabet
              (list/set set/eq/null 
                        '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
             (other '(a 1 e 5 i 9 o 15 u 21))
             (outside 
              ; Collect all members of other not appearing in alphabet (namely, the digits).
              (let loop ((other other) (contains null))
                (if (null? other)
                    contains
                    (if (not (set/contains? alphabet (car other)))
                        (loop (cdr other) (cons (car other) contains))
                        (loop (cdr other) contains))))))
        (sort outside <)))

    '(1 5 9 15 21)))

  (test-case
   "set/car"
   (check-equal?

    ((compile/start) 
     '(let loop ((digits (list/set set/eq/null '(1 5 9 15 21)))
                 (outcome null))
        (if (set/empty? digits)
            (sort outcome <)
            (loop (set/cdr digits) (cons (set/car digits) outcome)))))

    '(1 5 9 15 21)))

  (test-case
   "set/length"
   (check-equal?

    ((compile/start)
     '(let ((alphabet
             (list/set set/eq/null 
                       '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
            (digits
             (set/new set/eq/null 0 1 2 3 4 5 6 7 8 9))
            (empty
             (set/new set/equal/null)))
        (list (set/length alphabet) (set/length digits) (set/length empty))))

    '(26 10 0)))

  (test-case
   "set/subset?"
   (check-equal? 

    ((compile/start)
     '(let ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
            (digits   (set/new set/eq/null 0 1 2 3 4 5 6 7 8 9))
            (empty    (set/new set/equal/null))
            (vowels   (list/set set/eq/null '(a e i o u)))
            (mixed    (list/set set/eq/null '(a e 7))))
        (list
         (set/subset? alphabet set/eq/null) ; The empty set is a subset of every set.
         (set/subset? alphabet digits)      ; Nope.
         (set/subset? alphabet vowels)      ; Yup.
         (set/subset? alphabet mixed))))    ; Nope.
  
    '(#t #f #t #f)))

  (test-case
   "set/fold"
   (check-equal? 

    ((compile/start)
     '(let* ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
             (digits   (set/new set/eq/null 0 1 2 3 4 5 6 7 8 9))
             (empty    (set/new set/equal/null))
             (vowels   (list/set set/eq/null '(a e i o u)))
             (mixed    (list/set set/eq/null '(a e 7)))
             (all      (set/new set/equal/null alphabet digits empty vowels mixed)))
        ; Sum the lengths of all sets whose cardinality > 3.
        (set/fold
         all
         (lambda (x seed)
           (if (> (set/length x) 3)
               (+ (set/length x) seed)
               seed))
         0)))

    41))

  (test-case
   "set/map"
   (check-equal?

    ((compile/start) 
     '(let* ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
             (digits   (set/new set/eq/null 0 1 2 3 4 5 6 7 8 9))
             (empty    (set/new set/equal/null))
             (vowels   (list/set set/eq/null '(a e i o u)))
             (mixed    (list/set set/eq/null '(a e 7)))
             (colors   (list/set set/eq/null '(red white blue)))
             (all      (set/new set/equal/null alphabet digits empty vowels mixed colors)))
        (sort
         (set/list
          ; Map all into the set lengths of its members.
          (set/map all (lambda (x) (set/length x))))
         <)))
  
    '(0 3 5 10 26)))

  (test-case
   "set/filter"
   (check-equal?

    ((compile/start)
     '(let ((s (set/new set/eq/null
                        1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                        16 17 18 19 20 21 22 23 24 25 26)))
        (sort (set/list (set/filter s odd?)) <)))

    '(1 3 5 7 9 11 13 15 17 19 21 23 25)))

  (test-case
   "set/partition"
   (check-equal?

    ((compile/start)
     '(let* ((all  (set/new set/eq/null
                            1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                            16 17 18 19 20 21 22 23 24 25 26))
             (odd  (set/filter all odd?))
             (even (set/filter all even?))
             (both (set/partition all odd?))) ; Should be (odd . even).
        (list
         (set/empty? (set/intersection odd even))
         (set/subset? (car both) odd)
         (set/subset? odd (car both))
         (set/subset? (cdr both) even)
         (set/subset? even (cdr both)))))

    '(#t #t #t #t #t)))
  
  (test-case
   "set/vector"
   (check-equal?
    (let* ((alphabet
            ((compile/start) 
             '(begin (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))))
           (v (set=>vector/racket alphabet))
           (less? (lambda (alpha beta)
                     (string<? (symbol->string alpha) (symbol->string beta)))))
      (sort (vector->list v) less?))

    '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
  
  (test-case
   "set (de)serialize #1"
   (check-equal?
   
    (let ((s ((compile/start)
               '(begin (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))))
          (less? (lambda (alpha beta)
                    (string<? (symbol->string alpha) (symbol->string beta)))))
      (pretty-display (motile/serialize s))
      (sort
       (set/list (motile/deserialize (motile/serialize s) #f))
       less?))

    '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
  
    (test-case
   "set (de)serialize #2"
   (check-equal?
   
    (let ((code
           ((compile/start)
            '(let ((s (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z))))
               (lambda () s))))
          (less? (lambda (alpha beta)
                    (string<? (symbol->string alpha) (symbol->string beta)))))
      (pretty-display (motile/serialize code))
      (sort
       (set/list (motile/call (motile/deserialize (motile/serialize code) #f) ENVIRON/TEST))
       less?))

    '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

  )



; ---------------------------

(define-test-suite tuple-tests
  (test-case
   "Basic tuple construction"
   (check-equal?
    ((compile/start) '(let ((t (tuple 'a 'b 'c 'd 'e)))
                      (tuple/list t)))
    '(a b c d e)))
  
  (test-case
   "Tuple/build"
   (check-equal?
    ((compile/start)
     '(let ((t (tuple/build 5 (lambda (i) (* i 3)))))
        (tuple/list t)))
    '(0 3 6 9 12)))
  
  (test-case
   "Tuple/filter"
   (check-equal?
    ((compile/start)
     '(tuple/list
       (tuple/filter
        (tuple 1 2 3 4 5)
        (lambda (x) (even? x)))))
  
    '(2 4)))

  (test-case
   "Tuple/map"
   (check-equal?
    ((compile/start)
     '(tuple/list
       (tuple/map (tuple 1 2 3 4 5) (lambda (n) (1- n)))))

    '(0 1 2 3 4)))


  (test-case
   "Tuple/partition"
   (check-equal? 
    ((compile/start)
     '(let ((p (tuple/partition (tuple 1 2 3 4 5) (lambda (n) (even? n)))))
        (cons (tuple/list (car p)) (tuple/list (cdr p)))))

    '((2 4) 1 3 5))))

; -------------------------

(define-test-suite box-tests
  (test-case
   "Box/set/unbox"
   (check-equal?
    ((compile/start)
     '(let* ((a (box 99))
             (b (box "foobar"))
             (c (string=? (unbox b) "foobar")))
        (box! b "zzz")
        (list (box? a) (unbox a) c (unbox b))))
    '(#t 99 #t "zzz"))))

; ----------------------------

(define-test-suite closure-tests
  (test-case
   "Closure test 1"
   (check-equal?
    '(100 200 300)
    ((compile/start)
     '(let ((f (lambda (n) (lambda () n))))
        (let ((f100 (f 100))
              (f200 (f 200))
              (f300 (f 300)))
          (list (f100) (f200) (f300)))))))
  
  (test-case
   "Shipping closures to motile/call"
   (check-equal?
    '(100 200 300)
    (map
     (lambda (c) (motile/call c ENVIRON/TEST))
     ((compile/start)
      '(let ((f (lambda (n) (lambda () n))))
         (let ((f100 (f 100))
               (f200 (f 200))
               (f300 (f 300)))
           (list f100 f200 f300)))))))
  
  (test-case
   "Forming closures over multiple lexical scopes"
   (check-equal?
    '((100 200 300) (109 209 309) (153 253 353))
    ((compile/start)
     '(let ()
        (define (f a b c)
          ((lambda (x)
             ((lambda (y)
                (lambda () (list x y c)))
              b))
           a))
        (let ((f100 (f 100 200 300))
              (f109 (f 109 209 309))
              (f153 (f 153 253 353)))
          (list (f100) (f109) (f153))))))))

; -------------------------------------


        
        

(define-test-suite record-tests
  (test-case
   "record/1"
   (check-equal?
    ((compile/start)
     '(let ()
        (record sample a 1 b 2 c "silly")))
    ;#(<record> sample #(a b c) 1 2 "silly")))
    (vector '<record> 'sample (hash/new hash/eq/null 'a 1 'b 2 'c "silly") #f)))
  
  (test-case
   "record does not confuse field names with bindings in lexical scope"
   (check-equal?
    ((compile/start)
     '(let ((a 1) (b 2) (c "silly"))
        (record sample a a b b c c)))
    ;#(<record> sample #(a b c) 1 2 "silly")))
    (vector '<record> 'sample (hash/new hash/eq/null 'a 1 'b 2 'c "silly") #f)))

  (test-case
   "record evaluates expressions for field values"
   (check-equal?
    ((compile/start)
     '(let ((x 1) (y 2) (z "foo"))
        (record sample a (add1 x) b (* 3 y) c (string-append z "bar"))))
    ;#(<record> sample #(a b c) 2 6 "foobar")))
    (vector '<record> 'sample (hash/new hash/eq/null 'a 2 'b 6 'c "foobar") #f)))
  
  (test-case
   "record expression evaluation interacts with call/cc correctly"
   (check-equal?
    ((compile/start)
     '(let ()
        (record sample a 1 b 2 c 3 d (call/cc (lambda (k) (* 5 (k 4)))) e 5 f 6)))
     ;#(<record> sample #(a b c d e f) 1 2 3 4 5 6)))
    (vector '<record> 'sample (hash/new hash/eq/null 'a 1 'b 2 'c 3 'd 4 'e 5 'f 6) #f)))
             
  (test-case
   "record/cons is persistent after one update"
   (check-equal?
    ((compile/start)
     '(let* ((r1 (record sample a 1 b 2 c "silly"))
             (r2 (record/cons r1 c "serious")))
        (list r1 r2)))
    ;'(#(<record> sample #(a b c) 1 2 "silly")
    ;  #(<record> sample #(a b c) 1 2 "serious"))))
    (list
     (vector '<record> 'sample (hash/new hash/eq/null 'a 1 'b 2 'c "silly")   #f)
     (vector '<record> 'sample (hash/new hash/eq/null 'a 1 'b 2 'c "serious") #f))))

 (test-case
   "record/cons is persistent after two successive updates"
   (check-equal?
    ((compile/start)
     '(let* ((r1 (record sample a 1 b 2 c "silly"))
             (r2 (record/cons r1 c "serious"))
             (r3 (record/cons r2 a 11)))
        (list r1 r3)))
    ;'(#(<record> sample #(a b c) 1 2 "silly")
    ;  #(<record> sample #(a b c) 11 2 "serious"))))
    (list
     (vector '<record> 'sample (hash/new hash/eq/null 'a 1  'b 2 'c "silly")   #f)
     (vector '<record> 'sample (hash/new hash/eq/null 'a 11 'b 2 'c "serious") #f))))
     
  (test-case
   "record/cons accepts a pair of updates"
   (check-equal?
    ((compile/start)
      '(let* ((r1 (record sample a 1 b 2 c "silly"))
             (r2 (record/cons r1 c "serious" a 11)))
         (list r1 r2)))
    ;'(#(<record> sample #(a b c) 1 2 "silly")
    ;  #(<record> sample #(a b c) 11 2 "serious"))))
    (list
     (vector '<record> 'sample (hash/new hash/eq/null 'a 1  'b 2 'c "silly")   #f)
     (vector '<record> 'sample (hash/new hash/eq/null 'a 11 'b 2 'c "serious") #f))))

  (test-case
   "record/cons accepts a triple of updates"
   (check-equal?
    ((compile/start)
     '(let* ((r1 (record sample a 1 b 2 c "silly"))
             (r2 (record/cons r1 c "serious" a 11 b 99)))
        (list r1 r2)))
    ;'(#(<record> sample #(a b c) 1 2 "silly")
    ;  #(<record> sample #(a b c) 11 99 "serious"))))
    (list
     (vector '<record> 'sample (hash/new hash/eq/null 'a 1  'b 2  'c "silly")   #f)
     (vector '<record> 'sample (hash/new hash/eq/null 'a 11 'b 99 'c "serious") #f))))
    
  (test-case
   "record/cons accepts a large number of updates"
   (check-equal?
    ((compile/start)
     '(let* ((r1 (record sample a 1 b 2 c "silly" d #f e #f f #f))
             (r2 (record/cons r1 c "serious" a 11 b 99 d 1 e 2 f 3 d 111 e 222 f 333))) ; Later fields overwrite earlier. 
        (list r1 r2)))
    ;'(#(<record> sample #(a b c d e f)  1  2 "silly"   #f  #f  #f)
    ;  #(<record> sample #(a b c d e f) 11 99 "serious" 111 222 333))))
    (list
     (vector '<record> 'sample (hash/new hash/eq/null 'a 1  'b 2  'c "silly"   'd #f  'e #f  'f #f)  #f)
     (vector '<record> 'sample (hash/new hash/eq/null 'a 11 'b 99 'c "serious" 'd 111 'e 222 'f 333) #f))))

  (test-case
   "record/ref accesses all fields"
   (check-equal?
    ((compile/start)
     '(let ((r (record sample a 1 b 2 c "silly")))
        (list
         (record/ref r c)
         (record/ref r a)
         (record/ref r b))))
    (list "silly" 1 2)))

  (test-case
   "record/ref provides a failure value"
   (check-equal?
    ((compile/start)
     '(let ((r (record sample a 1 b 2 c "silly")))
        (list
         (record/ref r c)
         (record/ref r foo (+ 29 (record/ref r b))) ; The failure value is 31.
         (record/ref r a)
         (record/ref r b))))
    (list "silly" 31 1 2)))

  (test-case
   "record?"
   (check-equal?
    ((compile/start)
     '(let ((r (record sample a 1 b 2 c "silly")))
        (list (record? r) (record? 33) (record? (tuple r)))))
    '(#t #f #f))) 
  
  (test-case
   "record/kind"
   (check-equal?
    ((compile/start)
     '(let ((r (record sample a 1 b 2 c "silly"))
            (s (record tiny x 999))
            (t (record large a 1 b 2 c "silly" d 44 e 55 f 66)))
        (list (record/kind r) (record/kind s) (record/kind t))))
    '(sample tiny large)))
  
  (test-case
   "record/fields"
   (check-equal?
    ((compile/start)
     '(let ((r (record sample a 1 b 2 c "silly"))
            (s (record tiny x 999))
            (t (record large a 1 b 2 c "silly" d 44 e 55 f 66)))

        ; Helper routine.
        ; We need this because the record keys don't necessarily appear internally
        ; in the same order as they were given in the (record ...) constructor.
        (define (set/equal? x y)
          (and
           (= (length x) (length y))
           (let loop ((x x))
             (cond
               ((null? x) #t)
               ((memq (car x) y) (loop (cdr x)))
               (else #f)))))

        ;(list (tuple/list (record/keys r)) (tuple/list (record/fields s)) (tuple/list (record/fields t)))))
        (list (set/equal? '(a b c) (record/keys r))
              (set/equal? '(x) (record/keys s)) 
              (set/equal? '(a b c d e f) (record/keys t)))))
    ;(list '(a b c) '(x) '(a b c d e f))))
    '(#t #t #t)))
)

; -------------------------------------

(define-test-suite binding-environment-tests
  (test-case
   "environ/null is available in the Motile BASELINE enviroment"
   (check-eq?
    ((compile/start) '(begin environ/null))
    environ/null))

  (test-case
   "environ/cons requires that all arguments after environ expression are symbols"
   (check-exn
    exn:fail?
    (lambda ()
      ((compile/start)
       '(let ((a "first")
              (b "second")
              (c 3))
          (environ/cons environ/null a 1951 x)))) ; 1951 is not a legal argument.
    "The Motile compiler should issue an error exception"))
   
  (test-case
   "environ/cons requires that all identifiers following the environ expression are in lexical scope"
   (check-exn
    exn:fail?
    (lambda ()
      ((compile/start)
       '(let ((a "first")
              (b "second")
              (c 3))
          (environ/cons environ/null a b cons c)))) ; cons is not in lexical scope.
    "The Motile compiler should issue an error exception"))

  (test-case
   "environ/cons adds a binding taken from lexical scope"
   (check-equal?
    (let ((E ((compile/start)
               '(let ((silly 1951))
                  (environ/cons environ/null silly))))) ; Create an environ with a single binding silly/1951.
       (environ/ref E 'silly #f))
    1951))
   
  (test-case
   "Add multiple bindings at once to a binding environment"
   (check-equal?
    (let ((E ((compile/start)
              '(let ((a "first")
                     (b "second")
                     (c 3))
                 (environ/cons environ/null a b c)))))
      ; Here we use the version of environ/symbol/value available to Racket.
      (list (environ/ref E 'c #f) (environ/ref E 'b #f) (environ/ref E 'a #f)))

    '(3 "second" "first")))
    
  (test-case
   "Query an environ using environ/ref"
   (check-equal?
    ((compile/start)
     '(let ((a 100) (b 200) (c 300))
        (let ((E (environ/cons environ/null a b c)))
          (list (environ/ref E c #f) (environ/ref E a #f) (environ/ref E b #f)))))
    
    '(300 100 200)))
    
  (test-case
   "Query an nested environ using a path to environ/ref"
   (check-equal?
    '(100 200 300)
    ((compile/start)
     '(let ((a 100) (b 200) (c 300))
        (let* ((E_c (environ/cons environ/null c))
               (E_b (environ/cons environ/null b E_c))
               (E_a (environ/cons environ/null a E_b)))
          (list (environ/ref E_a a #f) (environ/ref E_a #(E_b b) #f) (environ/ref E_a #(E_b E_c c) #f)))))))

  (test-case
   "environ/ref using a symbol not in the binding environ"
   (check-equal?
    ((compile/start)
     '(let ((a 100) (b 200) (c 300))
        (let ((E (environ/cons environ/null a b c)))
          (list (environ/ref E c #f)
                (environ/ref E a #f)
                (environ/ref E x #f) ; x is not a binding in E.
                (environ/ref E b #f)))))
    
    '(300 100 #f 200))) ; The value #f comes from the query for symbol x.

  (test-case
   "environ/remove"
   (check-equal?
    ((compile/start)
     '(let ((a 100) (b 200) (c 300))
        (let* ((E_0 (environ/cons environ/null a b c))
               (E_1 (environ/remove E_0 b)))
          (list (environ/ref E_1 c #f)
                (environ/ref E_1 a #f)
                (environ/ref E_1 b #f)))))
    '(300 100 #f))) ; The value #f comes from the query for symbol b in E_1.
  
  (test-case
   "environ/capture"
   (check-eq?
    ((compile/start) '(let () (environ/capture)))
    ENVIRON/TEST))
  
  (test-case
   "environ/reflect"   
   (check-equal?
    ((compile/start)
     '(let ((global (let ((a 100) (b 200) (c 300) (plus +))
                      (environ/cons environ/null a b c plus))))
        (environ/reflect global (plus a b c))))
    600)))


; -----------------------

;; There was a serious bug in the compiler with regard to nested defines (and hence nested letrecs)
;; that is illustrated by some of the tests in nested-define-tests.
(define-test-suite nested-define-tests
  (test-case
   "Basic factorial with nested def"
   (check-equal?
    ((compile/start)
     '((lambda ()
         (define (fact n) (if (= n 1) 1 (* n (fact (sub1 n)))))
         (fact 5))))
    120))

  (test-case
   "Hand compilation of nested defines in a letrec body"
   (check-equal?
    ((compile/start)
     '((lambda (a b)
         (letrec ((a* (lambda () (* a (b-))))
                  (b- (lambda () (sub1 b))))
           (a*)))
       8 -1))
    -16)

   (check-equal?
    ((compile/start)
     '((lambda (a b)
         (define (a*) (* a (b-)))
         (define (b-) (sub1 b))
         (a*))
       8 -1))
    -16))

  (test-case
   "Exposes a bug in the code generation for nested defines"
   (check-equal?
    ((compile/start)
     '((lambda (a b)
         (define (a*)
           (define (b-) -2)
           (* a (b-)))
         (a*))
       8 -1))
    -16))

(test-case
   "This test fails as well---returning a compiler-generated closure---indicating a problem with stack accounting and address generation for closed variables"
   (check-equal?
    ((compile/start)
     '((lambda (a b)
         (define (a*)
           (define (b-) -2)
           a)
         (a*))
       8 -1))
    8))

  (test-case
   "This test fails catastrophically with an exception in stack addressing"
   (check-equal? 
    ((compile/start)
     '((lambda (a b)
         (define (a*)
           (define (b-) -2)
           b)
         (a*))
       8 -1))
    -1))

  (test-case
   "Hand compilation of the above test case, fails same as above"
   ; This test is a hand-compilation of test 2d above into the letrec representation used by the compiler.
   ; It fails in exactly the same way as test 2d.
   ; There is an error in the stack accounting and address generation.
   (check-equal?
    ((compile/start)
     '((lambda (a b)
         (letrec
             ((a* (lambda ()
                    (letrec ((b- (lambda () (sub1 b))))
                      (* a (b-))))))
           (a*)))
       8 -1))
    -16)))

; --------------------

(define-test-suite frame-bug-tests
  (test-case
   "Nested let bindings expose bug"
   (check-equal?
    ((compile/start)
     '(let ((f (lambda ()
                 (let ((v (+ 2 1 3)))
                   (let loop ()
                     (let ((z 1))
                       (+ z v)))))))
        (f)))
    7))
  
  (test-case
   "A simpler version of the above"
   (check-equal?
    ((compile/start)
     '(let ((f (lambda ()
                 (let ((v 6))
                   (let loop ()
                     (let ((z 1))
                       (+ z v)))))))
        (f)))
    7))
  
  (test-case
   "Generates the same kind of error as the above"
   (check-equal?
    ((compile/start)
     '(let ((v 6))
        (let loop ()
          (let ((z 1))
            (+ z v)))))
    7)))

;; unit tests
(define (compile/tests/all)
  (run-tests constants)
  (run-tests baseline)
  (run-tests lambda-exprs)
  (run-tests let-exprs)
  (run-tests letrec-exprs)
  (run-tests let*-exprs)
  (run-tests named-let-exprs)
  (run-tests define-exprs)
  (run-tests begin-exprs)
  (run-tests when-exprs)
  (run-tests unless-exprs)
  (run-tests cond-exprs)
  (run-tests case-exprs)
  (run-tests do-exprs)
  (run-tests and-exprs)
  (run-tests or-exprs)
  (run-tests apply-exprs)
  (run-tests sort-tests)
  (run-tests quasiquote-exprs)
  (run-tests macro-exprs)
  (run-tests for-each-exprs)
  (run-tests map-exprs)
  (run-tests box-tests)
  (run-tests call/cc-exprs)
  (run-tests closure-tests)
  (run-tests binding-environment-tests)
  (run-tests tuple-tests)
  (run-tests record-tests)
  (run-tests vector-tests)
  (run-tests set-tests)
  (run-tests hash-tests)
  ;; regression tests
  (run-tests nested-define-tests)
  (run-tests frame-bug-tests))
