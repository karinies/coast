
#lang racket/base

(require "../compile/compile.rkt"
         "../generate/baseline.rkt"
         "../baseline.rkt")
         ;"../../persistent/environ.rkt")

;;#! /usr/bin/env racket


(define test01
  (motile/call
   
   (motile/compile
   '(let ((B (environ/capture)))
      (define-macro (:: e symbol . arguments)
        `((environ/ref ,e ,symbol #f) ,@arguments))
      
      (:: B + 1 2 3)))

   BASELINE))


;; NOTE: environ/cons is too clumsy.
;; An alternative syntax is (environ/cons <environ-expression> <addition> ... <addition>) where each addition is:
;; (1) a name that is defined in lexical scope or
;; (2) a pair (name_1 . name_2) where name_1 is a symbol and name_2 is a name within lexical scope.
;; In case (1) the value denoted by name is added to the environ with name as the key.
;; In case (2) the value denoted by name_2 is added to the environ with name_1 as the key.
;; In essence case (1) preserves the name/value pair as it appears in lexical scope and
;; case (2) is essentially a renaming where name_1 is the new name denoting the value affiliated with name_2 in lexical scope.


(define test02
  (motile/call
   
   (motile/compile
    '(let ()
       ; Apply the value of symbol in environ e to: e a_1 ... a_n where e is the environ and arguments = a_1 ... a_n.
       (define-macro (!! e symbol . arguments)
         `((environ/ref ,e ,symbol #f) ,e ,@arguments))
       
       (define (Point x y)
         (let
             ((x: (lambda (_) x))
              (y: (lambda (_) y)))
           (environ/cons environ/null x: x: y: y:)))
       
       (let ((p (Point 19 22)))
         (list (!! p x:) (!! p y:)))))

   BASELINE))

(define test03
  (motile/call
   
   (motile/compile
    '(let ()
       ; Apply the value of symbol in environ e to: e a_1 ... a_n where e is the environ and arguments = a_1 ... a_n.
       (define-macro (!! e symbol . arguments)
         `((environ/ref ,e ,symbol #f) ,e ,@arguments))
       
       (define (Point x y)
         (let
             ((x: (lambda (_) x))
              (y: (lambda (_) y)))
           (environ/cons environ/null x: x: y: y:)))
       
       (define (Rectangle left right)
         (let
             ((left:  (lambda (_) left)) ; Lower left corner.
              (right: (lambda (_) right)) ; Upper right corner.
              (width  (lambda (_) (- (!! right x:) (!! left x:))))
              (height (lambda (_) (- (!! right y:) (!! left y:))))
              (area   (lambda (self) (* (!! self width) (!! self height)))))
           (environ/cons
            environ/null
            left:  left:
            right: right:
            width  width
            height height
            area   area)))
       
       (let ((r (Rectangle (Point 2 3) (Point 7 6))))
         (list 'width (!! r width) 'height (!! r height) 'area (!! r area)))))

   BASELINE))

(define (go . arguments)
  (motile/call/3 test01 BASELINE arguments))
