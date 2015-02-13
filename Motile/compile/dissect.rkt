#lang racket/base

;; Copyright 2010 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; A collection of utility functions for dissecting Scheme forms.

(provide
 ; (quote ...)
 quote?
 quotation

 ; (if <test> <then> <else>)
 if?
 if/test
 if/then
 if/else
 
 ; (when <test> <thens>)
 when?
 when/test
 when/thens
 
 ; (unless <test> <elses>)
 unless?
 unless/test
 unless/elses
 
 ; (begin <expressions>)
 begin?
 begin/expressions
 
 ; (cond <clauses>)
 cond?
 cond/clauses
 cond/clause/else?
 cond/clause/=>?
 cond/clause/test
 cond/clause/procedure

 ; (case <clauses>)
 case?
 case/key
 case/clauses
 case/clause/datums?
 case/clause/else?
 case/clause/datums
 case/clause/expressions
 
 definition/name
 definition/value

 do?
 do/bindings
 do/test
 do/test/expressions
 do/commands
 do/binding/variable
 do/binding/initial
 do/binding/step

 ; (lambda <parameters> <body>)
 lambda?
 lambda/parameters
 lambda/body
 ; (and <expressions>)
 and?
 ; (or <expressions>)
 or?

 ; For picking apart the forms: (let ...), (let* ...), and (letrec ...).
 let/bindings
 let/named/bindings
 let/named/body
 let/body
 let/binding/symbol
 let/binding/expression
 let/bindings/variables
 let/bindings/expressions
 
 ; quasiquotation, as in, `(foo ,bar @,tail).
 quasiquote?
 unquote?
 unquote-splicing?
 
 ;; (record name <tag> <expression> <tag> <expression> ...)
 motile/record? ; To avoid conflict with Racket primitive.
 record/name
 record/pairs
 record/pairs/tag
 record/pairs/expression
 record/pairs/next
 
 ;; (record/cons r <tag_1> <expression_1> <tag_2> <expression_2> ...)
 record/cons?
 record/cons/record
 record/cons/pairs
 record/cons/pairs/tag
 record/cons/pairs/expression
 record/cons/pairs/next
 
 ;; (record/ref r <tag> [<failure>])
 record/ref?
 record/ref/record
 record/ref/tag
 record/ref/failure
 
 ; (environ/cons e x_1 ... x_m)
 environ/cons?
 environ/cons/e       ; Deprecated.
 environ/cons/environ ; Synonym for environ/cons/e.
 environ/cons/identifiers
 
 ; (environ/remove e x_1 ... x_m)
 environ/remove?
 environ/remove/e       ; Deprecated.
 environ/remove/environ ; Synonym for environ/remove/environ.
 environ/remove/symbols
 
 ; (environ/ref e x v)
 environ/ref?
 environ/ref/environ
 environ/ref/accessor
 environ/ref/substitute
 
 ; (environ/reflect e e_1 ... e_m)
 environ/reflect?
 environ/reflect/e       ; Deprecated.
 environ/reflect/environ ; Synonym for environ/reflect/e.
 environ/reflect/expressions
 
 ; Macros.
 definition/macro?
 definition/macro/name
 definition/macro/formals
 definition/macro/body
 
 ; (define <symbol> <expression>)
 ; (define (<symbol> ...) <body>)
 ; (define (<symbol> ... . <rest>) <body>)
 definition?
 definition/lambda?
 definition/variable?
 definition/variable/ok?
 definition/variable/name
 definition/variable/expression
 definition/lambda/name
 definition/lambda/formals
 definition/lambda/body
 
 ; Is the form a symbol?
 variable?
 
 ; Does a form have the expected structure?
 shape)

(define (quote? e) (eq? (car e) 'quote))
(define (quotation e) (cadr e))

(define (if? e) (eq? 'if (car e)))
(define (if/test e) (cadr e))
(define (if/then e) (caddr e))
(define (if/else e) (cadddr e))

(define (when? e) (eq? 'when (car e)))
(define (when/test e) (cadr e))
(define (when/thens e)
  (let ((thens (cddr e)))
    (if (null? thens)
        (error "empty <thens> in (when <test> <thens>)")
        (if (> (length thens) 1)
            `(begin ,@thens)
            (car thens)))))

(define (unless? e) (eq? 'unless (car e)))
(define (unless/test e) (cadr e))
(define (unless/elses e)
    (let ((elses (cddr e)))
    (if (null? elses)
        (error "empty <elses> in (unless <test> <elses>)")
        (if (> (length elses) 1)
            `(begin ,@elses)
            (car elses)))))

(define (begin? e) (eq? 'begin (car e)))
(define (begin/expressions e) (cdr e))

(define (cond? e) (eq? 'cond (car e)))
(define (cond/clauses e) (cdr e))
(define (cond/clause/else? c) (eq? 'else (car c))) ; (else e_1 ... e_N).
(define (cond/clause/=>? c) ; (test => procedure).
   (eq? '=> (cadr c))) 
(define (cond/clause/test c) (car c)) ; (test ...).
(define (cond/clause/procedure c) (caddr c)) ; (test => procedure).

(define (case? e) (eq? 'case (car e)))
(define (case/key e) (cadr e))
(define (case/clauses e) (cddr e))
(define (case/clause/datums? c) (pair? (car c)))
(define (case/clause/else? c) (eq? 'else (car c)))
(define (case/clause/datums c) (car c))
(define (case/clause/expressions c) (cdr c))


(define (do? e) (eq? 'do (car e)))
(define (do/bindings e) (cadr e))
(define (do/test e) (caddr e))
(define (do/test/expressions t) (cdr t))
(define (do/commands e) (cdddr e))
(define (do/binding/variable b) (car b))
(define (do/binding/initial b) (cadr b))
(define (do/binding/step b) (caddr b))


(define (lambda? e) (eq? 'lambda (car e)))
(define (lambda/parameters e) (cadr e))
(define (lambda/body e) (cddr e))

(define (and? e) (eq? 'and (car e)))
(define (or? e) (eq? 'or (car e)))

(define (call/operator e) (car e))
(define (call/arguments e) (cdr e))

;; (let <bindings> <body>) or
;; (let <variable> <bindings> <body>).
;; (letrec <bindings> <body).
(define (let/bindings e) (cadr e))
(define (let/named/bindings e) (caddr e))
(define (let/named/body e) (cdddr e))
(define (let/body e) (cddr e))
(define (let/binding/symbol binding) (car binding))
(define (let/binding/expression binding) (cadr binding))

(define (let/bindings/variables bindings)
  (map (lambda (binding) (let/binding/symbol binding)) bindings))

(define (let/bindings/expressions bindings)
  (map (lambda (binding) (let/binding/expression binding)) bindings))

(define (quasiquote? e)
  (eq? 'quasiquote (car e)))

(define (unquote? e)
  (eq? 'unquote (car e)))

(define (unquote-splicing? e)
  (eq? 'unquote-splicing (car e)))

;; (record name <tag> <expression> <tag> <expression> ...)
(define (motile/record? e) (eq? 'record (car e)))
(define-syntax-rule (record/name e)  (cadr e))
(define-syntax-rule (record/pairs e) (cddr e)) ; (tag_1 expression_1 ...).
(define-syntax-rule (record/pairs/tag x)        (car x))
(define-syntax-rule (record/pairs/expression x) (cadr x))
(define-syntax-rule (record/pairs/next x)       (cddr x))

;; (record/cons r <tag_1> <expression_1> <tag_2> <expression_2> ...)
(define (record/cons? e) (eq? 'record/cons (car e)))
(define-syntax-rule (record/cons/record e) (cadr e))
(define-syntax-rule (record/cons/pairs e)  (cddr e)) ; (tag_1 expression_1 ...).
(define-syntax-rule (record/cons/pairs/tag x)        (cadr x))
(define-syntax-rule (record/cons/pairs/expression x) (cadr x))
(define-syntax-rule (record/cons/pairs/next x)       (cdr x))

;; (record/ref r <tag> [<failure>])
(define (record/ref? e) (eq? 'record/ref (car e)))
(define-syntax-rule (record/ref/record e)  (cadr e))
(define-syntax-rule (record/ref/tag e)     (caddr e))
(define-syntax-rule (record/ref/failure e) (cadddr e))
  

;; (environ/cons e x_1 ... x_m)
;; where e is an expression that evaluates to a binding environment and
;; x_1 ... x_m are identifiers in lexical scope.
(define (environ/cons? e) (eq? 'environ/cons (car e)))
(define (environ/cons/e e) (cadr e)) ; Deprecated.
(define-syntax-rule (environ/cons/environ e) (cadr e))
(define-syntax-rule (environ/cons/expression e) (cadr e))
(define (environ/cons/identifiers e) (cddr e))

;; (environ/remove e x_1 ... x_m)
;; where e is an expression that evaluates to a binding environment and
;; x_1 ... x_m are symbols.
(define (environ/remove? e) (eq? 'environ/remove (car e)))
(define (environ/remove/e e) (cadr e)) ; Deprecated.
(define-syntax-rule (environ/remove/environ e) (cadr e))
(define-syntax-rule (environ/remove/expression e) (cadr e))
(define (environ/remove/symbols e) (cddr e))

;; (environ/ref e x v)
;; where e is an expression that evaluates to a binding environment, x is an identifier in lexical scope,
;; and v is an expression that evaluates to a substitute value.
(define (environ/ref? e) (eq? 'environ/ref (car e)))
(define (environ/ref/e e) (cadr e))
(define-syntax-rule (environ/ref/environ e) (cadr e))
(define-syntax-rule (environ/ref/expression e) (cadr e))
(define (environ/ref/accessor e) (caddr e))
(define (environ/ref/substitute e) (cadddr e))

;; (environ/reflect e e_1 ... e_m)
;; where e is an expression that evaluates to a binding environment and e_1 ... e_m
;; are arbirary expressions that will be evaluated in the context of e.
(define (environ/reflect? e) (eq? 'environ/reflect (car e)))
(define (environ/reflect/e e) (cadr e)) ; Deprecated.
(define-syntax-rule (environ/reflect/environ e) (cadr e))
(define (environ/reflect/expressions e) (cddr e))

;; Returns #t if x is a symbol otherwise raises an error exception.
(define (variable? x)
  (if (symbol? x) #t (error (format "Identifier expected: ~a" x))))

;; Pick apart (define-macro (<name> <formals>) <body>).
(define (definition/macro? e) (eq? 'define-macro (car e)))
(define (definition/macro/name e) (caadr e)) 
(define (definition/macro/formals e) (cdadr e))
(define (definition/macro/body e) (cddr e))

;; Returns #t if e has form (define ...) and #f otherwise.
(define (definition? e) (and (pair? e) (eq? 'define (car e))))
;; Returns 3t if e has form (define <name> ...) and #f otherwise.
(define (definition/variable? e) (symbol? (cadr e))) 
(define (definition/lambda? e) (pair? (cadr e)))

(define (definition/variable/ok? e) ; (define <variable> <expression>).
  (shape e 3)
  (definition/variable? e))

(define (definition/variable/name e) (cadr e))
(define (definition/variable/expression e) (caddr e))

; Pick apart (define (<name> <formals>) <body>).
(define (definition/lambda/name e) (caadr e)) 
(define (definition/lambda/formals e) (cdadr e))
(define (definition/lambda/body e) (cddr e))

;; Extract the <name> from a definition:
;;   (define (f ...) <body>) or (define x <body>)
(define (definition/name e) 
  (shape e 3)
  (let ((name (if (definition/lambda? e) (definition/lambda/name e) (definition/variable/name e))))
    (if (symbol? name)
        name
        (error (format "Identifier expected in ~a but got ~a" e name)))))

;; e has one of two forms:
;;   (define <name> <expression>) or
;;   (define (<name> <parameters>) <body>) where <parameters> may be empty.
;; Return the expression for the intended value of <name>.
(define (definition/value e)
  (if (definition/lambda? e)
      `(lambda ,(definition/lambda/formals e) ,@(definition/lambda/body e))
      (definition/variable/expression e)))

;; Returns #t if the given form is a list containing at least n >= 0 elements.
(define (shape form n)
  (let loop ((form form) (n n) (l form))
    (cond
      ((<= n 0))
      ((pair? l)
       (loop form (- n 1) (cdr l)))
      (else
       (error "Ill-constructed form" form)))))