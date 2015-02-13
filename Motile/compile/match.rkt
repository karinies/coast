#lang racket/base

(require
 (only-in racket/list make-list)
 (only-in racket/vector vector-memq)
 racket/pretty
 (only-in "../persistent/tuple.rkt" tuple? tuple/length tuple/ref)
 (only-in "../persistent/vector.rkt" vector/persist? vector/length vector/null vector/ref))

(provide
 match/translate
 shape/exact
 shape/inexact)

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

;; Contact: mgorlick@acm.org

;; This is an non-hygienic macro implementation of (match ...)
;; inspired by jas-match.scm from http://schemecookbook.org/Cookbook/MacroExampleASimplePatternMatcher

;; Presently only (match <expression> <clause> ...+) is implemented
;; where <clause> may be (<pattern> (where <guard) <action> ...+) or (<pattern> <action> ...+).
;; The common variants:
;;    (match-lambda <clause> ...+)
;;    (match-let    ((<pattern> <expression>) ...) <expression> ...)
;;    (match-let*   ((<pattern> <exprression>) ...) <expression> ...)
;;    (match-letrec ((<pattern> <exprression>) ...) <expression> ...)
;; are NOT implemented.

;; The syntax of patterns are a subset of those found in the match implementation of Racket Scheme.
;; Many of its features are missing including:
;;   - quasi-patterns
;;   - match-define
;;   - the ooo and ook extension in list and vector patterns

;; The calling pattern is layered as follows:
;; match/translate =>
;;   match/guarded/translate =>
;;     match/logical/translate =>
;;       match/compound/translate =>
;;         match/simple/translate =>
;;           match/literal/translate
;; Whenever a translator is unsure of which layer applies it simply calls match/guarded/translate recursively.

;;; Returns #t if the given form is a list containing at least n > 0 elements.
;(define (shape form n)
;  (let loop ((form form) (n n))
;    (cond
;      ((zero? n) #t)
;      ((pair? form) (loop (cdr form) (sub1 n)))
;      (else #f))))

;; Simple accessors to pick a <clause> or <pattern> apart.
(define (element/1 x)    (cadr x))
(define (element/2 x)    (caddr x))
(define (element/rest x) (cddr x))

;; Detects (quote a) returning #t on success and #f otherwise.
(define (quote? x)
  (and (pair? x) (eq? 'quote (car x)) (shape/exact x 2)))

;; Returns #t if the pattern is a quoted symbol and #f otherwise.
(define (symbol/quoted? pattern)
  (and (quote? pattern) (symbol? (element/1 pattern))))

;; All of the translations are written in continuation-passing style (CPS) where success
;; denotes the success continuation and failure denotes the failure continuation.

;; Generate the source for comparing a value (bound to variable) to a literal datum.
(define (match/literal/translate variable datum success failure)
  (cond
    ((null? datum)
     ; Empty list.
     `(if (null? ,variable) ,success ,failure))
    ((boolean? datum)
     ; #t or #f.
     `(if (eq? ,variable ,datum) ,success ,failure))
    ((or (string? datum) (number? datum) (char? datum))
     ; strings, numbers, and characters.
     `(if (equal? ,variable ,datum) ,success ,failure))
    (else failure))) ; Not a literal.

;; Generate the source for comparing a value to a pattern that is either a quotation or a symbol.
;; An unquoted symbol means that a variable, denoted by the symbol, will be bound to the value
;; under match.
(define (match/simple/translate variable pattern success failure)
  (cond
    ((quote? pattern)
     ; pattern is (quote <element>).
     (if (symbol? (element/1 pattern))
         `(if (eq?    ,variable ,pattern) ,success ,failure)   ; <element> is a symbol. Success if variable is bound to that symbol.
         `(if (equal? ,variable ,pattern) ,success ,failure))) ; <element> is a literal datum. Success if variable is equal to that datum.

    ((symbol? pattern)
     ; pattern is a symbol.
     (if (eq? pattern '_)
         success                                  ; The pattern is the "ignore" variable.
         `(let ((,pattern ,variable)) ,success))) ; Bind a variable, named by the pattern.

    (else ; The pattern must be a literal as we have tried everything else.
     (match/literal/translate variable pattern success failure))))

;; Returns #t if the given form is a list containing at least n >= 0 elements and #f otherwise.
(define (shape/inexact form n)
  (let loop ((l form) (n n))
    (cond
      ((zero? n) #t)
      ((pair? l) (loop (cdr l) (sub1 n)))
      (else #f))))

;; Returns #t if the given form is a list containing exactly n >= 0 elements and #f otherwise.
(define (shape/exact form n)
  (let loop ((l form) (n n))
    (cond
      ((and (null? l) (zero? n)) #t)
      ((pair? l) (loop (cdr l) (sub1 n)))
      (else #f))))

;; Helper routine that generates the nested source code for matching a persistent vector.
(define (match/vector/translate variable patterns success failure i n)
  (if (= i n)
      success
      
      (let ((pattern (car patterns)))
        (if (eq? pattern '_) ; The pattern is the anonymous "ignore" variable.
            ; Don't even bother generating code for this pattern. Just skip to the next pattern.
            (match/vector/translate variable (cdr patterns) success failure (add1 i) n)
            
            ; It is a non-trivial pattern so generate the code.
            (let ((v_i (gensym (string-append "v_" (number->string i) "."))))
              `(let ((,v_i (vector/ref ,variable ,i)))
                 ,(match/unguarded/translate
                   v_i (car patterns)
                   (match/vector/translate variable (cdr patterns) success failure (add1 i) n) ; Success continuation is patterns p_{i+1} ...
                   failure)))))))

;; Helper routine that generates the nested source code for matching a Motile tuple.
(define (match/tuple/translate variable patterns success failure i n)
  (if (= i n)
      success
      
      (let ((pattern (car patterns)))
        (if (eq? pattern '_) ; The pattern is the anonymous "ignore" variable.
            ; Don't even bother generating code for this pattern. Just skip to the next pattern.
            (match/tuple/translate variable (cdr patterns) success failure (add1 i) n)
            
            ; It is a non-trivial pattern so generate the code.
            (let ((t_i (gensym (string-append "v_" (number->string i) "."))))
              `(let ((,t_i (tuple/ref ,variable ,i)))
                 ,(match/unguarded/translate
                   t_i (car patterns)
                   (match/tuple/translate variable (cdr patterns) success failure (add1 i) n) ; Success continuation is patterns p_{i+1} ...
                   failure)))))))

; fields should look like (<symbol_1> p_1 <symbol_2> p_2 ...).
; When called we know that fields contains an even number of elements.
(define (match/record/fields/ok? fields)
  (cond
    ((null? fields) #t)
    ((symbol? (car fields)) (match/record/fields/ok? (element/rest fields))) ; Skip over the pattern following the field tag.
    (else #f)))

(define (match/record/translate variable patterns success failure)
  (if (null? patterns)
      success

      (let ((tag     (car patterns))        ; Field name.
            (pattern (element/1 patterns))) ; Actual field pattern.
        (if (eq? pattern '_) ; The pattern is the anonymous "ignore" variable.
            ; In this context we interpret the anonymous variable as a test for the presence/absence of the named field.
            (let ((junk (gensym 'junk.)))
              `(let ((,junk (cons null null)))
                 (if (eq? (record/ref ,variable ,tag ,junk) ,junk) ; Try to access the field.
                     ,failure                                       ; No such field in this record.
                     ,(match/record/translate variable (element/rest patterns) success failure))))

            (let ((field (gensym (string-append (symbol->string tag) "."))))
              `(let ((,field (record/ref ,variable ,tag)))
                 ,(match/unguarded/translate
                   field pattern
                   (match/record/translate variable (element/rest patterns) success failure)
                   failure)))))))
                 
(define (list/nonempty? x) (and (list? x) (not (null? x))))

;; Generate the source for the compound patterns:
;;    (cons p_1 p_2)
;;    (list p_1 ...)
;;    (List-rest p_1 ... p_n)
;;    (tuple p_1 ...)
;;    (term (head <symbol>) p_1 ...)
;;    (vector p_1 ...)
;;    (app f p_1)
;;    (... p_1) 
;; where the p_i are themselves patterns.
;; If the given pattern is not one of the compound patterns then we assume it is a simple pattern.
(define (match/compound/translate variable pattern success failure)  
  (if (list/nonempty? pattern) ; Excludes null and improper lists.
      (let ((n (sub1 (length pattern))))
        (case (car pattern)
          ((cons) ; (cons p_1 p_2).
           (when (not (= n 2))
             (error 'match "malformed (cons ...) pattern ~s" pattern))

           (let* ((failure/thunk (gensym 'thunk/))
                  (p_2 (match/unguarded/translate `(cdr ,variable) (element/2 pattern) success `(,failure/thunk)))
                  (p_1 (match/unguarded/translate `(car ,variable) (element/1 pattern) p_2     `(,failure/thunk))))
             `(let ([,failure/thunk (lambda () ,failure)])
                (if (pair? ,variable) ,p_1 (,failure/thunk)))))

          ((list)
           (cond
             ((zero? n) ; (list).
              (match/literal/translate variable null success failure))
             ((= n 1)   ; (list p_1)
              (match/compound/translate variable `(cons ,(element/1 pattern) ()) success failure))
             (else      ; (list p_1 p2 ...)
              (match/compound/translate variable `(cons ,(element/1 pattern) (list ,@(element/rest pattern))) success failure))))

          ((list-rest)  ; (list-rest p_1 p_2 ... q). Final pattern q matches the rest of the list after patterns p_1 p_2 ...
           (cond
             ((< n 2)
              (error 'match "malformed (list-rest ...) pattern ~s" pattern))
             ((= n 2)
              ; (list-rest p_1 p_2) => (cons p_1 p_2).
              (match/compound/translate variable `(cons ,@(cdr pattern)) success failure))
             (else
              ; (List-rest p_1 p_2 ...) => (cons p_1 (list-rest p_2 ...)).
              (match/compound/translate variable `(cons ,(element/1 pattern) (list-rest ,@(element/rest pattern))) success failure))))

          ((tuple) ; (tuple p_1 p_2 ...)
           (if (zero? n)
               ; The empty tuple.
               `(if (and (tuple? ,variable) (zero? (tuple/length ,variable)))
                    ,success
                    ,failure)
               ; The non-empty tuple.
               `(if (and (tuple? ,variable) (= (tuple/length ,variable) ,n))
                    ,(match/tuple/translate variable (cdr pattern) success failure 0 n)
                    ,failure)))

          ((record) ; (record <kind> field_1 p_1 field_2 p_2 ...)
           ;(display (format "compiler working on pattern: ~s\n" pattern))
           (if (< n 2)
               (error "malformed record pattern ~s" pattern)
               (let ((kind (element/1 pattern))
                     (fields (element/rest pattern)))
                 ;(display (format "pattern fields: ~s\n" fields))
                 (if (and (symbol? kind) (even? (length fields)) (match/record/fields/ok? fields))
                     `(if (and (record? ,variable) (eq? (record/kind ,variable) (quote ,kind)))
                          ,(match/record/translate variable fields success failure)
                          ,failure)
                     (error "malformed (record <kind> ...) pattern ~s" pattern)))))

          ((term)  ; (term (head <symbol>) p_1 p_2 ...)
             (if (zero? n)
                 (error 'match "malformed (term ...) pattern ~s" pattern)

                 (let ((head (element/1 pattern)))
                   (if (and (shape/exact head 2) (eq? (car head) 'head) (symbol? (element/1 head)))
                       (match/compound/translate variable `(tuple (quote ,(element/1 head)) ,@(element/rest pattern)) success failure)
                       (error "malformed head in (term ...) pattern ~s" pattern)))))

          ((vector) ; (vector p_1 p_2 ...)
           (cond
             ((zero? n) ; The empty persistent vector.
              `(if (and (vector/persist? ,variable) (eq? vector/null ,variable))
                   ,success
                   ,failure))

             (else
              `(if (and (vector/persist? ,variable) (= (vector/length ,variable) ,n))
                   ,(match/vector/translate variable (cdr pattern) success failure 0 n)
                   ,failure))))

          ((app)
           (if (= n 2)
               ; (app expression p_1)
               (let ((value (gensym 'value.)))
                 `(let ((,value (,(element/1 pattern) ,variable)))
                    ,(match/unguarded/translate value (element/2 pattern) success failure)))

               (error 'match "malformed (app ...) pattern ~s" pattern)))

          (else ; Pattern is a list but not one of (cons ...), (list ...) (list-rest ...), (vector ...) or (app ...).
           (match/unguarded/translate variable pattern success failure))))

      (match/simple/translate variable pattern success failure))) ; Pattern is NOT a list.

;; Generate the source for the logical patterns:
;;    (and p_1 ...)
;;    (or p_1 ...)
;;    (not p_1 ...)
;;    (? <predicate> p_1 ...)
;; where the p_i are themselves patterns.
;; If the given pattern is not one of the compound patterns but is a list then we assume it is a compound pattern.
;; If the given pattern is not a list then we assume it is a simple pattern.
(define (match/logical/translate variable pattern success failure)
  (if (list/nonempty? pattern)
      (let ((n (sub1 (length pattern))))
        (case (car pattern)
          ((and)
           (cond
             ((zero? n) success) ; (and).
              
             ((= n 1)  ; (and p_1).
              (match/unguarded/translate variable (element/1 pattern) success failure))

             (else ; (and p_1 p_2 ...).
              (let ((p_1 (element/1 pattern))
                    (k/s (match/logical/translate variable `(and ,@(element/rest pattern)) success failure))) ; k/s is success continuation: (and p_2 ...).
                (match/unguarded/translate variable p_1 k/s failure)))))

          ((or)
           (cond
             ((zero? n) failure) ; (or)
              
             ((= n 1)  ; (or p_1)
              (match/compound/translate variable (element/1 pattern) success failure))

             (else ; (or p_1 p_2 ...).
              (let ((p_1 (element/1 pattern))
                    (k/f (match/logical/translate variable `(or ,@(element/rest pattern)) success failure))) ; k/f is failure continuation (or p_2 ...).
                (match/unguarded/translate variable p_1 success k/f)))))

          ((not)
           (cond
             ((zero? n) failure) ; (not)
              
             ((= n 1)  ; (not p_1).
              (match/unguarded/translate variable (element/1 pattern) failure success))

             (else 
              (let ((rewrite `(and ,@(map (lambda (p) (list 'not p)) (cdr pattern))))) ; (not p_1 p_2 ...) => (and (not p_1) (not p_2) ...).
                (match/logical/translate variable rewrite success failure)))))

          ((?) ; (? expression p_1 ...).
           (let ((expression (element/1 pattern))
                 (rest       (element/rest pattern)))
             `(if (,expression ,variable)
                  ,(match/logical/translate variable `(and ,@rest) success failure)
                  ,failure)))
          
          (else ; Pattern is a list but NOT one of (and ...), (or ...), (not ...), or (? ...).
           (match/unguarded/translate variable pattern success failure))))
          
      ; Pattern is either (), a symbol, or a literal.
      (match/simple/translate variable pattern success failure)))
      ;(match/compound/translate variable pattern success failure)))

;; Note: I need to rewrite this to eliminate use of case-lambda.
(define (match/guarded/translate variable pattern guard success failure)
  (match/unguarded/translate variable pattern `(if ,guard ,success ,failure) failure))

(define COMPOUNDS #(cons list record tuple list-rest term vector app))
(define LOGICALS  #(and or not ?))

(define (match/unguarded/translate variable pattern success failure)
  (if (and (pair? pattern) (list? pattern))
      (cond
        ((vector-memq (car pattern) COMPOUNDS) (match/compound/translate variable pattern success failure))
        ((vector-memq (car pattern) LOGICALS)  (match/logical/translate  variable pattern success failure))
        (else (match/simple/translate variable pattern success failure)))

      (match/simple/translate variable pattern success failure)))


;; Match has the form (match <expression> <clause> ...+) where
;; a match clause has two forms:
;;  (<pattern> <action> ...)
;;    If <pattern> matches the value then <action> ... is evaluated.
;;    The value of the last <action> is the value of the match.
;;
;;  (<pattern> <guard> <action> ...) where <guard> is (where <expression>)
;;     If <pattern> matches the value then <action> ... is evaluated.
;;     After the last <action> the <guard> expression is evaluated.
;;     If the <guard> returns any value other than #f then the value of the last action is returned
;;     as the value of the match.
;;     Otherwise the following pattern clause is evaluated.

(define (match? x) (eq? (car x) 'match))
(define (match/expression x) (cadr x))
(define (match/clauses x)    (cddr x)) ; Returns (<clause> ...+).
(define (match/clause/pattern clause)  (car clause)) ; Returns the <pattern> portion of a <clause>.
(define (match/clause/actions clause)  (cdr clause)) ; Returns (<action> ...+) portion of an UNGUARDED clause.
;; Returns #t iff clause is (<pattern> (where <guard>) <action>).
(define (match/clause/guard? clause)
  (and 
   (> (length clause) 2)
   (eq? 'where (car (element/1 clause)))))

;; Extract the guard (where <guard>) from (<pattern> (where <guard>) action ...).
(define (match/clause/guard clause)
  (let ((where (element/1 clause)))
    (element/1 where)))

(define (match/clause/guard/actions clause) (cddr clause)) ; Returns (<action> ...+) portion of a GUARDED clause.

;; Return #t if list l is a singleton list (x) and #f if l is (x y ...).
(define (singleton? l) (null? (cdr l)))

(define (match/exploded/translate expression clauses)
  (let ((value (gensym 'value.)))
    (if (null? clauses) ; (match <expression>)
        (if (symbol? expression)
            #f
            `(let ((,value ,expression)) #f)) ; We have to evaluate the expression in case it has side-effects.
        
        (let* ((clause (car clauses))
               (pattern (match/clause/pattern clause))
               (failure (match/exploded/translate (if (symbol? expression) expression value) (cdr clauses))))

          (if (match/clause/guard? clause)
              ; clause is (<pattern> <guard> <action> ...).
              (let* ((guard   (match/clause/guard clause))
                     (actions (match/clause/guard/actions clause))
                     (success `(begin ,@actions)))
                (if (symbol? expression)
                    (match/guarded/translate expression pattern guard success failure)

                    ; Evaluate the expresson.
                    `(let ((,value ,expression))
                       ,(match/guarded/translate value pattern guard success failure))))
              
              ; clause is (<pattern> <action> ...).
              (let* ((actions (match/clause/actions clause))
                     (success `(begin ,@actions)))
                (if (symbol? expression)
                    (match/unguarded/translate expression pattern success failure)

                    ; Evaluate the expression.
                    `(let ((,value ,expression))
                       ,(match/unguarded/translate value pattern success failure)))))))))

;; Translate a (match ...) expression given by x into lowe-level source code.
(define (match/translate x)
  (match/exploded/translate (match/expression x) (match/clauses x)))

;; The following is part of jas-match. I left it in here to remind myself to implement
;; match-lambda, ... and the like later on.

;(define-syntax match-lambda
;  (syntax-rules ()
;    [(_ (pat expr ...) ...)         (lambda (x) (match x (pat expr ...) ...))]))
;
;(define-syntax match-lambda*
;  (syntax-rules ()
;    [(_ (pat expr ...) ...)         (lambda x   (match x (pat expr ...) ...))]))
;
;(define-syntax match-let*
;  (syntax-rules ()
;    [(_ () body ...)                                (let () body ...)]
;    [(_ ((pat expr)) body ...)                      ((match-lambda (pat body ...)) expr)]
;    [(_ ((pat expr) (pat2 expr2) ...) body ...)     (match-let* ([pat expr])
;                                                      (match-let* 
;                                                          ((pat2 expr2) ...) 
;                                                        body ...))]))
;
;(define-syntax match-let 
;  (syntax-rules ()
;    [(_ () body ...)               (let () body ...)]
;    [(_ ((pat expr) ...) body ...) (match-let* ([(list pat ...) (list expr ...)]) body ...)]))
;
;
;(define-syntax match-define-values-helper
;  (syntax-rules ()
;    [(_ (id ...) (pat) (expr))                   (match expr
;                                                   [pat  (values id ...)])]
;    [(_ (id ...) (pat . pats) (expr . exprs))    (match expr
;                                                   [pat  (values id ...)]
;                                                   [else (match-define-values-helper (id ...) pats exprs)])]))
;
;(define-syntax match-define-values
;  (syntax-rules ()
;    [(_ (id ...) [pat expr])                   (define-values (id ...)
;                                                 (match-define-values-helper (id ...) (pat) (expr)))]
;    [(_ (id ...) [pat expr] ...)               (define-values (id ...)
;                                                 (match-define-values-helper (id ...) (pat ...) (expr ...)))]))

;;;
;;; TEST
;;;

;'MATCH-LET*
;(match-let* ([(list x y z)    (list 1 2 3)]
;             [(vector a b c)  (vector 4 5 6)])
;  (if (= (+ x y z a b c) 21)
;      'ok
;      'fail))
;
;(match-let* ([(list x y)    (list 1 2)]
;            [(vector a b)   (vector 3 x)])
;  (if (= (+ x y a b) 7)
;      'ok
;      'fail))
;
;'MATCH-LET
;(match-let ([(list x y z)    (list 1 2 3)]
;            [(vector a b c)  (vector 4 5 6)])
;  (if (= (+ x y z a b c) 21)
;      'ok
;      'fail))
;
;
;'MATCH-DEFINE-VALUES
;(match-define-values (x y z)
;                     [(vector x (list y z))  (list 1 (list 2 3))]
;                     [(list x (list y z))    (list 1 (list 2 3))])
;
;(list x y z)
