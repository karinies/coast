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


(require
 (only-in
  racket/list
  flatten))
          
(require "dissect.ss")
(require "set.ss")

(provide
 address/frame
 address/offset
 lexical/address
 lexical/dump
 lexical/frame/closed
 lexical/frame/empty?
 lexical/frame/parameters
 lexical/macro/fetch
 lexical/macro/push
 lexical/offset
 lexical/pop
 lexical/push
 lexical/push/closed
 lexical/push/parameters
 lexical/variable?
 macro?
 variables/closed
 )

;; The lexical stack, constructed at compile time, is a list of frames, one frame per lexical scope.
;; Each frame is a vector #(prior parameters closed macros) where
;; prior: the immediately preceeding frame (#f if the frame is the bottom frame of the lexical stack),
;; parameters: a list of the formal parameters for that lexical scope
;; closed: a sorted set of the closed variables appearing in that lexical scope
;; macros: either an association list ((name . procedure) ...) of macro definitions where name is the syntactic keyword of the macro
;;   and procedure is its implementation OR #f if no macros are defined

;; For example,
;;   (lambda (a b)
;;      (lambda (c d)
;;         (+ a c d)))
;; yields a lexical stack with two frames: (#(#f (a b) () #f) (c d) (a) #f) where:
;;   #(#f (a b) () #f) is the bottom frame containing two parameters (a b), no closed variables, and no macro definitions
;;   and the top frame contains parameters (c d), a single closed variable a, and no macro definitions.

;; To simplify macro processing all of the macros defined within lexical scope are available in stack order within the top frame.

;; Definition. A closed variable is a variable appearing within the body of a lambda that is
;; not among the parameters of the lambda and is bound in an outer enclosing scope.

;; Push a new frame onto the lexical stack where the new frame introduces:
;;   zero or more formal parameters
;;   zero or more closed variables
;;   no macros.
(define-syntax-rule (lexical/push lexical parameters closed)
  (vector lexical parameters closed (and lexical (lexical/frame/macros lexical))))

;; Push a new frame onto the lexical stack where the new frame introduces closed variables but nothing else.
(define-syntax-rule (lexical/push/closed closed)
  (vector lexical null closed (and lexical (lexical/frame/macros lexical))))

;; Push a new frame onto the lexical stack where the new frame introduces new formal parameters but nothing else
(define-syntax-rule (lexical/push/parameters lexical parameters)
  (vector lexical parameters null (and lexical (lexical/frame/macros lexical))))

;; Pop the top frame off of the lexical stack.
(define-syntax-rule (lexical/pop lexical) (vector-ref lexical 0))

;; Returns #t if the lexical stack is empty and #f otherwise.
(define (lexical/empty? lexical) (not lexical))

;(define (lexical/frame/make parameters closed) (vector #f parameters closed)) ; UNUSED.

;; Extractors for the lexical stack.
(define-syntax-rule (lexical/frame/prior frame)      (vector-ref frame 0))
(define-syntax-rule (lexical/frame/parameters frame) (vector-ref frame 1))
(define-syntax-rule (lexical/frame/closed frame)     (vector-ref frame 2))
(define-syntax-rule (lexical/frame/macros frame)     (vector-ref frame 3))

; (define-syntax-rule (lexical/closed/set lexical closed) (vector-set! lexical 2 closed)) ; UNUSED.

;; Push a macro definition under the given name onto the current lexical scope and
;; return the modified lexical stack as the value.
(define-syntax-rule (lexical/macro/push lexical name procedure)
  (let ((frame/macros (lexical/frame/macros lexical)))
    (vector-set!
     lexical 3
     (if frame/macros 
         (cons (cons name procedure) frame/macros) ; Push the definition onto the association list.
         (list (cons name procedure))))            ; This is the first macro definition.
    lexical))

;; name/procedure is the name/value pair of a macro definition in the current scope.
;; Return a lexical stack identical to the existing lexical stack except that the
;; macro hash table now contains the pair name/procedure.
;; Note that the update is purely functional.
(define-syntax-rule (xlexical/macro/push lexical name procedure)
  (vector
   (lexical/frame/prior      lexical)
   (lexical/frame/parameters lexical)
   (lexical/frame/closed     lexical)
   (hash/cons (lexical/frame/macros lexical) name procedure)))


;; Returns #t if the syntactic keyword of expression e is the name of a macro definition and #f otherwise.
(define (macro? e lexical)
  (if (and (symbol? (car e)) lexical)
      (let ((macros (lexical/frame/macros lexical)))
        (and macros (assq (car e) macros) #t))
      #f))

;; Return the implementation of the named macro.
(define (lexical/macro/fetch lexical name)
  (cdr (assq name (lexical/frame/macros lexical))))

(define-syntax-rule (address/frame address)  (car address))
(define-syntax-rule (address/offset address) (cdr address))

;; A frame with no formal parameters and no closed variables appearing within its scope is an empty frame.
;; Returns #t if the lexical frame is empty and #f otherwise.
(define (lexical/frame/empty? frame)
  (and
   (null? (lexical/frame/parameters frame))
   (null? (lexical/frame/closed frame))))

;; Returns #t if frame contains formal parameters or closed variables or both and #f otherwise.
(define (lexical/frame/nonempty? frame)
  (or
   (pair? (lexical/frame/parameters frame))
   (pair? (lexical/frame/closed frame))))

;; Returns #t if the top frame of the lexical stack contains the given formal parameter.
(define (lexical/parameter/member lexical symbol)
  (memq symbol (lexical/frame/parameters lexical)))

;; We index (address) frames from the top of the lexical stack down, with the topmost frame as 0.
;; Locate the nearest enclosing scope that defines the given symbol.
;; If the symbol is in lexical scope returns an integer i >= 0, the index of the defining frame for the symbol.
;; Otherwise returns the symbol itself if the symbol does not appear in lexical scope.
(define (lexical/frame/depth lexical symbol)
  (let loop ((lexical lexical) (symbol symbol) (i 0))
    (cond
      ((lexical/empty? lexical) symbol)                          ; symbol not within lexical scope.
      ((set/member? symbol (lexical/frame/parameters lexical)) i) ; symbol closed over lexical scope i.
      (else (loop (lexical/pop lexical) symbol (add1 i))))))

(define (lexical/dump lexical)
  (let loop ((i 0) (lexical lexical))
    (when lexical
      (display (format
                "~a: local: ~a closed: ~a macros: ~a\n"
                i (vector-ref lexical 1) (vector-ref lexical 2) (vector-ref lexical 3)))
      (loop (add1 i) (lexical/pop lexical)))))

;; Return #t if symbol s is a variable in lexical scope and #f otherwise.
(define (lexical/variable? lexical s) (integer? (lexical/frame/depth lexical s)))
  

;;; Validates the formal parameters of a lambda expression and returns them as a flat list.
;;; The rest parameter, if any, appears as the last element of the list.
;(define (parameters/flat parameters)
;  (cond
;    ((null? parameters) null)
;    ((pair? parameters)
;     (let ((x (car parameters)))
;       ;(variable? x)
;       (cons x (parameters/flat (cdr parameters)))))
;    (else
;     ;(variable? parameters)
;     (list parameters))))



;; Returns the set of closed variables in lambda expression e, (lambda <parameters> <body>),
;; as a list, representing an lexigraphically ordered set.
;; The implementation is still incomplete as it does not handle let, let*, letrec or cond.
(define (lambda/variables/closed e lexical)
  (let ((environ
         (lexical/push/parameters lexical (flatten (lambda/parameters e)))))
    (foldl
     set/union null
     (map (lambda (x) (variables/closed x environ)) (lambda/body e)))))
   
;; If symbol appears in the list of parameters of the topmost lexical frame return its offset = 1, ...
;; within the run time frame. If symbol is not a parameter in this frame return #f.
(define (lexical/offset lexical symbol)
  (let ((parameters (lexical/frame/parameters lexical)))
    (cond
      ((memq symbol parameters) => (lambda (tail) (add1 (- (length parameters) (length tail)))))
      (else #f))))

;; The last step prior to passing control to the body of a lambda expression is constructing the environment frame F
;; for that lambda body. F contains both the arguments a_1, ..., a_M and the bindings of the closed variables
;; c_1, ..., c_M appearing in the body. At least one of M, N > 0 as we don't bother pushing empty frames (M = N = 0) onto
;; the run-time environment stack. Frame F is a vector #(prior a_1 ... a_M c_1 ... c_N), 1+M+N elements wide, where
;; prior is the immediately preceeding frame in the run-time environment stack.


;; Compute the address = (frame . offset) of the given variable (symbol) where the variable is a lambda parameter
;; in some enclosing scope. frame >= 0 is the number of frames from top of stack and offset is the index = 1, 2, ...
;; of the lambda parameter within its defining frame (in other words parameter P_i has offset = i).
(define (lexical/address lexical symbol)
  (let loop ((lexical lexical) (frame 0))
    (cond
      ((lexical/empty? lexical)
       ; Paranoia. Should never happen.
        (error (format "Unable to locate closed variable ~a in lexical stack" symbol)))
      ((lexical/frame/empty? lexical)
       ; Skip over the empty lexical frame.
       (loop (lexical/pop lexical) frame))
      ((lexical/offset lexical symbol)
       => (lambda (offset) (cons frame offset))) ; The closed variable appears as a parameter in this frame.
      (else
       ; Go deeper into the stack.
       (loop (lexical/pop lexical) (add1 frame))))))

;; e: lambda body
;; lexical: lexical environment including parameters (if any).
;; Returns the set (as a ordered list of symbols) of all closed variables in expression e.
;; Returns the empty set (null) if expression e contains no closed variables.
(define (variables/closed e lexical)
  (cond
    ((symbol? e)
     (let ((depth (lexical/frame/depth lexical e)))
       (cond
         ((symbol? depth) null) ; Bound in the global environment.
         ((= depth 0) null)     ; Local variable, that is, a parameter.
         (else (list e)))))     ; depth > 0, hence a closed variable bound in a higher enclosing lexical scope.

    ((not (pair? e)) null) ; Constant.
    
    ((quote? e) null)

    ((if? e)
     (set/union
      (variables/closed (if/test e) lexical)
      (set/union
       (variables/closed (if/then e) lexical)
       (variables/closed (if/else e) lexical))))
    
    ((lambda? e) null) ; Ignore embedded lambda expressions.
    
    ((definition/macro? e) null) ; Ignore (define-macro ...).

    ((when? e)
     (set/union
      (variables/closed (when/test e) lexical)
      (foldl set/union null
       (map (lambda (x) (variables/closed x lexical))
            (when/thens e)))))

    ((unless? e)
     (set/union
      (variables/closed (unless/test e) lexical)
      (foldl set/union null
       (map (lambda (x) (variables/closed x lexical)) (unless/elses e)))))
    
    ; Special forms for binding environments.

    ; (environ/cons E s_1 s_2 ... s_n) where s_1, ..., s_n must be in lexical scope.
    ((environ/cons? e)
     (set/union
      (variables/closed (environ/cons/environ e) lexical)
      (foldl set/union null
       (map (lambda (x) (variables/closed x lexical)) (environ/cons/identifiers e)))))

    ; (environ/remove E s_1 s_2 ... s_n) where s_1, ..., s_n are all symbols.
    ((environ/remove? e)
     (variables/closed (environ/remove/environ e) lexical))

    ; (environ/ref E s x) where s is a symbol and x is an expression for a
    ; substitute value if s does not appear in environ E.
    ((environ/ref? e)
     (set/union
      (variables/closed (environ/ref/environ e) lexical)
      (variables/closed (environ/ref/substitute e) lexical)))

    ; (environ/reflect E e_1 ... e_n) where each e_i is an expression.
    ((environ/reflect? e)
     (set/union
      (variables/closed (environ/reflect/environ e) lexical)
      (foldl set/union null
       (map (lambda (x) (variables/closed x lexical)) (environ/reflect/expressions e)))))

    ((or (begin? e) (and? e) (or? e))
     (foldl set/union null
      (map (lambda (x) (variables/closed x lexical)) (cdr e))))

    ((quasiquote? e)
     (quasiquote/variables/closed e lexical))

    (else
     (foldl set/union null
      (map (lambda (x) (variables/closed x lexical)) e)))))

(define (quasiquote/variables/closed e lexical)
  (quasiquotation/variables/closed (cadr e) 1 lexical))

(define (quasiquotation/variables/closed form level lexical)
  (cond
    ((= level 0) null) ; We are not inside a quasiquotation.

    ((pair? form)
     (cond
       ((eq? (car form) 'quasiquote)
        ;; We've descended into another level of quasiquotation.
        (quasiquotation/list/variables/closed form (+ level 1) lexical))
       
       ((eq? (car form) 'unquote) ; form is (unquote <object>) or more commonly ,<object>.
        (if (= level 1)
            ; Compile <object> as we are situated at the outermost quasiquotation.
            (variables/closed (cadr form) lexical)
            (quasiquotation/list/variables/closed form (- level 1) lexical)))
       
       ((eq? (car form) 'unquote-splicing) ; form is (unquote-splicing <object>) or more commonly ,@<object>.
        (when (= level 1)
          (error (format "Ill-placed 'unquote-splicing' ~a" form)))
        (quasiquotation/list/variables/closed form (- level 1) lexical))
       
       (else
        (quasiquotation/list/variables/closed form level lexical))))
    
    ((vector? form)
      (quasiquotation/list/variables/closed (vector->list form) level lexical))
    
    (else null)))

(define (quasiquotation/list/variables/closed l level lexical)
  (if (pair? l)
    (let ((first (car l)))
      (if (= level 1)
        (if (and (pair? first) (unquote-splicing? first))
          (begin
            (shape first 2)
            (set/union
             (variables/closed (cadr first) lexical)
             (quasiquotation/variables/closed (cdr l) 1 lexical)))

          (set/union
           (quasiquotation/variables/closed first level lexical)
           (quasiquotation/variables/closed (cdr l) level lexical)))

        ; level > 1.
        (set/union
         (quasiquotation/variables/closed first level lexical)
         (quasiquotation/variables/closed (cdr l) level lexical))))

    (quasiquotation/variables/closed l level lexical)))

(define (should-be id expected outcome)
  (if (equal? expected outcome)
      (display (format "PASS id: ~a expected: ~s outcome: ~s\n" id expected outcome))
      (display (format "FAIL id: ~a expected: ~s outcome: ~s\n" id expected outcome))))

;; Helper for defining a lexical stack for testing purposes.
(define (lexical/new outlines)
  (let loop ((outlines outlines) (lexical #f))
    (if (null? outlines)
        lexical
        (loop (cdr outlines) (lexical/push/parameters lexical (car outlines))))))

(define (test/lexical/address)
  (define lexical (lexical/new '((u) (x a y) () (d) (e f))))

  (define (test/lexical/address/1)
    (should-be
     'lexical/address/1 '(0 . 0)
     (lexical/address lexical 'e)))

  (define (test/lexical/address/2)
    (should-be
     'lexical/address/2 '(2 . 1)
     (lexical/address lexical 'a)))

  (define (test/lexical/address/3)
    (should-be
     'lexical/address/2 '(2 . 2)
     (lexical/address lexical 'y)))

  (define (test/lexical/address/4)
    (should-be
     'lexical/address/4 '(3 . 0)
     (lexical/address lexical 'u)))
  
  ; This should throw an error.
  (define (test/lexical/address/5)
    (should-be
     'lexical/address/4 '(3 . 0)
     (lexical/address lexical 'z))) ; No variable z in lexical stack.
  
  (test/lexical/address/1)
  (test/lexical/address/2)
  (test/lexical/address/3)
  (test/lexical/address/4)
  (test/lexical/address/5))

(define (test/variables/closed)  
  (define (test/variables/closed/1)
    (should-be
     'variables/closed/1 '()
     (lambda/variables/closed '(lambda () a) #f)))

  (define (test/variables/closed/2)
    (should-be
     'variables/closed/2
     '(x y)
     ; (lambda (x y) (lambda (a) (+ a y x)))
     (lambda/variables/closed
      '(lambda (a) (+ a y x))
      (lexical/new '((x y))))))
  
  (define (test/variables/closed/3)
    (should-be
     'variables/closed/3 '(x y)
     ; (lambda (x y)
     ;   (lambda (a) (+ a y) (* x a a)))
     (lambda/variables/closed
      '(lambda (a) (+ a y) (* x a a))
      (lexical/new '((x y))))))

  (define (test/variables/closed/4)
    (should-be
     'variables/closed/4 '(a x y)
     ; (lambda (x a y)
     ;   (lambda ()
     ;     (+ a y)
     ;     (* x 33 a)))
     (lambda/variables/closed
      '(lambda ()
         (+ a y)
         (* x 33 a))
      (lexical/new '((x a y))))))
  
  (define (test/variables/closed/5)
    (should-be
     'variables/closed/5 '(a u x y)

     (lambda/variables/closed
      '(lambda ()
         (if y (+ a x) (* u 19)))
      (lexical/new '((x a y) (u))))))

  (define (test/variables/closed/6)
    (should-be
     'variables/closed/6 '(a u x y)
     
     (lambda/variables/closed
      '(lambda ()
         (when (> y 0) (+ a x) (* u 19)))
      (lexical/new '((x a y) (u))))))
  
  (define (test/variables/closed/7)
    (should-be
     'variables/closed/7 '(a u x y)

     (lambda/variables/closed 
      '(lambda ()
         (unless (> y 0)
           (if (< u 19) #t x)
           (* a x x)))
      (lexical/new '((x a y) (u))))))


  (define (test/variables/closed/8a)
    (should-be
     'variables/closed/8a
     '(a e f u x y)

     ; (lambda (u)
     ;  (lambda (x a y)
     ;    (lambda (d)
     ;      (lambda (e f)
     ;        (lambda ()
     ;          (and (> u 19) (= x a) f (or (= y 0) (< y e))))))))    
     (lambda/variables/closed
      '(lambda ()
         (and (> u 19) (= x a) f (or (= y 0) (< y e))))
      (lexical/new '((u) (x a y) (d) (e f))))))

  (define (test/variables/closed/8b)
    (should-be
     'variables/closed/8b
     '(a e f u x y)

     ; (lambda (u)
     ;   (lambda (x a y)
     ;     (lambda ()
     ;      (lambda (d)
     ;        (lambda (e f)
     ;          (lambda ()
     ;            (and (> u 19) (= x a) f (or (= y 0) (< y e)))))))))   
     (lambda/variables/closed
      '(lambda ()
         (and (> u 19) (= x a) f (or (= y 0) (< y e))))
      (lexical/new '((u) (x a y) (d) (e f))))))

  (define (test/variables/closed/9)
    (should-be
     'variables/closed/9
     '(a f)

     ; (lambda (u)
     ;  (lambda (x a y)
     ;    (lambda (d)
     ;      (lambda (e f)
     ;        (lambda ()
     ;          `(u ,f d ,a))))))
     (lambda/variables/closed
      '(lambda ()
         `(u ,f d ,a))
      (lexical/new '((u) (x a y) (d) (e f))))))

  (define (test/variables/closed/10)
    (should-be
     'variables/closed/10
     '(d e f u y)
     
     ; (lambda (u)
     ;  (lambda (x a y)
     ;    (lambda (d)
     ;      (lambda (e f)
     ;        (lambda ()
     ;          `(u ,f ,@(list (+ d e) (> y u))))))))          
     (lambda/variables/closed
      '(lambda ()
         å`(u ,f ,@(list (+ d e) (> y u))))
      (lexical/new '((u) (x a y) (d) (e f))))))
  
  ;  (let ((u 1) (x 'xx) (a 'aa) (y 33) (d '7) (e 8) (f 'ff))
  ;    `(u ,f ,@(list `(+ ,d ,e) (> y u))))
  ;  =>
  ;  (u ff (+ 7 8) #t)
  (define (test/variables/closed/11)
    (should-be
     'variables/closed/11
     '(d e f u y)

     ; (lambda (u)
     ;  (lambda (x a y)
     ;    (lambda (d)
     ;      (lambda (e f)
     ;        (lambda ()
     ;          `(u ,f ,@(list `(+ ,d ,e) (> y u))))))))     
     (lambda/variables/closed
      '(lambda ()
                 `(u ,f ,@(list `(+ ,d ,e) (> y u))))
      (lexical/new '((u) (x a y) (d) (e f))))))
  
  ;  (let ((u 1) (x 'xx) (a 'aa) (y 33) (d '7) (e 8) (f 'ff))
  ;    `(u ,f ,@(list ``(+ ,d ,e) (> y u))))
  ;  =>
  ;  (u ff `(+ ,d ,e) #t)
  (define (test/variables/closed/12)
    (should-be
     'variables/closed/12
     '(f u y)

     ; (lambda (u)
     ;  (lambda (x a y)
     ;    (lambda (d)
     ;      (lambda (e f)
     ;        (lambda ()
     ;          `(u ,f ,@(list ``(+ ,d ,e) (> y u))))))))
     (lambda/variables/closed
      '(lambda ()
                 `(u ,f ,@(list ``(+ ,d ,e) (> y u))))
      (lexical/new '((u) (x a y) (d) (e f))))))
  
  ;  (let ((u 1) (x 'xx) (a 'aa) (y 33) (d '7) (e 8) (f 'ff))
  ;    `(u ,f ,@(list `(+ d e) (> y u))))
  ;  =>
  ;  (u ff (+ d e) #t)
  (define (test/variables/closed/13)
    (should-be
     'variables/closed/13
     '(f u y)

     ; (lambda (u)
     ;  (lambda (x a y)
     ;    (lambda (d)
     ;      (lambda (e f)
     ;        (lambda ()
     ;          `(u ,f ,@(list `(+ d e) (> y u))))))))     
     (lambda/variables/closed
      '(lambda ()
                 `(u ,f ,@(list `(+ d e) (> y u))))
      (lexical/new '((u) (x a y) (d) (e f))))))
  
  (test/variables/closed/1)
  (test/variables/closed/2)
  (test/variables/closed/3)
  (test/variables/closed/4)
  (test/variables/closed/5)
  (test/variables/closed/6)
  (test/variables/closed/7)
  (test/variables/closed/8a)
  (test/variables/closed/8b)
  (test/variables/closed/9)
  (test/variables/closed/10)
  (test/variables/closed/11)
  (test/variables/closed/12)
  (test/variables/closed/13))