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

;; The recompiler accepts a Motile Assembly Graph node as input and returns
;; a regenerated closure as output.

(require
 (only-in racket/vector vector-argmax vector-map)
 (only-in "utility.rkt" vector/all?)
 (only-in "../../persistent/hash.rkt" pairs/hash hash/eq/null hash/ref)
 (only-in "../../persistent/environ.rkt" environ/null)

 (only-in "../generate/closure.rkt" closure/generate closure/rest/generate)
 (only-in "../generate/combination.rkt" combination/generate)
 (only-in "../generate/constant.rkt" constant/generate)

 (only-in "../generate/control.rkt"
          and/generate      if/generate   or/generate
          sequence/generate when/generate unless/generate)

 (only-in "../generate/environ.rkt"
          environ/cons/generate environ/reflect/generate
          environ/remove/generate environ/ref/path/generate environ/ref/symbol/generate)

 (only-in "../generate/frame.rkt"   frame/pop stack/depth global/get/generate variable/get/generate)
 (only-in "../generate/lambda.rkt"  lambda/generate lambda/rest/generate)
 (only-in "../generate/letrec.rkt"  letrec/set/generate letrec*/set/generate)
 (only-in "../generate/quasiquote.rkt" quasiquote/append/generate quasiquote/cons/generate quasiquote/tuple/generate)
 (only-in "../generate/record.rkt" record/cons/generate record/generate record/ref/generate)
 (only-in "../generate/utility.rkt" k/RETURN))

(provide
 motile/recompile
 motile/recompile/active?)

(provide closure/inner/ok?) ; Debugging.
 

(define (recompile/error where e)
  (error where "ill-formed MAG node: ~a" e))

;; Returns #t if x is apparently a MAG (Motile Assembly Graph) instruction
;; and #f otherwise.
(define (MAG? x)
  (and (vector? x) (positive? (vector-length x)) (symbol? (vector-ref x 0))))

;; Returns #t for an integer > 0 and #f otherwise.
(define (integer/positive? n) (and (integer? n) (positive? n)))

;; Return #t iff the arity of a lambda without a rest parameter is >= 0.
(define (arity/ok? n)
  (and (integer? n) (>= n 0)))
;; Return #t iff the arity of a lambda with a rest parameter is > 0.
(define (arity/rest/ok? n) (integer/positive? n))
;; Return #t iff the number of closed variables n in a closure > 0.
(define (closed/ok? n) (integer/positive? n))

;; Returns #t if lexical stack s is structurally well-formed and #f otherwise.
(define (lexical/ok? s)
  (or
   (not s) ; Completely empty lexical stack.

   ; Nonempty lexical stack.
   (let loop ((depth 0) (s s))
     (cond
       ((not s) (positive? depth))   ; We've reached the bottom. Stack must have depth > 0.
       ((and (vector? s) (> (vector-length s) 1)) ; Frame must be nonempty as we never push empty frames.
        (loop (add1 depth) (frame/pop s)))
       (else #f)))))

;; Return the max frame index in a vector of frame addresses.
(define (address/frame/max addresses)
  ; Take car of (frame . offset) where frame is max among all addresses.
  (car (vector-argmax car addresses)))

;; #(constant <value>)
;; <value> - any primitive or constant data value
(define-syntax-rule (constant/value e) (vector-ref e 1))
(define (constant/ok? e)
  (if (= (vector-length e) 2)
      #t
      (recompile/error 'recompile/constant e)))

;; #(lambda/outer <arity> <body>) where
;; <arity> - arity of lambda defintion
;; <body> - MAG for body of lambda definition
;; Used for a lambda definition that has no rest argument and no closed variables.
(define-syntax-rule (lambda/outer/arity e) (vector-ref e 1))
(define-syntax-rule (lambda/outer/body e)  (vector-ref e 2))
;; Return #t if a lambda/outer instruction passes the sniff test 
(define (lambda/outer/ok? e)
  (if (and
       (= (vector-length e) 3)
       (arity/ok? (lambda/outer/arity e))
       (MAG? (lambda/outer/body e)))
      #t
      (recompile/error 'recompile/lambda/outer e)))

;; #(lambda/inner <arity> <lexical> <body>) where
;; <arity> - arity of lambda definition
;; <lexical> - lexical-scope stack captured at point of definition
;; <body> - code body of lambda definition
;; Used for a lambda value whose definition had no rest argument and no closed variables.
(define-syntax-rule (lambda/inner/arity e)   (vector-ref e 1))
(define-syntax-rule (lambda/inner/lexical e) (vector-ref e 2))
(define-syntax-rule (lambda/inner/body e)    (vector-ref e 3))
(define (lambda/inner/ok? e)
  (if (and
       (= (vector-length e) 4)
       (arity/ok? (lambda/inner/arity e))
       (lexical/ok? (lambda/inner/lexical e))
       (MAG? (lambda/inner/body e)))
      #t
      (recompile/error 'recompile/lambda/inner e)))

;; #(lambda/rest/outer <arity> <body>)
;; <arity> - arity of lambda definition INCLUDING its rest argument
;; <body> - MAG for body of lambda definition
;; Used for a lambda definition that has a rest argument but no closed variables.
(define-syntax-rule (lambda/rest/outer/arity e) (vector-ref e 1))
(define-syntax-rule (lambda/rest/outer/body e)  (vector-ref e 2))
(define (lambda/rest/outer/ok? e)
  (if (and
       (= (vector-length e) 3)
       (arity/rest/ok?  (lambda/rest/outer/arity e))
       (MAG? (lambda/rest/outer/body e)))
       #t
      (recompile/error 'recompile/lambda/rest e)))

;; #(lambda/rest/inner <arity> <lexical> <body>) where
;; <arity> - arity of lambda definition INCLUDING its rest argument
;; <lexical> - lexical scope stack captured at point of definition
;; <body> - MAG for body of lambda definition
;; Used for a lambda value whose definition had a rest argument but no closed variables.
(define-syntax-rule (lambda/rest/inner/arity e)   (vector-ref e 1))
(define-syntax-rule (lambda/rest/inner/lexical e) (vector-ref e 2))
(define-syntax-rule (lambda/rest/inner/body e)    (vector-ref e 3))
(define (lambda/rest/inner/ok? e)
  (if (and
       (= (vector-length e) 4)
       (arity/rest/ok? (lambda/rest/inner/arity e))
       (lexical/ok? (lambda/rest/inner/lexical e))
       (MAG? (lambda/rest/inner/body e)))
       #t
      (recompile/error 'recompile/lambda/rest/inner e)))

;; #(variable/get <offset>)
;; <offset> - offset > 0 within topmost stack frame for binding
;; Denotes an accessor for either a local or closed variable.
(define-syntax-rule (variable/get/offset e) (vector-ref e 1))
(define (variable/get? e) (eq? 'variable/get (vector-ref e 0)))
(define (variable/get/ok? e)
  (if (and
       (= (vector-length e) 2)
       (integer/positive? (variable/get/offset e)))
      #t
      (recompile/error 'recompile/variable/get e)))

;; #(global/get <symbol>)
;; <symbol> - name of binding in global binding environment
;; Denotes an accessor for a binding that is neither local nor within lexical scope.
(define-syntax-rule (global/get/symbol e) (vector-ref e 1))
(define (global/get/ok? e)
  (if (and
       (vector? e)
       (= (vector-length e) 2)
       (symbol? (global/get/symbol e)))
      #t
      (recompile/error 'recompile/global/get e)))

;; #(combination <arity> <operator> <arguments>) where
;; <arity> - number of arguments >= 0 in combination
;; <operator> - MAG for operator of combination
;; <arguments> - if <arity> = 0 then null else a list (a_1 ... a_n) n = <arity>
;;    where a_i is the MAG for argument i
;; Denotes a combination (e a_1 ... a_n).
(define-syntax-rule (combination/arity e)     (vector-ref e 1))
(define-syntax-rule (combination/operator e)  (vector-ref e 2))
(define-syntax-rule (combination/arguments e) (vector-ref e 3))
(define (combination/arguments/ok? x)
  (cond
    ((zero? (combination/arity x)) (null? (combination/arguments x)))
    ((list? (combination/arguments x))
     (and
      (= (length (combination/arguments x)) (combination/arity x)) ; Arity and length of arguments list must agree.
      (andmap MAG? (combination/arguments x)))) ; Each a_i in arguments list must be a MAG.
    (else #f)))

(define (combination/ok? e)
  (if (and
        (= (vector-length e) 4)
        (arity/ok? (combination/arity e))
        (MAG? (combination/operator e))
        (combination/arguments/ok? e))
      #t
      (recompile/error 'recompile/combination e)))
  
;; Returns #t iff predicate f is true for each element of v.
(define (vector/and/map v f)
  (let ((n (vector-length v)))
    (let loop ((i 0))
      (cond 
        ((>= i n) #t)
        ((f (vector-ref v i)) (loop (add1 i)))
        (else #f)))))

;; #(closure/outer <arity> <closed> <addresses> <body>)
;; <arity> - arity of lambda definition
;; <closed> - number of closed variables (> 0) in lambda definition
;; <addresses> - vector of run-time stack addresses of bindings for closed variables
;; <body> - MAG for body of lambda definition
;; Denotes a lambda with no rest argument and whose body contains one or more closed variables
(define-syntax-rule (closure/outer/arity e)     (vector-ref e 1))
(define-syntax-rule (closure/outer/closed e)    (vector-ref e 2))
(define-syntax-rule (closure/outer/addresses e) (vector-ref e 3))
(define-syntax-rule (closure/outer/body e)      (vector-ref e 4))

;; An offset is a index into a stack frame and is always > 0.
(define (address/offset/ok? x) (and (integer? x) (positive? x)))
;; Frames are counted from the top of stack starting at zero.
(define (address/frame/ok? x)  (and (integer? x) (>= x 0)))
;; Return #t if an address (frame . offset) pair is kosher and #f otherwise.
(define (address/ok? a)
  (and
   (pair? a)
   (address/frame/ok?  (car a))
   (address/offset/ok? (cdr a))))
;; Return #t if the list of addresses is well-structured and #f otherwise.
(define (addresses/ok? addresses)
  (vector/and/map addresses address/ok?))

(define (closure/outer/ok? e)
  (if (and
       (= (vector-length e) 5)
       (arity/ok? (closure/outer/arity e))
       (closed/ok? (closure/outer/closed e))
       (= (closure/outer/closed e) (vector-length (closure/outer/addresses e)))
       (addresses/ok? (closure/outer/addresses e))
       (MAG? (closure/outer/body e)))
      #t
      (recompile/error 'recompile/closure/outer e)))

;; #(closure/rest/outer <arity> <closed> <addresses> <body>)
;; <arity> - arity of lambda definition including rest parameter
;; <closed> - number of closed variables (> 0) in lambda definition
;; <addresses> - vector of run-time stack addresses of bindings for closed variables
;; <body> - MAG for body of lambda definition
;; Denotes a lambda with a rest argument and whose body contains one or more closed variables
(define (closure/rest/outer/ok? e)
  (if (and
       (= (vector-length e) 5)
       (arity/rest/ok? (closure/outer/arity e))
       (closed/ok? (closure/outer/closed e))
       (= (closure/outer/closed e) (vector-length (closure/outer/addresses e)))
       (addresses/ok? (closure/outer/addresses e))
       (MAG? (closure/outer/body e)))
      #t
      (recompile/error 'recompile/closure/outer e)))

;; #(closure/inner <arity> <closed> <lexical> <addresses> <body>)
;; <arity> - arity of lambda definition
;; <closed> - number of closed variables (> 0) in lambda definition
;; <lexical> - run-time lexical stack at point of definition
;; <addresses> - vector of run-time stack addresses of bindings for closed variables
;; <body> - MAG of lambda body
;; Denotes a lambda with no rest parameter for which it is safe to resolve the bindings of its closed variables.
(define-syntax-rule (closure/inner/arity e)     (vector-ref e 1))
(define-syntax-rule (closure/inner/closed e)    (vector-ref e 2))
(define-syntax-rule (closure/inner/lexical e)   (vector-ref e 3))
(define-syntax-rule (closure/inner/addresses e) (vector-ref e 4))
(define-syntax-rule (closure/inner/body e)      (vector-ref e 5))

(define (closure/inner/ok? e)
  (if
   (and
    (= (vector-length e) 6)
    (arity/ok? (closure/inner/arity e))
    (closed/ok? (closure/inner/closed e))
    (= (closure/inner/closed e) (vector-length (closure/inner/addresses e)))
    (lexical/ok? (closure/inner/lexical e))
    (addresses/ok? (closure/inner/addresses e))
    (MAG? (closure/inner/body e))
    ; The introduction of letrec* leads to a circularity in the reconstruction of closures where
    ; the set of closed variables are constructed piecewise as deserialzation proceeds but the
    ; lexical stack may still be incomplete. Here is a small example:
    ; (let ((f (lambda (n)
    ;           (define (that) (+ 11 n))
    ;           that)))
    ;   (f 9999))
    ; that leads to the problem behavior.
    (let ((stack (closure/inner/lexical e)))
      (or
       (vector/all? stack not) ; The stack looks like #(#f ... #f).
       (>
        (stack/depth (closure/inner/lexical e))
        (address/frame/max (closure/inner/addresses e)))))) ; The stack depth must be greater than the max frame 0, 1, ... addressed.

   #t
   (recompile/error 'recompile/closure/inner e)))

;; #(closure/rest/inner <arity> <closed> <lexical> <addresses> <body>)
;; <arity> - arity of lambda definition
;; <closed> - number of closed variables (> 0) in lambda definition
;; <lexical> - run-time lexical stack at point of definition
;; <addresses> - vector of run-time stack addresses of bindings for closed variables
;; <body> - MAG of lambda body
;; Denote a lambda with a rest parameer for which it is safe to resolve the bindings of its closed variables.
(define (closure/rest/inner/ok? e)
  (if (and
       (= (vector-length e) 6)
       (arity/rest/ok? (closure/inner/arity e))
       (closed/ok? (closure/inner/closed e))
       (= (closure/inner/closed e) (vector-length (closure/inner/addresses e)))
       (lexical/ok? (closure/inner/lexical e))
       (addresses/ok? (closure/inner/addresses e))
       (MAG? (closure/inner/body e))
       (>
        (stack/depth (closure/inner/lexical e))
        (address/frame/max (closure/inner/addresses e)))) ; The stack depth must be greater than the max frame 0, 1, ... addressed.
       #t
      (recompile/error 'recompile/closure/rest e)))

;; #(sequence <length> <expressions>)
;; <length> - the number (> 0) of expressions in the sequence
;; <expressions> - a list (e_1 ... e_n), n = <length> of MAG forms where each e_i corresponds to the i'th expression
;;    in the sequence
;; Denotes a sequence of expressions that must be evaluated in sequence order, for example, the expressions appearing
;; in a lambda body
(define-syntax-rule (sequence/length e)    (vector-ref e 1))
(define-syntax-rule (sequence/elements e)  (vector-ref e 2))
(define (sequence/elements/ok? x)
  (let ((elements (sequence/elements x)))
  (and
   (vector? elements)
   (= (sequence/length x) (vector-length elements))
   (vector/and/map elements MAG?))))

(define (sequence/ok? e)
  (if (and
       (= (vector-length e) 3)
       (integer/positive? (sequence/length e))
       (sequence/elements/ok? e))
      #t
      (recompile/error 'recompile/sequence e)))

;; #(letrec/set n)
;; n - number (> 0) of letrec bindings to set
;; A specialized construction required for letrec evaluation that sets all of the letrec bindings
;; "simultaneously" after all letrec bindings have been evaluated.
(define-syntax-rule (letrec/set/span e) (vector-ref e 1))
(define (letrec/set/ok? e)
  (if (and
       (= (vector-length e) 2)
       (integer/positive? (letrec/set/span e)))
      #t
      (recompile/error 'recompile/letrec/set e)))

;; #(letrec*/set n).
(define-syntax-rule (letrec*/set/offset e) (vector-ref e 1))

(define (letrec*/set/ok? e)
  (if (and
       (= (vector-length e) 2)
       (integer/positive? (letrec*/set/offset e))) ; Frame index > 0.
      #t
      (recompile/error 'recompile/letrec*)))

;; #(if <test> <then> <else>)
;; <test> - MAG for test expression
;; <then> - MAG for then expression
;; <else> - MAG for else expression
(define-syntax-rule (if/test e) (vector-ref e 1))
(define-syntax-rule (if/then e) (vector-ref e 2))
(define-syntax-rule (if/else e) (vector-ref e 3))
(define (if/ok? e)
  (if (and
       (= (vector-length e) 4)
       (MAG? (if/test e))
       (MAG? (if/then e))
       (MAG? (if/else e)))
      #t
      (recompile/error 'recompile/if e)))

;; #(and <head> <tail>)
;; <head> - MAG code for head of and special form
;; <tail> - MAG code for tail of and special form
;; Denotes an and special form whose structure is (and <head> . <tail>).
(define-syntax-rule (and/head e) (vector-ref e 1))
(define-syntax-rule (and/tail e) (vector-ref e 2))
(define (and/ok? e)
  (if (and
       (= (vector-length e) 3)
       (MAG? (and/head e))
       (MAG? (and/tail e)))
      #t
      (recompile/error 'recompile/and e)))

;; #(or <head> <tail>)
;; <head> - MAG code for head of or special form
;; <tail> - MAG code for tail of or special form
;; Denotes an and special form whose structure is (or <head> . <tail>).
(define-syntax-rule (or/head e) (vector-ref e 1))
(define-syntax-rule (or/tail e) (vector-ref e 2))
(define (or/ok? e)
  (if (and
       (= (vector-length e) 3)
       (MAG? (or/head e))
       (MAG? (or/tail e)))
      #t
      (recompile/error 'recompile/or e)))

;; #(when <test> <thens>)
;; <test> - MAG code for test expression of when
;; <thens> - MAG code for body of when
;; Denotes the special form (when <test> e_1 ... e_n).
(define-syntax-rule (when/test e)  (vector-ref e 1))
(define-syntax-rule (when/thens e) (vector-ref e 2))
(define (when/ok? e)
  (if (and
       (= (vector-length e) 3)
       (MAG? (when/test e))
       (MAG? (when/thens e)))
      #t
      (recompile/error 'recompile/when e)))

;; #(unless <test> <thens>)
;; <test> - MAG code for test expression of unless
;; <thens> - MAG code for body of unless
;; Denotes the special form (unless <test> e_1 ... e_n)
(define-syntax-rule (unless/test e)  (vector-ref e 1))
(define-syntax-rule (unless/thens e) (vector-ref e 2))
(define (unless/ok? e)
  (if (and
       (= (vector-length e) 3)
       (MAG? (unless/test e))
       (MAG? (unless/thens e)))
      #t
      (recompile/error 'recompile/unless e)))

;; Recompilation for quasiquotation.
;; #(quasiquote/cons <head> <tail>)
;; <head> - MAG for head of append
;; <tail> - MAG for tail of append
(define-syntax-rule (quasiquote/append/head x) (vector-ref x 1))
(define-syntax-rule (quasiquote/append/tail x) (vector-ref x 2))
(define (quasiquote/append/ok? x)
  (if (and
       (= (vector-length x) 3)
       (MAG? (quasiquote/append/head x))
       (MAG? (quasiquote/append/tail x)))
      #t
      (recompile/error 'recompile/quasiquote/append x)))
  
;; #(quasiquote/cons <head> <tail>)
;; <head> - MAG for head of cons
;; <tail> - MAG for tail of cons
(define-syntax-rule (quasiquote/cons/head x) (vector-ref x 1))
(define-syntax-rule (quasiquote/cons/tail x) (vector-ref x 2))
(define (quasiquote/cons/ok? x)
  (if (and
       (= (vector-length x) 3)
       (MAG? (quasiquote/append/head x))
       (MAG? (quasiquote/append/tail x)))
      #t
      (recompile/error 'recompile/quasiquote/cons x)))

;; #(quasiquote/tuple <elements>)
;; <elements> - list of MAGs for tuple elements
(define-syntax-rule (quasiquote/tuple/pattern x) (vector-ref x 1))
(define (quasiquote/tuple/ok? x)
  (if (and
       (= (vector-length x) 2)
       (MAG? (quasiquote/tuple/pattern x)))
      #t
      (recompile/error 'recompile/quasiquote/elements x)))

;; Recompilation for binding environment special forms.

;; #(environ/cons <length> <environ> <symbols> <values>)
;; <length> - length n > 0 of vector of symbols
;; <environ> - MAG code for expression returning an environ E
;; <symbols> - vector of #(s_1 ... s_n)
;; <values> - vector of MAG codes #(v_1 ... v_n) for the bindings of s_1, ..., s_n respectively.
(define-syntax-rule (environ/cons/length e)  (vector-ref e 1))
(define-syntax-rule (environ/cons/environ e) (vector-ref e 2))
 ; A vector of identifiers NOT a list!
(define-syntax-rule (environ/cons/symbols e) (vector-ref e 3))
 ; A vector of Motile codes, one per symbol.
(define-syntax-rule (environ/cons/values e)  (vector-ref e 4))

(define (environ/cons/symbols/ok? x)
  (and
   (vector? (environ/cons/symbols x))
   (= (environ/cons/length x) (vector-length (environ/cons/symbols x)))
   (vector/all? (environ/cons/symbols x) symbol?))) ; Every identifier must be a symbol.
(define (environ/cons/values/ok? x)
  (and
   (vector? (environ/cons/values x))
   (= (environ/cons/length x) (vector-length (environ/cons/values x)))
   ; Every element of the values vector must be a valid argument or closed variable reference.
   (vector/all? (environ/cons/values x) (lambda (e) (and (variable/get? e) (variable/get/ok? e))))))

(define (environ/cons/ok? e)
  (if (and
       (= (vector-length e) 5)
       (integer/positive? (environ/cons/length e))
       (MAG? (environ/cons/environ e))
       (environ/cons/symbols/ok? e)
       (environ/cons/values/ok? e))
      #t
      (recompile/error 'recompile/environ/cons e)))
       
;; #(environ/ref <environ> <accessor> <failure>)
;; <environ> - MAG for expression whose evaluation yields a binding environment E
;; <accessor> - either name of binding in E or symbol path of a binding in E
;; <failure> - MAG for expression whose evaluation yields an alternate value if <symbol> not in E
(define-syntax-rule (environ/ref/environ e)   (vector-ref e 1))
(define-syntax-rule (environ/ref/accessor e)  (vector-ref e 2))
(define-syntax-rule (environ/ref/failure e)   (vector-ref e 3))
(define (environ/path/ok? x)
  (and
   (vector? x)
   (immutable? x)
   (positive? (vector-length x))
   (vector/all? x symbol?)))
(define (environ/ref/ok? e)
  (if (and
       (= (vector-length e) 4)
       (MAG? (environ/ref/environ e))
       (or (symbol? (environ/ref/accessor e)) (environ/path/ok? (environ/ref/accessor e)))
       (MAG? (environ/ref/failure e)))
      #t
      (recompile/error 'recompile/environ/ref e)))

;; #(environ/remove <length> <environ> <symbols>)
;; <length> - length of vector <symbols>
;; <environ> - MAG for expression whose evaluation yields a binding environment E
;; <symbols> - vector of symbols whose bindings are to be removed from E
(define-syntax-rule (environ/remove/length e)  (vector-ref e 1))
(define-syntax-rule (environ/remove/environ e) (vector-ref e 2))
(define-syntax-rule (environ/remove/symbols e) (vector-ref e 3))
(define (environ/remove/symbols/ok? x)
  (= (environ/remove/length x) (vector-length (environ/remove/symbols x)))
  (vector/all? (environ/remove/symbols x) symbol?))

(define (environ/remove/ok? e)
  (if (and
       (= (vector-length e) 4)
       (integer/positive? (environ/remove/length e))
       (environ/remove/symbols/ok? e))
      #t
      (recompile/error 'recompile/environ/remove e)))

;; #(environ/reflect <environ> <body>)
;; <environ> - MAG for binding environment expression
;; <body> - MAG for body of environ/reflect
(define-syntax-rule (environ/reflect/environ e) (vector-ref e 1))
(define-syntax-rule (environ/reflect/body e)    (vector-ref e 2))

(define (environ/reflect/ok? e)
  (if (and
       (= (vector-length e) 3)
       (MAG? (environ/reflect/environ e))
       (MAG? (environ/reflect/body e)))
      #t
      (recompile/error 'recompile/environ/reflect e)))

(define-syntax-rule (MAG/tag x) (vector-ref x 0))

;; Recompilation for record special forms.

;; #(record name arity tags expressions)
;; name - type name of record (a symbol)
;; arity - n > 0 number of fields (tags) in record
;; tags - vector of symbols (field names)
;; expressions - n MAGs, one for each tag
(define-syntax-rule (record/name  e)       (vector-ref e 1))
(define-syntax-rule (record/arity e)       (vector-ref e 2))
(define-syntax-rule (record/tags  e)       (vector-ref e 3))
(define-syntax-rule (record/expressions e) (vector-ref e 4))
(define (record/ok? x)
  (if (and
       (= (vector-length x) 5)
       (symbol? (record/name x))
       (integer/positive? (record/arity x))
       (vector? (record/tags x))
       (vector? (record/expressions x))
       (= (record/arity x) (vector-length (record/tags x)))
       (= (record/arity x) (vector-length (record/expressions x)))
       (vector/all? (record/tags x) symbol?)
       (vector/all? (record/expressions x) MAG?))
      #t
      (recompile/error 'recompile/record x)))

;; #(record/cons record span fields expressions)
;; r - Motile closure for record instance
;; span - number of fields named in record/cons
;; tags - vector of symbols naming modified fields
;; expressions - Motile closures for new field values
(define-syntax-rule (record/cons/record e) (vector-ref e 1))
(define-syntax-rule (record/cons/span   e) (vector-ref e 2))
(define-syntax-rule (record/cons/tags e)   (vector-ref e 3))
(define-syntax-rule (record/cons/expressions e) (vector-ref e 4))
(define (record/cons/ok? x)
  (if (and
       (= (vector-length x) 5)
       (MAG? (record/cons/record x))
       (integer/positive? (record/cons/span x))
       (vector? (record/cons/tags x))
       (vector? (record/cons/expressions x))
       (= (record/cons/span x)
          (vector-length (record/cons/tags x))
          (vector-length (record/cons/expressions x)))
       (vector/all? (record/cons/tags x) symbol?)
       (vector/all? (record/cons/expressions x) MAG?))
      #t
      (recompile/error 'recompile/record/cons x)))

;; #(record/ref record tag failure)
(define-syntax-rule (record/ref/record e)  (vector-ref e 1))
(define-syntax-rule (record/ref/tag e)     (vector-ref e 2))
(define-syntax-rule (record/ref/failure e) (vector-ref e 3))
(define (record/ref/ok? x)
  (if (and
       (= (vector-length x) 4)
       (MAG? (record/ref/record x))
       (symbol? (record/ref/tag x))
       (or (MAG? (record/ref/failure x)) (eq? (record/ref/failure x) #f)))
      #t
      (recompile/error 'recompile/record/ref x)))


(define (motile/recompile x)
  (cond
    ((and (vector? x) (symbol? (MAG/tag x)) (hash/ref ACTIONS (MAG/tag x) #f))
     => (lambda (action) (action x)))
    (else (error 'motile/recompile "unknown Motile descriptor: ~a" x))))

(define (lambda/outer/recompile x)
  (lambda/outer/ok? x)
  (lambda/generate (lambda/outer/arity x) (motile/recompile (lambda/outer/body x))))

(define (lambda/rest/outer/recompile x)
  (lambda/rest/outer/ok? x)
  (lambda/rest/generate (lambda/rest/outer/arity x) (motile/recompile (lambda/rest/outer/body x))))

(define (lambda/inner/recompile x)
  (lambda/inner/ok? x)
  (let ((f (lambda/generate (lambda/inner/arity x) (motile/recompile (lambda/inner/body x)))))
    (f k/RETURN (lambda/inner/lexical x) environ/null)))

(define (lambda/rest/inner/recompile x)
  (lambda/rest/inner/ok? x)
  (let ((f (lambda/rest/generate (lambda/rest/inner/arity x) (motile/recompile (lambda/rest/inner/body x)))))
    (f k/RETURN (lambda/rest/inner/lexical x) environ/null)))

(define (closure/inner/recompile x)
  (closure/inner/ok? x)
  (let ((f (closure/generate
            (closure/inner/arity x) (closure/inner/closed x) (closure/inner/addresses x) (motile/recompile (closure/inner/body x)))))
    (f k/RETURN (closure/inner/lexical x) environ/null)))

(define (closure/rest/inner/recompile x)
  (closure/rest/inner/ok? x)
  (let ((f (closure/rest/generate
            (closure/inner/arity x) (closure/inner/closed x) (closure/inner/addresses x) (motile/recompile (closure/inner/body x)))))
    (f k/RETURN (closure/inner/lexical x) environ/null)))

(define (closure/outer/recompile x)
  (closure/outer/ok? x)
  (closure/generate
   (closure/outer/arity x) (closure/outer/closed x) (closure/outer/addresses x) (motile/recompile (closure/outer/body x))))

(define (closure/rest/outer/recompile x)
  (closure/rest/outer/ok? x)
  (closure/rest/generate
   (closure/outer/arity x) (closure/outer/closed x) (closure/outer/addresses x) (motile/recompile (closure/outer/body x))))

(define (letrec/set/recompile x)
  (letrec/set/ok? x)
  (letrec/set/generate (letrec/set/span x)))

(define (letrec*/set/recompile x)
  (letrec*/set/ok? x)
  (letrec*/set/generate (letrec*/set/offset x)))

(define (variable/get/recompile x)
  (variable/get/ok? x)
  (variable/get/generate (variable/get/offset x)))

(define (global/get/recompile x)
  (global/get/ok? x)
  (global/get/generate (global/get/symbol x)))

(define (constant/recompile x)
  (constant/ok? x)
  (constant/generate (constant/value x)))

(define (and/recompile x)
  (and/ok? x)
  (and/generate (motile/recompile (and/head x)) (motile/recompile (and/tail x))))

(define (combination/recompile x)
  (combination/ok? x)
  (combination/generate (motile/recompile (combination/operator x)) (map motile/recompile (combination/arguments x))))

(define (if/recompile x)
  (if/ok? x)
  (if/generate (motile/recompile (if/test x)) (motile/recompile (if/then x)) (motile/recompile (if/else x))))

(define (or/recompile x)
  (or/ok? x)
  (or/generate (motile/recompile (or/head x)) (motile/recompile (or/tail x))))

(define (sequence/recompile x)
  (sequence/ok? x)
  (sequence/generate (sequence/length x) (vector-map motile/recompile (sequence/elements x))))
  
(define (when/recompile x)
  (when/ok? x)
  (when/generate (motile/recompile (when/test x)) (motile/recompile (when/thens x))))

(define (unless/recompile x)
  (unless/ok? x)
  (unless/generate (motile/recompile (unless/test x)) (motile/recompile (unless/thens x))))

(define (quasiquote/append/recompile x)
  (quasiquote/append/ok? x)
  (quasiquote/append/generate
   (motile/recompile (quasiquote/append/head x))
   (motile/recompile (quasiquote/append/tail x))))

(define (quasiquote/cons/recompile x)
  (quasiquote/cons/ok? x)
  (quasiquote/cons/generate
   (motile/recompile (quasiquote/cons/head x))
   (motile/recompile (quasiquote/cons/tail x))))

(define (quasiquote/tuple/recompile x)
  (quasiquote/tuple/ok? x)
  (quasiquote/tuple/generate (motile/recompile (quasiquote/tuple/pattern x))))

(define (environ/cons/recompile x)
  (environ/cons/ok? x)
  (environ/cons/generate
   (motile/recompile (environ/cons/environ x)) (environ/cons/symbols x) (vector-map motile/recompile (environ/cons/values x))))

(define (environ/reflect/recompile x)
  (environ/reflect/ok? x)
  (environ/reflect/generate (motile/recompile (environ/reflect/environ x)) (motile/recompile (environ/reflect/body x))))

(define (environ/remove/recompile x)
  (environ/remove/ok? x)
  (environ/remove/generate (motile/recompile (environ/remove/environ x)) (environ/remove/symbols x)))

(define (environ/ref/recompile x)
  (environ/ref/ok? x)
  (let ((generator
         (if (symbol? (environ/ref/accessor x)) environ/ref/symbol/generate environ/ref/path/generate)))
  (generator
   (motile/recompile (environ/ref/environ x)) (environ/ref/accessor x) (motile/recompile (environ/ref/failure x)))))

(define (record/recompile x)
  (record/ok? x)
  (record/generate (record/name x) (record/tags x) (vector-map motile/recompile (record/expressions x))))

(define (record/cons/recompile x)
  (record/cons/ok? x)
  (record/cons/generate
   (motile/recompile (record/cons/record x))
   (record/cons/tags x)
   (vector-map motile/recompile (record/cons/expressions x))))

(define (record/ref/recompile x)
  (record/ref/ok? x)
  (record/ref/generate
   (motile/recompile (record/ref/record x))
   (record/ref/tag x)
   (if (record/ref/failure x) (motile/recompile (record/ref/failure x)) #f)))

;; x is a Motile descriptor.
;; Returns #t iff x is a descriptor for a lambda or closure that is "in use" at the time of recompilation.
(define (motile/recompile/active? x)
  (let ((flavor (MAG/tag x)))
    (and (memq flavor '(lambda/inner closure/inner lambda/rest/inner closure/rest/inner)) #t)))

(define ACTIONS
  (pairs/hash
   hash/eq/null
   (list
    ; Lambdas.
    (cons 'lambda/inner      lambda/inner/recompile)
    (cons 'lambda/outer      lambda/outer/recompile)
    (cons 'lambda/rest/inner lambda/rest/inner/recompile)
    (cons 'lambda/rest/outer lambda/rest/outer/recompile)

    ; Closures.
    (cons 'closure/inner      closure/inner/recompile)
    (cons 'closure/outer      closure/outer/recompile)
    (cons 'closure/rest/inner closure/rest/inner/recompile)
    (cons 'closure/rest/outer closure/rest/outer/recompile)
    
    ; Letrec.
    (cons 'letrec/set  letrec/set/recompile)
    (cons 'letrec*/set letrec*/set/recompile)

    ; Variable and global references.
    (cons 'variable/get variable/get/recompile)
    (cons 'global/get   global/get/recompile)
    
    ; Data constants.
    (cons 'constant constant/recompile)

    ; Control.
    (cons 'and         and/recompile)
    (cons 'combination combination/recompile)
    (cons 'if          if/recompile)
    (cons 'or          or/recompile)
    (cons 'sequence    sequence/recompile)
    (cons 'when        when/recompile)
    (cons 'unless      unless/recompile)

    ; Quasiquotation.
    (cons 'quasiquote/append quasiquote/append/recompile)
    (cons 'quasiquote/cons   quasiquote/cons/recompile)
    (cons 'quasiquote/tuple  quasiquote/tuple/recompile)

    ; Binding environments.
    (cons 'environ/cons    environ/cons/recompile)
    (cons 'environ/reflect environ/reflect/recompile)
    (cons 'environ/remove  environ/remove/recompile)
    (cons 'environ/ref     environ/ref/recompile)

   ; Records.
   (cons 'record      record/recompile)
   (cons 'record/cons record/cons/recompile)
   (cons 'record/ref  record/ref/recompile)

)))
  


