#lang racket/base

(require
 (only-in racket/unsafe/ops unsafe-struct*-ref)
 "persistent/environ.rkt"
 "uuid.rkt")

(provide
 Object
 root@
 reroot@
 methods@
 define/subobject
 root/extend
 methods/extend
 
 define/object/flavor ; Object type definition.
 define/object?       ; Define object type predicate.
 object?              ; Base object predicate.
 object/id            ; Extract id of object instance.
 object/method?       ; Test for methods of object by name.
 object/new           ; Construct an object instance.
 ::                   ; Method invocation.
 ::@                  ; Extract state environ of an object.
 ::!                  ; Update state environ of an object.
 :.                   ; Shorthand for extracting value of binding in an environ.
 :!                   ; Shorthand for extending an environ with a binding.
 :!!                  ; Shorthand for extending an environ with a function application to a binding.
 environ/new          ; Helper for defining a new environ.
 environ/extend       ; Helper for extending an environ (adding bindings)
 environ/trim         ; Helper for trimming an environ (removing bindings)
 )

;; Prototype-like objects whose instance state and methods are based on environs.

(define-syntax-rule (define/object/flavor flavor)
  (struct
   flavor
   (id) ; UUID
   #:property
   prop:procedure (lambda (self) (unsafe-struct*-ref self 0))))
(struct object (id (state #:mutable) methods) #:transparent)

;; Returns the id of object o.
(define (object/id o) ((object-id o)))
;; Define the type? predicate for an object with type predicate flavor?
(define-syntax-rule (define/object? name flavor?)
  (define name (lambda (x) (and (object? x) (flavor? (object-id x))))))

;(struct Object ((state #:mutable) methods) #:transparent) ; The primal base class.
(struct Object ((root #:mutable)))
;; Access to object components from the root on down.
(define-syntax root@
  (syntax-rules ()
    [(_ self)          (Object-root self)]
    [(_ self name)     (environ/ref (Object-root self) 'name #f)]
    [(_ self name ...) (environ/ref/vector (Object-root self) #(name ...) #f)]))
(define-syntax methods@
  (syntax-rules ()
    [(_ self) (root@ self methods)]
    [(_ self name ...) (root@ self methods name ...)]))
(define-syntax-rule (reroot@ self r) (set-Object-root! self r))
(define-syntax-rule (root/extend self name ...) (environ/extend (Object-root self) name ...))
(define-syntax-rule (methods/extend self name ...) (environ/extend (environ/ref (Object-root self) 'methods #f) name ...))
;; Extend the environ whose fully qualified root name is (name ...).
(define-syntax-rule (any/extend self (name ...) other ...) (environ/extend (root@ self name ...) other ...))
(define-syntax-rule (define/subobject sub super) (struct sub super ()))

(define-syntax state@
  (syntax-rules ()
    [(_ self)          (Object-state self)]
    [(_ self name)     (environ/ref (Object-state self) 'name #f)]
    [(_ self name ...) (environ/ref/vector (Object-state self) #(name ...) #f)]))
(define-syntax-rule (restate@ self s) (set-Object-state! self s))
;(define (methods@ self) (Object-methods self))
(define-syntax-rule (state/extend self name ...) (environ/extend (Object-state self) name ...))


;; flavor - an object flavor (type) defined via define/object/flavor
;; state - an environ containing instance bindings
;; methods - an environ containing object methods
(define (object/new flavor state methods)
  (object (flavor (uuid/symbol)) state methods))
          
;; Method invocation for an object o.
(define-syntax-rule (:: o method argument ...)
  ((environ/ref/symbol (object-methods o) 'method #f) o argument ...))

;; Extract and return the instance state of object o.
(define-syntax-rule (::@ o)   (object-state o))
;; Set the instance state of object o.
(define-syntax-rule (::! o s) (set-object-state! o s))

;; Returns the value of the environ binding given by name.
(define-syntax-rule (:. e name)       (environ/ref e 'name #f))
;; Returns the environ derived from environ e in which name is bound to value.
(define-syntax-rule (:! e name value) (environ/cons e 'name value))
;; Returns the environ derived from environ e in which name is bound to (f value).
(define-syntax-rule (:!! e name f)    (environ/cons e 'name (f (environ/ref e 'name #f))))

;; Construct a new environ containing the bindings given by name ...
(define-syntax-rule (environ/new name ...)
  (let ((symbols #(name ...))
        (values  (vector name ...)))
    (vectors/environ environ/null symbols values)))
;; Extend an existing environ e with the bindings given by name ...
(define-syntax-rule (environ/extend e name ...)
    (let ((symbols #(name ...))
          (values  (vector name ...)))
    (vectors/environ e symbols values)))
;; Trim an existing environ e by removing the bindings denoted by name ...
(define-syntax environ/trim
  (syntax-rules ()
    [(_ e name) (environ/remove e name)]
    [(_ e name ...) (environ/vector/remove e #(name ...))]))

;; Returns #t if name is a method in object o and #f otherwise.
(define (object/method? o name)
  (and (environ/ref (methods@ o) name #f) #t))

;; Examples of use.
;(define/object/flavor example)
;(define/object? object/example? example?)
;(define test (object/new example environ/null environ/null))


(struct foo (id) #:transparent)
(struct bar foo ())
(define x (foo 77))
(define y (bar 99))
