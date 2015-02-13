#lang racket/base

;; A vector-based struct library.

;; Vector structs are fast, efficient structs based on vectors.
;; Using syntax-rules and macro-defining macros, we can construct 
;; a portable, expressive vector-struct library.

;; The first element of a vector struct is an information list.
;; The information list contains the struct name and its field names.
;; This list is computed entirely at compile time, and so carries
;; no run-time overhead. Carrying this information in the struct
;; allows run-time reflective operations.  The remaining elements 
;; of the struct vector are the struct fields themselves.

;; (define-vector-struct name field ...) defines a new macro: name.
;; (name <clause>) then expands -- at compile time -- into the 
;; values and methods for accessing the struct, querying the struct
;; and mutating the struct.

;; In practice, this method is exactly as efficient as using vectors,
;; but with the added convenience of symbolic names for entries.

;; (make name <value> ...) creates a new vector struct.

;; (name fields) expands into a list of symbolic field names.

;; (name index-of <field>) expands into the offset at compile-time.

;; (name ?) expands into a predicate for this struct type.

;; (name ? <value>) inlines the predicate test.

;; (name <field> set!) expands into the mutator for <field>.

;; (name <field> set! <struct>) is a partially applied mutator.

;; (name <field> set! <struct> <value>) inlines the mutation.

;; (name <field>) expands into the accessor for <field>.

;; (name <field> <struct>) inlines the access to <field>.

;; It is also possible to resolve the name of the field being
;; accessed or modified at run-time, although now the cost
;; is linear in the number of fields, instead of constant:

;; (name set! <exp>) is a mutator for the symbol value of <exp>.

;; (name set! <exp> <value> <struct>) inlines the mutation.

;; (name get <exp>) is an accessor for the symbol value of <exp>.

;; (name get <exp> <struct>) inlines the access.

;; Because vector structs carry run-time type information (which
;; is computed at compile-time), it is possible to ask whether
;; a value is a vector struct, and then query/modify its fields:

;; (vector-struct? <value>) is true if <value> is a vector struct.

;; (vector-struct->name <struct>) returns the struct's symbol name.

;; (vector-struct->fields <struct>) returns the symbol field names.

;; (vector-struct-get <struct> <exp>) returns the value of the field.

;; (vector-struct-set! <struct> <exp> <value>) mutates the field.


; list-index-of : symbol list[symbol] -> integer
(define (list-index-of symbol list)
  (define (f list i)
    (if (not (pair? list))
        #f
        (if (eq? (car list) symbol)
            i
            (f (cdr list) (+ 1 i)))))
  (f list 0))


(define-syntax define-vector-struct 
  (syntax-rules ()
    ((_ name field ...)
     ; => 
     (define-syntax name
       (syntax-rules (alloc get fields set! ? index-of in field ...)
         
         ;; Field names.
         ((name fields)
          ; =>
          '(field ...))
         
         ((name index-of f) 
          ; =>
          (name index-of f in field ...))
         
         ((name index-of field in field . others)
          ; => 
          1) ...
         
         ((name index-of field in not-field . others)
          ; =>
          (+ 1 (name index-of field in . others))) ...

         
         ;; The type predicate.
         ((name ? obj)
          ; => 
          (and (vector? obj)
               (> (vector-length obj) 0)
               (pair? (vector-ref obj 0))
               (pair? (cdr (vector-ref obj 0)))
               (eq? 'name (cadr (vector-ref obj 0)))))
                  
         ((name ?)
          ; =>
          (lambda (obj)
            (name ? obj)))
         
         
         ;; Run-time field look-up.
         ((name get exp obj)
          ; =>
          (let ((i (list-index-of exp '(field ...))))
            (if i
                (vector-ref obj (+ i 1))
                (error "no such field in vector-struct " 'name))))
         
         ((name get)
          ; =>
          (lambda (exp obj)
            (name get exp obj)))
         
         ((name get exp)
          ; =>
          (lambda (obj)
            (rame get exp obj)))
         
         
         ;; Run-time field modification.
         ((name set! exp value obj)
          ; =>
          (let ((i (list-index-of exp '(field ...))))
            (if i
                (vector-set! obj (+ i 1) value)
                (error "no such field in vector-struct " 'name))))
         
         ((name set!)
          ; =>
          (lambda (exp value obj)
            (name set! exp value obj)))
         
         ((name set! exp)
          ; =>
          (lambda (value obj)
            (name set! exp value obj)))
         
         ((name set! exp value)
          ; => 
          (lambda (obj)
            (name set! exp value obj)))
         
         
         ;; Allocation.
         ((name alloc . values)
          ; => 
          (vector '(vector-struct name field ...) . values))

         ((name alloc)
          ; => 
          (lambda values
            (apply vector (cons 'name values))))
         
         
         ;; Compile-time field modification.
         ((name f set! obj value)
          ; => 
          (vector-set! obj (name index-of f) value))

         ((name f set!)
          ; => 
          (lambda (obj value)
            (name f set! obj value)))
         
         
         ;; Compile-time field look-up.
         ((name f obj)
          ; => 
          (vector-ref obj (name index-of f)))
         
         ((name f) 
          ; =>
          (lambda (obj)
            (name f obj)))
         
         
         )))))


(define-syntax make
  (syntax-rules (alloc)
    ((_ name value ...) (name alloc value ...))))


; vector-struct? : value -> boolean
(define (vector-struct? v)
  (and (vector? v)
       (> (vector-length v) 0)
       (pair? (vector-ref v 0))
       (eq? (car (vector-ref v 0)) 'vector-struct)))
       
; vector-struct->name : vector-struct -> symbol
(define (vector-struct->name v)
  (cadr (vector-ref v 0)))

; vector-struct->fields : vector-struct -> list[symbol]
(define (vector-struct->fields v)
  (cddr (vector-ref v 0)))

; vector-struct-get : vector-struct symbol -> value
(define (vector-struct-get v field)
  (vector-ref v (+ 1 (list-index-of field (vector-struct->fields v)))))

; vector-struct-set! : vector-struct-set! symbol value -> void
(define (vector-struct-set! v field value)
  (vector-set! v (+ 1 (list-index-of field (vector-struct->fields v))) value))
