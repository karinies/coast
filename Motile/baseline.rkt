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

(provide
 BASELINE
 ENVIRON/TEST
 pairs/environ)

(require
 (only-in racket/list empty?)
 racket/pretty
 (only-in "persistent/environ.rkt" environ/merge environ/null pairs/environ)
 (only-in "persistent/record.rkt" record? record/contains? record/kind record/keys) 
 "persistent/vector.rkt"
 "persistent/hash.rkt"
 "persistent/set.rkt"
 "persistent/tuple.rkt"
 
 (only-in
  "generate/baseline.rkt"
  define/global/0
  define/global/1
  define/global/2
  define/global/3
  define/global/N
  define/combinator/2
  define/combinator/2-2
  define/combinator/3
  define/combinator/3-3
  descriptor/global
  motile/call)
 
 (only-in
  "generate/frame.rkt"
  a/1 a/2 a/3 a/last
  a/arity
  a/flatten
  a/rest
  arguments/pack
  arguments/list/pack
  arity/verify
  arity/rest/verify
  vector/list/copy!)
 
 (only-in
  "generate/utility.rkt"
  decompile?
  error/motile/internal/call
  error/motile/type
  k/RETURN))

(provide
 BASELINE
 ENVIRON/TEST
 define/global/0
 define/global/N
 define/combinator/2
 define/combinator/3
 define/combinator/3-3)

;; Motile-specific reworkings of map, apply, and for-each.

;; Generates a wrapper for map-like combinators.
;; symbol - name of map-like combinator
;; base - host function for combinator
(define (motile/metamap symbol base)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g) ; a = #(#f f L_1 L_2 ... L_n).
      (cond
        ((procedure? k)
         (arity/rest/verify a 2 symbol)
         (let ((f (a/1 a))
               (arity (a/arity a)))
           (cond
             ((= arity 2) ; Common case of (base f L).
              (k (base (lambda (x) (f k/RETURN (arguments/pack x) g)) (a/2 a))))
             ((= arity 3) ; (base f L_1 L_2).
              (k (base (lambda (x y) (f k/RETURN (arguments/pack x y) g)) (a/2 a) (a/3 a))))
             (else ; (base f L_1 L_2 ... L_n).
              (let ((Ls (a/rest a 2))) ; Ls = (L_1 ... L_n).
                (k (apply base (lambda X (f k/RETURN (arguments/list/pack X) g)) Ls))))))) ; Awkward but unavoidable.
        
        ((decompile? k a g) descriptor)
        
        (else (error/motile/internal/call symbol))))))

(define (motile/map)
  (let ((descriptor (descriptor/global 'map)))
    (lambda (k a g) ; a = #(#f f L_1 L_2 ... L_n).
      (cond
        ((procedure? k)
         (arity/rest/verify a 2 'map)
         (let ((f (a/1 a))
               (arity (a/arity a)))
           (cond
             ((= arity 2) ; Common case of (map f L).
              (k (map (lambda (x) (f k/RETURN (arguments/pack x) g)) (a/2 a))))
             ((= arity 3) ; (map f L_1 L_2).
              (k (map (lambda (x y) (f k/RETURN (arguments/pack x y) g)) (a/2 a) (a/3 a))))
             (else ; (map f L_1 L_2 ... L_n).
              (let ((Ls (a/rest a 2))) ; Ls = (L_1 ... L_n).
                (k (apply map (lambda X (f k/RETURN (arguments/list/pack X) g)) Ls))))))) ; Awkward but unavoidable.

        ((decompile? k a g) descriptor)

        (else (error/motile/internal/call 'map))))))

(define (motile/apply)
  (let ((descriptor (descriptor/global 'apply)))
    (lambda (k a g) ; a = #(#f f x_1 ... x_m L) where L = (y_1 ... y_n), a list, is required.
      (cond
        ((procedure? k)
         (arity/rest/verify a 2 'apply) ; Must be a minimum of 2 arguments.
         (let ((f    (a/1 a))
               (last (a/last a)))       ; List L in the arguments.
           (unless (list? last)
             (error/motile/type 'apply "proper list" (a/arity a) last))
           (if (= (a/arity a) 2)
               ; Common special case of (apply f L).
               (f k (arguments/list/pack last) g)
               ; General case of (apply f x_1 ... x_m L).
               (f k (a/flatten a 1) g)))) ; Call f with argument frame #(#f x_1 ... x_m y_1 ... y_n).
        
        ((decompile? k a g) descriptor)
        
        (else (error/motile/internal/call 'apply))))))

(define (motile/sort)
  (let ((descriptor (descriptor/global 'sort)))
    (lambda (k a g) ; a = #(#f L less?) where L is a list of items to be sorted and less? is a comparison predicate.
      (cond
        ((procedure? k)
         (arity/verify a 2 'sort)
         (let ((items (a/1 a))
               (less? (a/2 a)))
           (k (sort items (lambda (alpha beta) (less? k/RETURN (arguments/pack alpha beta) g))))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call 'sort))))))

(define (motile/call-with-continuation)
  (let ((descriptor (descriptor/global 'call/cc)))
    (lambda (k a g) ; a = #(#f f) where f is the single-argument function passed to call/cc.
      (cond
        ((procedure? k)
         (arity/verify a 1 'call/cc)
         (let ((f (a/1 a))
               (reification (lambda (_k a _g) (k (a/1 a))))) ; Continuation k reified as a one-argument Motile function.
           ; Now call f with the reification as an argument.
           (f k (arguments/pack reification) g))) ; NOTE: k here is continuation at call position of call/cc.
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call 'call/cc))))))

(define (motile/environ/capture)
  (let ((descriptor (descriptor/global 'environ/capture)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 0 'environ/capture)
         (k g))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call 'environ/capture))))))

;; Motile-specific call/cc.
;(define (call/cc k f)
;  (let ((descriptor (vector 'reference/global 'call/cc)))
;    (if k
;        (f k                           ; Continuation at call position of call/cc.
;           (lambda (k/other v) (k v))) ; Continuation k reified as a function.
;        descriptor)))

(define motile/CALL-CC (motile/call-with-continuation))


;; The set of primitive procedures available to all Motile mobile code.
(define BASELINE
  (pairs/environ
   environ/null
   (list
    ; Type testing.
    (define/global/1 'boolean?   boolean?)
    (define/global/1 'list?      list?)
    (define/global/1 'null?      null?)
    (define/global/1 'pair?      pair?)
    (define/global/1 'symbol?    symbol?)
    (define/global/1 'number?    number?)
    (define/global/1 'real?      real?)
    (define/global/1 'integer?   integer?)
    (define/global/1 'exact?     exact?)
    (define/global/1 'inexact?   inexact?)
    (define/global/1 'procedure? procedure?)
    
    ; More precise type tests for numbers.
    (define/global/1 'integer/exact?          exact-integer?)
    (define/global/1 'natural/exact?          exact-nonnegative-integer?)
    (define/global/1 'integer/positive/exact? exact-positive-integer?)
    (define/global/1 'real/inexact?           inexact-real?)
    
    ; Type transformers for numbers.
    (define/global/1 'exact->inexact exact->inexact) ; Deprecated.
    (define/global/1 'exact/inexact  exact->inexact)
    (define/global/1 'inexact->exact inexact->exact) ; Deprecated.
    (define/global/1 'inexact/exact  inexact->exact)
    (define/global/1 'number->string number->string) ; Deprecated.
    (define/global/1 'number/string  number->string)
    (define/global/1 'string->number string->number) ; Deprecated.
    (define/global/1 'string/number  string->number)    
    
    ; Equivalence predicates.
    (define/global/2 'eqv?    eqv?)
    (define/global/2 'eq?     eq?)
    (define/global/2 'equal?  equal?)
    
    ; List test and manipulation.
    (define/global/1 'length length)
    (define/global/N 'append append)
    (define/global/1 'reverse reverse)
    (define/global/1 'empty? empty?)
    
    ; List construction and deconstruction.
    (define/global/2 'cons     cons)
    (define/global/N 'list     list)
    (define/global/N 'list*    list*)
    (define/global/1 'car      car)
    (define/global/1 'cdr      cdr)
    (define/global/1 'caar     caar)
    (define/global/1 'cadr     cadr)
    (define/global/1 'cdar     cdar)
    (define/global/1 'cddr     cddr)
    (define/global/1 'caaar    caaar)
    (define/global/1 'caadr    caadr)
    (define/global/1 'cadar    cadar)
    (define/global/1 'caddr    caddr)
    (define/global/1 'cdaar    cdaar)
    (define/global/1 'cdadr    cdadr)
    (define/global/1 'cddar    cddar)
    (define/global/1 'cdddr    cdddr)
    (define/global/1 'caaaar   caaaar)
    (define/global/1 'caaadr   caaadr)
    (define/global/1 'caadar   caadar)
    (define/global/1 'caaddr   caaddr)
    (define/global/1 'cadaar   cadaar)
    (define/global/1 'cadadr   cadadr)
    (define/global/1 'caddar   caddar)
    (define/global/1 'cadddr   cadddr)
    (define/global/1 'cdaaar   cdaaar)
    (define/global/1 'cdaadr   cdaadr)
    (define/global/1 'cdadar   cdadar)
    (define/global/1 'cdaddr   cdaddr)
    (define/global/1 'cddaar   cddaar)
    (define/global/1 'cddadr   cddadr)
    (define/global/1 'cdddar   cdddar)
    (define/global/1 'cddddr   cddddr)
    (define/global/2 'list-ref list-ref)
    (define/global/2 'memq     memq)
    (define/global/2 'memv     memv)
    (define/global/2 'member   member)
    (define/global/2 'assq     assq)
    (define/global/2 'assv     assv)
    (define/global/2 'assoc    assoc)
    
    ; Logical negation.
    (define/global/1 'not not)
    
    ; Numerics.
    (define/global/N '<           <)
    (define/global/N '>           >)
    (define/global/N '<=          <=)
    (define/global/N '>=          >=)
    (define/global/2 '=           =)
    (define/global/1 'zero?       zero?)
    (define/global/1 'positive?   positive?)
    (define/global/1 'negative?   negative?)
    (define/global/1 'odd?        odd?)
    (define/global/1 'even?       even?)
    (define/global/1 'add1        add1)
    (define/global/1 'sub1        sub1)
    (define/global/1 '1-          sub1) ; Handy alias.
    (define/global/1 '1+          add1) ; Handy alias.
    (define/global/N '+           +)
    (define/global/N '-           -)
    (define/global/N '*           *)
    (define/global/N '/           /)
    (define/global/2 'quotient    quotient)
    (define/global/2 'remainder   remainder)
    (define/global/2 'modulo      modulo)
    (define/global/N 'max         max)
    (define/global/N 'min         min)
    (define/global/N 'gcd         gcd)
    (define/global/N 'lcm         lcm)
    (define/global/1 'numerator   numerator)
    (define/global/1 'denominator denominator)
    (define/global/1 'floor       floor)
    (define/global/1 'ceiling     ceiling)
    (define/global/1 'truncate    truncate)
    (define/global/1 'round       round)
    (define/global/2 'rationalize rationalize)   
    (define/global/1 'exp         exp)
    (define/global/1 'log         log)
    (define/global/1 'sin         sin)
    (define/global/1 'cos         cos)
    (define/global/1 'tan         tan)
    (define/global/1 'asin        asin)
    (define/global/1 'acos        acos)
    (define/global/1 'atan        atan)
    (define/global/1 'sqrt        sqrt)
    (define/global/1 'expt        expt)   
    
    ; Characters.
    (define/global/1 'char?            char?)
    (define/global/2 'char=?           char=?)
    (define/global/2 'char<?           char<?)
    (define/global/2 'char>?           char>?)
    (define/global/2 'char<=?          char<=?)
    (define/global/2 'char>=?          char>=?)
    (define/global/2 'char-ci=?        char-ci=?)
    (define/global/2 'char-ci<?        char-ci<?)
    (define/global/2 'char-ci>?        char-ci>?)
    (define/global/2 'char-ci<=?       char-ci<=?)
    (define/global/2 'char-ci>=?       char-ci>=?)
    (define/global/1 'char-alphabetic? char-alphabetic?)
    (define/global/1 'char-numeric?    char-numeric?)
    (define/global/1 'char-whitespace? char-whitespace?)
    (define/global/1 'char-lower-case? char-lower-case?)
    (define/global/1 'char->integer    char->integer) ; Deprecated.
    (define/global/1 'char/integer     char->integer)
    (define/global/1 'integer->char    integer->char) ; Deprecated
    (define/global/1 'integer/char     integer->char)
    (define/global/1 'char-upcase      char-upcase)
    (define/global/1 'char-downcase    char-downcase)
    
    ; Strings.
    (define/global/1 'string?       string?)
    (define/global/N 'make-string   make-string)
    (define/global/N 'string        string)
    (define/global/1 'string-length string-length)
    (define/global/2 'string-ref    string-ref)
    (define/global/2 'string=?      string=?)
    (define/global/2 'string<?      string<?)
    (define/global/2 'string>?      string>?)
    (define/global/2 'string<=?     string<=?)
    (define/global/2 'string>=?     string>=?)
    (define/global/2 'string-ci=?   string-ci=?)
    (define/global/2 'string-ci<?   string-ci<?)
    (define/global/2 'string-ci>?   string-ci>?)
    (define/global/2 'string-ci<=?  string-ci<=?)
    (define/global/2 'string-ci>=?  string-ci>=?)
    (define/global/3 'substring     substring)
    (define/global/N 'string-append string-append)
    (define/global/N 'format        format)
    
    ; Byte strings.
    (define/global/1 'bytes?        bytes?)
    (define/global/N 'bytes         bytes)
    (define/global/1 'byte?         byte?)
    (define/global/1 'bytes/length  bytes-length)
    (define/global/2 'bytes/ref     bytes-ref)
    (define/global/N 'subbytes      subbytes)
    (define/global/N 'bytes/append  bytes-append)
    ;(define/global/N 'bytes/append* bytes-append*) ; Not available in racket/base.
    (define/global/1 'bytes/list    bytes->list)
    (define/global/1 'list/bytes    list->bytes)
    (define/global/N 'bytes=?       bytes=?)
    (define/global/N 'bytes>?       bytes>?)
    (define/global/N 'bytes<?       bytes<?)
    
    ; Symbols.
    (define/global/1 'symbol->string symbol->string)
    (define/global/1 'symbol/string  symbol->string)
    (define/global/1 'string->symbol string->symbol)
    (define/global/1 'string/symbol  string->symbol)
    ;(define/global/1 'gensym         gensym) ; Uninterned symbols will not be deserialized as uninterned.
    
    ; Persistent functional vectors.
    (define/global/1     'vector/persist?   vector/persist?)
    (define/global/2     'list/vector       list/vector)
    (define/combinator/2 'vector/build      vector/build)
    (define/combinator/3 'vector/fold/left  vector/fold/left)
    (define/combinator/3 'vector/fold/right vector/fold/right)
    (define/global/1     'vector/length     vector/length)
    (define/global/1     'vector/list       vector/list)
    (cons                'vector/null       vector/null)  ; Pre-defined constant.
    (define/global/1     'vector/null?      vector/null?)
    (define/global/2     'vector/cons       vector/cons)
    (define/global/1     'vector/cdr        vector/cdr)
    (define/combinator/2 'vector/filter     vector/filter)
    (define/combinator/2 'vector/map        vector/map)
    (define/global/2     'vector/ref        vector/ref)
    (define/global/N     'vector/subvector  vector/subvector)
    (define/global/3     'vector/update     vector/update)
    
    ; Persistent functional hash tables.
    ; Type test for hash tables.
    (define/global/1 'hash/persist?      hash/persist?)
    ; Empty hash tables.
    (cons            'hash/eq/null       hash/eq/null)    ; Pre-defined constant.
    (cons            'hash/eqv/null      hash/eqv/null)   ; Pre-defined constant.
    (cons            'hash/equal/null    hash/equal/null) ; Pre-defined constant.
    ; Hash table constructors.
    (define/global/N    'hash/new        hash/new)
    (define/global/2    'list/hash       list/hash)
    (define/global/2    'pairs/hash      pairs/hash)
    (define/global/3    'hash/cons       hash/cons)
    (define/global/2    'hash/remove     hash/remove)
    (define/global/2    'hash/merge      hash/merge)
    ; Hash table deconstructors.
    (define/global/1     'hash=>list     hash=>list)
    (define/global/1     'hash=>pairs    hash=>pairs)
    (define/global/1     'hash/keys      hash/keys) 
    (define/global/1     'hash/values    hash/values)
    ; Hash table lookup.
    (define/global/3     'hash/ref       hash/ref)
    (define/global/1     'hash/car       hash/car)
    (define/global/1     'hash/cdr       hash/cdr)
    ; Cardinality and membership.
    (define/global/1     'hash/length    hash/length)
    (define/global/1     'hash/empty?    hash/empty?)
    (define/global/2     'hash/contains? hash/contains?)
    ; Hash table combinators.
    (define/combinator/3-3 'hash/fold      hash/fold)
    (define/combinator/2-2 'hash/map       hash/map)
    (define/combinator/2-2 'hash/for-each  hash/for-each)
    (define/combinator/2-2 'hash/filter    hash/filter)
    (define/combinator/2-2 'hash/partition hash/partition)
    
    ; Persistent functional unordered sets.
    (define/global/1     'set/persist?     set/persist?)
    (cons                'set/eq/null      set/eq/null)
    (cons                'set/eqv/null     set/eqv/null)
    (cons                'set/equal/null   set/equal/null)
    ; Set constructors.
    (define/global/N     'set/new          set/new)
    (define/global/2     'list/set         list/set)
    (define/global/2     'set/cons         set/cons)
    (define/global/2     'set/remove       set/remove)
    (define/global/2     'set/union        set/union)
    (define/global/2     'set/intersection set/intersection)
    (define/global/2     'set/difference   set/difference)
    ; Set deconstructors.
    (define/global/1     'set/list         set/list)
    ; Set lookup.
    (define/global/2     'set/contains?    set/contains?)
    (define/global/1     'set/car          set/car)
    (define/global/1     'set/cdr          set/cdr)
    ; Set cardinality and membership.
    (define/global/1     'set/length       set/length)
    (define/global/1     'set/empty?       set/empty?)
    (define/global/2     'set/subset?      set/subset?)
    ; Set combinators.
    (define/combinator/3 'set/fold         set/fold)
    (define/combinator/2 'set/map          set/map)
    (define/combinator/2 'set/filter       set/filter)
    (define/combinator/2 'set/partition    set/partition)
    
    ; Tuples.
    (define/global/1     'tuple?           tuple?)
    (define/global/1     'tuple/length     tuple/length)
    (define/global/1     'tuple/list       tuple/list)
    (define/global/2     'tuple/ref        tuple/ref)
    ; Tuple constructors.
    (define/global/N     'tuple            tuple)
    (define/global/1     'list/tuple       list/tuple)
    (define/global/N     'tuple/append     tuple/append)
    ; Tuple slicing.
    (define/global/N     'tuple/copy       tuple/copy)
    (define/global/2     'tuple/drop/left  tuple/drop/left)
    (define/global/2     'tuple/drop/right tuple/drop/right)
    (define/global/2     'tuple/take/left  tuple/take/left)
    (define/global/2     'tuple/take/right tuple/take/right)
    ; Tuple combinators.
    (define/combinator/2 'tuple/build      tuple/build)
    (define/combinator/2 'tuple/filter     tuple/filter)
    (define/combinator/2 'tuple/map        tuple/map)
    (define/combinator/2 'tuple/partition  tuple/partition)
    
    ; Records.
    ; The functions record/new, record/cons, and record/ref are implemented as special forms.
    (define/global/1 'record?          record?)
    (define/global/2 'record/contains? record/contains?)
    (define/global/1 'record/kind      record/kind)
    (define/global/1 'record/keys      record/keys)
    
    (define/global/1 'displayln        displayln)

    ; Binding environments
    (cons 'environ/null environ/null)
    (cons 'environ/capture (motile/environ/capture))
    (define/global/2 'environ/merge environ/merge)
    
    ; Higher order functions.
    (cons            'apply     (motile/apply))
    (cons            'map       (motile/metamap 'map      map))
    (cons            'for-each  (motile/metamap 'for-each for-each))
    (cons            'foldr     (motile/metamap 'foldr    foldr))
    (cons            'foldl     (motile/metamap 'foldl    foldl))
    (cons            'filter    (motile/metamap 'filter   filter))

    ; Control
    (cons 'call/cc motile/CALL-CC)
    (cons 'call-with-current-continuation motile/CALL-CC) ; Synonym.

    ; Box
    (define/global/1 'box      box)
    (define/global/1 'box?     box?)
    (define/global/1 'unbox    unbox)
    (define/global/2 'box!     set-box!)
    (define/global/2 'set-box! set-box!) ; For compatibility with Racket.
    
    ; Generic list sort.
    (cons 'sort (motile/sort))
    )))

(define ENVIRON/TEST
  (pairs/environ
   BASELINE
   (list
    ; Mutable vectors for some regression tests.
    (define/global/1 'vector?       vector?)
    (define/global/N 'make-vector   make-vector)
    (define/global/N 'vector        vector)
    (define/global/1 'vector-length vector-length)
    (define/global/1 'vector->list  vector->list)
    (define/global/2 'vector-ref    vector-ref)
    (define/global/3 'vector-set!   vector-set!)
    
    ; Testing serialization of hosh tables.
    (define/global/1 'hash=>vector/racket   hash=>vector/racket) ; Converts persistent hash table to flat Racket vector.
    
    ; For simple test output.
    (define/global/N 'sleep          sleep)
    (define/global/1 'display        display)
    (define/global/0 'newline        newline)
    (define/global/1 'pretty-display pretty-display))))

;(define/global/1 'make-rectangular               make-rectangular)
;;(def-proc 'make-polar                     make-polar)
;;(def-proc 'real-part                      real-part)
;;(def-proc 'imag-part                      imag-part)
;;(def-proc 'magnitude                      magnitude)
;;(def-proc 'angle                          angle)
;(def-proc 'call-with-input-file           call-with-input-file)
;(def-proc 'call-with-output-file          call-with-output-file)
;(def-proc 'input-port?                    input-port?)
;(def-proc 'output-port?                   output-port?)
;(def-proc 'current-input-port             current-input-port)
;(def-proc 'current-output-port            current-output-port)
;(def-proc 'open-input-file                open-input-file)
;(def-proc 'open-output-file               open-output-file)
;(def-proc 'close-input-port               close-input-port)
;(def-proc 'close-output-port              close-output-port)
;(def-proc 'eof-object?                    eof-object?)
;(def-proc 'read                           read)
;(def-proc 'read-char                      read-char)
;(def-proc 'peek-char                      peek-char)
;(def-proc 'write                          write)
;(def-proc 'display                        display)
;(def-proc 'newline                        newline)
;(def-proc 'write-char                     write-char)

