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
 racket/contract/base
 "../getters.rkt"
 (only-in "ordered_trie.rkt" define-accessor)
 [only-in
  "hash.rkt"
  hash/and
  hash/cons
  hash/eq/null
  hash/for-each
  hash/length
  hash/merge
  hash/or
  hash/ref
  hash/remove
  hash/vector/remove
  hash/persist?
  list/hash
  pairs/hash
  vector/hash
  vectors/hash
  hash/persist/flatten
  hash/persist/unflatten])            

(provide
 (rename-out
  [environ/ref environ/get]
  [vector/environ vector-to-environ])
 environ
 environ/null
 environ/map
 environ/and
 environ/for-each
 environ/or
 environ?
 (contract-out
  [environ/cons
   (case->
    (-> environ? symbol? any/c environ?)
    (-> environ? #:rest any/c  environ?))]
  [pair-to-environ (-> environ? (cons/c symbol? any/c) environ?)]
  [pairs-to-environ (-> environ? (listof (cons/c symbol? any/c)) environ?)]

  [environ/ref (environ? (or/c symbol? (vectorof symbol?) (listof symbol?)) any/c . -> . any/c)]
  [environ/remove (environ? symbol? . -> . environ?)]
  [environ/length (-> environ? exact-nonnegative-integer?)]

  )
 vector/environ
 vectors/environ
 environ/merge
 environ/vector/remove

 environ/ref/symbol
 environ/ref/vector
 environ/ref/list
 list/environ
 
 ; Serialization.
 environ/flatten
 environ/unflatten
 )
          
;; A binding environment is just a wrapping around an eq? persistent hash table
;; whose keys are the names of lexical variables (given as symbols).
(struct environ (map) #:constructor-name environ/construct)
(struct/getters/define environ map)

;(define ENVIRON/FLAVOR '<environ/persist>) ; Motile type identifier for binding environments.

;(define (environ/construct h)
;  (vector ENVIRON/FLAVOR h))

;; The canonical empty binding environment.
(define environ/null (environ/construct hash/eq/null))

(define (environ/length e) (hash/length (environ/map e)))

;(define (environ? e)
;  (and
;   (vector? e)
;   (= (vector-length e) 2)
;   (eq? (vector-ref e 0) ENVIRON/FLAVOR)
;   (hash/persist? (vector-ref e 1))))

(define environ/cons
  (case-lambda
    ((e s_1 v_1)
     (environ/construct (hash/cons (environ/map e) s_1 v_1)))
    ((e . rest)
     (environ/construct (list/hash (environ/map e) rest)))))

;; Given a list (input) of bindings (k_1 v_1 ... k_m v_m) join bindings k_1/v_1, ..., k_m/v_m to environ e returning the successor
;; environ containing k_1/v_1, ..., k_m/v_m.
;; e remains unchanged.
(define (list/environ e input)
  (environ/construct (list/hash (environ/map e) input)))

(define (list=>environ e input)
    (environ/construct (list/hash (environ/map e) input)))

(define (pairs-to-environ e pairs)
  (environ/construct (pairs/hash (environ/map e) pairs)))

(define (pair-to-environ e pair) (environ/cons e (car pair) (cdr pair)))

;; Given a list of pairs ((k_1 . v_1) ... (k_m . v_m)) join bindings k_1/v_1, ..., k_m/v_m
;; to environ e returning the successor environ containing k_1/v_1, ..., k_m/v_m.
;; e remains unchanged.
(define (pairs=>environ e pairs)
  (environ/construct (pairs/hash (environ/map e) pairs)))

;; Given an environ e and vector v = #(s_0 v_0 ... s_{n-1} v_{n-1}) where s_i, v_i is a symbol/value pair
;; return a successor environ containing bindings s_0/v_0 ... s_{n-1}/v_{n-1}.
;; Environ e remains unchanged.
(define (vector/environ e v)
  (environ/construct (vector/hash (environ/map e) v)))

;; Given an environ e and two vectors, symbols = #(s_0 ... s_{n-1}) and values = #(v_0 ... v_{n-1})
;; return a successor environ containing bindings s_0/v_0 ... s_{n-1}/v_{n-1}.
;; Environ e remains unchanged.
(define (vectors/environ e symbols values)
  (environ/construct (vectors/hash (environ/map e) symbols values)))

;; Let variables be a list (possibly empty) of variable names (k_1 ... k_m).
;; Remove the bindings k_1/v_1, ..., k_m/v_m from environ e returning the successor environ that does not contain any
;; of k_1/v_1, ..., k_m/v_m.
;; e remains unchanged.
(define (environ/vector/remove e variables)
  (if (zero? (vector-length variables)) e (environ/construct (hash/vector/remove (environ/map e) variables))))

;; Returns the merge of environ beta into environ alpha returning a successor environ
;; where the bindings of beta are joined with those of alpha. The bindings of beta take precedence over those of alpha.
(define (environ/merge alpha beta)
  (environ/construct (hash/merge (environ/map alpha) (environ/map beta))))

(define (environ/ref e accessor failure)
  (cond
    ((symbol? accessor) (environ/ref/symbol e accessor failure))
    ((vector? accessor) (environ/ref/vector e accessor failure))
    ((pair?   accessor) (environ/ref/list   e accessor failure))
    (else failure)))

(define (environ/ref/symbol e symbol failure)
  (hash/ref (environ/map e) symbol failure))

;; path is a vector of symbols.
(define (environ/ref/vector e path failure)
  (let loop ((e e) (i 0) (n (vector-length path)))
    (cond
      ((= i n) e)
      ((environ? e)
       (loop (environ/ref/symbol e (vector-ref path i) failure) (add1 i) n))
      (else failure))))

;; path is a list of symbols.
(define (environ/ref/list e path failure)
  (let loop ((e e) (path path))
    (cond
      ((null? path) e)
      ((environ? e)
       (loop (environ/ref/symbol e (car path) failure) (cdr path)))
      (else failure))))

(define (environ/remove e symbol)
  (environ/construct (hash/remove (environ-map e) symbol)))

(define (environ/and e f)
  (hash/and (environ/map e) f))
(define (environ/or e f)
  (hash/or (environ/map e) f))
(define (environ/for-each e f)
  (hash/for-each (environ/map e) f))

;; Serialization
(define (environ/flatten v loop)
  (vector 'struct:environ (hash/persist/flatten (environ/map v) loop)))

;; v is #(struct:environ #(struct:hash ...)) where
;; #(struct:hash ...) is a flattened eq? persistent hash.
(define (environ/unflatten v loop)
  (environ/construct (hash/persist/unflatten (vector-ref v 1) loop)))
  