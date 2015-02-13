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
 (only-in
  "ordered_trie.rkt"
  define-accessor)
 (only-in
  "hash.rkt"
  hash/cons
  hash/eq/null
  hash/merge
  hash/ref
  hash/vector/remove
  hash/persist?
  list/hash
  pairs/hash
  vector/hash
  vectors/hash)
 
 )

(provide
 environ/null
 environ?
 environ/cons
 vector/environ
 vectors/environ
 environ/merge
 environ/vector/remove
 environ/ref
 environ/ref/symbol
 environ/ref/path
 list/environ
 pairs/environ
 )
          
;; A binding environment is just a wrapping around an eq? persistent hash table
;; whose keys are the names of lexical variables (given as symbols) and arbirary Motile values.
(struct environ (map) #:constructor-name environ/construct)
(define-syntax-rule (environ/map e) (environ-map e))

;(define ENVIRON/FLAVOR '<environ/persist>) ; Motile type identifier for binding environments.

;(define (environ/construct h)
;  (vector ENVIRON/FLAVOR h))

;; The canonical empty binding environment.
(define environ/null (environ/construct hash/eq/null))

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

(define (pairs/environ e pairs)
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
    ((vector? accessor) (environ/ref/path   e accessor failure))
    (else (error 'environ/ref "accessor is neither symbol nor path but ~a" accessor))))

(define (environ/ref/symbol e symbol failure)
  (hash/ref (environ/map e) symbol failure))

;; path is a vector of symbols.
(define (environ/ref/path e path failure)
  (let loop ((e e) (i 0) (n (vector-length path)))
    (cond
      ((= i n) e)
      ((environ? e)
       (loop (environ/ref/symbol e (vector-ref path i) failure) (add1 i) n))
      (else failure))))

  