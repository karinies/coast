#lang racket/base

(require
 racket/contract/base
 "../gate.rkt"
 "../../curve.rkt"
 "../../islet.rkt"
 "../../murmur.rkt"
 "../../persistent/set.rkt")

(provide
 (contract-out
  [gate/whitelist/island (-> (or/c (listof kp/base64/c) kp/base64/c) gate:whitelist:island?)]
  [gate/whitelist/islet  (-> (or/c (listof islet?)      islet?)      gate:whitelist:islet?)]
  [gate/blacklist/island (-> (or/c (listof kp/base64/c) kp/base64/c) gate:blacklist:island?)]
  [gate/blacklist/islet  (-> (or/c (listof islet?)      islet?)      gate:blacklist:islet?)])
 gate:whitelist?
 gate:whitelist:island?
 gate:whitelist:islet?
 gate:blacklist?
 gate:blacklist:island?
 gate:blacklist:islet?)


(struct gate:whitelist gate ())
(struct gate:whitelist:island gate:whitelist ()) ; Intended for access:send points.
(struct gate:whitelist:islet  gate:whitelist ()) ; Intended for access:receive points.

;; whites - list of island kp/base64 public keys.
(define (gate/whitelist/island whites)
  (let ([whites (if (pair? whites) (list/set set/equal/null whites) whites)])
    (gate:whitelist:island
     'gate:whitelist:island
     (cond
       [(set/persist? whites) (lambda (m) (set/contains? whites (murmur/origin m)))]
       [else                  (lambda (m) (bytes=?       whites (murmur/origin m)))])           
     #f
     (lambda () #f)
     (lambda () (vector-immutable 'gate:whitelist:island whites))
     #f)))

(define (gate/whitelist/islet callers)
  (let ([callers (if (pair? callers) (list/set set/eq/null callers) callers)])
    (gate:whitelist:islet
     'gate:whitelist:islet
     (cond
       [(set/persist? callers) (lambda (_) (set/contains? callers (this/islet)))]
       [else                   (lambda (_) (eq?           callers (this/islet)))])
     #f
     (lambda () #f)
     (lambda () (vector-immutable 'gate:whitelist:islet callers))
     #f)))

(struct gate:blacklist gate ())
(struct gate:blacklist:island ()) ; Intended for access:send points.
(struct gate:blacklist:islet ())  ; Intended for access:receive points.

(define (gate/blacklist/island blacks)
  (let ([blacks (if (pair? blacks) (list/set set/equal/null blacks) blacks)])
    (gate:blacklist:island
     'gate:blacklist:island
     (cond
       [(set/persist? blacks) (lambda (m) (not (set/contains? blacks (murmur/origin m))))]
       [else                  (lambda (m) (not (bytes=?       blacks (murmur/origin m))))])           
     #f
     (lambda () #f)
     (lambda () (vector-immutable 'gate:blacklist:island blacks))
     #f)))

(define (gate/blacklist/islet callers)
  (let ([callers (if (pair? callers) (list/set set/eq/null callers) callers)])
    (gate:blacklist:islet
     'gate:blacklist:islet
     (cond
       [(set/persist? callers) (lambda (_) (not (set/contains? callers (this/islet))))]
       [else                   (lambda (_) (not (eq?           callers (this/islet))))])
     #f
     (lambda () #f)
     (lambda () (vector-immutable 'gate:blacklist:islet callers))
     #f)))
