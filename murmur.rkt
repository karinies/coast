#lang racket/base

(require
 racket/contract/base
 "curl/base.rkt"
 "curve.rkt"
 "getters.rkt")

(provide
 murmur/origin
 murmur/target
 murmur/payload
 
 rustle/destination
 rustle/target
 rustle/payload
 
 (contract-out
  [struct
   murmur
   ((origin kp/base64/c)
    (target curl?)
    (payload any/c))]
  [murmur/local? (-> murmur? kp/base64/c boolean?)]
  [murmur/remote? (-> murmur? kp/base64/c boolean?)]
  [murmur/target/local? (-> murmur? kp/base64/c boolean?)]
  [struct
   rustle
   ((destination kp/base64/c)
    (target bytes?)
    (payload bytes?))]))

(struct murmur (origin target payload) #:transparent)
(struct/getters/define murmur origin target payload)

;; Returns #t if murmur m was a local transmission and #f if it was a remote transmission
;; where kp/base64 is the public key (base64-encoded) of this island.
(define (murmur/local? m kp/base64)
  (bytes=? (murmur/origin m) kp/base64))
;; Returns #t if murmur m was a remote transmission and #f if it was a local transmission
;; where kp/base64 is the public key (base64-encoded) of this island.
(define (murmur/remote? m kp/base64) (not (murmur/local? m kp/base64)))
;; Returns #t if the CURL (target) of the murmur originated on this island and #f otherwise
;; where kp/base64 is the public key (base64-encoded) of this island.
(define (murmur/target/local? m kp/base64)
  (bytes=? (curl/origin (murmur/target m)) kp/base64))

;; Used only in transmission from an islet to its island's egress thread.
(struct rustle (destination target payload) #:transparent)
(struct/getters/define rustle destination target payload)
        