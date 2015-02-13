#lang racket/base

(require
 racket/contract/base
 "../base64.rkt"
 "../bindings/libczmq/zconfig.rkt"
 "../curve.rkt"
 "../getters.rkt"
 "../persistent/hash.rkt"
 "../time.rkt"
 "../transport/access.rkt"
 "../z85.rkt")

(provide
 neighbor/kp
 neighbor/kp/sign
 neighbor/kp/base64
 neighbor/kp/sign/base64
 neighbor/seen
 neighbor/zyre/id
 archipelago/c
 zyre/id/c
 (contract-out
  [struct
   neighbor
   ((kp kp/c)
    (kp/sign kp/sign/c)
    (kp/base64 kp/base64/c)
    (kp/sign/base64 kp/sign/base64/c)
    (seen time/utc?)
    (zyre/id zyre/id/c))]
  [archipelago/new    (-> archipelago/c)]
  [archipelago/add    (-> archipelago/c zyre/id/c kp/base64/c kp/sign/base64/c void?)]
  [archipelago/remove (-> archipelago/c (or/c zyre/id/c kp/base64/c) void?)]
  [archipelago/look   (-> archipelago/c (or/c zyre/id/c kp/base64/c) (or/c neighbor? #f))]
  [archipelago/neighbors (-> archipelago/c list?)]
  [neighbor/pretty (-> neighbor? vector?)]))
 
(define archipelago/c (flat-named-contract 'archipelago (box/c hash/persist? #:flat? #t)))

(define ZYRE/ID/LENGTH 32)
(define zyre/id/c
  (flat-named-contract 'zyre/id (and/c string? (lambda (s) (= (string-length s) ZYRE/ID/LENGTH)))))

(struct
 neighbor
 (kp             ; Public key of neighbor island (bytes)
  kp/sign        ; Public signing key of neighbor (bytes)
  kp/base64      ; Public key of neighbor (base64 bytes)
  kp/sign/base64 ; Public signing key of neighbor (base64 bytes). 
  seen           ; Timestamp when neighbor first appeared on the network
  zyre/id))      ; ZyRE id of neighbor (ASCII 32 character hex string)

(struct/getters/define neighbor kp kp/sign kp/base64 kp/sign/base64 seen zyre/id)
(define (neighbor/new id kp/base64 kp/sign/base64)
  (neighbor
   (base64/url/decode kp/base64)
   (base64/url/decode kp/sign/base64)
   kp/base64
   kp/sign/base64
   (time/now)
   id))

;; For logging and debugging.
(define (neighbor/pretty n)
  (vector-immutable
   'neighbor
   (neighbor/kp/base64 n) (time-to-ISO8601 (neighbor/seen n)) (neighbor/zyre/id n)))

;; Utility routines for update of the island archipelago map.
;; For each neighboring island J that appears on the network two key:value pairs
;; are inserted into the map: kp/z85:n and zyre/id:n where kp/z85 and zyre/id
;; are the public key and ZyRE uuid of J respectively and n is a single instance
;; of a neighbor instance for J.

(define (archipelago/new) (box hash/equal/null))

;; For now (until zauth is tweaked to associate the a credential with each message)
;; each island includes its public CURVE encryption and signing keys in the headers
;; that it advertises to other ZyRE peers.
;; The archipelago maps are updated when a new COAST peer is detected on the network.
;;   i - island structure
;;   id - ZyRE id of island J (as ASCII string in hex)
;;   headers -ZyRE headers (as hash/persist) advertised by island J
;; Returns an updated archipelago in which both the ZyRE id and the CURVE public 
;; key are hash keys to a fresh neighbor structure for J.
(define (archipelago/add b id kp/base64 kp/sign/base64)
  (let ([n (neighbor/new id kp/base64 kp/sign/base64)])
    ; Pay attention only if the neighboring island is advertising its CURVE keys.
    ; Note: this is just a stopgap until CZMQ implements association of CURVE kp keys with messages.
    (when (and kp/base64 kp/sign/base64)
      (let loop ([before (unbox b)])
        (when (not (box-cas! b before (hash/new before kp/base64 n id n)))
          (loop (unbox b)))))))

#|
The inverse of archipelago/add above. The archipelago map is updated when
a COAST peer exits the network.
   a - achipelago map
   k - either a ZyRE UUID for an island J or the CURVE public key of J.
Both map entries (one indexed by the ZyRE UUID of J and the other by the CURVE
public key of J) are removed from the map.
Returns an updated archipelago map less the two map entries.
|#

;; Given either a zyre/id or an island kp/base64 for an island return
;; both of the keys.
(define (keys/both a k)
  (let ([n (hash/ref a k #f)])
    (and n (list (neighbor/zyre/id n) (neighbor/kp/base64 n)))))

(define (archipelago/remove b k)
  (let* ([n (hash/ref (unbox b) k #f)])
    (when n
      (let ([keys (keys/both (unbox b) k)])
        (let loop ([before (unbox b)])
          (when (not (box-cas! b before (hash/list/remove before keys)))
            (loop (unbox b))))))))

(define (archipelago/look b k) (hash/ref (unbox b) k #f))

;; Returns a list of the public keys of all neighboring islands.
(define (archipelago/neighbors b)
  (hash/fold
   (unbox b)
   (lambda (k v seed) (if (= (bytes-length k) KP/BASE64/LENGTH) (cons k seed) seed))
   null))



    


