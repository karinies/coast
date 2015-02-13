#lang racket/base

(require
 racket/contract/base
 "accessor.rkt"
 "archipelago.rkt"
 "keystore.rkt"
 "../bindings/libzyre/libzyre.rkt"
 "../curve.rkt"
 "../getters.rkt"
 "../transport/transports/thread.rkt")

(provide
 island/nickname
 island/keys
 island/bridge
 island/ingress
 island/router
 island/egress
 island/reaper
 island/peer
 island/underpeer
 island/boot
 island/archipelago
 island/accessors
 island/keystore
 island/curl/bytes/max
 island/payload/bytes/max
 island/bridge/set
 island/ingress/set
 island/router/set
 island/egress/set
 island/reaper/set
 island/peer/set
 island/keystore/set
 thunk/c
 (contract-out
  [struct
   island
   ((nickname symbol?)
    (keys curve?)
    ; Transit threads.
    (bridge  (or/c thread? #f))
    (ingress (or/c thread? #f))
    (router  (or/c thread? #f))
    (egress  (or/c thread? #f))
    (reaper  (or/c thread? #f))
    (peer    (or/c thread? #f))
    ;(egress/transport transport:thread?) ; Future upgrade.
    ; ZyRE peer
    (underpeer zyre?)
    ; Boot function (lambda (i) ...)
    (boot thunk/c)
    ; Maps
    (archipelago archipelago/c)
    (accessors accessor/c)
    (keystore keystore/c)
    ; Island-specific constants
    (curl/bytes/max exact-positive-integer?)
    (payload/bytes/max exact-positive-integer?))]))

(struct
 island
 (nickname ; Pet name (symbol) for debugging and logging.
  keys     ; CURVE instance containing island crypto keys.
  ; Transit threads.
  (bridge  #:mutable) ; Hoists incoming island messages from underlying ZyRE peer.
  (ingress #:mutable) ; Unpacks messages from bridge and forwards commands carried in WHISPERS to router
  (router  #:mutable) ; Routes legitimate messages to access:send points
  (egress  #:mutable) ; Lowers outgoing island messages to the underlying ZyRE peer.
  (reaper  #:mutable) ; Reaps closed access:send points found in accessors.
  (peer    #:mutable) ; Peer service and client thread.
  ;egress/transport    ; transport:thread instance for egress
  underpeer           ; Underlying ZyRE peer.
  boot                ; Peer boot thunk.
  ; Maps
  archipelago ; Box holding the archipelogo map of all known neighboring islands.
  accessors   ; Box holding the set of all access:send points referenced by exported CURLs.
  (keystore #:mutable)   ; Box holding a map of kp/kp/sign for both this and other islands.
  ; Island-specific constants.
  (curl/bytes/max    #:mutable)   ; Upper bound on serialized CURL the island is willing to accept.
  (payload/bytes/max #:mutable))) ; Upper bound on serialized paylaod the island is willing to accept.

(struct/getters/define
 island
 nickname keys
 bridge ingress router egress reaper peer
 underpeer boot
 archipelago accessors keystore
 curl/bytes/max payload/bytes/max)
(struct/setters/define island bridge ingress router egress reaper peer keystore curl/bytes/max payload/bytes/max)

;; Contract for thunks (zero-argument closures).
(define thunk/c
  (flat-named-contract
   'thunk
   (and/c
    procedure?
    (lambda (f)
      (let ((n (procedure-arity f)))
        (and (exact-integer? n) (zero? n)))))))