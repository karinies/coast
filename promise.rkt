#lang racket/base

(require
  racket/contract/base
  "promise/base.rkt"
  "Island/accessor.rkt"
  [only-in "curl/base.rkt" curl/access]
  "curl/curl.rkt"
  "curve.rkt"
  "getters.rkt"
  "islet.rkt"
  "murmur.rkt"
  "persistent/environ.rkt"
  "transport/access.rkt"
  "transport/transport.rkt"
  "transport/gate.rkt"
  "transport/gates/whitelist.rkt"
  "transport/transports/bankers.rkt"
  "transport/transports/promise.rkt"
  [only-in "Island/island-comet.rkt" island/monitoring/log]
  "comet/comet-types.rkt")

(provide
 (rename-out
  [duplet:promise? promise?]
  [islet/curl/new islet/curl]) ; DEPRECATED.
 
 duplet?
 duplet/receiver
 duplet/resolver
 (contract-out
  [duplet/new (-> access:receive? curl? duplet?)]
  [duplet/block (-> duplet? (or/c murmur? #f))]
  [duplet/try   (-> duplet? (or/c murmur? #f))]
  [duplet/wait  (-> duplet? (>=/c 0) (or/c murmur? #f))]
  
  [promise/new (-> duplet:promise?)]
  [promise/block (-> duplet:promise? (or/c murmur? #f))]
  [promise/wait (-> duplet:promise? (>=/c 0)(or/c murmur? #f))]
  [promise/try  (-> duplet:promise? (or/c murmur? #f))]
  [promise/resolved? (-> duplet:promise? boolean?)]
  ;[promise/receiver (-> duplet:promise? access:send?)]
  [promise/receiver (-> duplet:promise? access:receive?)]
  [promise/resolver (-> duplet:promise? curl?)]
  
  
  [islet/curl/new       (-> (listof symbol?)         gate? (or/c environ? #f) place/c duplet?)]
  [islet/curl/known/new (-> (listof symbol?) symbol? gate? (or/c environ? #f)        duplet?)]
  [islet/curl/meta (-> transport? gate? gate? duplet?)]))

;(define island/promise/c (flat-named-contract 'promise (cons/c access:receive? curl?)))

(struct duplet (receiver resolver event) #:property prop:evt (struct-field-index event))
(struct/getters/define duplet receiver resolver)
(define (duplet/event/generate self)
  (wrap-evt
   (transport/trigger (access/transport (duplet/receiver self)))
   (lambda (_) self)))

(define (duplet/new a/receive u)
  (duplet a/receive u duplet/event/generate))

(struct duplet:promise duplet ())
(define (promise/receiver p) (duplet/receiver p))
(define (promise/resolver p) (duplet/resolver p))


;; The promise API given here barely scratches the surface.
;; If one uses the lower level primitives that promise/new relies upon
;; then it is possible to contruct promises that are time, space, and state-dependent
;; (for example, promises that have limited lifespans, are non-delegable, that
;; must be resolved on the island on which they were created, or that type check
;; the resolution of the promise.

;; Create a simple generic promise that is suitable for both intra-
;; and inter-island use.
;; nickname - a symbol meaningful to the creator of the promise
;; Returns a cons cell (cons/c a u) where
;; a is an access:receive point and u is a CURL to be transmitted to islets
;; or islands that have an interest in satisfying the promise.

;; The CURL u is extremely simple.
;; Its access:send point contains only a gate/once,
;; the pathname is (promise <nickname), and it contains no metadata.
;; Once the CURL has been used to satisfy the promise it is useless.
;; The value of the promise is read from the access:receive point as a murmur that
;; contains the kp/base64 public key of the island satisfying the promise, a copy
;; of the CURL u, and the value of the promise.
;; The access:receive point can be used in any (sync ...) form.

;; Promises may be used both intra- and inter-island. If it proves necessary it
;; is trivial using the transport and access point primitives to construct a promise
;; that can be used only intra-island.
;; Similarly, one could construct a promise whose use is inter-island only.
;; Perhaps an additional parameter which would specify inter, intra, or both.
(define (promise/new)
  (let* ([t (transport:promise/new)]
         [a/send (access:send/new t 'promise (gate/once) EMBARGO/NO)]
         [a/receive (access:receive/new t 'promise (gate/whitelist/islet (this/islet)))]
         [keys (this/curve)]
         [c (curl/core/new* keys '(promise) a/send #f)])
    (duplet:promise a/receive (curl/new* c keys) duplet/event/generate)))


;; Blocking read of the resolution of a promise.
(define (promise/block p) 
  (let ([result (access/receive (duplet/receiver p) #f)])
    (island/monitoring/log #:type COMET/CURL/RECEIVE
                           #:place #f
                           #:curl (duplet/resolver p))
    result))

;; Attempt to read the promise without blocking while hiding all of the machinery underneath.
;; Returns the payload of the transmitted murmur (as described above)
;; on success and the failure value if the access:receive? would block.
(define (promise/try p) 
  (let ([result (access/receive/try (duplet/receiver p) #f)])
    (island/monitoring/log #:type COMET/CURL/RECEIVE
                           #:place #f
                           #:curl (duplet/resolver p))
    result))

;; A blocking read with timeout again while hiding as much machinery as possible.
;; If the wait timeouts before the promise is resolved then the failure value
;; is returned. If the promise is resolved before the timeout then the payload
;; of the murmur that resolved the promised is returned.
(define (promise/wait p timeout)
  (let ([x (sync/timeout timeout p)])
    (and x (access/receive (duplet/receiver x) #f))))

;; Returns #t if promise has been resolved and #f otherwise.
(define (promise/resolved? p)
  (not (transport/empty? (access/transport (duplet/receiver p)))))

;; Generate a fresh bankers queue transport, its access points, and a CURL that references the
;; generated access:send point.
;; Returns a doublet containing the access:receive point and the CURL.
;; path - path of duplet CURL
;; gate - gate for access:send point of CURL
;; metadata - metadata for CURL (as environ?)
;; place - place where CURL may be exercised
(define (islet/curl/new path gate metadata place)
  (let* ([t (transport:bankers/new)]
         [name (this/islet/nickname)]
         [a/send     (access:send/new   t name gate (place/intra? place))]
         [a/receive (access:receive/new t name (gate/whitelist/islet (this/islet)))]
         [keys (this/curve)]
         [core
          (curl/core/new* keys path (if (place/inter? place) (access/id a/send) a/send) metadata)])
    (when (place/inter? place) (accessor/add (this/accessors) a/send))
    (duplet/new a/receive (curl/new core (curve/kp/sign keys) (curve/ks/sign keys)))))


;; START HERE !!!!!!!!
(define (islet/curl/known/new path access/id gate metadata)
  (let* ([t (transport:bankers/new)]
         [name (string->symbol (format "access:receive.~a" (symbol->string (this/islet/nickname))))]
         [a/send    (access:send/known/new t access/id gate EMBARGO/NO)]
         [a/receive (access:receive/new    t name      (gate/whitelist/islet (this/islet)))]
         [keys (this/curve)]
         [core (curl/core/new* keys path access/id metadata)])
    (accessor/add (this/accessors) a/send)
    (duplet/new a/receive (curl/new* core keys))))

;; Generate a version of islet/curl whose transport and access points are given ahead
;; of time. This allows islands to confine a spawn to a particular transport whose
;; access points contain spawn-specific gates.
(define (islet/curl/meta t a/send/gate a/receive/gate)
  (lambda (path gate metadata place)
    (let* ([name (this/islet/nickname)]
           [whitelist (gate/whitelist/islet (this/islet))]
           [a/send (access:send/new t name a/send/gate (place/intra? place))]
           [a/receive (access:receive/new t name (gate/and whitelist a/receive/gate))]
           [keys (this/curve)]
           [core
            (curl/core/new* keys path (if (place/inter? place) (access/id a/send) a/send) metadata)])
      (when (place/inter? place) (accessor/add (this/accessors) a/send))
      (duplet/new a/receive (curl/new core (curve/kp/sign keys) (curve/ks/sign keys))))))

(define (duplet/block d)
  (let ([result (access/receive (duplet/receiver d) #f)])
    (island/monitoring/log #:type COMET/CURL/RECEIVE
                           #:place #f
                           #:curl (duplet/resolver d))
    result))

(define (duplet/try d)
  (let ([result (access/receive/try (duplet/receiver d) #f)])
    (island/monitoring/log #:type COMET/CURL/RECEIVE
                           #:value #f
                           #:curl (duplet/resolver d))
    result))

(define (duplet/wait d timeout)
  (let ([x (sync/timeout timeout d)])
    (and x (access/receive (duplet/receiver x) #f))))
(define (duplet/backlog? d) (transport/nonempty? (access/transport (duplet/receiver d))))

