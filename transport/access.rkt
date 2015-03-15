#lang racket/base

(require
 racket/contract/base)

(provide
 ; Is access point revoked?
 (rename-out
  [access/revoked access/revoked?]
  [access:send/embargo access:send/embargo?])
  
 (contract-out
  [access/id (-> access? symbol?)]
  [access/nickname (-> access? (or/c string? symbol?))]
  [access/channel  (-> access? channel?)]
  [access/gate (-> access? gate?)]
  ; Permanently revoke access point.
  [access/revoke   (-> access? void?)]
  ; Transmit a value via an access:send point.
  [access/send     (-> access:send? any/c boolean?)]
  ; Receive a value via an access:receive point.
  [access/receive  (-> access:receive? any/c any/c)]
  [access/receive/try (-> access:receive? any/c any/c)]
  [access/closed?  (-> access? boolean?)]
  
  [access:send/metrics (-> access:send? vector?)]
  [access:receive/metrics (-> access:receive? vector?)]
  [access:send/new (-> transport? symbol? gate? boolean? access:send?)]
  [access:send/known/new (-> transport? symbol? gate? boolean? access:send?)]
  [access:receive/new (-> transport? symbol? gate? access:receive?)])
 
 access/transport
 access?
 access:send?
 access:receive?
 EMBARGO/NO
 EMBARGO/YES
 GATE/NONE)

(require
  "../uuid.rkt"
  "../getters.rkt"
  "gate.rkt"
  "transport.rkt")
#|
An access point for a transport is the means by which Motile sprites send and receive
messages on that transport.
There are two flavors of access points access:send and access:receive, for message
transmission and message reception respectively.
Every access point contains zero or more gates that regulate and monitor the exploitation
of the access point.
A single tranport may be referenced by multiple access points, for example, a transport t
may be referenced by m >= 0 access:send points and n >=0 access:receive points.

Multiple access points of the same transport may share the same gate(s) among
themselves. This is thread-safe as the gates are evaluated only if the
transport is locked by a single thread. Sharing a gate among the access points of
two (or more) transports t and t' is unsafe.

Each access point is tagged with a unique UUID (used by CURLs to reference a particular access:send point).
An access point may be "revoked" guaranteeing that all future attempts to employ this access point
for transmission (reception) will fail.
Revocation for an access point can not be reversed. Once revoked an access point can never again
be used for transmission (reception).
|#
(struct
 access
 (id        ; UUID (as symbol)
  nickname  ; Informal name (as symbol) for logging and debugging.
  transport ; Underlying transport for this access point.
  gate      ; Entry gate for this access point.
  (revoked  #:mutable)) #:transparent) ; #t if revoked and #f otherwise.
(define GATE/NONE   GATE/ALWAYS)
(define EMBARGO/NO  #f)
(define EMBARGO/YES #t)
(struct/getters/define access id nickname transport gate revoked)
(struct/setters/define access revoked)

(define ANONYMOUS '<anonymous>)
;; Constructor.
(define (access/new id tansport nickname gate)
  (access id nickname transport ACCESS/UNREVOKED gate))
;; Return the channel implementing the underlying transport of the access point.
(define (access/channel self) (transport/channel (access/transport self)))
;; Poison the access point.
(define (access/revoke self) (access/revoked/set self #t))

;; An access point is closed if it has been revoked or if at least one of its gates is closed?.
;; Returns #t if the access point is closed and #f otherwise.
(define (access/closed? self)
  (or (access/revoked self) (gate/closed? (access/gate self))))

;; An access point for message transmission.
(struct
 access:send access
 ((embargo   #:mutable)   ; #t iff no CURL may reference this access point and #f otherwise.
  (sends     #:mutable)   ; Total number of sends via this access point.
  (denials   #:mutable)) #:transparent) ; Total number of times a gate has blocked access.
(define ACCESS/UNREVOKED   #f)
(define ACCESS:SEND/SENDS   0)
(define ACCESS:SEND/DENIALS 0)
(struct/getters/define access:send embargo sends denials)
(struct/setters/define access:send sends denials)
(struct/++/define access:send sends denials)

;; Constructor.
(define (access:send/new transport nickname gate embargo)
     (access:send
      (uuid/symbol) nickname transport gate ACCESS/UNREVOKED
      embargo ACCESS:SEND/SENDS ACCESS:SEND/DENIALS))
(define (access:send/known/new transport nickname gate embargo)
  (access:send
   nickname nickname transport gate ACCESS/UNREVOKED
    embargo ACCESS:SEND/SENDS ACCESS:SEND/DENIALS))

;; Evaluate gate predicates.
;; Returns #t if all gates permit access and  #f if any single gate denies access.
;; self - access?
;; message - the message being transmitted via the access point
(define (access:send/pass? self message) (gate/pass? (access/gate self) message))

;; Evaluate the actions affiliated with the gates of the access point.
;; self - the access point
;; message - the message being transmitted via the access point
(define (access:send/acts self message) (gate/act (access/gate self) message))

;; Utility for access:send metrics.
;; Return a metrics snapshot as an immutable vector.
(define (access:send/metrics self)
  (vector-immutable 'access:send/metrics 'sends (access:send/sends self) 'denials (access:send/denials self)))

#|
Transmit value x via the given access:send? point.
Returns #t if the access:send? point accepted the value x for transmission.
Returns #f if the access:send? point gates (if any) rejected the transmission attempt
for some reason or if the underlying transport channel rejected the transmission
for some channel-specific reason.
Senders are free to retry a rejected attempt but there is NO guarantee that the retry
will ever succeed (for example, if the gate is a use count that is exhausted then
the access point is ineffectual from that point forward).
|#
(define (access/send access x)
  (let ((t (access/transport access)))
    (cond
      [(access/revoked access) #f]
      [else
       (transport/lock t)
       (let ((outcome
              (cond
                [(access:send/pass? access x)
                 (access:send/acts  access x)
                 (cond
                   [(transport/put t x)
                    (transport/sends++ t)
                    (access:send/sends++ access)
                    (transport/notify t) ; Notify anyone waiting to receive.
                    #t]
                   [else
                    (transport/overflows++ t)
                    #f])]
                [else
                 (access:send/denials++ access)
                 #f])))
         (transport/unlock t)
         outcome)])))

;; An access point for message reception.
(struct access:receive access
        (event (receives #:mutable) (denials #:mutable))
        #:property prop:evt (struct-field-index event))
(struct/getters/define access:receive receives denials)
(struct/setters/define access:receive receives denials)
(struct/++/define access:receive receives denials)
(define ACCESS:RECEIVE/RECEIVES 0)
(define ACCESS:RECEIVE/DENIALS  0)
;; So we can use an access:receive structure as an event in a (sync ...) or (sync/timeout ...).
(define (event/generate self)
  (wrap-evt
   (transport/trigger (access/transport self))
   (lambda (_) self)))

;; Constructor
(define (access:receive/new transport nickname gate)
  (access:receive
   (uuid/symbol)
   nickname
   transport
   gate
   ACCESS/UNREVOKED ; Revocation flag
   event/generate   ; Event for Racket (sync ...).
   ACCESS:RECEIVE/RECEIVES  ; Passes counter.
   ACCESS:RECEIVE/DENIALS)) ; Denials counter.

;; Evaluate gate predicates.
(define (access:receive/pass? self) (gate/pass? (access/gate self) #f))

;; Evaluate gate actions.
(define (access:receive/acts self) (gate/act (access/gate self) #f))

(define (access:receive/metrics self)
  (vector-immutable
   'access:receive/metrics
   'receives (access:receive/receives self) 'denials  (access:receive/denials self)))

#|
Blocking receive via the given access point.
Returns the "next" available value from the transport referenced by the
given access point (the meaning of "next" is transport-specific).
If the gates of the access point prevent access then the given failure value
is returned.
Receivers are free to retry if the failure value is returned but there is NO guarantee
that the retry will ever succeed (for example if the gate is a deadline that has expired
then the access point if ineffectual from that point forward).
|#
(define (access/receive access failure)
  (let ((t (access/transport access)))
    (cond
      [(access/revoked access) failure]
      [else
       (transport/lock t)
       (cond
         [(access:receive/pass? access)
          (access:receive/acts access)
          (let loop ()
            (cond
              [(transport/empty? t)
               (transport/underflows++ t)
               (transport/unlock t)
               (transport/wait t) ; Wait until something arrives.
               (loop)]            ; Try again.
              [else
               (let ((value (transport/take t)))
                 (transport/receives++ t)
                 (access:receive/receives++ access)
                 (transport/unnotify t)
                 (transport/unlock t)
                 value)]))]
         [else
          (access:receive/denials++ access)
          (transport/unlock t)
          failure])])))

;; Nonblocking receive.
(define (access/receive/try access failure)
  (let ((t (access/transport access)))
    (cond
      [(access/revoked access) failure]
      [else
       (transport/lock t)
       (cond
         [(access:receive/pass? access)
          (access:receive/acts access)
          (cond
            [(transport/empty? t)
             (transport/underflows++ t)
             (transport/unlock t)
             failure]
            [else
             (let ((value (transport/take t)))
               (transport/receives++ t)
               (access:receive/receives++ access)
               (transport/unnotify t)
               (transport/unlock t)
               value)])]
         [else
          (access:receive/denials++ access)
          (transport/unlock t)
          failure])])))


#| 
Metrics:
<access:send>
   sends denials
   <transport>
      sends receives overflows underflows
      <channel>
         puts takes

<access:receive>
   receives denials
   <transport>
      sends receives overflows underflows
     <channel>
        puts takes
|#