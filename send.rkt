#lang racket/base

(require
 racket/contract/base
 "curl/base.rkt"
 "curve.rkt"
 "islet.rkt"
 "murmur.rkt"
 "island.rkt"
 "persistent/environ.rkt"
 "serialize.rkt"
 "Island/base.rkt"
 "Island/logger.rkt"
 "transport/access.rkt"
 "accounting/como-types.rkt")

(provide
 (rename-out [send/strong send])
 (contract-out
  [send/weak   (-> curl? any/c boolean?)]
  [send/strong (-> curl/verified? any/c boolean?)]))

;; Accepts only inter-island CURLs whose signatures have been verified
;; or intra-island CURLs (which are by definition and construction verified).
(define (send/strong target payload) (send/base target payload))
(define (send/weak target payload)   (send/base target payload))
(define send send/strong)

;; Accepts any CURL as a target.
(define (send/base target payload)
  (let ([a (curl/access target)]
        [kp/base64 (curve/kp/base64 (this/curve))]) ; Public key of this island
    (cond
      [(access:send? a) ; Transmission is intra-island.
       (and
        (bytes=? kp/base64 (curl/origin target)); Paranoia. Ensure CURL originated on this island.
        (access/send a (murmur (curl/origin target) target payload)))]
      
      [(symbol? a) ; Transmission is inter-island
       (log/debug (this/island/nickname) "send inter-island" #t)
       ; Sender pays the costs of serialization.
       (let ([destination (curl/origin target)])
         (cond
           [(bytes=? kp/base64 destination) ; Is this an inter-island send to yourself?
            (log/warning (this/islet/nickname) "attempted inter-island send to home" #f)
            #f]
           [else
            (with-handlers
                ; Catch any serialization failures here.
                ([exn:fail?
                  (lambda (e)
                    (log/warning (this/islet/nickname) "sending nonserializable" (exn-message e))
                    #f)])
              (let ([t/bytes (curl/zpl/signed target)]
                    [p/bytes (motile/flat-to-bytes (motile/serialize payload))])
                ; This thread-send could be replaced by an access/send to modulate or limit use
                ; of inter-island transmissions.
                (with-handlers
                    ([exn:fail:contract?
                      (lambda (_)
                        (log/fatal (this/islet/nickname) "egress thread is dead" #f)
                        (log/fatal (this/islet/nickname) "committing suicide" #f)
                        (kill-thread (islet/thread (this/islet))) ; Is this the right response?
                        #f)])
                  (and
                   (thread-send (island/egress (this/island)) (rustle destination t/bytes p/bytes) #f)
                   #t))))]))]
      [else #f]))
  (island/monitoring/log #:type CURL-SEND
                         #:value #f))
          
;; Note: Should the egress thread have a dedicated transport with specialized access:send
;; points to modulate access to interisland transmission? We could generate a custom
;; version of send for each binding environment where it was appropriate.
;; In other words, implement a transport whose transmisison sink is the mailbox of a specific thread.

(define (receive)
  (let* ([e (islet/environ (this/islet))]
         [a/receive (environ/ref e 'this/access:receive #f)])
    (and a/receive (access/receive a/receive #f))))

(define (receive/try)
    (let* ([e (islet/environ (this/islet))]
           [a/receive (environ/ref e 'this/access:receive #f)])
      (and a/receive (access/receive/try a/receive #f))))
       