#lang racket/base
 
(require
 "../../include/base.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define NOTIFY_SERVICE/SECRET/PATH (string-append CERTIFICATE/SECRET "notify_service_secret"))

(define NOTIFY_SERVICE/CURVE/SECRET (path-to-curve NOTIFY_SERVICE/SECRET/PATH))

;; Code for market price notification service.
(define (notify_service/boot)
  (define (service/notify)
    (let* ([t (transport:bankers/new)] ; Generic queuing transport.
           [a/send    (access:send/known/new t 'access:send:notify GATE/NONE EMBARGO/NO)]
           [a/receive (access:receive/new t 'access:receive:notify GATE/NONE)]
           [nickname (log/name/build (this/island/nickname) 'notify)]
           [keystore (this/keystore)])
      (accessor/add (this/accessors) a/send) ; So the island router knows where to find us.
      
      ; Wait until an island makes a notification request.
      (let loop ([in (sync a/receive)]) ; block until something arrives on transport t.
        (let ([m (access/receive in #f)]) ; Now accept it.
          (when (murmur? m)
            (let ([payload (murmur/payload m)])
              (when (and (boolean? payload) payload) ; The payload must be the value #t.
                (display (format
                          "  received notfication request from ~a\n"
                          (keystore/petname/look keystore (murmur/origin m)))))))
          (loop (sync a/receive))))))
  
  (service/notify))

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define notify_service (island/new 'notify_service NOTIFY_SERVICE/SECRET/PATH notify_service/boot))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set notify_service KEYSTORE)
(island/log/level/set 'info)




