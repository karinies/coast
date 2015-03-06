#lang racket/base
 
(require
 "../../include/base.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define NOTIFY_CLIENT/SECRET/PATH (string-append CERTIFICATE/SECRET "notify_client_secret"))

(define NOTIFY_SERVICE/KP/BASE64 #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc")

;; The CURL for market price notification service.
(define/curl/inline NOTIFY_SERVICE/CURL/NOTIFY
#<<!!
SIGNATURE = #"HW4HnazYpgNMfODd1KgiM8D-q7wXlIw0HA1z11OT-3TGwz-909PM6GnuI9RP1eHNT8YqEclHOZKyPldPeKBtBQ"
CURL
    id = 4aef50e7-12f7-4f3c-979a-10ed833bf0b5
    origin = #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc"
    path = (service notify)
    access/id = access:send:notify
    created = "2015-03-06T13:54:04Z"
    metadata = #f

!!
)

;; Code for a client island.
(define (client/boot u)
  ; Wait under the island with the given kp/base64 key enters the network.
  (define (island/enter/wait a kp/base64)
    (let loop ()
      (unless (archipelago/look a kp/base64)
        (sleep 2.0) ; Sleep for 2 seconds before looking again.
        (loop)))
    (sleep 1.0)) ; Make sure that the other island has seen us as well.
  
  ; The client that notify_client will spawn to request a market price notification from notification_service.
  ; u is the CURL that the notify_serice provides for its "notify" service.
  (define (notify/client u)
    ; Create a new islet on notify_client to act as a service client.
    (let* ([nickname (log/name/build (this/island/nickname) 'client.notify)]
           [x (islet/new (this/island) nickname TRUST/LOW environ/null environ/null)])
      (islet/jumpstart
       x
       (lambda ()
           ; Wait for notify service to enter the network.
           (island/enter/wait (this/archipelago) NOTIFY_SERVICE/KP/BASE64)
           ; Request notify service to say make a notification request.
           (send u #t)))))
  
  (notify/client u))


(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

;; notify_client is given a CURL for notify_service at birth.
(define notify_client
  (let ([u (curl/zpl/safe-to-curl NOTIFY_SERVICE/CURL/NOTIFY KEYSTORE)])
    (island/new 'notify_client NOTIFY_CLIENT/SECRET/PATH (lambda () (client/boot u)))))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set notify_client KEYSTORE)
(island/log/level/set 'info)


