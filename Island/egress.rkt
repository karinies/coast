#lang racket/base

;; An island egress thread is the outbound equivalent of the bridge thread.
;; It shuttles outbound messages into the island ZyRE underpeer as ZyRE WHISPERS.

(require
 racket/contract/base
 "base.rkt"
 "archipelago.rkt"
 "keystore.rkt"
 "logger.rkt"
 "../bindings/libczmq/zmessage.rkt"
 "../bindings/libczmq/zstring.rkt"
 "../bindings/libczmq/zframe.rkt"
 "../bindings/libzyre/zyre.rkt"
 "../islet.rkt"
 "../murmur.rkt")

(provide
 (contract-out
  [egress/worker thunk/c]))

(define (egress/worker)
  (let ([archipelago (island/archipelago (this/island))]
        [underpeer   (island/underpeer   (this/island))]
        [nickname    (log/name/build (this/island/nickname) 'egress)]
        [keystore    (island/keystore    (this/island))])
    (let loop ([r (thread-receive)])
      (if (rustle? r)
          (let* ([destination (rustle/destination r)]
                 [n (archipelago/look archipelago destination)]
                 [petname (keystore/petname/look keystore destination)])
            (log/debug nickname "egress to" petname)
            (if n
              ; Compose a WHISPER message to the remote island.
              (let ([id (neighbor/zyre/id n)])
                (log/debug nickname "whispering to" petname)
                (if (zyre/island/whisper underpeer id (rustle/payload r) (rustle/target r))
                    (log/debug nickname   "zyre/whisper" petname)
                    (log/warning nickname "zyre/whisper failed" petname)))
                
              (log/warning nickname (format "~a not in archipelago" petname) #f)))

          (log/warning nickname "not rustle" r))
      (loop (thread-receive)))))



#|
(define (egress/worker)
  (let ([archipelago (island/archipelago (this/island))]
        [underpeer   (island/underpeer   (this/island))]
        [nickname    (this/island/nickname)]
        [keystore    (island/keystore    (this/island))])
    (let loop ([r (thread-receive)])
      (if (rustle? r)
          (let* ([destination (rustle/destination r)]
                 [n (archipelago/look archipelago destination)]
                 [petname (keystore/petname/look keystore destination)])
            (log/debug nickname "egress saw rustle to" petname)
            (log/debug nickname "egress to" (neighbor/pretty n))
            (displayln (rustle/target r))
            (displayln (rustle/payload r))
            (if n
              ; Compose a WHISPER message to the remote island.
              (let ([id (neighbor/zyre/id n)]
                    [m (zmessage/new)])
                (zmessage/push m (zframe/new (rustle/payload r)))
                (zmessage/push m (zframe/new (rustle/target r)))
                (log/debug nickname
                           (format "egress frames:~a size:~a" (zmessage/frames m) (zmessage/size m)) #t)
                (log/debug nickname "egress whispering to" (cons id petname))
                (if (zyre/whisper underpeer id m)
                    (log/debug nickname "zyre/whisper ok" #t)
                    (log/warning nickname "zyre/whisper failed" petname)))
;                (unless (zyre/whisper underpeer id m)
;                  (log/warning nickname (format "whisper to ~a failed" petname) #f)))
                
              (log/warning nickname (format "egress says ~a not in archipelago" petname) #f)))
          (log/warning nickname "egress saw something other than rustle" #f))
      (loop (thread-receive)))))
|#
