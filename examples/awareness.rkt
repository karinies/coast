#lang racket/base

(require
 [only-in typed/racket/base assert]
 "../bindings/libczmq/zconfig.rkt"
 [only-in "../Island/base.rkt" island/archipelago]
 [only-in "../Island/archipelago.rkt" archipelago/neighbors]
 [only-in "../island.rkt" island/new island/start]
 [only-in "../z85.rkt" zmq/z85/decode]
 [only-in "../certificate.rkt" zconfig/sign zconfig/sign/verify? zconfig/curl/make zconfig/curl/ok?]
 )

(provide
 alice/start
 curl/test curl+/test curl/make/test
 duplicate/test
 wrap/test
 )

(define CERTIFICATE_STORE "./certificates/secret")
(define CERTIFICATE/ALICE (format "~a/~a_secret" CERTIFICATE_STORE 'alice))
(define CERTIFICATE/BOB   (format "~a/~a_secret" CERTIFICATE_STORE 'bob))

(define (alice/start)
  (let ((alice (island/new (zconfig/load CERTIFICATE/ALICE)))
        (bob   (island/new (zconfig/load CERTIFICATE/BOB))))
    ; Bob is going to advertise a simple CURL.
    (island/start alice)
    (island/start bob)
    (sleep 1.0)
    ; Make sure that alice saw bob enter the network and that bob saw alice enter the network.
    (display (format "alice: archipelago:~a\n" (archipelago/neighbors (island/archipelago alice))))
    (display (format "bob: archipelago:~a\n"   (archipelago/neighbors (island/archipelago bob))))
    (read)
    (displayln "Goodbye!")))


(define (curl/test)
  (let* ([u (zconfig/load "./curl.zpl")]
         [alice/s (zconfig/load CERTIFICATE/ALICE)]
         [ks/sign (zconfig/resolve alice/s "curve/secret-sign-key" #f)]
         [kp/sign (zconfig/resolve alice/s "curve/public-sign-key" #f)]) ; Verify key in z85 format.
    (assert ks/sign)
    (assert kp/sign)
    (zconfig/sign u (zmq/z85/decode kp/sign) (zmq/z85/decode ks/sign))
    (zconfig/save u (current-output-port))
    (display (zconfig/sign/verify? u)) (newline)
    ))

(define (curl+/test)
  (let* ([u (zconfig/load "./curl+.zpl")])
    (assert u)
    ;(zconfig/save u (current-output-port))
    (zconfig/file/save u "-")
    (zconfig/curl/ok? u)))


(define (curl/make/test)
    (let* ([alice/s (zconfig/load CERTIFICATE/ALICE)]
           [kp      (zconfig/resolve alice/s "/curve/public-key" #f)]
           [ks/sign (zconfig/resolve alice/s "/curve/secret-sign-key" #f)]
           [kp/sign (zconfig/resolve alice/s "/curve/public-sign-key" #f)] ; Verify key in z85 format.
           [u
            (zconfig/curl/make
             kp kp/sign ks/sign
             '(foo bar baz "seventy-three" 99)
             "soda" "thirsty"
             'sweetness 77)])
      (zconfig/save u (current-output-port))))

(define (duplicate/test)
  (let* ([z (zconfig/load "./curl.zpl")]
         [d (zconfig/duplicate z #f)])
    (assert (not (equal? d z)))
    (zconfig/save z (current-output-port))
    (newline)
    (zconfig/save d (current-output-port))))
    
(define (wrap/test)
  (let* ([z (zconfig/load "./curl.zpl")]
         [top (zconfig/new "top" #f)]
         [d (zconfig/duplicate z top)])
    ;(zconfig/value/set subject "")
    (zconfig/save z (current-output-port))
    (newline)
    (zconfig/name/set (zconfig/child top) "SUBJECT")
    (zconfig/save top (current-output-port))))    

;; racket -l racket/base -t awareness.rkt -e '(alice/start)' &