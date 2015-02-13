#lang racket/base

;; These parameters are defined to break cycles between serializer.rkt and other modules and to reduce
;; the length and complexity of calling sequences for functions that depend on various island
;; cryptographic keys.
;; This ambient authority is exploited only by trusted code and none of these parameters are
;; directly available to visiting mobile code.

(provide
 this/accessors
 this/curve
 ;this/egress
 this/island
 this/islet
 this/keystore
 this/trust
; this/kp
; this/kp/sign
; this/ks/sign
)



(define this/island (make-parameter #f)) ; The island on which this islet is executing.
(define this/islet (make-parameter #f)) ; The islet animating the caller.
(define this/trust  (make-parameter #f)) ; The trust rating of this islet.
(define this/roles (make-parameter #f))  ; The roles that this islet may play.
(define this/curve (make-parameter #f)) ; Complete set of CURVE crypto keys.
(define this/keystore (make-parameter #f)) ; Box containing a map from islands kp to kp/sign.
(define this/accessors (make-parameter #f)) ; Box containing the island-wide set of exported accessors.

;(define this/kp  (make-parameter #f)) ; Bytes public key (serves as island identity).
;(define this/ks/sign (make-parameter #f)) ; Bytes secret signing key.
;(define this/kp/sign (make-parameter #f)) ; Bytes public signing key.

;(define (this/egress) (island/egress (this/island)))

