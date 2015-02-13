#lang racket/base

;; Defines a convenient container for an island's CURVE crypto keys.

(require
 racket/contract/base
 "bindings/libczmq/libczmq.rkt"
 "bindings/libczmq/zconfig.rkt"
 "bindings/libsodium/crypto.rkt"
 "base64.rkt"
 "z85.rkt"
 "getters.rkt")

(provide
 ; Contracts for island crypto keys.
 kp/c
 ks/c
 kp/sign/c
 ks/sign/c
 kp/base64/c
 ks/base64/c
 kp/sign/base64/c
 ks/base64/c
 kp/z85/c
 ks/z85/c
 kp/sign/z85/c
 ks/sign/z85/c

 
 ; Lengths of crypro keys as strings in z85 armour.
 KP/Z85/LENGTH
 KS/Z85/LENGTH
 KP/SIGN/Z85/LENGTH
 KS/SIGN/Z85/LENGTH
 ; Lengths of cyrpto keys as base64 URL-encoded byte strings.
 KP/BASE64/LENGTH
 KS/BASE64/LENGTH
 KP/SIGN/BASE64/LENGTH
 KS/SIGN/BASE64/LENGTH
 ; Lengths of keys as byte strings.
 KP/LENGTH
 KS/LENGTH
 KP/SIGN/LENGTH
 KS/SIGN/LENGTH
 ; CURVE signature in bytes.
 CURVE/SIGNING/LENGTH
 ; z85 armoured CURVE signature in bytes.
 CURVE/SIGNING/Z85/LENGTH
 ; base64/url armoured CURVE signature in bytes.
 CURVE/SIGNING/B64/LENGTH
 (contract-out
  [struct
   curve 
   ((kp kp/c) (ks ks/c) (kp/sign kp/sign/c) (ks/sign ks/sign/c)
    ;(kp/z85 kp/z85/c) (ks/z85 ks/z85/c) (kp/sign/z85 kp/sign/z85/c) (ks/sign/z85 ks/sign/z85/c)
    (kp/base64 kp/base64/c)
    (ks/base64 ks/base64/c)
    (kp/sign/base64 kp/sign/base64/c)
    (ks/sign/base64 ks/sign/base64/c))]
  
  [struct
   curve/public
   ((kp kp/c) (kp/sign kp/sign/c) (kp/base64 kp/base64/c) (kp/sign/base64 kp/sign/base64/c))]
   
  [certificate-to-curve (-> zconfig? curve?)]
  [certificate-to-curve/public (-> zconfig? curve/public?)]
  [curve/generate (-> curve?)]
  ; Predicates for base64 armoured CURVE keys.
  [kp/base64? (-> any/c boolean?)]
  [ks/base64? (-> any/c boolean?)]
  [kp/sign/base64? (-> any/c boolean?)]
  [ks/sign/base64? (-> any/c boolean?)]
  ; Predicates for z85 armoured CURVE keys.
  [kp/z85/ok? (-> (or/c bytes? string?) boolean?)]
  [ks/z85/ok? (-> (or/c bytes? string?) boolean?)]
  [kp/sign/z85/ok? (-> (or/c bytes? string?) boolean?)]
  [ks/sign/z85/ok? (-> (or/c bytes? string?) boolean?)]
  [signature/z85/ok? (-> (or/c bytes? string?) boolean?)]
  
  ; Encoding predicates and translators.
  [z85? (-> (and/c string? (lambda (x) (zero? (remainder (string-length x) 5)))) boolean?)]
  [base64? (-> bytes? boolean?)]
  [z85-to-base64 (-> z85? bytes?)]
  ; For testing.
  [CURVE/TEST   curve?]  ; Generated anew every time.
  [CURVE/STATIC any/c]) ; Fixed and static.
 ; Accessors.
 curve/kp
 curve/ks
 curve/kp/sign
 curve/ks/sign
 curve/kp/base64
 curve/ks/base64
 curve/kp/sign/base64
 curve/ks/sign/base64
; curve/kp/z85
; curve/ks/z85
; curve/kp/sign/z85
; curve/ks/sign/z85
 )

;; Every 3 bytes of x expands into 4 ASCII characters.
(define (base64/expansion n)
  (let-values ([(q r) (quotient/remainder n 3)])
    (if (zero? r)
        (* 4 q)
        (+ (* 4 q) (if (= r 1) 2 3)))))

;; Lengths of keys as bytes.
(define KP/LENGTH CRYPTO/BOX/KP/SIZE)
(define KS/LENGTH CRYPTO/BOX/KS/SIZE)
(define KP/SIGN/LENGTH CRYPTO/SIGN/KP/SIZE)
(define KS/SIGN/LENGTH CRYPTO/SIGN/KS/SIZE)
;; Lengths of keys as strings in z85 ASCII encoding.
(define KP/Z85/LENGTH      (z85/expansion KP/LENGTH))
(define KS/Z85/LENGTH      (z85/expansion KS/LENGTH))
(define KP/SIGN/Z85/LENGTH (z85/expansion KP/SIGN/LENGTH))
(define KS/SIGN/Z85/LENGTH (z85/expansion KS/SIGN/LENGTH))
;; Length of keys in base64 URL armour.
(define KP/BASE64/LENGTH (base64/expansion KP/LENGTH))
(define KS/BASE64/LENGTH (base64/expansion KS/LENGTH))
(define KP/SIGN/BASE64/LENGTH (base64/expansion KP/SIGN/LENGTH))
(define KS/SIGN/BASE64/LENGTH (base64/expansion KS/SIGN/LENGTH))

;; Length of CURVE signature.
(define CURVE/SIGNING/LENGTH 64)
(define CURVE/SIGNING/Z85/LENGTH 80)
(define CURVE/SIGNING/B64/LENGTH (base64/expansion CURVE/SIGNING/LENGTH))
;; Container for an island's crypto keys.
(struct
 curve
 (kp ks kp/sign ks/sign ; Binary
  ;kp/z85 ks/z85 kp/sign/z85 ks/sign/z85  ; Z85 armour.
  kp/base64 ks/base64 kp/sign/base64 ks/sign/base64) ; Base64 URL armour.
 #:transparent)
(struct/getters/define curve kp ks kp/sign ks/sign kp/base64 ks/base64 kp/sign/base64 ks/sign/base64)

(struct curve/public (kp kp/sign kp/base64 kp/sign/base64) #:transparent)
(struct/getters/define curve/public kp kp/sign kp/base64 kp/sign/base64)

(define (z85? x)
  (and
   (zero? (remainder (string-length x) 5))
   (regexp-match-exact? #rx"[]0-9a-zA-Z.:+=^!/*?&<>()[{}@%$#-]+" x)))
(define (base64? x) (regexp-match-exact? #rx"[-_a-zA-Z0-9]+" x))
;; Convert a z85 string to a base64/url encoded byte string.
(define (z85-to-base64 x) (base64/url/encode (zmq/z85/decode x)))
    
;; Contracts for CURVE keys as bytes.
(define kp/c
  (flat-named-contract 'kp (and/c bytes? (lambda (b) (= (bytes-length b) KP/LENGTH)))))
(define ks/c
    (flat-named-contract 'ks (and/c bytes? (lambda (b) (= (bytes-length b) KS/LENGTH)))))
(define kp/sign/c
  (flat-named-contract 'kp/sign (and/c bytes? (lambda (b) (= (bytes-length b) KP/SIGN/LENGTH)))))
(define ks/sign/c
    (flat-named-contract 'ks/sign (and/c bytes? (lambda (b) (= (bytes-length b) KS/SIGN/LENGTH)))))

;; Starting with either a secret or public certificate c extract the public keys.
(define (certificate-to-curve/public c)
  (let ([kp/z85      (zconfig/resolve c "/curve/public-key" #f)]
        [kp/sign/z85 (zconfig/resolve c "/curve/public-sign-key" #f)])
    (and
     kp/z85 kp/sign/z85
     (let ([kp (zmq/z85/decode kp/z85)]
           [kp/sign (zmq/z85/decode kp/sign/z85)])
       (curve/public kp kp/sign (base64/url/encode kp) (base64/url/encode kp/sign))))))
       
         

;; Exract the CURVE crypto keys from a secret CZMQ key certificate and return
;; a curve instance containing both the binary and base64 forms of the keys.
(define (certificate-to-curve secret)
  (let ([kp/z85 (zconfig/resolve secret "/curve/public-key" #f)]
        [ks/z85 (zconfig/resolve secret "/curve/secret-key" #f)]
        [kp/sign/z85 (zconfig/resolve secret "/curve/public-sign-key" #f)]
        [ks/sign/z85 (zconfig/resolve secret "curve/secret-sign-key"  #f)])
    (and
     kp/z85 ks/z85 kp/sign/z85 ks/sign/z85
     (let ([kp (zmq/z85/decode kp/z85)]
           [ks (zmq/z85/decode ks/z85)]
           [kp/sign (zmq/z85/decode kp/sign/z85)]
           [ks/sign(zmq/z85/decode ks/sign/z85)])
     (curve
      kp ks kp/sign ks/sign
      (base64/url/encode kp) (base64/url/encode ks)
      (base64/url/encode kp/sign) (base64/url/encode ks/sign))))))

(define (curve/generate)
  (let-values ([(kp ks) (crypto/box/keys)]
               [(kp/sign ks/sign) (crypto/sign/keys)])
    (curve
     kp ks kp/sign ks/sign
     (base64/url/encode kp) (base64/url/encode ks)
     (base64/url/encode kp/sign) (base64/url/encode ks/sign))))

(define Z85/40/RX #px"^[]0-9a-zA-Z.:+=^!/*?&<>()[{}@%$#-]{40}$")
(define Z85/80/RX #px"^[]0-9a-zA-Z.:+=^!/*?&<>()[{}@%$#-]{80}$")
(define SIGNATURE/Z85/RX Z85/80/RX)
(define KP/Z85/RX        Z85/40/RX)
(define KS/Z85/RX        Z85/40/RX)
(define KP/SIGN/Z85/RX   Z85/40/RX)
(define KS/SIGN/Z85/RX   Z85/80/RX)
;; Predicates for CURVE keys as z85 armoured keys.
(define (signature/z85/ok? s) (regexp-match? SIGNATURE/Z85/RX s))
(define (kp/z85/ok? s) (regexp-match? KP/Z85/RX s))
(define (ks/z85/ok? s) (regexp-match? KS/Z85/RX s))
(define (kp/sign/z85/ok? s) (regexp-match? KP/SIGN/Z85/RX s))
(define (ks/sign/z85/ok? s) (regexp-match? KS/SIGN/Z85/RX s))
;; Contracts for CURVE keys as z85 armoured strings.
(define kp/z85/c (flat-named-contract 'kp/z85 (and/c string? kp/z85/ok?)))
(define ks/z85/c (flat-named-contract 'ks/z85 (and/c string? ks/z85/ok?)))
(define kp/sign/z85/c (flat-named-contract 'kp/sign/z85 (and/c string? kp/sign/z85/ok?)))
(define ks/sign/z85/c (flat-named-contract 'ks/sign/z85 (and/c string? ks/sign/z85/ok?)))

(define BASE64/RX #rx"[-A-Za-z0-9_]+")
(define (signature/base64/ok? s)
  (and (= (bytes-length s) CURVE/SIGNING/B64/LENGTH) (regexp-match-exact? BASE64/RX s)))

(define-syntax-rule (key/base64/test n)
  (lambda (x) (and (bytes? x) (= (bytes-length x) n) (regexp-match-exact? BASE64/RX x))))

(define kp/base64? (key/base64/test KP/BASE64/LENGTH))
(define kp/base64/c (flat-named-contract 'kp/base64 kp/base64?))
(define ks/base64? (key/base64/test KS/BASE64/LENGTH))
(define ks/base64/c (flat-named-contract 'ks/base64 ks/base64?))
(define kp/sign/base64? (key/base64/test KP/SIGN/BASE64/LENGTH))
(define kp/sign/base64/c (flat-named-contract 'kp/sign/base64 kp/sign/base64?))
(define ks/sign/base64? (key/base64/test KS/SIGN/BASE64/LENGTH))
(define ks/sign/base64/c (flat-named-contract 'ks/sign/base64 ks/sign/base64?))

;; For testing.
(define CURVE/TEST (curve/generate))
(define CURVE/STATIC
(curve
 ; kp
 #"\214\227\4TR\361\346\2140t)K\377'o2p\vK\364\37\307\311\376m-\263\366\200\24\233?"
 ; ks
 #"Y\225\272\2002\307 t\252\351\204\330\353\306\177v\201\311\246\350\246\305\263L\246\237\26\366\340U>d"
 ; kp/sign
 #"\2105\350\305\325\337\ts\341\26\327\236&gq\360a\256\e`\277\306\203\331\251h\24tb\261\256$"
 ; ks/sign
#"\272\37\307\231\262\331\32\0\34\\\220\305j\247\202\21\202\354E5^\243h\217;\364\365\201s\243\244*\2105\350\305\325\337\ts\341\26\327\236&gq\360a\256\e`\277\306\203\331\251h\24tb\261\256$"

; ; kp/z85
; "Jf:VDqT@[dfMX:p%0I.uA0]thaihI5z7S9/Fe1Fq"
; ; ks/z85
; "s^uNGgrgIVS{bh?(=aA$FYT!MRP2(nRK[T$&8I%K"
; ; kp/sign/z85
; "H=azn!.ZWO&tg4pctdt/vxHLSZS7]VSB##/vZi6Y"
; ; ks/sign/z85
; "X/X$XVE{b@99!PAynVT}G6U]-uzzg{jm@h%BebJXH=azn!.ZWO&tg4pctdt/vxHLSZS7]VSB##/vZi6Y"

 ; kp/b64
 #"jJcEVFLx5owwdClL_ydvMnALS_Qfx8n-bS2z9oAUmz8"
 ; ks/b64
 #"WZW6gDLHIHSq6YTY68Z_doHJpuimxbNMpp8W9uBVPmQ"
 ; kp/sign/b64
 #"iDXoxdXfCXPhFteeJmdx8GGuG2C_xoPZqWgUdGKxriQ"
 ; ks/sign/b64
 #"uh_HmbLZGgAcXJDFaqeCEYLsRTVeo2iPO_T1gXOjpCqINejF1d8Jc-EW154mZ3HwYa4bYL_Gg9mpaBR0YrGuJA"))