#lang racket/base

(require
 racket/contract/base
 "../curve.rkt"
 "../getters.rkt"
 "../persistent/environ.rkt"
 "../time.rkt"
 "../transport/access.rkt"
 "../uuid.rkt"
 "../z85.rkt"
 )

(provide
 CURL/ZPL/SIGNED/MIN
 CURL/ZPL/RX
 SIGNATURE/HEADER/LENGTH
 SIGNATURE/RX
 (contract-out
  ; Structures
  [struct
   curl/core
   ((id symbol?)             ; UUID
    (origin kp/base64/c)     ; island public key in base64 armour
    (path (listof symbol?))  ; path describing service
    ; id of access:send point or access:send point on receiving island
    (access (or/c symbol? access:send?))
    ;(access/id symbol?)      ; id of access:send point on receiving island
    (created time/utc?)      ; creation timestamp
    (metadata (or/c environ? #f)))] ; #f, environ/null, or a nonempty environ
  ;[curl/core/new (-> kp/z85/c (listof symbol?) access:send? environ? curl/core?)]
  [struct curl ((cache curl/core?) (verified kp/sign/c) (zpl/signed (or/c curl/zpl/signed? #f)))]
  [curl/zpl/signed? (-> any/c boolean?)]
  [curl/zpl/safe? (-> any/c boolean?)]
  ; Accessors 
  [curl/id (-> curl? symbol?)]
  [curl/origin (-> curl? kp/base64/c)]
  [curl/path (-> curl? (listof symbol?))]
  [curl/access (-> curl? (or/c symbol? access:send?))]
  [curl/created (-> curl? time/utc?)]
  [curl/metadata (-> curl? (or/c environ? #f))]
  ; Verified
  [curl/verified? (-> curl? boolean?)]
  ; Embargo
  [curl/embargo? (-> curl? boolean?)]
  [curl/core/signing? (-> curl/core? boolean?)])
 ; Accessors
 curl/cache      ; curl/core representation of curl/zpl/signed
 curl/verified   ; kp/sign of the given island of origin.
 curl/zpl/signed ; CURVE signed flat byte representation of curl/core
 curl/core/id
 curl/core/origin
 curl/core/path
 curl/core/access/id
 curl/core/access
 curl/core/created
 curl/core/metadata)

;(struct curl/core (id origin path access/id created metadata) #:transparent)
(struct curl/core (id origin path access created metadata) #:transparent)
(struct curl (cache verified zpl/signed) #:transparent)
;(struct/getters/define curl/core id origin path access/id created metadata)
(struct/getters/define curl/core id origin path access created metadata)
(struct/getters/define curl cache verified zpl/signed)

(define (curl/core/access/id c)
  (let ([x (curl/core/access c)])
    (if (symbol? x) x (access/id x))))
(define (curl/id u)     (curl/core/id (curl/cache u)))
(define (curl/origin u) (curl/core/origin (curl/cache u)))
(define (curl/path u)   (curl/core/path (curl/cache u)))
(define (curl/access u) (curl/core/access (curl/cache u)))
(define (curl/access/id u) (curl/core/access/id (curl/cache u)))
(define (curl/created u)   (curl/core/created (curl/cache u)))
(define (curl/metadata u)  (curl/core/metadata (curl/cache u)))
(define (curl/signature u) (subbytes (curl/zpl/signed u) 0 64))
(define (curl/verified? u) (and (curl/verified u) #t))

;(define (curl/core/new origin path access metadata)
;  (curl (uuid/symbol) origin path (access/id access) (time/now) metadata))

;; The body of a curl/zpl byte string representation (ignoring a CURVE/SIGNING prefix or a SIGNATURE header)
;; is always longer than CURL/ZPL/BODY/MIN
(define CURL/ZPL/BODY/MIN
  (+ (bytes-length #"CURL\n")
     (bytes-length #"    id = 19c53f7a-7274-4fcb-92e5-62da8ef44664\n")
     (bytes-length #"    origin = #\"jJcEVFLx5owwdClL_ydvMnALS_Qfx8n-bS2z9oAUmz8\"\n")
     (bytes-length #"    path = \n")
     (bytes-length #"    access/id = \n")
     (bytes-length #"    created = \"YYYY-MM-DDTHH:MM:SSZ\"\n")
     (bytes-length #"    metadata = \n")))
(define CURL/ZPL/SIGNED/MIN (+ CURVE/SIGNING/LENGTH CURL/ZPL/BODY/MIN)) ; CURVE signing is a 64-byte prefix.

(define
  CURL/ZPL/RX ; Both curl/zpl/signed and curl/zpl/safe will match this pattern.
  #rx"CURL\\\n    id = .+\\\n    origin = .+\\\n    path = .+\\\n    access/id = .+\\\n    created = .+\\\n    metadata = .+\\\n")
(define (curl/zpl/signed? x)
  (and
   (bytes? x)
   (< CURL/ZPL/SIGNED/MIN (bytes-length x))
   (not (regexp-match? SIGNATURE/RX x))
   (regexp-match? CURL/ZPL/RX x CURVE/SIGNING/LENGTH)))

(define SIGNATURE/Z85/RX #px"^SIGNATURE = \\\"[]0-9a-zA-Z.:+=^!/*?&<>()[{}@%$#-]{80}\\\"\\\n")
(define SIGNATURE/B64/RX #px"^SIGNATURE = #\\\"[0-9a-zA-Z_-]{86}\\\"\\\n")
(define SIGNATURE/RX SIGNATURE/B64/RX)

;; The length of a signature header includes the 86-character base64 armoured text, the 2 quotation
;; marks bracketing the 80-character string and the single #\newline character terminating the header.
(define SIGNATURE/HEADER/LENGTH (+ (bytes-length #"SIGNATURE = ") CURVE/SIGNING/B64/LENGTH 4)) ; #" + " + <newline> (2 + 1 + 1).
(define CURL/ZPL/SAFE/MIN (+ SIGNATURE/HEADER/LENGTH CURL/ZPL/BODY/MIN))
(define (curl/zpl/safe? x)
  (and
   (bytes? x)
   (< CURL/ZPL/SAFE/MIN (bytes-length x))
   (regexp-match? SIGNATURE/RX x)
   (regexp-match? CURL/ZPL/RX x SIGNATURE/HEADER/LENGTH)))

(define (curl/serialize u)
  (vector-immutable 'struct:curl (curl/origin u) (curl/zpl/signed u)))

;; If a CURL is embargoed then it can not be shipped off-island.
;; Returns #t if CURL u is embaragoed and #f otherwise.
(define (curl/embargo? u)
  (let ([a (curl/access u)])
    (and (access:send? a) (access:send/embargo? a))))

;; A signing is required if the core contains only an access:send/id
;; otherwise the core must contain an access:send? and
;; signing is required when the access:send point is NOT embargoed.
(define (curl/core/signing? core)
  (let ([a (curl/core/access core)])
    (or (symbol? a) (not (access:send/embargo? a)))))
        