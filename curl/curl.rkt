#lang racket/base

(require
  racket/contract/base
  racket/bytes
  racket/port
  "base.rkt"
  "../bindings/libsodium/crypto.rkt"
  "../base64.rkt"
  "../curve.rkt"
  "../persistent/environ.rkt"
  "../serialize.rkt"
  "../time.rkt"
  "../transport/access.rkt"
  "../uuid.rkt"
  "../zpl.rkt"
  "../accounting/como-types.rkt"
  (only-in "../Island/island-como.rkt" island/monitoring/log))

;; Notation: zpl (in lower case) refers to a byte string that is a legal zpl text
;; (in the sense of the zpl parser defined in zpl.rkt). ZPL (in upper case) refers
;; to an in-memory instance of a zpl? structure as defined in zpl.rkt
;; A curl/zpl/* is a byte string representation of a ZPL encoding of a curl/core?
;; curl/zpl/signed contains a 64-byte prefix that is a CURVE signing of the remainder
;; of the curl/zpl/* byte string.
;; Aside from the 64-byte prefix a curl/zpl/signed is a legitimate textual representation
;; of a ZPL tree.
;; curl/zpl/safe, on the other hand, includes the CURVE signing in such a way that it
;; is, in all respects, a legitimate textual representation of a ZPL tree.
;; curl/ZPL is the in-memory ZPL tree equivalent of a curl/zpl/safe byte string.
;; Transformations are defined among all three forms:
;;    curl/zpl/signed-to-curl/zpl/safe
;;    curl/zpl/signed-to-curl/ZPL
;;    curl/zpl/safe-to-curl/zpl/signed
;;    curl/zpl/safe-to-curl/ZPL
;;    curl/ZPL-to-curl/zpl/signed
;;    curl/ZPL-to-curl/zpl/safe
;;
;; A curl/core? structure contains all of the critical information required by a CURL.
;; Comparable transformations are available for curl/core:
;;    curl/zpl/signed-to-curl/core
;;    curl/zpl/safe-to-curl/core
;;    curl/ZPL-to-curl/core
;;    curl/core-to-curl/zpl/signed
;;    curl/core-to-curl/zpl/safe
;;    curl/core-to-curl/ZPL
;;
;; Each of these distinct forms, curl/zpl/signed, curl/zpl/safe, curl/ZPL, and curl
;; can read from or written to ports.
;; In addition, a curl can be written to, or read from, a file. 

(provide
 define/curl/inline
 curl?
 (contract-out
  ; Type predicates.
  [curl/ZPL?        (-> any/c boolean?)]
  ; Constructor
  [curl/core/new (-> kp/base64/c (listof symbol?) (or/c access:send? symbol?) (or/c environ? #f) curl/core?)]
  [curl/core/new* (-> curve? (listof symbol?) (or/c access:send? symbol?) (or/c environ? #f) curl/core?)]
  [curl/new (-> curl/core? kp/sign/c ks/sign/c curl?)]
  [curl/new* (-> curl/core? curve? curl?)]
  ; Transformations between curl/core and curl/zpl/signed
  [curl/core-to-curl/zpl/signed     (-> curl/core?        ks/sign/c          curl/zpl/signed?)]
  [curl/zpl/signed-to-curl/core     (-> curl/zpl/signed? (or/c kp/sign/c #f) curl/core?)]
  [curl/core-to-curl/zpl/safe       (-> curl/core? ks/sign/c curl/zpl/safe?)]
  ; Transformations between curl/zpl/signed and curl/zpl/safe
  [curl/zpl/signed-to-curl/zpl/safe (-> curl/zpl/signed? curl/zpl/safe?)]
  [curl/zpl/safe-to-curl/zpl/signed (-> curl/zpl/safe? curl/zpl/signed?)]
  ; Transformation from curl/core to curl/ZPL
  [curl/core-to-curl/ZPL (-> curl/core? ks/sign/c curl/ZPL?)]
  ; Transformation from  a curl/ZPL instance to a curl/zpl/safe byte string.
  [curl/ZPL-to-curl/zpl/safe   (-> curl/ZPL? curl/zpl/safe?)]
  [curl/ZPL-to-curl/zpl/signed (-> curl/ZPL? curl/zpl/signed?)]
  ; Transformations from curl/zpl/* to curl/ZPL.
  [curl/zpl/safe-to-curl/ZPL   (-> curl/zpl/safe?   curl/ZPL?)]
  [curl/zpl/signed-to-curl/ZPL (-> curl/zpl/signed? curl/ZPL?)]
  ; Reading and writing curl/zpl/* using ports.
  [curl/zpl/signed/load (-> input-port? curl/zpl/signed?)]
  [curl/zpl/safe/load (-> input-port? curl/zpl/safe?)]
  [curl/zpl/signed/save (-> curl/zpl/signed? output-port? void?)]
  [curl/zpl/safe/save (-> curl/zpl/safe? output-port? void?)]
  ;; Reading and writing CURLs using ports.
  ;[curl/safe/load (-> input-port? keystore/c (or/c curl? #f))]
  [curl/zpl/load (-> input-port? bytes?)]
  [curl/load (-> input-port? kp/sign/c (or/c curl? #f))]
  [curl/save (-> curl? output-port? void?)]
  ; Reading and writing CURLS from/to files.
  [curl/file/load (-> string? kp/sign/c (or/c curl? #f))]
  [curl/file/save (-> string? curl? void?)]
  ; Managing CURLs defined inline.
  [curl-as-bytes (-> curve? (listof symbol?) (or/c access:send? symbol?) (or/c environ? #f) bytes?)]
  [curl/zpl/safe-to-curl (-> bytes? keystore/c (or/c curl? #f))]))

;; curl/core constructor
(define (curl/core/new kp/base64 path access metadata) ; (struct curl/core (id origin path access/id created metadata) #:transparent)
  (let ([curl (curl/core (uuid/symbol) kp/base64 path access (time/now) metadata)]
        [place (if (symbol? access) 'INTER 'INTRA)])
    (island/monitoring/log #:type COMO/CURL/NEW
                           #:place #f)
    curl))

(define (curl/core/new* keys path access metadata)
  (let ([curl (curl/core (uuid/symbol) (curve/kp/base64 keys) path access (time/now) metadata)]
        [place (if (symbol? access) 'INTER 'INTRA)])
    (island/monitoring/log #:type COMO/CURL/NEW
                           #:place place)
    curl))

;(define (curl/core/new* kp path access/id metadata)
;  (curl/core
;   (uuid/symbol)

;; CURL constructors.
;; There is an optimization to reduce the cost of generating a CURL.
;; If the curl/core is intended for intra-island use only then no signed byte
;; representation of the CURL is required as the CURL will never leave its island of origin.
;; In this case the zpl/signing field of the CURL is #f.
(define (curl/new core kp/sign ks/sign)
  (curl
   core
   kp/sign
   (and (curl/core/signing? core) (curl/core-to-curl/zpl/signed core ks/sign)))) ; Optimization.

(define (curl/new* core keys)
  (curl
   core
   (curve/kp/sign keys)
   (and (curl/core/signing? core) (curl/core-to-curl/zpl/signed core (curve/ks/sign keys))))) ; Optimization.

;; Write a curl u to output-port out.
(define (curl/save u out) (write-bytes (curl/zpl/signed u)))
;; Save a curl u to the file named by path.
(define (curl/file/save path u)
  (call-with-output-file path
    (lambda (out) (curl/save u out))))

(define (curl-as-bytes keys path access metadata)
  (let ([core (curl/core/new (curve/kp/base64 keys) path access metadata)])
    (curl/core-to-curl/zpl/safe core (curve/ks/sign keys))))

;; Represent a CURL core as a byte string representation, a signed flat ZPL tree.
;; The byte string format is:
;; <signature>CURL
;;     id = <curl id>
;;     origin = <kp/base64 of island>
;;     path = (<symbol> ...)
;;     access/id = <id of access:send point>
;;     created = <ISO8601 timestamp>
;;     metadata = <motile/serialize of curl metadata written as byte string>
;; where <signature> is a 64-byte CURVE signature over the textual ZPL tree sketched above.
(define (curl/core-to-curl/zpl/signed c ks/sign)
  ; Hand construct a curl/zpl/signed as a byte string.
  (let ([p (open-output-bytes)])
    (display "CURL\n" p)
    (zpl/item p "id"        (curl/core/id c))
    (zpl/item p "origin"    (curl/core/origin c))
    (zpl/item p "path"      (curl/core/path c))
    (zpl/item p "access/id" (curl/core/access/id c))
    (zpl/item p "created"   (date-to-ISO8601 (time-to-date (curl/core/created c))))
    (zpl/item p "metadata"  (and (curl/core/metadata c)(motile/serialize (curl/core/metadata c))))
    (begin0
      (crypto/sign (get-output-bytes p #t) ks/sign)
      (close-output-port p))))
(define (zpl/item p n v) (display (format "    ~a = ~s\n" n v) p))

;; Represent a curl/core? as an in-memory ZPL tree that looks like:
;;
;; SIGNATURE = <base64 86-character string of 64-byte CURVE signature>
;; CURL
;;     id = <curl id>
;;     origin = <kp/base64 of island>
;;     path = (<symbol> ...)
;;     access/id = <id of access:send point>
;;     created = <ISO8601 timestamp>
;;     metadata = <motile/serialize of curl metadata written as byte string>
;;
;; Returns the ZPL tree equivalent of the representation of curl/core c
;; as a flat curl/zpl/safe byte string where ks/sign is the secret singing
;; key of the island of origin of curl/core c.
(define (curl/core-to-curl/ZPL c ks/sign)
  (let* ([z (zpl/root/new)]
         [CURL (zpl/new 'CURL (void) #f)])
    ; Transcribe the contents of curl/core c to the ZPL tree rooted at z.
    (zpl/new 'id        (curl/core/id c)        CURL)
    (zpl/new 'origin    (curl/core/origin c)    CURL)
    (zpl/new 'path      (curl/core/path c)      CURL)
    (zpl/new 'access/id (curl/core/access/id c) CURL)
    (zpl/new 'created   (date-to-ISO8601 (time-to-date (curl/core/created c))) CURL)
    (zpl/new 'metadata  (motile/serialize (curl/core/metadata c)) CURL)
    ; Now we have to generate a signing for ZPL tree z.
    (let ([p (open-output-bytes)])
      (zpl/save z p) ; Write z as a byte string.
      (let* ([signature (subbytes (crypto/sign (get-output-bytes p #t) ks/sign) 0 CURVE/SIGNING/LENGTH)]
             [signature/base64 (base64/url/encode signature)]
             [final (zpl/root/new)])
        (close-output-port p)
        (zpl/new 'SIGNATURE signature/base64 final)
        (zpl/attach final CURL)
        final))))

;; Convert a curl/zpl/safe byte string z to an equivalent ZPL tree.
(define (curl/zpl/safe-to-curl/ZPL z) (call-with-input-bytes z zpl/load))
;; Convert a curl/zpl/signed byte string z to an equivalent ZPL/tree.
(define (curl/zpl/signed-to-curl/ZPL z/signed)
  (call-with-input-bytes
   z/signed
   (lambda (in)
     (let* ([signing (read-bytes CURVE/SIGNING/LENGTH in)]
            [z (zpl/load in)])
       (let ([root (zpl/root/new)])
         (zpl/new 'SIGNATURE (base64/url/encode signing) root)
         (zpl/attach root (zpl/locate z 'CURL))
         root)))))
;; Convert a curl/ZPL tree z to an equivalent curl/zpl/safe byte string.
(define (curl/ZPL-to-curl/zpl/safe z) (call-with-output-bytes z (lambda (out) (zpl/save z out))))
;; Convert a curl/ZPL tree z to an equivalent curl/zpl/signed byte string.
(define (curl/ZPL-to-curl/zpl/signed z)
  (let ([signing (base64/url/decode (zpl/value (zpl/locate z 'SIGNATURE)))]
        [CURL (zpl/locate z 'CURL)]
        [root (zpl/root/new)])
    (zpl/attach root CURL)
    (call-with-output-bytes
     (lambda (out)
       (write-bytes signing out)
       (zpl/save root out)))))

;; A structural test for a CURL structured as a ZPL instance.
;; Returns #t if the in-memory structure is structurally correct and #f otherwise.
(define (curl/ZPL? z) 
  (let ([x (zpl/children/list z)])
    (cond
      [(>= (length x) 2)
       (let ([SIGNATURE (car x)] [CURL (cadr x)]) ; Children of z in insertion order.
         (and
          (eq? 'SIGNATURE (zpl/name SIGNATURE))
          (null? (zpl/children/list SIGNATURE))  ; The SIGNATURE header has no children.
          (regexp-match? SIGNATURE/RX (zpl/value SIGNATURE)) ; The signature is 80 characers long and z85-encoded.
          ;(= (string-length (zpl/value SIGNATURE)) CURVE/SIGNING/Z85/LENGTH)
          (eq? 'CURL (zpl/name CURL))
          ; The CURL subsection has at least these children.
          (zpl/locate CURL 'id)
          (zpl/locate CURL 'origin)
          (zpl/locate CURL 'path)
          (zpl/locate CURL 'access/id)
          (zpl/locate CURL 'created)
          (zpl/locate CURL 'metadata)
          #t))]
      [else #f])))

;; Loads a curl/zpl/* byte string from input port in.
(define (curl/zpl/load in)
  (let loop ([line (read-bytes-line in)] [lines null])
    (if (not (eof-object? line))
        (loop (read-bytes-line in) (cons line lines))
        (if (null? lines)
            ; input port was empty.
            #f
            ; Since real-bytes-line discards the \newline terminating each line
            ; we have to terminate each line (including the very last line) with a \newline.
            (bytes-join (reverse (cons #"" lines)) #"\n")))))
;; Read a curl/zpl/signed byte string from input port in.
(define (curl/zpl/signed/load in) (curl/zpl/load in))
;; Read a curl/zpl/safe byte string from input port in.
(define (curl/zpl/safe/load in) (curl/zpl/load in))
;; Write a curl/zpl/signed byte string z to output port out.
(define (curl/zpl/signed/save z out) (write-bytes z out))
;; Write a curl/zpl/safe byte string z to output port out.
(define (curl/zpl/safe/save z out) (write-bytes z out))
;; Read either a curl/zpl/signed or curl/zpl/safe byte string from input port in
;; and return the corresponding curl/core? instance.
;; kp/sign is the appropriate public signing key (if known) or #f.
;; If kp/sign is #f then the signing is left unvalidated otherwise the core/zpl/*
;; is validated against the public signing key.
;; If the byte string is malformed or if the signing can not be validated return #f.
(define (curl/load in kp/sign)
  (let ([z (curl/zpl/load in)])
    (cond
      [(curl/zpl/signed? z)
       (let ([core (curl/zpl/signed-to-curl/core z kp/sign)])
         (and core (curl core kp/sign z)))]
      [(curl/zpl/safe? z)
       (let* ([z/signed (curl/zpl/safe-to-curl/zpl/signed z)]
              [core (curl/zpl/signed-to-curl/core z/signed kp/sign)])
         (and core (curl core kp/sign z/signed)))]
      [else #f])))

;; Read a CURL in zpl "safe" (with SIGNATURE = ...) format and return
;; a curl? instance.
;; Returns #f if the read failed, if the CURL is ill-formed, if the reading
;; island does not have a certificate for the island originating the CURL,
;; or if the reading island can not verify the CURL.
#|
(define (curl/safe/load in keystore)
  (let ([z (curl/zpl/load in)])
    (display (format "curl/safe/load 1:~a\n" z))
    (if (curl/zpl/safe? z)
        (let ([Z (curl/zpl/safe-to-curl/ZPL z)])
          (let* ([kp/base64 (zpl/get Z '(CURL origin) #f)] ; CURL origin.
                 [p (and kp/base64 (keystore/look keystore kp/base64))] ; Obtain the keystore entry.
                 [kp/sign (and p (public/kp/sign p))]) ; kp/sign key of island that created the CURL.
            (display (format "curl/safe/load 1b:~a\n") kp/base64)
            (if kp/sign
                (let* ([z/signed (curl/zpl/safe-to-curl/zpl/signed z)]
                       [core (curl/zpl/signed-to-curl/core z/signed kp/sign)])
                  (display (format "curl/safe/load 2:~a\n") z/signed)
                  (display (format"curl/safe/load 3:~a\n") core)
                  (and core (curl core kp/sign z/signed)))
                #f))) ; Unable to verify CURL.
        #f))) ; CURL ill-formed.
|#

(define (curl/zpl/safe-to-curl z keystore)
  (if (curl/zpl/safe? z)
      (let* ([Z (curl/zpl/safe-to-curl/ZPL z)]
             [kp/base64 (zpl/get Z '(CURL origin) #f)]              ; CURL origin.
             [p (and kp/base64 (keystore/look keystore kp/base64))] ; Keystore entry for origin
             [kp/sign (and p (public/kp/sign p))]                   ; kp/sign of island of origin.
             [z/signed   (curl/zpl/safe-to-curl/zpl/signed z)])     ; CURVE signing of zpl form of CURL
        ;(display (format "curl/safe/load 1b:~a\n" kp/base64))
        (if kp/sign
            (let* ([z/unsigned (crypto/sign/verify z/signed kp/sign)]
                   [core (and z/unsigned (ZPL-to-curl/core Z))])
              ;(display (format "curl/safe/load 3:~a\n"  core))
              (and core (curl core kp/sign z/signed)))
            (let ([core (ZPL-to-curl/core Z)])
              (and core (curl core kp/sign z/signed)))))
      #f)) ; CURL ill-formed.

(define (ZPL-to-curl/core tree)
  (let ([CURL (zpl/locate tree 'CURL)])
    (and
     CURL
     (let ([id        (zpl/locate* CURL 'id)]
           [origin    (zpl/locate* CURL 'origin)]
           [path      (zpl/locate* CURL 'path)]
           [access/id (zpl/locate* CURL 'access/id)]
           [created   (ISO8601-to-time (zpl/locate* CURL 'created))]
           [metadata  (metadata/reconstruct (zpl/locate CURL 'metadata))])
       ; Any illegal field value will violate the curl/core contract.
       (curl/core id origin path access/id created metadata)))))


(define (curl/file/load path kp/sign)
  (call-with-input-file* path (lambda (in) (curl/load in kp/sign))))

;; Without the signature a curl/core instance as a curl/zpl looks like
;; (here the curl/core metadata is just the empty binding environment environ/null):
;; CURL
;;     id = a4f9344a-2ff1-4a26-bfc5-b9af422cec19
;;     origin = "r$+rHQ-hH:O2XP7+gV7dOm[z<d>a5>8u@DJl&C.Q"
;;     path = (a b c)
;;     access/id = some-access:send/id
;;     created = "2014-03-07T14:14:10Z"
;;     metadata = #(struct:motile/flat #(1 0 0) 0 #() () #(struct:environ #(struct:hash eq #())))

;; Given a signed textual zpl tree z (as bytes) this function verifies the signature
;; with the matching kp/sign key.
;; Returns the unsigned ZPL tree z as a bytes string if the signature matches and #f otherwise.
(define (curl/zpl/signed-to-curl/core z kp/sign)
  (if kp/sign
      (let ([z (crypto/sign/verify z kp/sign)])
        (and z (call-with-input-bytes z zpl-to-curl/core)))
      ; z was signed by a foreign island whose kp/sign is unavailable to us.
      (call-with-input-bytes
       z
       (lambda (in)
         (read-bytes CURVE/SIGNING/LENGTH in) ; Discard the signing.
         (zpl-to-curl/core in)))))

;; Locate by name a ZPL node x (an immediate child of parent ZPL node z) and return the value of x.
;; If no such child node exists return #f.
(define (zpl/locate* z name)
  (cond
    [(zpl/locate z name) => (lambda (x) (zpl/value x))]
    [else 0]))

;; In the CURL zpl representation a metadata field must either be #f or a motile/flat?
;; that deserializes to an environ.
;; If the metadata field is well-formed then the reconstruction will return either #f or an environ?
;; If the metadata field is ill-constructed or missing then motile/reconstruct will return some other Racket
;; value besides #f or an environ?.
(define (metadata/reconstruct node)
  (if node
      (let ([value (zpl/value node)]) ; Either #f or #(struct:motile/flat ...)
        (and value (motile/deserialize (motile/flat/reconstruct value))))
      0))

;; Given a bytes input port whose contents is a curl/zpl/signed that has been stripped
;; of its signing hash reconstruct a curl/core instance.
(define (zpl-to-curl/core in)
  (let* ([tree (zpl/load in)]
         [CURL (zpl/locate tree 'CURL)])
    (and
     CURL
     (let ([id        (zpl/locate* CURL 'id)]
           [origin    (zpl/locate* CURL 'origin)]
           [path      (zpl/locate* CURL 'path)]
           [access/id (zpl/locate* CURL 'access/id)]
           [created   (ISO8601-to-time (zpl/locate* CURL 'created))]
           [metadata  (metadata/reconstruct (zpl/locate CURL 'metadata))])
       ; Any illegal field value will violate the curl/core contract.
       (curl/core id origin path access/id created metadata)))))


;; Given a curl/zpl/signed byte string z return an equivalent curl/zpl/safe
;; where the raw 64-byte signature is replaced with SIGNATURE = <base64/url armour signing>
;; This form is intended for easy transmission via email or HTTP.
(define (curl/zpl/signed-to-curl/zpl/safe z)
  (let* ([signing (base64/url/encode (subbytes z 0 CURVE/SIGNING/LENGTH))]
         [zpl/item (format "~a = ~s\n" "SIGNATURE" signing)])
    (bytes-append (string->bytes/latin-1 zpl/item) (subbytes z CURVE/SIGNING/LENGTH))))

(define (curl/core-to-curl/zpl/safe c ks/sign)
  (let ([signed (curl/core-to-curl/zpl/signed c ks/sign)])
    (curl/zpl/signed-to-curl/zpl/safe signed)))

(define-syntax-rule (define/curl/inline name text)
  (define name (string->bytes/latin-1 text)))

;(define (curl/inline/define s)
;  (bytes-append (string->bytes/latin-1 s) #"\n")) ; Sad. We need to tack on an extra newline.

;; Given a zpl bytes representation of a CURL (one whose first line is: SIGNATURE = #"...")
;; and an island keystore convert the representation into a curl? instance.
;(define (curl/zpl/safe-to-curl b keystore)
;  (let ([in (open-input-bytes b)])
;    (with-handlers
;        ([exn:fail?
;          (lambda (_) (close-input-port in) #f)])
;      (begin0
;        (curl/safe/load in keystore)
;        (close-input-port in)))))

#| Example use of curl/inline/define
(define ALICE/CURL/HELLO
  (curl/inline/define
#<<!!
SIGNATURE = #"-r3QQsXIHTWM_OlPtWtur2WvivNwiEUJFeNP4ktX1e0YjL1FKIGAFc6qURCF8w1wxCMMDE9bmKSoYyo7Fxh1Cg"
CURL
    id = 908791e6-5896-4fee-8676-326682de2ce6
    origin = #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc"
    path = (hello)
    access/id = access:send:hello
    created = "2014-04-30T15:25:57Z"
    metadata = #f
!!
))
|#

;; A regular expression for the SIGNATURE header on the first line of a curl/zpl/safe byte string.
;; A match returns (<header> <z85>), for example,
;; (SIGNATURE = ":/Lai:$hB14*+*g2nI4DW2>qHhjnDw8#9Bj)(N#9zr=Wq(Hm/Ac)2o1(69=RyxR6?BocaB&DmD4u:5?$"
;;  :/Lai:$hB14*+*g2nI4DW2>qHhjnDw8#9Bj)(N#9zr=Wq(Hm/Ac)2o1(69=RyxR6?BocaB&DmD4u:5?$)
;; where header is the text of the first
;; line of the curl/zpl/safe:
;; SIGNATURE = "tRETJZoQ&H>sf:[p5]x{jetA3IFEfzJ!=QT-zYTKwN$yxmDowI(SObTAqIfpk$v0^x.LU:Btm+V[&xLH"
;; (including the quotation marks but omitting the termination #\newline)
;; and <z85> is the 80-character z85 ASCII encoding of the 64-byte binary signing:
;; tRETJZoQ&H>sf:[p5]x{jetA3IFEfzJ!=QT-zYTKwN$yxmDowI(SObTAqIfpk$v0^x.LU:Btm+V[&xLH
;; 0        1         2         3         4         5         6         7         8

;(define SIGNATURE/HEADER/RX #px"^SIGNATURE = \\\"([]0-9a-zA-Z.:+=^!/*?&<>()[{}@%$#-]{80})\\\"\\\n") ; z85 signature
(define SIGNATURE/HEADER/RX #px"^SIGNATURE = #\\\"([0-9a-zA-Z_-]{86})\\\"\\\n") ; Base64 URL-encoded signature.

(define (curl/zpl/safe-to-curl/zpl/signed safe)
  (let* ([all (regexp-match SIGNATURE/HEADER/RX safe)]
         [signature (cadr all)])
    (bytes-append
     (base64/url/decode signature) ; Reconstruct original 64-byte signing.
     (subbytes safe SIGNATURE/HEADER/LENGTH))))

;; TESTING
(require
  "../Island/keystore.rkt"
  "../persistent/hash.rkt"
  ;"../this.rkt"
  "../transport/access.rkt"
  "../transport/transports/bankers.rkt")

(define kp/base64 (curve/kp/base64 CURVE/STATIC))
(define kp/sign (curve/kp/sign CURVE/STATIC))
(define ks/sign (curve/ks/sign CURVE/STATIC))
(define ACCESSORS hash/eq/null)
(define KEYSTORE (hash/cons hash/equal/null (curve/kp CURVE/STATIC) (curve/kp/sign CURVE/STATIC)))


;(this/curve CURVE/STATIC)
;(this/keystore (box hash/equal/null))
;(keystore/add (this/keystore) (curve/kp CURVE/STATIC) (curve/kp/sign CURVE/STATIC))
;(this/accessors (box hash/eq/null))

;(define t/1 (transport:bankers/new))
;(define a/1 (access:send/known/new t/1 'a/1 GATES/NONE EMBARGO/NO))
;(define core/1 (curl/core/new  kp/base64 '(hello) 'a/1 environ/null))
;(define curl/1 (curl/new core/1 kp/sign ks/sign))
;(define serialize/1 (motile/serialize curl/1))
;(define deserialize/1
;  (motile/deserialize/offline serialize/1 (hash/cons hash/eq/null 'a/1 a/1) CURVE/STATIC KEYSTORE))
;(define serialize/1b (motile/serialize (list curl/1 curl/1)))
;(define deserialize/1b
;  (motile/deserialize/offline serialize/1b (hash/cons hash/eq/null 'a/1 a/1) CURVE/STATIC KEYSTORE))

#|
;; CURL with nontrivial metadata.
(define e/2 (environ/cons environ/null 'x 33 'y "some important string"))
(define core/2 (curl/core/new kp/base64 '(a b c) a/1 e/2))
(define curl/2 (curl/new core/2 kp/sign ks/sign))
(define serialize/2 (motile/serialize curl/2))
(define deserialize/2 (motile/deserialize serialize/2))
(define serialize/2b (motile/serialize (list curl/2 curl/2)))
(define deserialize/2b (motile/deserialize serialize/2b))

;; CURL whose metadata also includes another CURL.
(define e/3 (environ/cons e/2 'CURL curl/1))
(define core/3 (curl/core/new kp/base64 '(service X) a/1 e/3))
(define curl/3 (curl/new core/3 kp/sign ks/sign))
(define serialize/3 (motile/serialize curl/3))
(define deserialize/3 (motile/deserialize serialize/3))

;; Test that foreign signing keys are noted in CURLs.
(define X/kp (curve/kp CURVE/TEST))
(define X/kp/base64  (curve/kp/base64  CURVE/TEST))
(define X/kp/sign (curve/kp/sign CURVE/TEST))
(define X/ks/sign (curve/ks/sign CURVE/TEST))
;(foreign/kp/sign/add (this/foreign/kp/sign) X/kp/base64 X/kp/sign)
(keystore/add (this/keystore) X/kp X/kp/sign)
(define core/4 (curl/core/new X/kp/base64 '(service X) (uuid/symbol) e/3))
(define curl/4 (curl/new core/4 X/kp/sign X/ks/sign))
(define serialize/4 (motile/serialize curl/4))
(define deserialize/4 (motile/deserialize serialize/4))

;; Test that the absence of a foreign signing key is noted in a CURL.
;(foreign/kp/sign/remove (this/foreign/kp/sign) X/kp/base64)
(define core/5 (curl/core/new X/kp/base64 '(service X) a/1 e/3))
(define curl/5 (curl/new core/4 X/kp/sign X/ks/sign))
(define serialize/5 (motile/serialize curl/5))
(define deserialize/5 (motile/deserialize serialize/5))
|#

