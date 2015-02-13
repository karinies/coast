#lang racket/base

(require
 racket/contract
 "random.rkt"
 "crypto_authenticate.rkt"
 "crypto_box.rkt"
 "crypto_hash.rkt"
 "crypto_nonce.rkt"
 "crypto_secret.rkt"
 "crypto_short_hash.rkt"
 "crypto_sign.rkt")

;; Convenient one-stop shopping for all of the libsodium-based crypto/* functions plus
;; some convenient shorthand for generating random byte strings.

(define nonce/c
  (flat-named-contract 'crypto/nonce  (and/c bytes? (lambda (x) (= (bytes-length x) 24))))) ; See CRYPTO/SECRET/NONCE/SIZE.
(define secret/c
  (flat-named-contract 'crypto/key    (and/c bytes? (lambda (x) (= (bytes-length x) 32))))) ; See CRYPTO/SECRET/KEY/SIZE.
(define cipher/c
  (flat-named-contract 'crypto/cipher (and/c bytes? (lambda (x) (>= (bytes-length x) 16))))) ; See CRYPTO/SECRET/CIPHER/PREFIX/SIZE.
(define public/c
  (flat-named-contract 'crypto/public (and/c bytes? (lambda (x) (= (bytes-length x) 32))))) ; See CRYPTO/BOX/KEY/SIZE.

(define token/c
  (flat-named-contract 'crypto/token  (and/c bytes? (lambda (x) (= (bytes-length x) 32))))) ; See CRYPTO/AUTHENTICATE/TOKEN/SIZE.
(define hashing/c
  (flat-named-contract 'crypto/hash   (and/c bytes? (lambda (x) (= (bytes-length x) 64))))) ; See CRYPTO/HASH/SIZE.

(define hash/short/c   
  (flat-named-contract  'crypto/hash/short (and/c bytes? (lambda (x) (= (bytes-length x) 8))))) ; See CRYPTO/HASH/SHORT/SIZE.
(define hash/short/key/c
  (flat-named-contract 'crypto/hash/short/key (and/c bytes? (lambda (x) (= (bytes-length x) 16))))) ; See CRYPTO/HASH/SHORT/KEY/SIZE.

(define uint32/c
  (flat-named-contract 'random/uint32 (and/c fixnum? (lambda (x) (>= 0)))))

(define signing/verify/c
  (flat-named-contract 'crypto/sign/verify (and/c bytes? (lambda (x) (= (bytes-length x) 32))))) ; See CRYPTO/SIGN/VERIFY/SIZE.
(define signing/secret/c
  (flat-named-contract 'crypto/sign/secret (and/c bytes? (lambda (x) (= (bytes-length x) 64))))) ; See CRYPTO/SIGN/SECRET/SIZE.

;; Contract for a signed blob of bytes.
;; The signature is a 64-byte prefix to the signed material.
(define signing/c
  (flat-named-contract 'crypto/sign/signing (and/c bytes? (lambda (x) (>= (bytes-length x) 64)))))

(provide
 (contract-out
  ; crypto_secret.rkt
  [crypto/secret/key/size   (-> 32)]
  [crypto/secret/nonce/size (-> 24)]
  [crypto/secret/primitive  (-> bytes?)]
  [crypto/secret/key      (-> secret/c)]
  [crypto/secret/encrypt  (bytes?   nonce/c secret/c . -> . bytes?)]
  [crypto/secret/decrypt  (cipher/c nonce/c secret/c . -> . bytes?)]

  ; crypto_box.rkt
  [CRYPTO/BOX/KP/SIZE (and/c exact-positive-integer? even?)]
  [CRYPTO/BOX/KS/SIZE (and/c exact-positive-integer? even?)]
  [crypto/box/key/size    (-> 32)]
  [crypto/box/secret/size (-> 32)]
  [crypto/box/nonce/size  (-> 24)]
  [crypto/box/primitive   (-> bytes?)]
  [crypto/box/keys        (-> (values public/c secret/c))]
  [crypto/box/encrypt     (bytes?   nonce/c public/c secret/c . -> . bytes?)]
  [crypto/box/decrypt     (cipher/c nonce/c public/c secret/c . -> . bytes?)]
  [crypto/box/precompute  (public/c secret/c . -> . secret/c)]
  [crypto/box/pre/encrypt (bytes?   nonce/c secret/c . -> . bytes?)]
  [crypto/box/pre/decrypt (cipher/c nonce/c secret/c . -> . bytes?)]

  ; crypto_nonce.rkt
  [crypto/nonce/random    (-> nonce/c)]
  [crypto/nonce/define    (exact-nonnegative-integer? . -> . nonce/c)]
  ; crypto_authenticate.rkt
  [crypto/authenticate/token (bytes? secret/c . -> . token/c)]
  [crypto/authenticate?      (token/c bytes? secret/c . -> . boolean?)]

  ; crypto_hash.rkt
  [crypto/hash (bytes? . -> . hashing/c)]

  ; crypto_short_hash.rkt
  [crypto/hash/short/key/size  (-> 16)]
  [crypto/hash/short/size      (->  8)]
  [crypto/hash/short/primitive (-> bytes?)]
  [crypto/hash/short/key       (-> hash/short/key/c)]
  [crypto/hash/short           (bytes? hash/short/key/c . -> . hash/short/c)]

  ; crypto_sign.rkt
  [CRYPTO/SIGN/KP/SIZE (and/c exact-positive-integer? even?)]
  [CRYPTO/SIGN/KS/SIZE (and/c exact-positive-integer? even?)]
  [crypto/sign/keys   (-> (values signing/verify/c signing/secret/c))]
  [crypto/sign        (bytes?    signing/secret/c . -> . bytes?)]
  [crypto/sign/verify (signing/c signing/verify/c . -> . (or/c bytes? #f))]
  [crypto/sign/size        (-> 64)]
  [crypto/sign/key/size    (-> 32)]
  [crypto/sign/secret/size (-> 64)]
  [crypto/sign/primitive    (-> bytes?)]
  
  ; random.rkt
  [random/bytes/primitive (-> bytes?)]
  [random/uint32          (-> uint32/c)]
  [random/uint32/uniform  (uint32/c . -> . uint32/c)]
  [random/bytes/n         (uint32/c . -> . bytes?)]
  [random/bytes/4         (-> bytes?)]
  [random/bytes/8         (-> bytes?)]
  [random/bytes/16        (-> bytes?)]
  [random/bytes/24        (-> bytes?)]
  [random/bytes/32        (-> bytes?)]))


;; The code in the various crypto_*.rkt uses the following abbreviations for variables:
;; c  - ciphertext
;; m  - message or plaintext
;; n  - nonce
;; pk - public key
;; sk - matching secret key for a public key or a matching signing key for a public verification key
;; k  - precomputed combination of public and secret keys or a shared secret key
;; vk - public verification key for signatures
;; s  - signed material
;; t  - authentication token

















