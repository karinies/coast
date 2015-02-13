#lang racket/base

(require
 racket/contract/base
 "../bindings/libczmq/zconfig.rkt"
 "../base64.rkt"
 "../curve.rkt"
 "../getters.rkt"
 "../persistent/hash.rkt"
 "../z85.rkt")

;; A keystore is a hash/equal map an island public key kp to its public signing key kp/sign.
;; A keystore maintains all keys as raw byte strings without any armour encoding.

(provide
 keystore/c
 public/kp
 public/kp/sign
 public/petname
 (contract-out
  [struct public ((kp kp/c) (kp/sign kp/sign/c) (petname symbol?))]
  [keystore/new (-> keystore/c)]
  [keystore/load (-> keystore/c string? void?)]
  [keystore/add  (-> keystore/c (or/c symbol? kp/base64/c) public? void?)]
  [keystore/merge (-> keystore/c hash/equal? void?)]
  [keystore/remove (-> keystore/c (or/c kp/base64/c symbol?) void?)]
  [keystore/look (-> keystore/c kp/base64/c (or/c public? #f))]
  [keystore/petname/look (-> keystore/c kp/base64/c (or/c symbol? kp/base64/c))]
  [keystore/kp/sign/look (-> keystore/c kp/base64/c (or/c kp/sign/c #f))]
  ))

(define keystore/c (flat-named-contract 'keystore (box/c hash/equal? #:flat? #t)))

(define (keystore/new) (box hash/equal/null))

(struct public (kp kp/sign petname))
(struct/getters/define public kp kp/sign petname)

;; store is a box containing a keystore.
;; path is file system path to the directory containing public certificates of interest to the island.
;; Returns a hash table containing three different mappings:
;;   kp to kp/sign
;;   kp/base64 to petname
;;   petname to kp
(define (keystore/load store path)
  (for ([name (in-list (directory-list path #:build? #f))])
  ;(for ([path (in-list (directory-list path #:build? #t))])
    (let* ([z (zconfig/load (path->string (build-path path name)))] ; Load the public certificate.
           [kp/z85      (zconfig/resolve z "/curve/public-key" "")]
           [kp/sign/z85 (zconfig/resolve z "curve/public-sign-key" "")])
      ; Ignore a bad certificate.
      (when (and (kp/z85/ok? kp/z85) (kp/sign/z85/ok? kp/sign/z85))
          (let* ([petname (string->symbol (path->string name))]
                 [p (public
                      (zmq/z85/decode kp/z85)
                      (zmq/z85/decode kp/sign/z85)
                      petname)]
                 [kp/base64 (base64/url/encode (public/kp p))])
            (keystore/add store kp/base64 p)))
      ; Discard the public certificate.
      (zconfig/destroy z))))

;; Add an island's public keys to a keystore.
(define (keystore/add store x p)
  (let ([kp/base64 (base64/url/encode (public/kp p))])
    (let loop ([before (unbox store)])
      (when (not (box-cas! store before (hash/new before x p)))
        (loop (unbox store))))))

(define (keystore/merge store h)
  (let loop ([before (unbox store)])
    (when (not (box-cas! store before (hash/merge before h)))
      (loop (unbox store)))))

;; Remove an island's public keys from a keystore.
(define (keystore/remove store kp)
  (let loop ([before (unbox store)])
    (when (not (box-cas! store before (hash/remove before kp)))
      (loop (unbox store)))))

;; Obtain the public signing key kp/sign of the island whose public key is kp.
;; Returns #f if the island public key kp is not in the store.
(define (keystore/look store x) (hash/ref (unbox store) x #f))

;; Given an island kp/base64 return the island's petname (if available)
;; or its kp/base64.
(define (keystore/petname/look keystore kp/base64)
  (let ([p (keystore/look keystore kp/base64)])
    (if p (public/petname p) kp/base64)))

(define (keystore/kp/sign/look keystore kp/base64)
  (let ([p (keystore/look keystore kp/base64)])
    (and p (public/kp/sign p))))

;; Testing
;(define b (box hash/equal/null))
;(keystore/load b "/home/self/Repository/COAST/examples/certificates/public/")