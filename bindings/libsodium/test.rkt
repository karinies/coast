#lang racket/base

(require
 ;(only-in net/base64 base64-encode base64-decode)
 "../../base64.rkt"
 ;net/url
 "crypto.rkt")


;; A snippet of the Gettysburg Address (Abraham Lincoln, November 19, 1863) for test plaintext.
(define PLAINTEXT
  #"Four score and seven years ago our fathers brought forth on this continent a new nation, conceived in liberty, \
and dedicated to the proposition that all men are created equal. \
Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. \
We are met on a great battle field of that war. We have come to dedicate a portion of that field, \
as a final resting place for those who here gave their lives that that nation might live. \
It is altogether fitting and proper that we should do this.")


(define (test/crypto/box/encrypt)
  (let-values ([(pk/alice sk/alice) (crypto/box/keys)]
               [(pk/bob   sk/bob)   (crypto/box/keys)])
    (let ((nonce (crypto/nonce/random)))
      (crypto/box/encrypt PLAINTEXT nonce pk/bob sk/alice))))
    
(define (test/crypto/box/decrypt)
  (let-values ([(pk/alice sk/alice) (crypto/box/keys)]
               [(pk/bob   sk/bob)   (crypto/box/keys)])
    (let* ((nonce (crypto/nonce/random))
           (cipher (crypto/box/encrypt PLAINTEXT nonce pk/bob sk/alice)))
      (crypto/box/decrypt cipher nonce pk/alice sk/bob))))

(define (test/crypto/box/precompute)
  (let-values ([(pk/alice sk/alice) (crypto/box/keys)]
               [(pk/bob   sk/bob)   (crypto/box/keys)])
    (crypto/box/precompute pk/bob sk/alice)))

(define (test/crypto/box/pre)
    (let-values ([(pk/a sk/a) (crypto/box/keys)]
                 [(pk/b sk/b) (crypto/box/keys)])
      (let* ((pre/a (crypto/box/precompute pk/b sk/a)) ; For encrypting messages sent from a to b.
             (pre/b (crypto/box/precompute pk/a sk/b)) ; For decrypting messages sent to b from a.
             (nonce (crypto/nonce/random))
             (gold  (crypto/box/encrypt PLAINTEXT nonce pk/b sk/a)) ; GOLD ciphertext for a message from a to b.
             (cipher (crypto/box/pre/encrypt PLAINTEXT nonce pre/a)))
        (write (if (bytes=? pre/a pre/b) "same session key" "different session key"))
        (newline) (newline)

        (write gold)   (newline) (newline)
        (write cipher) (newline) (newline)
        (write (if (bytes=? gold cipher) "same cipher" "different cipher"))
        (newline) (newline)

        (write (crypto/box/decrypt cipher nonce pk/a sk/b))
        (newline) (newline)
        (write (crypto/box/pre/decrypt cipher nonce pre/b))
        (newline) (newline)
        (write (crypto/box/pre/decrypt cipher nonce pre/a))
        (newline))))

(define (test/crypto/sign)
  (let-values ([(pk sk) (crypto/sign/keys)])
    (let* ((signature (crypto/sign PLAINTEXT sk))
           (message   (crypto/sign/verify signature pk)))
      (write signature)
      (newline) (newline)
      (write message)
      (newline))))

(define (test/crypto/sign.1)
  (let-values ([(pk sk) (crypto/box/keys)])
    (let* ((key/signing (bytes-append sk pk))
           (signature (crypto/sign PLAINTEXT key/signing))
           (message  (crypto/sign/verify signature pk)))
      (write signature)
      (newline) (newline)
      (write message)
      (newline))))
 
(define (test/crypto/secret/)
  (let* ((k      (crypto/secret/key))
         (nonce  (crypto/nonce/random))
         (cipher (crypto/secret/encrypt PLAINTEXT nonce k)))
    (crypto/secret/decrypt cipher nonce k)))

(define (test/crypto/authenticate)
  (let* ((m PLAINTEXT)
         (k (crypto/secret/key))
         (t (crypto/authenticate/token m k)))
    (write t)
    (newline) (newline)
    (write m)
    (newline) (newline)
    (crypto/authenticate? t m k)))

(define (test/crypto/hash)
  (crypto/hash PLAINTEXT))

(define (test/crypto/hash/short)
  (let ((k (crypto/hash/short/key)))
    (crypto/hash/short PLAINTEXT k)))

