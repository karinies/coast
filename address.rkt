#lang racket/base

(require
 racket/contract
 (only-in "base64.rkt" base64/url/encoded?)
 (only-in "getters.rkt" struct/getters/define))

(define PORT/MIN 0)
(define PORT/MAX 65535)

(define kp/sha512/c (flat-named-contract 'island/kp/sha512 (and/c base64/url/encoded? immutable?)))
(define ip/c        (flat-named-contract 'IP               string?))
(define port/c      (flat-named-contract 'port             (integer-in PORT/MIN PORT/MAX)))

(provide
 island/address/kp/sha512
 island/address/ip
 island/address/port
 ip/c
 port/c
 (contract-out
  [struct island/address ((kp/sha512 kp/sha512/c) (ip ip/c) (port port/c))]
  [island/address/new (bytes? string? port/c . -> . island/address?)]
  ;[island/address=? (island/address? island/address? . -> . boolean?)]
  [island/address=>list         (island/address? . -> . list?)]
  [island/address=>vector       (island/address? . -> . (vector/c kp/sha512/c ip/c port/c #:immutable #t))]
  [island/address=>address:port (island/address? . -> . string?)]
  [island/address=>serial       (island/address? . -> . (vector/c 'struct:island/address kp/sha512/c ip/c port/c))]
  [serial=>island/address       ((vector/c 'struct:island/address any/c any/c port/c) procedure? . -> . island/address?)]
))
 

;; Islands use a base64 URL encoding of the island public key as the DNS name of the island.
(struct island/address
  (kp/sha512 ; Base64 URL encoding of island public key (see island/ip/sha512 in keys.rkt).
   ip        ; IPv4 or IPv6 address as string.
   port)     ; IP port number.
  #:transparent)
(struct/getters/define island/address kp/sha512 ip port)

(define (island/address/new kp/sha512 ip port)
  (island/address
   (bytes->immutable-bytes kp/sha512)
   (string->immutable-string ip)
   port))

;; The public key of a host should be a unique global identifier.
;; The same physical IP interface can be denoted by several distinct
;; byte strings, for example, a DNS name #"foo.example.com" and an IP address #"197.167.7.7".
;; It isn't clear if I should test for IP port number equality or not.
(define (island/address=? a b)
  (and
   (bytes=? (island/address/kp/sha512 a) (island/address/kp/sha512 b))
   (=       (island/address/port a)      (island/address/port b))))

(define (island/address=>list x)
  (list
   (island/address/kp/sha512 x)
   (island/address/ip x)
   (island/address/port x)))

(define (island/address=>vector x)
  (vector-immutable
   (island/address/kp/sha512 x)
   (island/address/ip x)
   (island/address/port x)))

(define (island/address=>address:port x)
  (format "~a:~a" (island/address/ip x) (island/address/port x)))

;; Generate the flat representation required by the serializer.
(define (island/address=>serial a)
  (struct->vector a))

;; #(struct:island/address kp/sha512 ip port)
(define (serial=>island/address v f)
  (island/address/new
   (f (vector-ref v 1))
   (f (vector-ref v 2))
   (vector-ref v 3)))
  