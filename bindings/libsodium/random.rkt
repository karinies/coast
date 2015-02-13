#lang racket/base

(require
 ffi/unsafe
 (only-in "sodium.rkt" sodium define-sodium-function))

(provide
 random/bytes/primitive
 random/uint32
 random/bytes/stir
 random/uint32/uniform
 random/bytes/fill
 random/bytes/4 random/bytes/8 random/bytes/16 random/bytes/24 random/bytes/32 random/bytes/n)

;; Generate random byte strings.

;; Return the name of the random bytes implementation used here as a bytes string, for example, #"sysrandom".
;; const char * randombytes_implementation_name(void)
(define-sodium-function random/bytes/primitive "randombytes_implementation_name" (_fun -> _bytes))

;; Return a random 32 bit unsigned integer.
;; uint32_t randombytes_random(void)
(define-sodium-function random/uint32 "randombytes_random" (_fun -> _uint32))

;; Synthesize a new key for the pseudorandom number generator.
;;void randombytes_stir(void)
(define-sodium-function random/bytes/stir "randombytes_stir" (_fun -> _void)) 

;; Return an integer value between 0 and upper using a uniform distribution.
;; uint32_t randombytes_uniform(const uint32_t upper)
(define-sodium-function random/uint32/uniform "randombytes_uniform" (_fun _uint32 -> _uint32))

;; Fill the give buffer with n random bytes.
;; void randombytes_buf(void* const buf, const size_t n)
(define-sodium-function random-bytes-buffer "randombytes_buf" (_fun _bytes _uint32 -> _void))

;; Unused.
;; int randombytes_close(void)
;;(define-sodium-function random-bytes-close "randombytes_close" (_fun -> _int))

;; Unused.
;; void randombytes(unsigned char * const buf, const unsigned long long buf_len)
;(define-sodium-function random-bytes "randombytes" (_fun _bytes _ullong -> _void))

;; Return a random bytes string of n bytes.
(define (random/bytes/n n)
  (let ((b (make-bytes n 0)))
    (random-bytes-buffer b n)
    b))

;; Common fixed-length random byte strings.
(define (random/bytes/4)  (random/bytes/n 4))
(define (random/bytes/8)  (random/bytes/n 8))
(define (random/bytes/16) (random/bytes/n 16))
(define (random/bytes/24) (random/bytes/n 24))
(define (random/bytes/32) (random/bytes/n 32))

;; Fill bytes string b with random byte values.
(define (random/bytes/fill b)
  (random-bytes-buffer b (bytes-length b))
  b)
