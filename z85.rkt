#lang racket/base

(require
 racket/contract/base
 [only-in "bindings/libzmq/zmq.rkt" zmq_z85_encode zmq_z85_decode])

(provide
 (contract-out
  [z85/expansion  (-> (and/c exact-positive-integer? (lambda (x) (zero? (modulo x 4)))) exact-positive-integer?)]
  [zmq/z85/pad    (-> bytes? (and/c bytes (lambda (s) (zero? (modulo (bytes-length s) 4)))))]
  [zmq/z85/encode (-> (and/c bytes? (lambda (s) (zero? (modulo (bytes-length s) 4)))) string?)]
  [zmq/z85/decode (-> (and/c string? (lambda (s) (zero? (modulo (string-length s) 5)))) bytes?)]))

;; z85 encodes every 4 binary bytes with 5 ASCII bytes.
(define (z85/expansion n) (/ (* n 5) 4))

;; Pad the input bytes (if necessary) to be a multiple of 4 bytes.
(define (zmq/z85/pad data)
  (let* ((n (bytes-length data))
         (tail (modulo n 4))) ; Do we need to pad the input data?
    (if (zero? tail)
        data
        (let ((data+ (make-bytes (+ n (- 4 tail)) 0)))
          (bytes-copy! data+ 0 data)
          data+))))

(define (zmq/z85/encode data)
  (let* ([n (bytes-length data)]
         [m (add1 (/ (* n 5) 4))] ; Length of output string.
         [r (make-string m)])     ; Output string/latin-1.
    (zmq_z85_encode r data n)))

(define (zmq/z85/decode data)
  (let* ([n (string-length data)]
         [outcome (make-bytes (* 4 (/ n 5)) 0)])
    (zmq_z85_decode outcome data)
    outcome))

;(require "bindings/fastlz/fastlz.rkt")