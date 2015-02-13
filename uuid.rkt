#lang racket/base

(require srfi/27)

;;From Gambit Scheme Released under the LGPL
;; UUID generation
;; See: http://www.ietf.org/rfc/rfc4122.txt
;;
;; Version 4 UUID, see section 4.4
(provide
 uuid/bytes
 uuid/string
 uuid/symbol
 uuid/urn)

;; Generate a random integer in the range [0 .. 65536).
(define random-integer-65536
  (let* ([rs (make-random-source)]
         [ri (random-source-make-integers rs)])
    (random-source-randomize! rs)
    (lambda () (ri 65536))))

(define hex '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))

;; Generate a UUID as an ordinary mutable string.
(define (uuid/new)
  (let ((n1 (random-integer-65536))
        (n2 (random-integer-65536))
        (n3 (random-integer-65536))
        (n4 (random-integer-65536))
        (n5 (random-integer-65536))
        (n6 (random-integer-65536))
        (n7 (random-integer-65536))
        (n8 (random-integer-65536)))
     (string
      ;; time_lo
      (vector-ref hex (extract-bit-field 4 12 n1))
      (vector-ref hex (extract-bit-field 4  8 n1))
      (vector-ref hex (extract-bit-field 4  4 n1))
      (vector-ref hex (extract-bit-field 4  0 n1))
      (vector-ref hex (extract-bit-field 4 12 n2))
      (vector-ref hex (extract-bit-field 4  8 n2))
      (vector-ref hex (extract-bit-field 4  4 n2))
      (vector-ref hex (extract-bit-field 4  0 n2))
      #\-
      ;; time_mid
      (vector-ref hex (extract-bit-field 4 12 n3))
      (vector-ref hex (extract-bit-field 4  8 n3))
      (vector-ref hex (extract-bit-field 4  4 n3))
      (vector-ref hex (extract-bit-field 4  0 n3))
      #\-
      ;; time_hi_and_version
      (vector-ref hex #b0100)
      (vector-ref hex (extract-bit-field 4  8 n4))
      (vector-ref hex (extract-bit-field 4  4 n4))
      (vector-ref hex (extract-bit-field 4  0 n4))
      #\-
      ;; clock_seq_hi_and_reserved
      (vector-ref hex (bitwise-ior (extract-bit-field 2 12 n5) #b1000))
      (vector-ref hex (extract-bit-field 4  8 n5))
      ;; clock_seq_low
      (vector-ref hex (extract-bit-field 4  4 n5))
      (vector-ref hex (extract-bit-field 4  0 n5))
      #\-
      ;; node
      (vector-ref hex (extract-bit-field 4 12 n6))
      (vector-ref hex (extract-bit-field 4  8 n6))
      (vector-ref hex (extract-bit-field 4  4 n6))
      (vector-ref hex (extract-bit-field 4  0 n6))
      (vector-ref hex (extract-bit-field 4 12 n7))
      (vector-ref hex (extract-bit-field 4  8 n7))
      (vector-ref hex (extract-bit-field 4  4 n7))
      (vector-ref hex (extract-bit-field 4  0 n7))
      (vector-ref hex (extract-bit-field 4 12 n8))
      (vector-ref hex (extract-bit-field 4  8 n8))
      (vector-ref hex (extract-bit-field 4  4 n8))
      (vector-ref hex (extract-bit-field 4  0 n8)))))

;; Generate a UUID as an immutable string.
(define (uuid/string) (string->immutable-string (uuid/new)))
;; Generate a UUID as a symbol.
(define (uuid/symbol) (string->symbol (uuid/new)))
;; Generate a UUID URN as a mutable string.
(define (uuid/urn)
  (string-append "urn:uuid:" (uuid/new)))

(define (extract-bit-field size position n)
  (bitwise-and (bitwise-not (arithmetic-shift -1 size))
               (arithmetic-shift n (- position))))

(define (nibbles-to-byte nibble_1 nibble_2)
  (bitwise-ior (arithmetic-shift nibble_1 4) nibble_2))

(define SEPARATOR 255) ; Replacement for the character - (dash) in the faux bytes UUID below.

;; Generate a faux UUID as an immutable byte string.
(define (uuid/bytes)
  (let ((n1 (random-integer-65536))
        (n2 (random-integer-65536))
        (n3 (random-integer-65536))
        (n4 (random-integer-65536))
        (n5 (random-integer-65536))
        (n6 (random-integer-65536))
        (n7 (random-integer-65536))
        (n8 (random-integer-65536)))
    (bytes->immutable-bytes 
     (bytes
      ;; time_lo
      (nibbles-to-byte (extract-bit-field 4 12 n1) (extract-bit-field 4  8 n1))
      (nibbles-to-byte (extract-bit-field 4  4 n1) (extract-bit-field 4  0 n1))
      (nibbles-to-byte (extract-bit-field 4 12 n2) (extract-bit-field 4  8 n2))
      (nibbles-to-byte (extract-bit-field 4  4 n2) (extract-bit-field 4  0 n2))
      SEPARATOR
      ;; time_mid
      (nibbles-to-byte (extract-bit-field 4 12 n3) (extract-bit-field 4  8 n3))
      (nibbles-to-byte (extract-bit-field 4  4 n3) (extract-bit-field 4  0 n3))
      SEPARATOR
      ;; time_hi_and_version
      (nibbles-to-byte  #b0100 (extract-bit-field 4  8 n4))
      (nibbles-to-byte (extract-bit-field 4  4 n4) (extract-bit-field 4  0 n4))
      SEPARATOR
      ;; clock_seq_hi_and_reserved
      (nibbles-to-byte (bitwise-ior (extract-bit-field 2 12 n5) #b1000) (extract-bit-field 4  8 n5))
      ;; clock_seq_low
      (nibbles-to-byte (extract-bit-field 4  4 n5) (extract-bit-field 4  0 n5))
      SEPARATOR
      ;; node
      (nibbles-to-byte (extract-bit-field 4 12 n6) (extract-bit-field 4  8 n6))
      (nibbles-to-byte (extract-bit-field 4  4 n6) (extract-bit-field 4  0 n6))
      (nibbles-to-byte (extract-bit-field 4 12 n7) (extract-bit-field 4  8 n7))
      (nibbles-to-byte (extract-bit-field 4  4 n7) (extract-bit-field 4  0 n7))
      (nibbles-to-byte (extract-bit-field 4 12 n8) (extract-bit-field 4  8 n8))
      (nibbles-to-byte (extract-bit-field 4  4 n8) (extract-bit-field 4  0 n8))))))
