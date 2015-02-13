#lang racket/base

(require ffi/unsafe)
(provide
 fastzl/bytes/pad
 (rename-out
  (fastlz-compress   bytes/compress)
  (fastlz-decompress bytes/decompress)))

(define LIBFASTLZ "/usr/local/lib/libfastlz")
(define fastlz (ffi-lib LIBFASTLZ #f)) 

;; Shorthand for binding fastlz functions.
(define-syntax-rule (define-fastlz-function binding name type)
  (define binding (get-ffi-obj name fastlz type)))

(define BYTES/IN/MIN  16)
(define BYTES/OUT/MIN 66)
(define BUFFER/EXPANSION 1.06)

;; int fastlz_compress(const void* input, int length, void* output)
;; input  - bytes input buffer
;; length - length of input buffer in bytes
;; output - bytes output buffer
;; The return value is the actual length of the compressed output in bytes.
;; The input buffer must be at least 16 bytes long.
;; The output buffer must be at least 5% larger than the input buffer
;; and can not be smaller than 66 bytes.
(define-fastlz-function
  fastlz-compress
  "fastlz_compress"
  (_fun (input) ::
        (_bytes = (if (>= (bytes-length input) BYTES/IN/MIN)
                      input
                      (raise-argument-error
                       'bytes/deflate
                       ; Expected.
                       (format "min input buffer length of ~a" BYTES/IN/MIN)         
                       ; Actual
                       (bytes-length input))))

        (_int = (bytes-length input))

        (output : (_bytes
                   o
                   (max BYTES/OUT/MIN (inexact->exact (round (+ 0.5 (* BUFFER/EXPANSION (bytes-length input))))))))

        -> (outcome : _int)
        -> (if (positive? outcome)
               (make-sized-byte-string output outcome)
               (raise-result-error 'bytes/compress "compressed byte string" "no compression"))))

;; int fastlz_decompress(const void* input, int length, void* output, int maxout)
;; input  - bytes input buffer
;; length - length of input buffer in bytes
;; output - bytes output buffer
;; maxout - max length of output buffer in bytes
(define-fastlz-function
  fastlz-decompress
  "fastlz_decompress"
  (_fun (input max) ::
        (input : _bytes)
        (_int = (bytes-length input)) ; length
        (output : (_bytes o max))
        (max : _int)
        -> (outcome : _int)
        -> (if (positive? outcome)
               (make-sized-byte-string output outcome)
               (raise-result-error 'bytes/decompress "decompressed byte string" "no decompression"))))

(define (fastzl/bytes/pad b)
  (let ((n (bytes-length b)))
    (if (< n BYTES/IN/MIN)
        (bytes-append b (make-bytes (- BYTES/IN/MIN n) (char->integer #\space)))
        b)))