#lang typed/racket/base

;; This code is adapted from net/base64-unit.rkt in the collections directory of Racket 5.0.

(provide  
 base64/encode base64/decode
 base64/url/decode base64/url/encode base64/url/encoded? base64/size)

(define: ranges/mime : (Listof (Pair Bytes Integer))
  '((#"AZ" . 0)
    (#"az" . 26)
    (#"09" . 52)
    (#"++" . 62)
    (#"//" . 63)))

(define: ranges/url : (Listof (Pair Bytes Integer))
  '((#"AZ" . 0)
    (#"az" . 26)
    (#"09" . 52)
    (#"--" . 62)
    (#"__" . 63)))

;; Given the ranges for a base64 encoding generates the pair (decode . encode) where:
;;   decode - a mapping from a base64 character to the 6-bit nibble that it denotes
;;   encode - a mapping from a 6-bit nibble to the base64 character denoting that nibble
(: codecs/generate ((Listof (Pair Bytes Integer)) -> (Pairof (Vectorof Integer) (Vectorof Integer))))
(define (codecs/generate ranges)
  (let: ([decode : (Vectorof Integer) (make-vector 256)]
        [encode : (Vectorof Integer) (make-vector 64)])
    (for: ([r : (Pair Bytes Integer) ranges]
          #:when #t
          [i : Integer (in-range (bytes-ref (car r) 0) 
                       (add1 (bytes-ref (car r) 1)))] ; The integer representation of the character.
          [n : Integer (in-naturals (cdr r))])        ; The 6-bit value that the character denotes.
      (vector-set! decode i n)   ; character -> 6-bit natural number
      (vector-set! encode n i))  ; 6-bit natural number -> base64 character.
    (cons (vector->immutable-vector decode) (vector->immutable-vector encode))))

(define: codecs/url : (Pair (Vectorof Integer) (Vectorof Integer)) 
  (codecs/generate ranges/url))

(define ones '#(0 1 3 7 15 31 63 127 255))

;; Translate from base64 encoding to a raw byte string.
(: base64-decode-stream ((Vectorof Integer) Input-Port Output-Port -> Void))
(define (base64-decode-stream decode in out)
  (let loop ([data 0] [bits 0])
    (if (>= bits 8)
        (let ([bits (- bits 8)])
          (write-byte (arithmetic-shift data (- bits)) out)
          (loop (bitwise-and data (vector-ref ones bits)) bits))
        
        (let ([c (read-byte in)]) ; c is a base64 character.
          (unless (eof-object? c)
            (let ([v (vector-ref decode c)]) ; v is the raw 6-bit value corresponding to the base64 character.
              (if v
                  (loop (+ (arithmetic-shift data 6) v) (+ bits 6))
                  (loop data bits))))))))

(: base64-encode-stream ((Vectorof Integer) Input-Port Output-Port -> Void))
;; Translate a raw byte string into a base64 encoding.
(define (base64-encode-stream encode in out)
  (let loop ([data 0] [bits 0])
    ; Write out the high-order six bits as a base64 encoded character.
    (define (write-char)
      (write-byte
       (vector-ref encode (arithmetic-shift data (- 6 bits))) 
       out))
    
    (if (>= bits 6)
        (let ([bits (- bits 6)])
          ; Write out the high-order six bits.
          (write-char)
          ; Zero out the high order six bits and loop to write the next base64 encoded character.
          (loop (bitwise-and data (vector-ref ones bits)) bits))
        
        (let ([c (read-byte in)]) ; c is a raw byte value 0 ... 255.
          (if (eof-object? c)
              ; Flush the extra bits
              (when (> bits 0) (write-char))
              
              (loop (+ (arithmetic-shift data 8) c) (+ bits 8)))))))

;; Decode the base64 source bytes to raw bytes using the given decode vector.
(: base64/decode ((Vectorof Integer) Bytes -> Bytes))
(define (base64/decode decode source)
  (let ([s (open-output-bytes)])
    (base64-decode-stream decode (open-input-bytes source) s)
    (get-output-bytes s)))

(: base64/encode ((Vectorof Integer) Bytes -> Bytes))
;; Encode the raw source bytes to base64 bytes using the given encode vector.
(define (base64/encode encode source)
  (let ([s (open-output-bytes)])
    (base64-encode-stream encode (open-input-bytes source) s)
    (get-output-bytes s)))

;; Decode the base64 source bytes to raw bytes using the base64 URL decoding.
(: base64/url/decode (Bytes -> Bytes))
(define base64/url/decode
  (let ((decode (car codecs/url)))
    (lambda (source) (base64/decode decode source))))

;; Encode the raw source bytes to base64 bytes using the base64 URL encoding.
(: base64/url/encode (Bytes -> Bytes))
(define base64/url/encode
  (let ((encode (cdr codecs/url)))
    (lambda (source) (base64/encode encode source))))

(: base64/url/encoded? ((U Bytes String) -> Boolean))
; test whether a string represents a base64-URL-encoded value
(define (base64/url/encoded? s)
  (cond
    [(string? s) (regexp-match-exact? #rx"[-|0-9|A-Z|a-z|_]+"  s)]
    [(bytes? s)  (regexp-match-exact? #rx#"[-|0-9|A-Z|a-z|_]+" s)]))

(: base64/size (Natural -> Natural))
(define (base64/size m)
  (let-values ([(q r) (quotient/remainder (* m 8) 6)])
    (+ q
       (cond
         ((zero? r) 0)
         ((< r 7)   1)
         (else      2)))))
      