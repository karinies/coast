#lang racket/base

;; Copyright 2012 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(provide bytes/hex)

(define DIGITS #"0123456789abcdef")

(define (bytes/hex b)
  (let* ([length (bytes-length b)]
         [hex (make-bytes (* length 2))])
    (for ([i (in-range length)])
      (let ([c (bytes-ref b i)]
            [j (* 2 i)])
        (bytes-set! hex j        (bytes-ref DIGITS (arithmetic-shift c -4))) ; Zero-filled right shift by 4 bits.
        (bytes-set! hex (add1 j) (bytes-ref DIGITS (bitwise-and c #xF)))))
    hex))
