#lang racket/base

(require racket/contract/base)

(provide
 bankers/null
 bankers?
 (contract-out
  [bankers/empty? (bankers? . -> . boolean?)]
  [bankers/nonempty? (bankers? . -> . boolean?)]
  [bankers/length (bankers? . -> . exact-nonnegative-integer?)]
  [bankers/put (bankers? any/c . -> . bankers?)]
  [bankers/take (bankers? . -> . (or/c #f (cons/c any/c bankers?)))]
  [list/bankers ((->* () () #:rest any/c any) . -> . bankers?)]
  [vector/bankers (vector? . -> . bankers?)]
  [bankers=>list (bankers? . -> . list?)]
  [bankers=>vector (bankers? . -> . vector?)]))

(struct bankers (front back) #:transparent)
(define bankers/null (bankers null null)) ; The empty bankers queue.
(define (bankers/empty? b)
  (and (null? (bankers-front b)) (null? (bankers-back b))))
(define (bankers/nonempty? b) (not (bankers/empty? b)))
(define (bankers/length b)
  (+ (length (bankers-front b)) (length (bankers-back b))))

;; Append element x to bankers queue b.
(define (bankers/put b x) (bankers (bankers-front b) (cons x (bankers-back b))))
;; Given nonempty bankers queue b return pair (<item> . <successor>) where
;; <item> - the next element in the queue and
;; <successor> - the bankers queue that is the tail of b.
;; If the queue is empty return #f.
(define (bankers/take b)
  (let ((front (bankers-front b)))
    (cond
      [(null? front)
       (let ((front (reverse (bankers-back b))))
         (and (not (null? front)) (cons (car front) (bankers (cdr front) null))))]
      [else
       (cons (car front) (bankers (cdr front) (bankers-back b)))])))

;; (list/bankers x_1 ... x_n) returns a bankers queue containing x_1 ... x_n
(define (list/bankers . rest) (bankers rest null))
;; (vector/bankers v) where v = #(x_1 ... x_n) returns a bankers queue containing
;; x_1 ... x_n.
(define (vector/bankers v) (bankers (vector->list v) null))
;; Return bankers queue b as a list in queue order.
(define (bankers=>list b) (append (bankers-front b) (reverse (bankers-back b))))
;; Return bankers queue b as a vector whose elements are in queue order.
(define (bankers=>vector b)
  (let ((v (make-vector (bankers/length b) #f)))
    ; Copy the front of the bankers queue into the vector.
    (let loop/front ((i 0) (front (bankers-front b)))
      (cond
        [(null? front)
         ; Now do the back of the bankers queue working your way backwards from
         ; the end of the vector towards the front of the vector.
         (let loop/back ([j (sub1 (vector-length v))] [back (bankers-back b)])
           (cond
             [(null? back) v]
             [else
              (vector-set! v j (car back))
              (loop/back (sub1 j) (cdr back))]))]
        [else
         (vector-set! v i (car front))
         (loop/front (add1 i) (cdr front))]))))

;(define (bankers/test/01)
;  (let* ((b0 bankers/null)
;         (b1 (bankers/put b0 33))
;         (b2 (bankers/put b1 66))
;         (b3 (bankers/put b2 99)))
;    (displayln (format "~a: ~a" 1 (list (bankers/s/s/empty? b0) (bankers/empty? b1) (bankers/nonempty? b2))))
;    (displayln (format "~a: ~a" 2 (list (bankers/length b0) (bankers/s/length b3))))
;    (displayln (format "~a: ~a" 3 (bankers/take b0)))
;    (displayln (format "~a: ~a" 4 (bankers/take b1)))
;    (displayln (format "~a: ~a" 5 (bankers/take b2)))))
;
;(define (bankers/test/02)
;  (let* ((b0 (list/bankers/bounded 7 33 66 99 104))
;         (b1 (bankers/put b0 111))
;         (b2 (bankers/put b1 222)))
;    (displayln (format "~a: ~a" 1 b1))
;    (displayln (format "~a: ~a" 2 b2))
;    (displayln (format "~a: ~a" 3 (bankers=>vector b2)))))



         