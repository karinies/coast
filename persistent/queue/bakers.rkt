#lang racket/base

(require racket/contract/base)

(provide
 bankers/null
 queue/bankers?
 queue/empty?
 queue/nonempty?
 queue/length
 queue/put
 queue/take
 list/queue
 vector/queue
 queue=>list
 queue=>vector)

(struct bankers (front back) #:transparent)
(define (queue/bankers? x) (bankers? x))
(define bankers/null (bankers null null)) ; The empty queue.
(define (queue/empty? b)
  (and (null? (bankers-front b)) (null? (bankers-back b))))
(define (queue/nonempty? b) (not (queue/empty? b)))
(define (queue/length b)
  (+ (length (bankers-front b)) (length (bankers-back b))))

;; Append element x to bankers queue b.
(define (queue/put b x) (bankers (bankers-front b) (cons x (bankers-back b))))
;; Given nonempty bankers queue b return pair (<item> . <successor>) where
;; <item> - the next element in the queue and
;; <successor> - the bankers queue that is the tail of b
(define (queue/take b)
  (if (null? (bankers-front b))
      (let ((front (reverse (bankers-back b))))
        (cons (car front) (bankers (cdr front) null)))
      
      (let ((front (bankers-front b)))
        (cons (car front) (bankers (cdr front) (bankers-back b))))))

;; (list/queue x_1 ... x_n) returns a bankers queue containing x_1 ... x_n
(define (list/queue . rest) (bankers rest null))
;; (vector/queue v) where v = #(x_1 ... x_n) returns a bankers queue containing
;; x_1 ... x_n.
(define (vector/queue v) (bankers (vector->list v) null))
;; Return bankers queue b as a list in queue order.
(define (queue=>list b) (append (bankers-front b) (reverse (bankers-back b))))
;; Return bankers queue b as a vector whose elements are in queue order.
(define (queue=>vector b)
  (let ((v (make-vector (queue/length b) #f)))
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
;         (b1 (queue/put b0 33))
;         (b2 (queue/put b1 66))
;         (b3 (queue/put b2 99)))
;    (displayln (format "~a: ~a" 1 (list (queue/empty? b0) (queue/empty? b1) (queue/nonempty? b2))))
;    (displayln (format "~a: ~a" 2 (list (queue/length b0) (queue/length b3))))
;    (displayln (format "~a: ~a" 3 (queue/take b0)))
;    (displayln (format "~a: ~a" 4 (queue/take b1)))
;    (displayln (format "~a: ~a" 5 (queue/take b2)))))
;
;(define (bankers/test/02)
;  (let* ((b0 (list/queue/bounded 7 33 66 99 104))
;         (b1 (queue/put b0 111))
;         (b2 (queue/put b1 222)))
;    (displayln (format "~a: ~a" 1 b1))
;    (displayln (format "~a: ~a" 2 b2))
;    (displayln (format "~a: ~a" 3 (queue=>vector b2)))))



         