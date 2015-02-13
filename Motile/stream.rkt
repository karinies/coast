#lang racket/base

;; Thread-safe multiple-writer, multiple-reader stream.

(require
 "persistent/vector.rkt")

(define (stream/new)
  (vector '<stream> (make-semaphore 1) 0 0 vector/null vector/null))

(define (stream? s)
  (and
   (vector? s)
   (eq? (vector-ref s 0) '<stream>)
   (= (vector-length s) 6)))

(define-syntax-rule (NEXT)        2)
(define-syntax-rule (HEAD/LENGTH) 3)
(define-syntax-rule (HEAD)        4)
(define-syntax-rule (TAIL)        5)

(define-syntax-rule (stream/semaphore s)   (vector-ref s 1))
(define-syntax-rule (stream/next s)        (vector-ref s 2))
(define-syntax-rule (stream/head/length s) (vector-ref s 3))
(define-syntax-rule (stream/head s)        (vector-ref s 4))
(define-syntax-rule (stream/tail s)        (vector-ref s 5))


(define-syntax-rule (EMPTY? s)
  (and
   (>= (stream/next s) (stream/head/length s))
   (vector/null? (stream/tail s))))

(define (stream/empty? s)
  (semaphore-wait (stream/semaphore s))
  (let ((b (EMPTY? s)))
    (semaphore-post (stream/semaphore s))
    b))
(define (stream/nonempty? s) (not (stream/empty? s)))

(define-syntax-rule (HEAD/NONEMPTY? s) (< (stream/next s) (stream/head/length s)))

;; Return the number n >= 0 of outstanding items enqueued in stream s.
(define (stream/length s)
  (semaphore-wait s)
  (let ((n (+ (- (stream/head/length s) (stream/next s)) (vector/length (stream/tail s)))))
    (semaphore-post s)
    n))

;; Add item x to the end of stream s.
(define (stream/enqueue s x)
  (semaphore-wait (stream/semaphore s))
  (cond
    ((EMPTY? s)
     (vector-set! s (HEAD) (vector/cons vector/null x))
     (vector-set! s (NEXT) 0)
     (vector-set! s (HEAD/LENGTH) 1))
    ((vector/null? (stream/head s))
     (vector-set! s (HEAD) (stream/tail s))
     (vector-set! s (NEXT) 0)
     (vector-set! s (HEAD/LENGTH) (vector/length (stream/tail s)))
     (vector-set! s (TAIL) (vector/cons vector/null x)))
    (else
     (vector-set! s (TAIL) (vector/cons (stream/tail s) x))))
  (semaphore-post (stream/semaphore s)))

;; Peek at the item at the head of stream s without removing it from the stream.
;; If the stream is empty then failure is returned.
(define (stream/peek s failure)
  (semaphore-wait (stream/semaphore s))
  (let ((x
         (cond
           ((HEAD/NONEMPTY? s)                      ; Peek at the head.
            (vector/ref (stream/head s) (stream/next s)))  ; Read the value.

           ((not (vector/null? (stream/tail s))) ; The head is empty but the tail is nonempty.
            (vector/ref (stream/tail s) 0))      ; Read the value.

           (else failure)))) ; Both the head and tail are empty.
    (semaphore-post (stream/semaphore s))
    x))
  

;; Remove and return the next item in the stream.
;; If the stream is empty then failure is returned.
(define (stream/dequeue s failure)
  (semaphore-wait (stream/semaphore s))
  (let ((x
         (cond
           ((HEAD/NONEMPTY? s)                               ; Take a value from the head.
            (let ((v (vector/ref (stream/head s) (stream/next s)))) ; Grab the value.
              (vector-set! s (NEXT) (add1 (stream/next s)))         ; Increment the head index.
              v))
           ((not (vector/null? (stream/tail s))) ; The head is empty but we have a nonempty stream tail.
            (let* ((tail (stream/tail s))
                   (v (vector/ref tail 0)))      ; Grab the first value of the tail.
              (vector-set! s (HEAD) tail)        ; Make the tail the new head.
              (vector-set! s (NEXT) 1)           ; We just grabbed the value at index 0.
              (vector-set! s (HEAD/LENGTH) (vector/length tail)) ; Set the length of the new head.
              (vector-set! s (TAIL) vector/null) ; Reset the tail to vector/null.
              v))
           (else failure)))) ; Both the head and tail are empty.
    (semaphore-post (stream/semaphore s))
    x))

