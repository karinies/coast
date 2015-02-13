#lang scheme

(provide
 list/set
 set/member?
 set/insert
 set/remove
 set/union
 set/difference
 set/sort)

;; A collection of utility functions for manipulating ordered sets of symbols.
;; Used primarily for closure construction in the compiler.

(define (set/member? symbol set)
  (if (memq symbol set) #t #f))

;; Returns #t if symbol alpha strictly precedes symbol beta in an alphabetic ordering and #f otherwise.
(define (symbol<? alpha beta)
  (string<?
   (symbol->string alpha)
   (symbol->string beta)))

(define (symbol<=? alpha beta)
  (string<=?
   (symbol->string alpha)
   (symbol->string beta)))

;; Insert a symbol into sorted set S and return the successor ordered set.
(define (set/insert symbol S)
  (cond
    ((not (pair? S)) (list symbol))
    ((eq? symbol (car S)) S)
    ((symbol<? symbol (car S)) (cons symbol S))
    (else (cons (car S) (set/insert symbol (cdr S))))))

;; Remove symbol from ordered set S and return the successor ordered set.
(define (set/remove symbol S)
  (cond
    ((not (pair? S)) null)
    ((eq? (car S) symbol) (cdr S))
    (else (cons (car S) (set/remove symbol (cdr S))))))

;; Returns the set union of two ordered sets of symbols, as an ordered set without duplicates.
(define (set/union alpha beta)
  ;; Merge two ordered sets discarding duplicates.
  (define (merge left right outcome)
    (cond
      ((and (pair? left) (pair? right))
       (cond
         ((eq? (car left) (car right))
          (merge (cdr left) (cdr right) (cons (car left) outcome)))
         ((symbol<? (car left) (car right))
          (merge (cdr left) right (cons (car left) outcome)))
         (else
          (merge left (cdr right) (cons (car right) outcome)))))
      ((null? left)
       (append (reverse outcome) right))
      (else
       (append (reverse outcome) left))))
  
  (merge alpha beta null))
  
;; Returns the set difference of two ordered sets, alpha and beta, as an ordered set.
(define (set/difference alpha beta)
  (define (difference left right outcome)
    (cond
      ((and (pair? left) (pair? right))
       (cond
         ((eq? (car left) (car right))
          (difference (cdr left) (cdr right) outcome))
         ((symbol<? (car left) (car right))
          (difference (cdr left) right (cons (car left) outcome)))
         (else
          (difference left (cdr right) outcome))))
      ((null? left)
       (reverse outcome))
      (else
       (append (reverse outcome) left))))

  (difference alpha beta null))

(define (set/sort set)
  (cond
    ((null? set) null)       ; Empty set.
    ((null? (cdr set)) set)  ; Singleton set.
    (else (sort set symbol<?))))

;; Convert  a list of symbols (possibly containing duplicates) to an ordered duplicate-free set.
(define (list/set source)
  (let loop ((rest (set/sort source))
             (target null))
    (if (null? rest)
        target
        (loop (cdr rest) (set/insert (car rest) target)))))
      
