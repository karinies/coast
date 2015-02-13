#lang racket/base

(require racket/pretty)
(require mzlib/defmacro)

;(define-macro (macro/for loopers . body)
  
  ; Extract the initial values from the ranges
  ; and adjust each range by slicing off the first value.
  (define (initials/make ranges)
    (let loop ((initials null) (ranges ranges))
      (cond
        ((null? ranges) (reverse initials))
        ((car ranges)
         =>
         (lambda (range) (loop (cons (car range) initials) (cdr ranges)))))))
  
  (define (for/generate loopers body)
    (let* ((loopers (loopers/unzip loopers))
           (variables (car loopers))
           (ranges    (cadr loopers)))
      (for/subgenerate variables ranges body)))
  
  
  ; Generate the following let loop.
  ;  (let loop ((a a_1) (b b_1) ... (r_1 (a_2 ...)) (r_2 (b_2 ...)) ...)
  ;    body
  ;    (when (not (or (null? r_1) (null? r_2) ...))
  ;      (loop (car r_1) (car r_2) ... (cdr r_1) (cdr r_2) ...)))
  (define (for/subgenerate variables ranges body)
    (let* ((range/names (map (lambda (s) (gensym s)) variables)) ; Variable names for the ranges.
           (loop (gensym 'loop))
           (initials (initials/make ranges)) ; Generate initial values for loop index variables.
           (ranges (map cdr ranges))) ; Generate initial values 
      `(let
           ,loop
         (,@(map (lambda (n v) (list n v)) variables initials)
          ,@(map (lambda (r v) `(,r (quote ,v))) range/names ranges))
         ,@body
         (when (not (or ,@(map (lambda (r) `(null? ,r)) range/names)))
           (,loop ,@(map (lambda (r) `(car ,r)) range/names) ,@(map (lambda (r) `(cdr ,r)) range/names))))))
  
  ; Unzips each looper L_i (n_i (v_1 ... v_m) in the list of loopers (L_1 ... L_n).
  (define (loopers/unzip loopers)
    (let loop ((loopers loopers) (variables null) (ranges null))
      (cond
        ((null? loopers) (list (reverse variables) (reverse ranges)))
        (else
         (let* ((looper (car loopers))
                (variable (car looper))
                (range    (cadr looper)))
           (loop (cdr loopers) (cons variable variables) (cons range ranges)))))))
  
  ;(for/generate loopers body))

  
  (define (for/test/1)
    (pretty-print
     (for/generate
      '((i (1 2 3)) (j (10 20 30)))
      '((foo i) (bar j)))))