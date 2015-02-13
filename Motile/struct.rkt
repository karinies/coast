#lang racket/base

(require "persistent/hash.rkt"
         (for-syntax racket/base)
         (for-syntax racket/function))
(provide define-motile-struct)

(define TT "__TYPE__")

(define-for-syntax syntax->string (compose symbol->string syntax->datum))
(define-for-syntax (strings->syntax kstx . strs)
  (datum->syntax kstx (string->symbol (apply string-append strs))))

(define-syntax (define-motile-struct stx)
  (syntax-case stx ()
    [(k id (fields ...))
     (let ([kstx #'k]
           [idstr (syntax->string #'id)])
       (with-syntax ([id? (strings->syntax kstx idstr "?")]
                     [(accessors ...)
                      (map (compose (curry strings->syntax kstx idstr ".") syntax->string)
                           (syntax->list #'(fields ...)))]
                     [(setters ...)
                      (map (compose (curry strings->syntax kstx idstr "!") syntax->string)
                           (syntax->list #'(fields ...)))]
                     [(args ...) (generate-temporaries #'(fields ...))])
         #'(begin
             (define (id? v)
               (and (hash/persist? v)
                    (equal? 'id (hash/ref v TT #f))
                    (hash/contains? v (symbol->string 'fields))
                    ...))
             (define (id args ...)
               (pairs/hash hash/equal/null (list (cons TT 'id)
                                                 (cons (symbol->string 'fields) args)
                                                 ...)))
             (define (accessors v)
               (hash/ref v (symbol->string 'fields) #f))
             ...
             (define (setters v k)
               (hash/new v (symbol->string 'fields) k))
             ...)))]))