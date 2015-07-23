#lang racket/base

(require
  "../baseline.rkt"
  "../persistent/environ.rkt"
  "examples-base.rkt")

(provide EXAMPLES/ENVIRON)

(define EXAMPLES/ENVIRON
  (pairs-to-environ
   environ/null
   (list
    (define/global/1 'log/info islet/log/info)
    (define/global/1 'log/error islet/log/info)
    (define/global/1 'log/debug islet/log/info)
    (define/global/1 'log/warning islet/log/info)
    )))