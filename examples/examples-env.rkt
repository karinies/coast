#lang racket/base

(require
  "../baseline.rkt"
  "../persistent/environ.rkt"
  "examples-base.rkt")

(provide EXAMPLES/ENVIRON)

(define EXAMPLES/ENVIRON
  (pairs-to-environ
   BASELINE
   (list
    (define/global/N 'islet/log/info islet/log/info)
    (define/global/N 'islet/log/error islet/log/error)
    (define/global/N 'islet/log/debug islet/log/debug)
    (define/global/N 'islet/log/warning islet/log/warning)
    )))