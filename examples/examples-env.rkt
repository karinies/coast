#lang racket/base

(require
  "../baseline.rkt"
  "../persistent/environ.rkt"
  "../islet.rkt"
  "examples-base.rkt"
  "market-notify/order-router-req.rkt")

(provide EXAMPLES/ENVIRON)

(define EXAMPLES/ENVIRON
  (pairs-to-environ
   BASELINE/SPAWN
   (list
    (define/global/N 'islet/log/info islet/log/info)
    (define/global/N 'islet/log/error islet/log/error)
    (define/global/N 'islet/log/debug islet/log/debug)
    (define/global/N 'islet/log/warning islet/log/warning)
    (define/global/N 'environ/merge environ/merge)
    (define/global/1 'islet/environ islet/environ)
    (define/global/0 'this/islet this/islet)
    (define/global/1 'order-request/pretty order-request/pretty)
    (define/global/1 'order-exec-report/pretty order-exec-report/pretty)
    )))
