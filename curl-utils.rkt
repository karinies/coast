#lang racket/base

(require
  racket/contract/base
  (only-in "curl/curl.rkt" curl/zpl/safe-to-curl curl/zpl/load)
  (only-in "curl/base.rkt" curl?)
  "Island/keystore.rkt")

(provide
 (contract-out
  [curl/zpl/file/load (-> string? keystore/c (or/c curl? #f))]))

;; Creates a CURL reading from a file in ZPL format.
;; This is a shortcut for (curl/zpl/safe-to-curl ...) and (curl/cpl/load ...).
(define (curl/zpl/file/load filepath keystore)
  (curl/zpl/safe-to-curl 
   (call-with-input-file filepath
     (lambda (inputfile)
       (curl/zpl/load inputfile))) keystore))