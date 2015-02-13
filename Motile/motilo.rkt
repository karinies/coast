#! /usr/bin/env racket

#lang racket/base

(require
 (only-in racket/function curry)
 (only-in "compile/compile.rkt" motile/compile)
 (only-in "compile/serialize.rkt" motile/serialize))

(provide
 motile/file/compile
 motile/port/compile)

(define (motile/file/compile name)
  (with-handlers
      ([exn:fail?
        (lambda (e) (log-error (format "motlile/file/compile: file: ~s ~s\n" name (exn-message e))))])
    (let* ((path (string->path name))
           (in  (open-input-file path))
           (out (open-output-file (path-replace-suffix path #".mag") #:mode 'binary #:exists 'replace)))
      (motile/port/compile in out)
      (close-input-port in)
      (close-output-port out))))


(define (motile/port/compile in out)
  (write (motile/serialize (motile/compile (read in))) out))

(define CLargs (map (curry regexp-split #rx"=") (vector->list (current-command-line-arguments))))
(display CLargs) (newline)

(define (argsassoc key #:call [call (λ (x) x)] #:no-val [no-val #f] #:default [default #t])
  (let ([entry (assoc key CLargs)])
    (if entry
        (if (> (length entry) 1)
            (call (cadr entry))
            default)
        no-val)))

;(define *LISTENING-ON* (argsassoc "--host" #:no-val *LOCALHOST*))
;(define *LOCALPORT* (argsassoc "--port" #:no-val 5000 #:call string->number))


