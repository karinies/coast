#lang racket/base

(require
 racket/string
 racket/contract/base)


(provide
 island/log/level/set
 log/name
 (rename-out [log/name log/name/build]) ; DEPRECATED.
 (contract-out
  [island/logger/new (-> symbol? logger?)]
  [log/debug   (-> symbol? string? any/c void?)]
  [log/info    (-> symbol? string? any/c void?)]
  [log/warning (-> symbol? string? any/c void?)]
  [log/error   (-> symbol? string? any/c void?)]
  [log/fatal   (-> symbol? string? any/c void?)]))


;; Create a single root logger for one or more islands
;; running in a single instance of Racket.
(define LOGGER (make-logger 'island #f))
(define LOG/RECEIVER (make-log-receiver LOGGER 'debug))
(define LOG/REPORTER
  (thread
   (lambda ()
     (let loop ([item (sync LOG/RECEIVER)])
       (write item (current-error-port)) ; #(<level> <message> <data> <nickname>)
       (newline (current-error-port))
       (loop (sync LOG/RECEIVER))))))

;; Create a logger instance for the island with the given nickname.
(define (island/logger/new nickname) (make-logger nickname LOGGER))
;; rest is (level_1 name_1 ... level_n name_n)
(define (island/log/level/set . rest)
  (set! LOG/RECEIVER (apply make-log-receiver LOGGER rest)))

;; Level-specific log messages from low level to high.
(define (log/debug nickname message data)
  (when (log-level? LOGGER 'debug)
    (log-message LOGGER 'debug nickname message data)))
(define (log/info nickname message data)
  (when (log-level? LOGGER 'info)
    (log-message LOGGER 'info nickname message data)))
(define (log/warning nickname message data)
  (when (log-level? LOGGER 'warning)
    (log-message LOGGER 'warning nickname message data)))
(define (log/error nickname message data)
  (when (log-level? LOGGER 'error)
    (log-message LOGGER 'error nickname message data)))
(define (log/fatal nickname message data)
  (when (log-level? LOGGER 'fatal)
    (log-message LOGGER 'fatal nickname message data)))

(define (log/name island . components)
  (let ([strings (map (lambda (c) (if (string? c) c (symbol->string c))) components)])
    (string->symbol (format "~a@~a" (string-join strings ".") island))))
        
        
  


;; Test.
;(define logger/alice (island/logger/new 'alice))
;(define logger/bob   (island/logger/new 'bob))
;(log/debug 'alice "sorry alice" 99)
;(log/debug 'bob "ok bob" 103)
