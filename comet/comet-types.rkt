#lang racket/base

(provide
 COMET/CURL/NEW
 COMET/CURL/SEND
 COMET/CURL/RECEIVE
 COMET/CURL/TRANSFER
 COMET/ISLAND/START
 COMET/CURL/RECEIVE/FAILED)

(define COMET/CURL/NEW "curl-new")
(define COMET/CURL/SEND "curl-send")
(define COMET/CURL/RECEIVE "curl-receive")
(define COMET/CURL/TRANSFER "curl-transfer")
(define COMET/ISLAND/START "island-start")
;(define COMET/CURL/RECEIVE/REVOKED "curl-receive-revoked")
(define COMET/CURL/RECEIVE/FAILED "curl-receive-failed") ; Message wasn't received.