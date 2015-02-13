#! /usr/bin/env racket
#lang racket/base

(require "../libcurl/libcurl.rkt")
(provide (all-defined-out)
         (all-from-out "../libcurl/libcurl.rkt"))

(define l1 (curl-slist-new "Hello"))
(CURL-SList-data l1)
(define l2 (curl-slist-new "Hello" "World"))
(string-append (CURL-SList-data l2) (CURL-SList-data (CURL-SList-next l2)))