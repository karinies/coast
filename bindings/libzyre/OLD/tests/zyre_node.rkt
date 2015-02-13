#lang racket/base

(require
 (only-in
  "../../libczmq/czmq.rkt"
  zcontext/new zsocket/new zcontext/destroy)
 (only-in "../zyre_node.rkt" zyre/node/new zyre/node/destroy))

(provide zyre/node/test)

(define (zyre/node/test)
  (printf " * zyre_node: ")
  (let* ((context (zcontext/new))
         (pipe (zsocket/new context 'PAIR))
         (node (zyre/node/new context pipe)))
    (zyre/node/destroy node)
    (zcontext/destroy context)
    (printf "OK\n")))

;; To execute the test use the shell command
;;    racket -l racket/base -t zyre_node.rkt -e '(zyre/node/test)' &
;; This guarantees that any IP connections created during the test will be closed
;; when the test process halts.