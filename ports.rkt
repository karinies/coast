#lang racket/base

(require scheme/foreign)
(unsafe!)

(provide
 file-descriptor-to-input-port
 file/descriptor-to-port/input
 file/descriptor-to-ports
 file-descriptor-to-semaphore
 MZFD_CREATE_READ)

(define file-descriptor-to-input-port
  (get-ffi-obj "scheme_make_fd_input_port" #f  (_fun _int _scheme _bool _bool      -> _scheme)))

(define file-descriptor-to-output-port
  (get-ffi-obj "scheme_make_fd_output_port" #f (_fun _int _scheme _int _int _int -> _scheme)))

(define file-descriptor-to-semaphore
  ;(get-ffi-obj "scheme_fd_to_semaphore" #f (_fun (_ptr i _int) _int _int -> _scheme)))
  (get-ffi-obj "scheme_fd_to_semaphore" #f (_fun _intptr _int _bool -> _racket)))
(define MZFD_CREATE_READ 1)


(define (file/descriptor-to-port/input fd name)
  (file-descriptor-to-input-port
   fd name
   #f   ; NOT a regular file hence capable of blocking.
   #f)) ; For Windows only to autoconvert CRLF to LF.

(define (file/descriptor-to-ports fd name)
  (file-descriptor-to-output-port
   fd name
   0  ; Not a regular file hence capable of blocking
   0  ; For Windows only to autoconvert CRLF to LF
   1)); Used for reading as well. Returns values (in out).


;; raw - raw Unix file descriptor
;; name - symbol used to name the port
;; Returns a Racket input port based on the given Unix file descriptor.
;(define (racket/port/unix/input/new raw name)
;  (file-descriptor-to-input-port
;   raw name
;   0   ; NOT a regular file hence capable of blocking.
;   0)) ; For Windows only to autoconvert CRLF to LF.

;; Returns a Racket output port based on the given Unix file descriptor.
(define (racket/port/unix/output/new raw name)
  (file-descriptor-to-output-port
   raw name
   0  ; Not a regular file hence capable of blocking
   0  ; For Windows only to autoconvert CRLF to LF
   0)); NOT used for reading as well.

;; Returns a pair of ports (input/output) as (values in out) based on the given
;; Unix file descriptor.
(define (racket/port/unix/both/new raw name)
  (file-descriptor-to-output-port
   raw name
   0   ; Not a regular file hence capable of blocking.
   0   ; For Windows only to autoconvert CRLF to LF.
   1)) ; To be used for reading as well as writing.

;(module fd mzscheme
;    (require (lib "foreign.ss")) (unsafe!)
;    (provide fd->input-port
;             fd->output-port)
;    
;    (define (fd->input-port fd name)
;      (scheme_make_fd_input_port fd name 0 0))
;
;    (define (fd->output-port fd name)
;      (scheme_make_fd_output_port fd name 0 0 0))
;
;    (define scheme_make_fd_input_port
;      (get-ffi-obj "scheme_make_fd_input_port" #f
;                   (_fun _int _scheme _int _int -> _scheme)))
;
;    (define scheme_make_fd_output_port
;      (get-ffi-obj "scheme_make_fd_output_port" #f
;                   (_fun _int _scheme _int _int _int -> _scheme))))