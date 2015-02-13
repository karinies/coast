#lang racket/base

(require
 (only-in srfi/1 unzip2)
 (only-in "../compile/match.rkt" shape/exact shape/inexact))

;; Copyright 2011 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Contact: mgorlick@acm.org

;; Motile persistent records.

(define (record/new/name x)   (cadr x))
(define (record/new/fields x) (cddr x))

;; Give a list (x_1 y_1 x_2 y_2 ...) returns #t if each x_i is a symbol and #f otherwise.
(define (initials/ok? initials)
  (cond
    ((null? initials) #t)
    ((symbol? (initial/tag initials)) (initials/ok? (initials/next initials)))
    (else #f)))

(define (initial/tag initials)        (car initials))
(define (initial/expression initials) (cadr initials))
(define (initials/next initials)      (cddr initials))

;; Given a list (x_1 y_1 x_2 y_2 ...) returns a pair of values (x_1 ...) (y_1 ...).
(define (initials/repack initials)
  (let loop ((initials initials) (tags null) (expressions null))
    (if (null? initials)
        (values (reverse tags) (reverse expressions))
        (loop (initials/next initials) (cons (initial/tag initials) tags) (cons (initial/expression initials) expressions)))))

;; Construct a record with the given name (a symbol) whose individual fields are named by the list of tags (tag_1 ... tag_n)
;; and whose initial values are given by the list (value_1 ... value_n).
;; Returns the new persistent record.
(define (record/build name tags values)
  (if (symbol? name)
      (let ((m (length tags))
            (n (length values)))
        (cond
          ((= m n) 
            (if (andmap symbol? tags)
                (let ((v (make-vector (+ 3 m))))
                  (vector-set! v 0 '<record>)
                  (vector-set! v 1 name)
                  (vector-set! v 2 (list->vector tags))
                  (let loop ((i 3) (values values))
                    (unless (null? values)
                      (vector-set! v i (car values))
                      (loop (add1 i) (cdr values))))
                  v)
                
                (error 'record/build "bad record field in ~a for record name: ~a" tags name)))
          
          ((< m n)
            (error 'record/build "fewer tags than values in record: ~a" name))

          (else (error 'record/build "fewer values than tags in record: ~a" name))))

      (error 'record/build "bad record name: ~a" name)))

;; Translate (record/new <name> tag_1 expression_1 tag_2 expression_2 ...)
;; where <name> is a symbol, tag_i a symbol and expression_i an arbitrary expression to:
;;   (record/build <name> '(tag_1 ...) (list expression_1 ...))
(define (record/new/translate x)
  (if (shape/inexact x 4)
      (let* ((name  (record/new/name x))
             (initials (record/new/fields x))
             (n (length initials)))
        (when (not (symbol? name))
          (error 'record/new "record name is not a symbol in ~a" x))
        (when (odd? n)
          (error 'record/new "odd number of field pairs in ~s" x))
        (when (not (initials/ok? initials))
          (error 'record/new "fields in ~s do not alternate symbol/expession: " name initials))
        (let-values ([(tags expressions) (initials/repack initials)])
          `(record/build (quote ,name) (quote ,tags) (list ,@expressions))))
        
      (error 'record/new "insufficient arguments: ~a" x)))
        
  