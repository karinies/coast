#lang racket/base

(require
 racket/contract/base
 [only-in typed/racket/base assert]
 (rename-in
  srfi/19
  [time<=? srfi:time<=?] [time<?  srfi:time<?]
  [time=?  srfi:time=?]  [time>=? srfi:time>=?]
  [time>?  srfi:time>?])
)

(provide
 date?
 (rename-out
  [current-time time/now]
  [current-inexact-milliseconds time/now/milliseconds]
  [time-second  time/second]
  [time-nanosecond time/nanosecond]
  [current-date date/now]
  ; Accessors
  [date-year date/year]
  [date-month date/month]
  [date-day date/day]
  [date-hour date/hour]
  [date-minute date/minute]
  [date-second date/second]
  [date-zone-offset date/zone/offset]
  [date-year-day date/year/day]
  [date-week-day date/week/day]
  )

 (contract-out
  [duration? (-> any/c boolean?)]
  [time/utc? (-> any/c boolean?)]
  [epoch/milliseconds (-> exact-positive-integer?)]
  
  [date/millisecond (-> date? (integer-in 0 999))]

  ; Constructors
  [duration/new
   (case->
    (-> span/c                      duration?) ; seconds
    (-> span/c span/c               duration?) ; minutes seconds
    (-> span/c span/c span/c        duration?) ; hours minutes seconds
    (-> span/c span/c span/c span/c duration?))] ; days hours minutes seconds
  [date/new
   (-> exact-positive-integer? ; year
       (integer-in 1 12)       ; month
       (integer-in 1 31)       ; day
       (integer-in 0 23)       ; hour
       (integer-in 0 59)       ; minute
       (between/c 0.0 60.0)    ; second
       date?)]
  ; Temporal arithmetic.
  [time+
   (->i ([x (or/c duration? time/utc? date?)] [_ duration?])
        [result (or/c duration? time/utc? date?)]
        #:post (x result) (if (date? x) (date? result) (eq? (time-type x) (time-type result))))]
  [time-
   (->i ([x (or/c time/utc? date? duration?)] [y (or/c time/utc? date? duration?)])
        [result (or/c duration? time/utc? date?)]
        #:post (x y result)
        (if (duration? y)
            (cond
              [(time/utc? x) (time/utc? result)]
              [(date?     x) (date?     result)]
              [(duration? x) (duration? result)]
              [else #f])
            (duration? result)))]
  [date+ (-> date? duration? date?)]
  [date- (-> date? (or/c date? duration?) (or/c date? duration?))]

  ; Temporal comparisons.
  [time>=?
   (->i ([x (or/c time/utc? date? duration?)] [y (or/c time/utc? date? duration?)])
        #:pre (x y) (if (duration? x) (duration? y) (not (duration? y)))
        [_ boolean?])]
  [time>?
   (->i ([x (or/c time/utc? date? duration?)] [y (or/c time/utc? date? duration?)])
        #:pre (x y) (if (duration? x) (duration? y) (not (duration? y)))
        [_ boolean?])]
  [time=?
   (->i ([x (or/c time/utc? date? duration?)] [y (or/c time/utc? date? duration?)])
        #:pre (x y) (if (duration? x) (duration? y) (not (duration? y)))
        [_ boolean?])]
  [time<?
   (->i ([x (or/c time/utc? date? duration?)] [y (or/c time/utc? date? duration?)])
        #:pre (x y) (if (duration? x) (duration? y) (not (duration? y)))
        [_ boolean?])]
  [time<=?
   (->i ([x (or/c time/utc? date? duration?)] [y (or/c time/utc? date? duration?)])
        #:pre (x y) (if (duration? x) (duration? y) (not (duration? y)))
        [_ boolean?])]

  ; Conversions.
  [date-to-time (-> date? time/utc?)]
  [time-to-date (-> time/utc? date?)]
  [date-to-ISO8601 (-> date? string?)]
  [time-to-ISO8601 (-> time/utc? string?)]
  [ISO8601-to-date (-> string? date?)]
  [ISO8601-to-time (-> string? time/utc?)]
  [ISO8601/now     (-> string?)]
  [duration-to-milliseconds (-> duration? inexact?)]
  
  ; Serialization.
  [time/flatten (-> time/utc? vector?)]
  [time/flatten/ok? (-> vector? boolean?)]
  [time/unflatten (-> vector? time/utc?)]
  [date/flatten (-> date? vector?)]
  [date/flatten/ok? (-> vector? boolean?)]
  [date/unflatten (-> vector? date?)]
       
  )
 )
               
(define-syntax-rule (milli-to-nano m)      (* 1000000 m))
(define-syntax-rule (minutes-to-seconds m) (* m 60))
(define-syntax-rule (hours-to-seconds h)   (* h 3600))
(define-syntax-rule (days-to-seconds d)    (* d 86400))

(define span/c (flat-named-contract 'timespan (and/c real? (>=/c 0))))

;; Given fractional seconds x returns (values s m) where
;; s is exact seconds and m is exact milliseconds.
;; Example (seconds-to-exact 39.4972) => (values 39 497)
(define (seconds-to-exact x)
  (let* ([s (truncate x)]
         [m (round (* 1000 (- x s)))])
    (values (inexact->exact s) (inexact->exact m))))

;; Constructor for durations.
(define duration/new
  (case-lambda
    [(seconds)
     (let-values ([(s/exact m/exact) (seconds-to-exact seconds)])
       (make-time time-duration (milli-to-nano m/exact) s/exact))]
    [(minutes seconds)
     (let-values ([(s/exact m/exact) (seconds-to-exact (+ (minutes-to-seconds minutes) seconds))])
       (make-time time-duration (milli-to-nano m/exact) s/exact))]
    [(hours minutes seconds)
     (let-values ([(s/exact m/exact) (seconds-to-exact (+ (hours-to-seconds hours) (minutes-to-seconds minutes) seconds))])
         (make-time time-duration (milli-to-nano m/exact) s/exact))]
    [(days hours minutes seconds)
     (let-values
         ([(s/exact m/exact)
           (seconds-to-exact (+ (days-to-seconds days) (hours-to-seconds hours) (minutes-to-seconds minutes) seconds))])
       (make-time time-duration (milli-to-nano m/exact) s/exact))]))

;; Returns #t if x is a duration and #f otherwise.
(define (duration? x)
  (and (time? x) (eq? (time-type x) time-duration)))
(define (time/utc? x)
  (and (time? x) (eq? (time-type x) time-utc)))

;; Temporal arithmetic.
;; Always returns the type of its first argument which may be duration?, time/utc? or date?.
;; Returns the sum of a time t and a duration d as a time t + d
;; the sum of two durations, d and d' as a duration d + d'
;; or the sum of a date D and a duration d as date D + d.
(define (time+ a b)
  (cond
    [(time/utc? a) (add-duration a b)]
    [(duration? a) (add-duration a b)]
    [(date? a)     (date+ a b)]
    [else #f]))

;; Returns the difference of a time t and a duration d as t - d (a time),
;; the difference of a date D and a duration d as D - d (a date),
;; the difference of times t and t' as t - t' (a duration),
;; the difference of dates D and D' as D - D' (a duration),
;; or the difference of two durations d and d' as d - d' (a duration).
(define (time- a b)
  (cond
    [(time/utc? a)
     (cond
       [(time/utc? b) (time-difference a b)]
       [(date? b)     (time-difference a (date-to-time b))]
       [(duration? b) (subtract-duration a b)])]
    [(date? a) (date- a b)]
    [(duration? a) (and (duration? b) (subtract-duration a b))]))

(define (date+ a b) (time-utc->date (add-duration (date-to-time a) b)))

(define (date- a b)
  (cond
    [(duration? b) (time-utc->date (subtract-duration (date-to-time a) b))]
    [(date? b)     (time-difference (date-to-time a) (date-to-time b))]
    [(time/utc? b) (time-difference (date-to-time a) b)]))

(define (date-to-time d) (date->time-utc d))
(define (time-to-date t) (time-utc->date t))

;(define (date<=? a b) (time<=? (date-to-time a) (date-to-time b)))
;(define (date<?  a b) (time<?  (date-to-time a) (date-to-time b)))

(define-syntax-rule (comparison p?)
  (lambda (a b)
    (cond
      [(time? a)
       (cond
         [(time? b) (p? a b)]
         [(date? b) (p? a (date-to-time b))])]
      [(date? a)
       (cond
         [(time? b) (p? (date-to-time a) b)]
         [(date? b) (p? (date-to-time a) (date-to-time b))])]
      [(duration? a) (p? a b)])))
(define time<=? (comparison srfi:time<=?))
(define time<?  (comparison srfi:time<?))
(define time=?  (comparison srfi:time=?))
(define time>=? (comparison srfi:time>=?))
(define time>?  (comparison srfi:time>?))

(define (duration/positive? d) (positive? (time-second d)))
(define (duration/negative? d) (negative? (time-second d)))
(define (duration/zero? d)     (and (zero? (time-second d)) (zero? (time-nanosecond d))))

(define (date/millisecond d) (round (/ (date-nanosecond d) 1000.0)))

;; String conversion for dates
(define ISO8601 "~Y-~m-~dT~H:~M:~SZ") 
(define (date-to-ISO8601 d) (date->string d ISO8601))
(define (time-to-ISO8601 t) (date-to-ISO8601 (time-to-date t)))
;; YYYY-MM-DDThh:mm:ssZ
(define (ISO8601-to-date s) (string->date s ISO8601))
(define (ISO8601-to-time s) (date-to-time (string->date s ISO8601)))
;; Timestamp for log messages.
(define (ISO8601/now) (date-to-ISO8601 (current-date)))

;; UTC date constructor.
(define (date/new year month day hour minute seconds)
  (let-values ([(s/exact m/exact) (seconds-to-exact seconds)])
    (make-date (milli-to-nano m/exact) s/exact minute hour day month year 0)))

(define (duration-to-milliseconds d)
  (+ (* (time-second d) 1000) (/ (time-nanosecond d) 1000000.0)))

(define (epoch/milliseconds)
  (inexact->exact (round (current-inexact-milliseconds))))

;; Serialization.
(define (time/flatten t)
  (vector 'struct:time (time-nanosecond t) (time-second t)))
(define (time/flatten/ok? x)
  (and
   (vector? x)
   (= 3 (vector-length x))
   (eq? 'struct:time (vector-ref x 0))
   (exact-nonnegative-integer? (vector-ref x 1))   ; nanosecond
   (<= 0 (vector-ref x 1) 999999999)
   (exact-nonnegative-integer? (vector-ref x 2)))) ; second

(define (time/unflatten v)
  (make-time time-utc (vector-ref v 1) (vector-ref v 2)))

(define (date/flatten d)
  (let ([t (date-to-time d)])
    (vector 'struct:date (time-nanosecond t) (time-second t))))

(define (date/flatten/ok? x)
  (and
   (vector? x)
   (= 3 (vector-length x))
   (eq? 'struct:date (vector-ref x 0))
   (exact-nonnegative-integer? (vector-ref x 1))   ; nanosecond
   (<= 0 (vector-ref x 1) 999999999)
   (exact-nonnegative-integer? (vector-ref x 1)))) ; second   

(define (date/unflatten v)
  (time-to-date (time/unflatten v)))

;; Testing.
(define d/now (current-date))
(define t/now (current-time))
(define x/30 (duration/new 30))
(define x/29 (duration/new 29.12))
(define x/0 (duration/new 0))
(define t/future (time+ t/now x/30))