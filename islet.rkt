#lang racket/base

;; Copyright 2013 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Defines the structure of an actor and provide base primitives
;; to create and manipulate actors.

(require
 racket/contract/base
 "Island/base.rkt"
 "Island/accessor.rkt"
 "Island/archipelago.rkt"
 "Island/keystore.rkt"
 "Island/logger.rkt"
 "curve.rkt"
 "getters.rkt"
 "time.rkt"
 "uuid.rkt"
 "persistent/environ.rkt"
 "transport/gate.rkt")

;; Contract for thunks (zero-argument closures).
;(define thunk/c
;  (flat-named-contract
;   'thunk
;   (and/c
;    procedure?
;    (lambda (f)
;      (let ((n (procedure-arity f)))
;        (and (exact-integer? n) (zero? n))))))) 

(provide
 (contract-out
  [struct islet
    ([id symbol?] ; Island-unique identifier.
     [home island?] ; The island on which the islet resides.
     [created time/utc?] ; Timestamp.
     [nickname (or/c symbol? kp/base64/c)] ; For debugging and logging.
     [thread thread?] ; Animus of islet 
     [trust trust/c] ; Trust ranking.
     [site/engine #f] ; Execution site engine. Unused for now.
     [site/global environ?] ; Execution site global binding environment.
     [environ (box/c environ? #:immutable #f)])]
     ;[environ environ?])] ; Contains islet-specific parameters and properties.
  

  [islet/new       (-> island? symbol? trust/c environ? environ? islet?)]
  [subislet/new    (-> symbol? trust/c environ? islet?)]
  [islet/faux      (-> island? symbol? trust/c islet?)]
  [islet/jumpstart (-> islet? thunk/c void?)]
  
  [islet/pretty     (-> islet? (and/c vector? immutable?))]
  [islet/dead?      (-> islet? boolean?)]
  [islet/alive?     (-> islet? boolean?)]
  [islet/suspended? (-> islet? boolean?)]
  [event/islet/dead (-> islet? evt?)]
  [islet/kill       (-> islet? void?)]
  [islet/self?      (-> islet? boolean?)]
  [islet/name/make  (-> island? (or/c string? symbol?) symbol?)]
  [this/island (-> island?)]
  [this/island/nickname (-> symbol?)]
  [this/islet/nickname  (-> (or/c symbol? kp/base64/c))]
  [this/islet/trust (-> trust/c)]
  [this/accessors (-> accessor/c)]
  [this/archipelago (-> archipelago/c)]
  [this/curve (-> curve?)]
  [this/keystore (-> keystore/c)]
  
  [gate/trust (-> trust/c gate:trust?)])

 this/islet
 islet/id
 islet/home
 islet/created
 islet/nickname
 islet/thread
 islet/trust
 islet/site/global
 islet/environ
 
 ; Trust rankings for islets.
 trust/c
 TRUST/LOWEST
 TRUST/LOW
 TRUST/MODERATE
 TRUST/HIGH
 TRUST/HIGHEST
 gate:trust?)

(struct islet
  (id        ; UUID
   (home #:mutable) ; Island on which the islet resides.
   created   ; Timestamp.
   nickname  ; For debugging and logging.
   thread    ; Animus of islet.
   trust     ; Trust ranking.
   site/engine ; Execution site engine (unused for now).
   site/global ; Execution site global binding environment.
   environ)    ; Islet-specific binding environment.
  #:transparent)
(struct/getters/define
 islet id home created nickname thread trust site/engine site/global environ)
;; Note: the home field is mutable so that we can cut circular references
;; between an island its islets.

;; Trust rankings.
;; Omer Ben-Shalom et. al. "Granular Trust Model Improves Enterprise Security,"
;; IT@Intel White Paper, November 2012.
(define TRUST/LOWEST   1)
(define TRUST/LOW      2)
(define TRUST/MODERATE 3)
(define TRUST/HIGH     4)
(define TRUST/HIGHEST  5)

(define trust/c (flat-named-contract 'trust (integer-in TRUST/LOWEST TRUST/HIGHEST)))

;; Sensitivity ratings.
;; Omer Ben-Shalom et. al. "Granular Trust Model Improves Enterprise Security,"
;; IT@Intel White Paper, November 2012.
;; Unused for now.
(define SENSITIVE/LOWEST   1) ; Very low consequence
(define SENSITIVE/LOW      2) ; Low consequence
(define SENSITIVE/MODERATE 3) ; Moderate consequence
(define SENSITIVE/HIGH     4) ; High consequence
(define SENSITIVE/HIGHEST  5) ; Very high consequence

;; If the actor is executing trusted code the environ element of the actor structure
;; may or may not be directly accessible to the trusted code.
;; If the actor is executing visiting Motile code then the environ is NOT directly accessible
;; but a select portion of its contents may be revealed by one or more functions
;; in the binding environment B of the execution site of the actor.
;; In general the environ of the actor structure contains state information and functions
;; that are required by portions of the Island infrastructure or by functions
;; within B delivering higher-level services. In particular the actor environ may contain:
;;  * (this/curve) - returns the public key of island executing the actor
;;  * (this/clans) - a list of clan structures for the containing clans of the actor starting with the root clan
;;  * (this/keyset) - returns the keyset of the island (for selected actors only)
;;  * one or more budgets assigned to the actor for use counts, rate limits, and such
;;  * debugging or logging information

;; Contrary to Racket threads, islet creation and actor start
;; are two separate and distinct operations. It simplifies
;; the definition of actors that are mutually dependent, helping
;; to eliminate race conditions among those islets as they come to life.
;; Starting an islet is actually a three step process as the islet must
;; have access to its own defining structure.
;; May be used by trusted code to generate an islet whose islet/id is known in advance.


(define SITE/ENGINE #f)
(define this/islet (make-parameter #f))

;; i - island on which islet resides
;; s - name to be assigned to islet
(define (islet/name/make i s)
  (string->symbol (format "~a@~a" s (island/nickname i))))

(define (islet/new i name trust global environ)
  (let* ([t (thread (lambda () ((thread-receive))))] ; Step 1.
         [a
          (islet
           (uuid/symbol) i (time/now)
           (islet/name/make i name) t trust SITE/ENGINE global (box environ))]
         [shrinkwrap
          (lambda ()
            (with-handlers
                ([exn:fail?
                  (lambda (e)
                    (log/info (this/islet/nickname) "fatal exception" (exn-message e))
                    #f)])
              (parameterize ([this/islet a]) ; Self reference.
                (let ([jumpstart (thread-receive)])
                  (when (procedure? jumpstart) (jumpstart))))))])
    (thread-send t shrinkwrap) ; Step 2
    a))

(define (islet/faux i nickname trust)
  (islet (uuid/symbol) i (time/now) nickname (current-thread) trust SITE/ENGINE environ/null environ/null))

;; Convenient shorthand when an islet wants to create a subordinate worker islet.
(define (subislet/new nickname trust global)
  (and
   ; We can't trust the subordinate any more than we trust the creator.
   (<= trust (this/islet/trust))
   (islet/new (this/island) nickname trust global environ/null)))

;; Step 3, the last and final step, for islet start seen at user level.
;; a - islet to jumpstart
;; thunk - closure for isle to execute
(define (islet/jumpstart a thunk)
  (thread-send (islet/thread a) thunk))

;; Returns an easy to read, printable representation of islet x.
(define (islet/pretty x)
  (vector-immutable
   '<islet>
   (cons 'nickname (islet/nickname x))
   (cons 'created (time-to-ISO8601 (islet/created x)))))

;; Tests for the execution state of an islet.
(define (islet/dead? x)
  (thread-dead? (islet/thread x)))

(define (islet/alive? x)
  (thread-running? (islet/thread x)))

(define (islet/suspended? x)
  (let ((t (islet/thread x)))
    (and (not (thread-dead? t)) (not (thread-running? t)))))

;; To be effective the thread x issuing the kill must be in the
;; same or an ancestor custodian of the immediate custodian of the thread of islet a.
(define (islet/kill a)
  (kill-thread (islet/thread a))
  ; Smash any potential cycles between an island and a resident islet.
  (set-islet-home! a #f))

;; Returns an event for actor death usable by (sync ...).
(define (event/islet/dead a)
  (thread-dead-evt (islet/thread a)))

;; Returns #t if this islet is actually x and #f otherwise.
(define (islet/self? x)
  (eq? (this/islet) x))

;; When called by an islet returns the island on which the islet resides.
(define (this/island) (islet/home (this/islet)))
(define (this/island/nickname) (island/nickname (this/island)))
(define (this/islet/nickname)  (islet/nickname (this/islet)))
(define (this/islet/trust)     (islet/trust (this/islet)))

(define (this/accessors) (island/accessors (islet/home (this/islet))))
(define (this/curve)     (island/keys      (islet/home (this/islet))))
(define (this/keystore)  (island/keystore  (islet/home (this/islet))))
(define (this/archipelago) (island/archipelago (islet/home (this/islet))))

(struct gate:trust gate ())
(define (gate/trust t)
  (gate:trust
   'gate:trust
   (lambda (_message) (>= (islet/trust (this/islet)) t))
   (lambda () #f)
   (lambda () (vector-immutable 'gate:trust t))
   #f))
        