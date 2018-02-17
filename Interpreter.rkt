#lang racket

; We want to use multiple files, at least in development, to make VCS easier
; To do this, we require racket/include in this main Interpreter.rkt file
; Every other .rkt file we want to include should add '(require "otherfile.rkt)'
; In the other .rkt file, you must have '(provide myfunctionname)' for each of the functions in that file
(require racket/include)
(require "mathOperators.rkt")
(require "comparisonOperators.rkt")
(require "assignDeclare.rkt")

; Alex Marshall awm48
; Nathan Walls nfw10
; Anna Burkhart alb171
; Main Interpreter


(define stateInit
  (lambda ()
    ('())))

(define stateAdd
  (lambda (item value state)
    (cond
      ((null? item) error)
      ((null? value) error)
      (else (cons (list item value) (state))))))

(define stateRemove
  (lambda (item state)
    (cond
      ((eq? (caar state) item) (cdr state))
      (else (stateRemove item (cdr state))))))

(define stateGet
  (lambda (item state)
    (cond
      ((eq? (caar state) item) (cadr state))
      ((null? (cdr state)) error)
      (else (stateGet item (cdr state))))))

(define Mvalue.atom
  (lambda (a state)
    (cond
      ((number? a) a)
      ((boolean? a) a)
      ((eq? a 'true) #t)
      ((eq? a 'false) #f)
      (else (stateGet a state)))))