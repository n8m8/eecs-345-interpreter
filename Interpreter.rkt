#lang racket

; We want to use multiple files, at least in development, to make VCS easier
; To do this, we require racket/include in this main Interpreter.rkt file
; Every other .rkt file we want to include should add '(require "otherfile.rkt)'
; In the other .rkt file, you must have '(provide myfunctionname)' for each of the functions in that file
(require racket/include)
(require "mathOperators.rkt")
(require "comparisonOperators.rkt")
(require "assignDeclare.rkt")
(require "simpleParser.scm")

; Alex Marshall awm48
; Nathan Walls nfw10
; Anna Burkhart alb171
; Main Interpreter

(define interpret
  (lambda (file_name)
    (Mstate.statement (parser file_name) '(()())  )))

(define Mstate.statement
  (lambda (tree state)
    (cond
      ((null? tree) state)
      ((equal? (firststatement tree) 'return) (M_Bool (cadar tree)))
      ((eq? (firststatement tree) 'var) (Mstate.declare (car tree) state))
      ;((eq? (firststatement tree) 'if) (doifstuff))
      ;((eq? (firststatement tree) 'while) (dowhilestuff))
      ((eq? (firststatement tree) '=) (Mstate.assign (car tree)))
      (else (error)))))

(define Mstate.return
  (lambda (s state)
    'notimplementedException
    ))

(define Mstate.init
  (lambda (s state)
    'notimplementedException
    ))

(define Mstate.if
  (lambda (s state)
    'notimplementedException
    ))

(define Mstate.while
  (lambda (s state)
    'notimplementedException
    ))

(define Mstate.assign
  (lambda (s state)
    (cond
      ((null? s) state)
      (else (assign (car s) (cdr s) state)))))

(define Mstate.declare
  (lambda (d state)
    (cond
      ((null? d) state)
      (else (declare (car d) (cdr d))))))

(define declare
  (lambda (var state)
    (cond
      ((eq? (stateGet var state) ('itemDoesNotExist)) (stateAdd var state))
      (else ((stateRemove var state) (stateAdd var state))))))

(define assign
  (lambda (var value state)
    (cond
      ((null? state) 'noStateException)
      ((eq? (stateGet var state) 'itemDoesNotExist) 'variableToAssignWasntDeclaredException)
      (else ((stateRemove var state) (stateAdd var value state))))))

(define firststatement caar)
(define operator car)

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
      ((null? state) 'itemDoesNotExist)
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

;Takes an operator as its input
;It then matches up the operator with the closest operator in Scheme
(define M_Bool
  (lambda (op)
    (cond
      ((null? op) '())
      ((eq? op '+) +)
      ((eq? op '-) -)
      ((eq? op '*) *)
      ((eq? op '/) quotient)
      ((eq? op '%) remainder)
      ((eq? op '==) =)
      ((eq? op '!=) !=)
      ((eq? op '<) <)
      ((eq? op '>) >)
      ((eq? op '<=) <=)
      ((eq? op '>=) >=)
      ((eq? op '&&) (lambda (a b) (and a b)))
      ((eq? op '||) (lambda (a b) (or  a b)))
      ((eq? op '!) not)
      ((number? op) op)
      (else (error "Unidentified operator")))))

;There is no != in Scheme so Imma make my own
(define !=
  (lambda (x y)
    (not (= x y))))

(interpret "tests1/assignTest.lmao")
