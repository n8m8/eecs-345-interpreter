#lang racket

; Alex Marshall awm48
; Nathan Walls nfw10
; Anna Burkhart alb171
; EECS 345 - Main Interpreter Part I

; We want to use multiple files, at least in development, to make VCS easier
; To do this, we require racket/include in this main Interpreter.rkt file
; Every other .rkt file we want to include should add '(require "otherfile.rkt)'
; In the other .rkt file, you must have '(provide myfunctionname)' for each of the functions in that file

(require "simpleParser.scm")

;====================================================
; Makes more code more readable and was easier to add but technically could be omitted and could just call
; explicitly in every instance (ex. (caar x))

(define firststatement caar)
(define operator car)
(define operand1 cadr)
(define operand2 caddr)

;====================================================

; Input is the return parse tree from simpleParser and the intial state is '(() ()) so that our binding pairs will
; be stored in two lists
(define interpret
  (lambda (file_name)
    (statement (parser file_name) '(()())  )))

; Check what kind of statement and interpret accordingly (ex. return, var, if, while, or =)
(define statement
  (lambda (tree state)
    (cond
      ((null? tree) state)
      ((equal? (firststatement tree) 'return) (return (cadar tree) state))
      ((eq? (firststatement tree) 'var) (statement (cdr tree) (declare (car tree) state)))
      ;((eq? (firststatement tree) 'if) (doifstuff))
      ;((eq? (firststatement tree) 'while) (dowhilestuff))
      ((eq? (firststatement tree) '=) (statement (cdr tree) (assign (car tree) state)))
      (else (error)))))


; ***Took out methods that had no function (we can add them back if needed)***


; If we reach this line we must at least have (var) so first condition checks for "var" + "number/letter"
; and returns the state if the "number/letter" is null, and otherwise calls declare on "var" + "number/letter"
(define declare
  (lambda (declaration state)
    (cond
      ((null? (cdr declaration)) state)
      (else (cons (cons (car (cdr declaration)) (car state)) (cdr state))))))

;Check if the variable is part of the state.
;If it is part of the state, remove it and its value
;Re-add the variable and new value to the state
(define assign
  (lambda (assignment state)
    (cond
      ((null? state) 'VariableNotDeclared)
<<<<<<< HEAD
      ((member*? (car (cdr assignment)) state) ('Dosomething))
      ;((eq? (stateGet var state) 'itemDoesNotExist) 'variableToAssignWasntDeclaredException) 
=======
      ((member*? (car (cdr assignment)) state) ((stateRemove (car (cdr assignment)) state) (stateAdd (car (cdr assignment)) (cdr (cdr assignment)) state)))
      ;((eq? (stateGet var state) 'itemDoesNotExist) 'variableToAssignWasntDeclaredException)
>>>>>>> 4ca4f3f764f462aa67e9a07a01bf5416f452fe4d
      ;(else ((stateRemove var state) (stateAdd var value state))))))
      (else 'triedToAssignNotANumber))))

(define Mstate.assign
  (lambda (s state)
    (cond
      ((null? s) state)
      ((eq? operator))
      (else (assign (car s) (cdr s) state)))))

(define stateAdd
  (lambda (item value state)
    (cond
      ((null? item) error)
      ((null? value) error)
      ((eq? (car state) '()) ((cons '() item) (cons '() value)))
      ((eq? (stateRemove item state) 'itemDoesNotExist) ((list (car state) item) (list (car (cdr state)) value )))
      (else (state)))))

(define stateRemove
  (lambda (item state)
    (cond
      ((null? (cadr state)) 'itemDoesNotExist)
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

;Evaluates mathmatical expressions
;The order of operations is +,-,*,/,%
(define expression
  (lambda (state expr)
    (cond
      ((null? expr) '())
      ((number? expr) expr)
<<<<<<< HEAD
      ((eq? '+ (operator expr)) (+ (expression state (operand1 expr)) (expression state (operand2 expr))))
      ((eq? '- (operator expr))
=======
      ;((null? (cdr expr)) (stateGet expr state))
      ((eq? '+ (car expr)) (+ (expression state (cadr expr)) (expression (caddr expr))))
      ((eq? '- (car expr))
>>>>>>> 4ca4f3f764f462aa67e9a07a01bf5416f452fe4d
       (if (isNegativeNumber expr)
           (- 0 (expression state (operand1 expr)))
           (- (expression state (operand1 expr)) (expression state (operand2 expr)))))
      ((eq? '* (operator expr)) (* (expression state (operand1 expr)) (expression state (operand2 expr))))
      ((eq? '/ (operator expr)) (quotient (expression state (operand1 expr)) (expression state (operand2 expr))))
      ((eq? '% (operator expr)) (remainder (expression state (operand1 expr)) (expression state (operand2 expr))))
      (else (error 'badoperation "Unknown operator")))))

(define isNegativeNumber
  (lambda (expr)
    (null? (cddr expr))))


; Takes an operator as its input
; It then matches up the operator with the closest operator in Scheme
(define return
  (lambda (op state)
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
      ((number? (stateGet op state)) (stateGet op state))
      (else (expression state op)))))

;There is no != in Scheme so Imma make my own
(define !=
  (lambda (x y)
    (not (= x y))))

;member*? - returns true if x is in lis, where lis can contain sublists
(define member*?
  (lambda (x lis)
    (cond
      ((null? lis) #f)
      ((pair? (car lis)) (or (member*? x (car lis)) (member*? x (cdr lis))))
      ((eq? x (car lis)) #t)
      (else (member*? x (cdr lis))))))

(interpret "tests1/assignTest.lmao")
