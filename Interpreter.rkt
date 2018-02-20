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
      ((equal? (firststatement tree) 'return) (fixtf (return (cadar tree) state)))
      ((eq? (firststatement tree) 'var) (statement (cdr tree) (declare (car tree) state)))
      ((eq? (firststatement tree) 'if) (statement (cdr tree) (ifstatement (cdr (car tree)) state)))  ;(cdr (car tree)) returns everything after 'if
      ((eq? (firststatement tree) 'while) (statement (cdr tree) (while (cdr (car tree)))))
      ((eq? (firststatement tree) '=) (statement (cdr tree) (assign (cdr (car tree)) state))) ;(cdr (car tree)) gets rid of = sign because it's not important
      (else (error)))))

(define fixtf
  (lambda (value)
    (cond
      ((eq? value #t) 'true)
      ((eq? value #f) 'false)
      (else value))))

(define while
  (lambda (stmnt state)
    (cond
      ((null? (getWhileCondition stmnt)) 'conditionCannotBeNull)
      ((eq? (return (getWhileCondition stmnt) state) #f) state)
      (else (while stmnt (statement stmnt state))))))

(define getWhileCondition (lambda (s) (car (cdr s))))
(define getWhileAction (lambda (s) (car (cdr (cdr s)))))

; If we reach this line we must at least have (var) so first condition checks for "var" + "number/letter"
; and returns the state if the "number/letter" is null, and otherwise calls declare on "var" + "number/letter"
(define declare
  (lambda (declaration state)
    (cond
      ((null? state) 'StateUndeclared) ;should never really be reached but here for safety
      ((null? (cddr declaration)) (list (cons (car (cdr declaration)) (car state)) (cons '() (car (cdr state)))))
      ((member*? (car (cdr declaration)) state) (error 'RedefiningError))
      (else (assign (cdr declaration) (list (cons (car (cdr declaration)) (car state)) (cons '() (car (cdr state))))))))) ;adds the variable to the state but not the value(ass

;Check if the variable is part of the state.
;If it is part of the state, remove it and its value
;Re-add the variable and new value to the state
(define assign
  (lambda (assignment state)
    (cond
      ((null? state) 'stateWasNull)
      ((member*? (car assignment) state) (stateAdd (car assignment) (return (car (cdr assignment)) state) (stateRemove (car assignment) state)))
      ;((eq? (stateGet var state) 'itemDoesNotExist) 'variableToAssignWasntDeclaredException)
      ;(else ((stateRemove var state) (stateAdd var value state))))))
      (else (error 'VariableNotDeclaredYet)))))

;If statement
(define ifstatement
  (lambda (ifstmt state)
    (cond
      ((null? ifstmt) state)
      ((eq? (return (car ifstmt) state) 'true)(statement (cons (cadr ifstmt) '()) state)) ; need to cons with '() so that statement can evaluate firststatement (caar)
      ((eq? (return (car ifstmt) state) 'false)(statement (cons (car (cddr ifstmt)) '()) state)) ; need to cons with '() so that statement can evaluate firststatement (caar)
      ((return (car ifstmt) state) (statement (cons (cadr ifstmt) '()) state))
      ((null? (cddr ifstmt)) state) ; else: do nothing
      (else (statement (cons (car (cddr ifstmt)) '()) state)))))

(define stateAdd
  (lambda (var value state)
    (cond
      ((null? var) 'variableNameWasNull)
      ((null? state) 'stateWasNullException)
      ((null? (car state)) (list (list var) (list value)))
      ((not (member*? var state)) (list (cons var (car state)) (cons value (car (cdr state)))))
      (else state))))

(define getvarsfromstate
  (lambda (state)
    (cond
      ((null? state) 'stateWasNull)
      ((list? (car state)) (car state))
      (else (cons (car state) '())))))


(define getvaluesfromstate
  (lambda (state)
    (cond
      ((null? state) 'stateWasNull)
      ((list? (cadr state)) (cadr state))
      (else (cons (cadr state) '())))))

(define stateRemove
  (lambda (var state)
    (cond
      ((null? (car state)) state)
      ;((not (list? (caar state)))
         ;(if (eq? (caar state) var)
             ;'(() ())
             ;state))
      ((eq? (caar state) var) (list (cdr (car state)) (cdr (cadr state))))
      ((null? (cdar state)) state); then we're on the last item, it wasn't ==, so we return state
      (else (list (append (cons (car (car state)) '()) (car (stateRemove var (append (cons (cdr (car state)) '()) (cons (cdr (cadr state)) '()))))) (append (cons (car (cadr state)) '()) (cadr (stateRemove var (append (cons (cdr (car state)) '()) (cons (cdr (cadr state)) '()))))))))))

(define stateGet
  (lambda (var state)
    (cond
      ((null? state) (error 'itemDoesNotExist))
      ;((null? (car state)) (error 'UsedVariableBeforeDeclaring))
      ((eq? (caar state) var) (caar (cdr state)))
      ((null? (cdr state)) (error 'VariableNotAssignedValue))
      (else (stateGet var (list (cdr (car state)) (cdr (cadr state)))))))) ; call recursively removing item from first and second lists of state


;Evaluates mathmatical expressions
;The order of operations is +,-,*,/,%
(define expression
  (lambda (expr state)
    (cond
      ((null? expr) '())
      ((number? expr) expr)
      ((member*? expr state) (if (null? (stateGet expr state)) ; need to check if it's a variable right here
                                 (error 'VariableWasNotAssigned)
                                 (stateGet expr state)))
      ((not (list?  expr)) (error 'VariableInExpressionNotDeclared))
      ((eq? '+ (operator expr)) (+ (expression (operand1 expr) state) (expression (operand2 expr) state)))
      ((eq? '- (operator expr))
       (if (isNegativeNumber expr)
           (- 0 (expression (operand1 expr) state))
           (- (expression (operand1 expr) state) (expression (operand2 expr) state))))
      ((eq? '* (operator expr)) (* (expression (operand1 expr) state) (expression (operand2 expr) state)))
      ((eq? '/ (operator expr)) (quotient (expression (operand1 expr) state) (expression (operand2 expr) state)))
      ((eq? '% (operator expr)) (remainder (expression (operand1 expr) state) (expression (operand2 expr) state)))
      (else (error 'badoperation 'UnknownOperator)))))

(define isNegativeNumber
  (lambda (expr)
    (null? (cddr expr))))


; Takes an operator as its input
; It then matches up the operator with the closest operator in Scheme
(define return
  (lambda (op state)
    (cond
      ((null? op) '())
      ((number? op) op)
      ((member*? op state) (if (null? (stateGet op state)) ; need to check if it's a variable right here
                                 (error 'VariableWasNotAssigned)
                                 (stateGet op state)))
      ((eq? op 'true) 'true)
      ((eq? op 'false) 'false)
      ((not (list?  op)) (error 'VariableInExpressionNotDeclared))
      ((eq? (car op) '==) (eq? (return (cadr op) state) (return (caddr op) state)))
      ((eq? (car op) '!=) (!= (return (cadr op) state) (return (caddr op) state)))
      ((eq? (car op) '<) (< (return (cadr op) state) (return (caddr op) state)))
      ((eq? (car op) '>) (> (return (cadr op) state) (return (caddr op) state)))
      ((eq? (car op) '<=) (<= (return (cadr op) state) (return (caddr op) state)))
      ((eq? (car op) '>=) (>= (return (cadr op) state) (return (caddr op) state)))
      ((eq? (car op) '&&) (and (return (cadr op) state) (return (caddr op) state)))
      ((eq? (car op) '||) (or (return (cadr op) state) (return (caddr op) state)))
      ((eq? (car op) '!) (not (return (cadr op) state)))
      (else (expression op state)))))

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

;(interpret "tests/1.txt")(interpret "tests/2.txt")(interpret "tests/3.txt")(interpret "tests/4.txt") (interpret "tests/5.txt")(interpret "tests/6.txt")(interpret "tests/7.txt")(interpret "tests/8.txt")(interpret "tests/9.txt")(interpret "tests/10.txt")
;(interpret "tests/11.txt")
;(interpret "tests/12.txt")
;(interpret "tests/13.txt")
;(interpret "tests/14.txt")
;(interpret "tests/15.txt")(interpret "tests/16.txt")(interpret "tests/17.txt")
;(interpret "tests/18.txt")
(interpret "tests/19.txt")
;(interpret "tests/20.txt")