#lang racket
; Alex Marshall awm48
; Nathan Walls nfw10
; Anna Burkhart alb171
; EECS 345 - Main Interpreter Part II

(require "simpleParser.scm")

;====================================================

#|
Alex's comments
We will most likely need to make our 'interpret' function implement call/cc
for throwing errors
I put some dummy lines of code in statement for what we are going to have to implement
We also need to add abstraction? where we (define operator1 caar) in all our functions because it is
pretty hard to tell what is going on.
|#

; Input is the return parse tree from simpleParser and the intial state is '(() ()) so that our binding pairs will
; be stored in two lists
(define interpret
  (lambda (file_name)
    (statement (parser file_name) initialState)))

; Check what kind of statement and interpret accordingly (ex. return, var, if, while, or =)
; Takes (cdr (car tree)) to eliminate statement word (ex. return, var, if, while, or =) if
; necessary input for functions they correlate to
(define statement
  (lambda (tree state)
    (cond
      ((null? tree) state)
      ((equal? (firstSymbol tree) 'return) (fixtf (Mboolean.return (returnValue tree) state)))
      ((eq? (firstSymbol tree) 'var) (statement (restOfParseTree tree) (Mstate.declare (statementWithoutSymbol tree) state)))
      ((eq? (firstSymbol tree) 'if) (statement (restOfParseTree tree) (Mstate.if (statementWithoutSymbol tree) state))) 
      ((eq? (firstSymbol tree) 'while) (statement (restOfParseTree tree) (Mstate.while (statementWithoutSymbol tree) state)))
      ((eq? (firstSymbol tree) '=) (statement (restOfParseTree tree) (Mstate.assign (statementWithoutSymbol tree) state)))
      ;((eq? (firstSymbol tree) 'begin) (doSomething))s
      ;((eq? (firstSymbol tree) 'break) (doSomething))
      ;((eq? (firstSymbol tree) 'continue) (doSomething))
      ;((eq? (firstSymbol tree) 'throw) (doSomething))
      ;((eq? (firstSymbol tree) 'try) (doSomething))
      (else (error 'InvalidStatement)))))

; Takes an operator and state as its input and
; then matches up the operator with the closest operator in Scheme
; Mboolean because just checks whether operate is equal to or not equal to a value
(define Mboolean.return
  (lambda (op state)
    (cond
      ((null? op) '())
      ((number? op) op)
      ((member*? op state) (if (null? (Mstate.getstate op state)) ; need to check if it's a variable right here
                                 (error 'VariableWasNotAssigned)
                                 (Mstate.getstate op state)))
      ((eq? op 'true) 'true)
      ((eq? op 'false) 'false)
      ((not (list?  op)) (error 'VariableInExpressionNotDeclaredYet))
      ((eq? (operator op) '==) (eq? (Mboolean.return (firstBoolExpression op) state) (Mboolean.return (secondBoolExpression op) state)))
      ((eq? (operator op) '!=) (!= (Mboolean.return (firstBoolExpression op) state) (Mboolean.return (secondBoolExpression op) state)))
      ((eq? (operator op) '<) (< (Mboolean.return (firstBoolExpression op) state) (Mboolean.return (secondBoolExpression op) state)))
      ((eq? (operator op) '>) (> (Mboolean.return (firstBoolExpression op) state) (Mboolean.return (secondBoolExpression op) state)))
      ((eq? (operator op) '<=) (<= (Mboolean.return (firstBoolExpression op) state) (Mboolean.return (secondBoolExpression op) state)))
      ((eq? (operator op) '>=) (>= (Mboolean.return (firstBoolExpression op) state) (Mboolean.return (secondBoolExpression op) state)))
      ((eq? (operator op) '&&) (and (Mboolean.return (firstBoolExpression op) state) (Mboolean.return (secondBoolExpression op) state)))
      ((eq? (operator op) '||) (or (Mboolean.return (firstBoolExpression op) state) (Mboolean.return (secondBoolExpression op) state)))
      ((eq? (operator op) '!) (not (Mboolean.return (firstBoolExpression op) state)))
      (else (Mvalue.expression op state)))))

; Evaluates mathmatical expressions
; The order of operations is +,-,*,/,%
; Mvalue because expression will use operators to
; calculate and return a value
(define Mvalue.expression
  (lambda (expr state)
    (cond
      ((null? expr) '())
      ((number? expr) expr)
      ((member*? expr state) (if (null? (Mstate.getstate expr state)) ; need to check if it's a variable right here
                                 (error 'VariableWasNotAssigned)
                                 (Mstate.getstate expr state)))
      ((not (list?  expr)) (error 'VariableInExpressionNotDeclared))
      ((eq? '+ (operator expr)) (+ (Mvalue.expression (operand1 expr) state) (Mvalue.expression (operand2 expr) state)))
      ((eq? '- (operator expr))
       (if (isNegativeNumber expr)
           (- 0 (Mvalue.expression (operand1 expr) state))
           (- (Mvalue.expression (operand1 expr) state) (Mvalue.expression (operand2 expr) state))))
      ((eq? '* (operator expr)) (* (Mvalue.expression (operand1 expr) state) (Mvalue.expression (operand2 expr) state)))
      ((eq? '/ (operator expr)) (quotient (Mvalue.expression (operand1 expr) state) (Mvalue.expression (operand2 expr) state)))
      ((eq? '% (operator expr)) (remainder (Mvalue.expression (operand1 expr) state) (Mvalue.expression (operand2 expr) state)))
      (else (error 'UnknownOperator)))))

; If we reach this function we must at least have (var) so first condition checks for "var" + "number/letter"
; and return the state if the "number/letter" is null, and otherwise calls Mstate.declare on "var" + "number/letter"
; Mstate because variable is added to state with no value, returns the state
(define Mstate.declare
  (lambda (declaration state)
    (cond
      ((null? state) 'StateUnDeclared) ;should never really be reached but here for safety
      ((null? (variableValue declaration)) (list (cons (variableName declaration) (variablesOfState state)) (cons '() (valuesOfState state))))
      ((member*? (variableName declaration) state) (error 'RedefiningError)) 
      (else (Mstate.assign declaration (list (cons (variableName declaration) (variablesOfState state)) (cons '() (valuesOfState state)))))))) ;adds the variable to the state but not the value

; Check if the variable is part of the state.
; If it is part of the state, remove it and its value
; Re-add the variable and new value to the state
; Mstate because adds value for a variable to the state
; (could argue Mvalue since specifically inserts a value
; even though assign returns the state still)
(define Mstate.assign
  (lambda (assignment state)
    (cond
      ((null? state) 'stateWasNull)
      ((member*? (car assignment) state) (Mstate.add (car assignment) (Mboolean.return (car (cdr assignment)) state) (Mstate.remove (car assignment) state)))
      (else (error 'VariableNotDeclaredYet)))))

; If statement: evaluates the if statement, and changes the state using the second
; statement if true, otherwise does nothing if there is no else statement, or
; changes the state using the else statement
; Mstate because it changes the state according to the conditional statement
(define Mstate.if
  (lambda (ifstmt state)
    (cond
      ((null? ifstmt) state)
      ((eq? (Mboolean.return (conditionalStatement ifstmt) state) 'true)(statement (cons (statement1 ifstmt) '()) state)) ; need to cons with '() so that statement can evaluate firststatement (caar)
      ((eq? (Mboolean.return (conditionalStatement ifstmt) state) 'false)(statement (cons (statement2 ifstmt) '()) state)) ; need to cons with '() so that statement can evaluate firststatement (caar)
      ((Mboolean.return (conditionalStatement ifstmt) state) (statement (cons (statement1 ifstmt) '()) state))
      ((null? (cddr ifstmt)) state) ; checks if there is an 'else' statement
      (else (statement (cons (statement2 ifstmt) '()) state)))))

; Mstate.while statement: loops if while statement is true, updating the state or
; (second line checks if #t instead of true for while loop), otherwise return
; state
; Mstate because changes variable state as long as while condition is true
(define Mstate.while
  (lambda (Mstate.whileloop state)
    (cond
      ((null? Mstate.whileloop) state)
      ((eq? (Mboolean.return (conditionalStatement Mstate.whileloop) state) 'true) (Mstate.while Mstate.whileloop (statement (cons (statement1 Mstate.whileloop) '()) state)))
      ((Mboolean.return (conditionalStatement Mstate.whileloop) state) (Mstate.while Mstate.whileloop (statement (cons (statement1 Mstate.whileloop) '()) state)))
      (else state))))


; Adds the variable's value to the state
; Mstate because adds value with associated variable to state
(define Mstate.add
  (lambda (var value state)
    (cond
      ((null? var) 'variableNameWasNull)
      ((null? state) 'stateWasNullException)
      ((null? (car state)) (list (list var) (list value)))
      ((not (member*? var state)) (list (cons var (car state)) (cons value (car (cdr state)))))
      (else state))))


; Takes a variable and removes its value from the state
; Mstate because removes value from state assigned to variable
(define Mstate.remove
  (lambda (var state)
    (cond
      ((null? (car state)) state)
      ((eq? (caar state) var) (list (cdr (car state)) (cdr (cadr state))))
      ((null? (cdar state)) state)
      (else (list (append (cons (car (car state)) '()) (car (Mstate.remove var (append (cons (cdr (car state)) '()) (cons (cdr (cadr state)) '()))))) (append (cons (car (cadr state)) '()) (cadr (Mstate.remove var (append (cons (cdr (car state)) '()) (cons (cdr (cadr state)) '()))))))))))

; Gets the value of the variable from the state or throws error if it does not have one
; Mstate because returns the state of a variable
(define Mstate.getstate
  (lambda (var state)
    (cond
      ((null? state) (error 'itemDoesNotExist))
      ((eq? (caar state) var) (caar (cdr state)))
      ((null? (cdr state)) (error 'VariableNotAssignedValue))
      (else (Mstate.getstate var (list (cdr (car state)) (cdr (cadr state)))))))) ; call recursively removing item from first and second lists of state

;====================================================
;Helper functions
;====================================================

; Check if a "-" symbol is for a negative number or a minus sign
(define isNegativeNumber
  (lambda (expr)
    (null? (cddr expr))))

; Not equals method to check if x != y for a given x and y
(define !=
  (lambda (x y)
    (not (= x y))))

; Member*? - return true if x is in our list lis, where lis can contain sublists
(define member*?
  (lambda (x lis)
    (cond
      ((null? lis) #f)
      ((pair? (car lis)) (or (member*? x (car lis)) (member*? x (cdr lis))))
      ((eq? x (car lis)) #t)
      (else (member*? x (cdr lis))))))

; Fix #t/#f as the code runs so true/false are returned instead
(define fixtf
  (lambda (value)
    (cond
      ((eq? value #t) 'true)
      ((eq? value #f) 'false)
      (else value))))

;====================================================
; Makes more code more readable and was easier to add but technically could be omitted and could just call
; explicitly in every instance (ex. (caar x))

(define initialState '(()()))
(define firstSymbol caar)
(define firstStatement car)
(define statementWithoutSymbol cdar)
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define firstBoolExpression cadr)
(define secondBoolExpression caddr)
(define returnValue cadar)
(define restOfParseTree cdr)
(define variableValue cdr)
(define variableName car)
(define variablesOfState car)
(define valuesOfState cadr)
(define conditionalStatement car)
(define statement1 cadr)
(define statement2 caddr)

;====================================================
;*****TESTING*********
;====================================================

;(interpret "tests/1.txt")(interpret "tests/2.txt")(interpret "tests/3.txt")(interpret "tests/4.txt") (interpret "tests/5.txt")(interpret "tests/6.txt")(interpret "tests/7.txt")(interpret "tests/8.txt")(interpret "tests/9.txt")(interpret "tests/10.txt")
;(interpret "tests/11.txt")
;(interpret "tests/12.txt")
;(interpret "tests/13.txt")
;(interpret "tests/14.txt")
;(interpret "tests/15.txt")(interpret "tests/16.txt")(interpret "tests/17.txt")
;(interpret "tests/18.txt")(interpret "tests/19.txt")(interpret "tests/20.txt")