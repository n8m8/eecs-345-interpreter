#lang racket
; Alex Marshall awm48
; Nathan Walls nfw10
; Anna Burkhart alb171
; EECS 345 - Main Interpreter Part II

(require "simpleParser.scm")

;====================================================

(define call/cc call-with-current-continuation)

; Input is the return parse tree from simpleParser and the intial state is '(() ()) so that our binding pairs will
; be stored in two lists
(define interpret
  (lambda (file_name)
    (statement (parser file_name) '((()())) )))

; Our statement function which implements call/cc
(define statement
  (lambda (tree state)
    (call/cc
     (lambda (break)
       (statement-cc tree state break)))))

; Check what kind of statement and interpret accordingly (ex. return, var, if, while, or =)
(define statement-cc
  (lambda (tree state break)
    (cond
      ((null? tree) state)
      ((equal? (firstSymbol tree) 'return) (break (fixtf (Mboolean.return (returnValue tree) state))))
      ((eq? (firstSymbol tree) 'var) (statement-cc (restOfParseTree tree) (Mstate.declare (statementWithoutSymbol tree) state) break))
      ((eq? (firstSymbol tree) 'if) (statement-cc (restOfParseTree tree) (Mstate.if (statementWithoutSymbol tree) state break) break)) 
      ((eq? (firstSymbol tree) 'while) (statement-cc (restOfParseTree tree) (Mstate.while (statementWithoutSymbol tree) state break) break))
      ((eq? (firstSymbol tree) '=) (statement-cc (restOfParseTree tree) (Mstate.assign (statementWithoutSymbol tree) state) break))
      ((eq? (firstSymbol tree) 'begin) (statement-cc (restOfParseTree tree) (removeLocalState (statement-cc (statementWithoutSymbol tree) (addLocalState initialState state) break)) break))
      ;((eq? (firstSymbol tree) 'break) (doSomething))
      ;((eq? (firstSymbol tree) 'continue) (doSomething))
      ;((eq? (firstSymbol tree) 'throw) (doSomething))
      ;((eq? (firstSymbol tree) 'try) (doSomething))
      (else (error 'InvalidStatement)))))

; Adds a new layer to the state for local variables
(define addLocalState
  (lambda (stateToAdd state)
    (cons stateToAdd state)))

; Removes the top most layer of the state
(define removeLocalState
  (lambda (state)
    (if (null? state)
         (error 'NoState)
         (popState state))))

; Takes an operator and state as its input and
; then matches up the operator with the closest operator in Scheme
; Mboolean checks whether operate is equal to or not equal to a value
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

; Takes care of decrlaring variables
; This function is only called when a new variable is introduced
; Calls addValToLayer which will add the var and value to the appropriate layer
(define Mstate.declare
  (lambda (declaration state)
    (cond
      ((null? state) 'StateUnDeclared) ;should never really be reached but here for safety
      ((null? (variableValue declaration)) (cons (Mstate.addValToLayer (variableName declaration) '() (firstState state)) (popState state)))
      ((member*? (variableName declaration) state) (error 'RedefiningError)) 
      (else (cons (Mstate.addValToLayer (variableName declaration) (Mvalue.expression (variableValue declaration) state) (firstState state) lambdavv) (popState state))))))
      
; Check if the variable is part of the state.
; If it is part of the state, remove its value
; Add the new value to the state
; Mstate because adds value for a variable to the state
(define Mstate.assign
  (lambda (assignment state)
    (cond
      ((null? state) 'stateWasNull)
      ((member*? (car assignment) state) (Mstate.add (assignmentVarToSave assignment) (Mboolean.return (assignmentValueToSave assignment) state) (Mstate.remove (assignmentVarToSave assignment) state)))
      (else (error 'VariableNotDeclaredYet)))))

(define assignmentVarToSave car)
(define assignmentValueToSave cadr)

; If statement: evaluates the if statement, and changes the state using the second
; statement if true, otherwise does nothing if there is no else statement, or
; changes the state using the else statement
; Mstate because it changes the state according to the conditional statement
(define Mstate.if
  (lambda (ifstmt state break)
    (cond
      ((null? ifstmt) state)
      ((eq? (fixtf (Mboolean.return (conditionalStatement ifstmt) state)) 'true)(statement-cc (cons (statement1 ifstmt) '()) state break)) ; need to cons with '() so that statement can evaluate firststatement (caar)
      ((eq? (Mboolean.return (conditionalStatement ifstmt) state) 'false)(statement-cc (cons (statement2 ifstmt) '()) state break)) ; need to cons with '() so that statement can evaluate firststatement (caar)
      ((Mboolean.return (conditionalStatement ifstmt) state) (statement-cc (cons (statement1 ifstmt) '()) state break))
      ((null? (cddr ifstmt)) state) ; checks if there is an 'else' statement
      (else (statement-cc (cons (statement2 ifstmt) '()) state break)))))

; Mstate.while statement: loops if while statement is true, updating the state or
; (second line checks if #t instead of true for while loop), otherwise return
; state
; Mstate because changes variable state as long as while condition is true
(define Mstate.while
  (lambda (Mstate.whileloop state break)
    (cond
      ((null? Mstate.whileloop) state)
      ((eq? (fixtf (Mboolean.return (conditionalStatement Mstate.whileloop) state)) 'true) (Mstate.while Mstate.whileloop (statement-cc (cons (statement1 Mstate.whileloop) '()) state break) break))
      ((Mboolean.return (conditionalStatement Mstate.whileloop) state) (Mstate.while Mstate.whileloop (statement-cc (cons (statement1 Mstate.whileloop) '()) state break)))
      (else state))))


; Adds the variable and the variable's value to the top most layer
; Mstate because adds value with associated variable to state
(define Mstate.addValToLayer
  (lambda (var value state return)
    (cond
      ((null? var) 'variableNameWasNull)
      ((null? state) 'stateWasNullException)
      ((not (member*? var state)) (return (list (cons var (stateNames state)) (cons value (stateValues state)))))
      ((eq? (stateFirstName state) var) (return (list (variablesOfState state) (cons value (stateRestOfValues state)))))
      (else (Mstate.addValToLayer var value (cons (stateRestOfVars state) (cons (stateRestOfValues state) '())) (lambda (v) (return (list (cons (stateFirstName state) (stateNames v)) (cons (stateFirstValue state) (stateValues v))))))))))

(define stateNames car)
(define stateValues cadr)
(define stateFirstName caar)
(define stateFirstValue caadr)
(define stateRestOfValues cdadr)
(define stateRestOfVars cdar)

(define lambdavv (lambda (v) v))

; This function finds which layer the variable is located in
; When the layer is found, it calls addValToLayer to add the var and val to the correct layer
(define Mstate.add
  (lambda (var value state)
    (cond
      ((null? state) 'stateIsNull)
      ((member*? var (firstStateVariables state)) (cons (Mstate.addValToLayer var value (firstState state) lambdavv) (popState state)))
      (else (cons (firstState state) (Mstate.add var value (popState state)))))))

; Removes the variable value from the state and adds an empty list as a place holder
(define Mstate.removeValFromLayer
  (lambda (var state)
    (cond
      ((null? (car state)) state)
      ((eq? (caar state) var) (list (car state) (cons '() (cdadr state))))
      ((null? (cdar state)) state)
      (else (list (append (cons (firstVarName state) '()) (car (Mstate.removeValFromLayer var (append (cons (cdar state) '()) (cons (cdadr state) '()))))) (append (cons (caadr state) '()) (cadr (Mstate.removeValFromLayer var (append (cons (cdar state) '()) (cons (cdadr state) '()))))))))))

(define firstVarName caar)

; Finds the correct layer the variable is in
; When it finds the layer it calls removeValFromLayer to remove the val from layer
(define Mstate.remove
  (lambda (var state)
    (cond
      ((null? state) (error 'varIsNotInState))
      ((member*? var (firstStateVariables state)) (cons (Mstate.removeValFromLayer var (firstState state)) (popState state)))
      (else (cons (firstState state) (Mstate.remove var (popState state)))))))

; Gets the value of the variable from the layer or throws error if it does not have one
; Mstate because returns the state of a variable
(define Mstate.getValFromLayer
  (lambda (var state)
    (cond
      ((null? state) (error 'itemDoesNotExist)) 
      ((eq? (caar state) var) (caadr state))
      ((null? (cdr state)) (error 'VariableNotAssignedValue))
      ;(else (Mstate.getValFromLayer var (cdr state))))))
      (else (Mstate.getValFromLayer var (list (cdar state) (cdadr state))))))) ; call recursively removing item from first and second lists of state

; Finds the correct layer that the variable is in
(define Mstate.getstate
  (lambda (var state)
    (cond
      ((null? state) (error 'itemDoesNotExist))
      ((member*? var (firstStateVariables state)) (Mstate.getValFromLayer var (firstState state)))
      (else (Mstate.getstate var (popState state))))))

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
(define variableValue cadr)
(define variableName car)
(define variablesOfState car)
(define valuesOfState cadr)
(define conditionalStatement car)
(define statement1 cadr)
(define statement2 caddr)
(define firstState car)
(define firstStateVariables caar)
(define popState cdr)

;====================================================
;*****TESTING*********
;====================================================

(interpret "tests/1.txt")
(interpret "tests/2.txt")
(interpret "tests/3.txt")
(interpret "tests/4.txt")
(interpret "tests/5.txt")
;(interpret "tests/6.txt")
;(interpret "tests/7.txt")
;(interpret "tests/8.txt")(interpret "tests/9.txt")(interpret "tests/10.txt")
;(interpret "tests/11.txt")
;(interpret "tests/12.txt")
;(interpret "tests/13.txt")
;(interpret "tests/14.txt")
;(interpret "tests/15.txt")(interpret "tests/16.txt")(interpret "tests/17.txt")
;(interpret "tests/18.txt")(interpret "tests/19.txt")(interpret "tests/20.txt")