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
    (statement (parser file_name) initialBaseLayer )))

(define initialBaseLayer '((() ())))

; Our statement function which implements call/cc
(define statement
  (lambda (tree state)
    (call/cc
     (lambda (break)
       (statement-cc tree state break (lambda (v) (error 'ContinueCalledOutsideLoop)) (lambda (b) (error 'BreakCalledOutsideLoop)) (lambda (lmao) (error 'throwWithNoCatch)))))))

; Check what kind of statement and interpret accordingly (ex. return, var, if, while, =, begin, continue, break try, throw, finally)
(define statement-cc
  (lambda (tree state break whileContinue whileBreak throw)
    (cond
      ((null? tree) state)
      ((equal? (firstSymbol tree) 'return) (break (fixtf (Mboolean-return (returnValue tree) state))))
      ((eq? (firstSymbol tree) 'var) (statement-cc (restOfParseTree tree) (Mstate-declare (statementWithoutSymbol tree) state) break whileContinue whileBreak throw))
      ((eq? (firstSymbol tree) 'if) (statement-cc (restOfParseTree tree) (Mstate-if (statementWithoutSymbol tree) state break whileContinue whileBreak throw) break whileContinue whileBreak throw)) 
      ((eq? (firstSymbol tree) 'while) (statement-cc (restOfParseTree tree)
                                                     (call/cc
                                                      (lambda (nestedWhileBreak)
                                                           (statement-cc (restOfParseTree tree)
                                                                 (Mstate-while (statementWithoutSymbol tree) state break whileContinue nestedWhileBreak throw)
                                                                 break whileContinue nestedWhileBreak throw))) break whileContinue whileBreak throw))
      ((eq? (firstSymbol tree) '=) (statement-cc (restOfParseTree tree) (Mstate-assign (statementWithoutSymbol tree) state) break whileContinue whileBreak throw))
      ((eq? (firstSymbol tree) 'begin) (statement-cc (restOfParseTree tree)
                                                     (removeLocalState (statement-cc (statementWithoutSymbol tree) (addLocalState initialState state) break whileContinue whileBreak throw)) break whileContinue whileBreak throw))
      ((eq? (firstSymbol tree) 'continue) (whileContinue (popState state)))
      ((eq? (firstSymbol tree) 'break) (whileBreak (popState state)))
      ((eq? (firstSymbol tree) 'try) (statement-cc (restOfParseTree tree) (Mstate-try (statementWithoutSymbol tree) state break whileContinue whileBreak throw) break whileContinue whileBreak throw))
      ((eq? (firstSymbol tree) 'throw) (throw (addLocalState (Mstate-addValToLayer 'throw (Mboolean-return (trimthrowClause (statementWithoutSymbol tree)) state) initialState lambdavv) state)))
      ((eq? (firstSymbol tree) 'finally) (statement-cc (insideFinallyStatement tree) state break whileContinue whileBreak throw))
      (else (error 'InvalidStatement)))))

(define insideFinallyStatement cadar)
(define statementWithoutSymbol cdar)
(define tryStatement car)
(define finallyStatement cddr)
(define trimthrowClause car)


; Mstate-try
; Returns state as a result of trying something
; takes all the appropriate continuation functions
(define Mstate-try
  (lambda (tree state break whileContinue whileBreak throw)
    (cond
      ((null? tree) state)
      ((null? (cadr tree))  (statement-cc (finallyStatement tree)  ; No catch clause so don't do the catch part
                          (call/cc
                                  (lambda (nestedThrow)
                                     (statement-cc (tryStatement tree) state break whileContinue whileBreak nestedThrow)))
                          break whileContinue whileBreak throw))
      ((hasFinally? tree) (statement-cc (finallyStatement tree)   ; has finally and catch, so call try inside the catch inside the finally
                          (Mstate-catch (catchStatement tree)
                                        (call/cc
                                         (lambda (nestedThrow)
                                           (statement-cc (tryStatement tree) state break whileContinue whileBreak nestedThrow)))
                                        break whileContinue whileBreak throw)
                          break whileContinue whileBreak throw))
      (else (Mstate-catch (catchStatement tree)       ; doesn't have finally so just do the try inside the catch
                                        (call/cc
                                         (lambda (nestedThrow)
                                           (statement-cc (tryStatement tree) state break whileContinue whileBreak nestedThrow)))
                                        break whileContinue whileBreak throw)))))
; helper function for Mstate-try
(define hasFinally?
  (lambda (tree)
    (if (null? (caddr tree))
        #f
        #t)))

(define catchStatement cdadr)

; Mstate-catch
; returns a state resulting from catch
; takes all the appropriate continuation functions
(define Mstate-catch
  (lambda (statement state break whileContinue whileBreak throw)
    (cond
      ((null? statement) 'CatchStatementNotDefined)
      ((member*? 'throw state) (statement-cc (cadr statement) (Mstate-declare (list (getCatchVar statement) (Mvalue-getVal 'throw state)) state) break whileContinue whileBreak throw))
      (else state))))

(define getCatchVar caar)

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
(define Mboolean-return
  (lambda (op state)
    (cond
      ((null? op) '())
      ((number? op) op)
      ((member*? op state) (if (null? (Mvalue-getVal op state)) ; need to check if it's a variable right here
                                 (error 'VariableWasNotAssigned)
                                 (Mvalue-getVal op state)))
      ((eq? op 'true) 'true)
      ((eq? op 'false) 'false)
      ((not (list?  op)) (error 'VariableInExpressionNotDeclaredYet))
      ((eq? (operator op) '==) (eq? (Mboolean-return (firstBoolExpression op) state) (Mboolean-return (secondBoolExpression op) state)))
      ((eq? (operator op) '!=) (!= (Mboolean-return (firstBoolExpression op) state) (Mboolean-return (secondBoolExpression op) state)))
      ((eq? (operator op) '<) (< (Mboolean-return (firstBoolExpression op) state) (Mboolean-return (secondBoolExpression op) state)))
      ((eq? (operator op) '>) (> (Mboolean-return (firstBoolExpression op) state) (Mboolean-return (secondBoolExpression op) state)))
      ((eq? (operator op) '<=) (<= (Mboolean-return (firstBoolExpression op) state) (Mboolean-return (secondBoolExpression op) state)))
      ((eq? (operator op) '>=) (>= (Mboolean-return (firstBoolExpression op) state) (Mboolean-return (secondBoolExpression op) state)))
      ((eq? (operator op) '&&) (and (Mboolean-return (firstBoolExpression op) state) (Mboolean-return (secondBoolExpression op) state)))
      ((eq? (operator op) '||) (or (Mboolean-return (firstBoolExpression op) state) (Mboolean-return (secondBoolExpression op) state)))
      ((eq? (operator op) '!) (not (Mboolean-return (firstBoolExpression op) state)))
      (else (Mvalue-expression op state)))))

; Evaluates mathmatical expressions
; The order of operations is +,-,*,/,%
; Mvalue because expression will use operators to
; calculate and return a value
(define Mvalue-expression
  (lambda (expr state)
    (cond
      ((null? expr) '())
      ((number? expr) expr)
      ((member*? expr state) (if (null? (Mvalue-getVal expr state)) ; need to check if it's a variable right here
                                 (error 'VariableWasNotAssigned)
                                 (Mvalue-getVal expr state)))
      ((not (list?  expr)) (error 'VariableInExpressionNotDeclared))
      ((eq? '+ (operator expr)) (+ (Mvalue-expression (operand1 expr) state) (Mvalue-expression (operand2 expr) state)))
      ((eq? '- (operator expr))
       (if (isNegativeNumber expr)
           (- 0 (Mvalue-expression (operand1 expr) state))
           (- (Mvalue-expression (operand1 expr) state) (Mvalue-expression (operand2 expr) state))))
      ((eq? '* (operator expr)) (* (Mvalue-expression (operand1 expr) state) (Mvalue-expression (operand2 expr) state)))
      ((eq? '/ (operator expr)) (quotient (Mvalue-expression (operand1 expr) state) (Mvalue-expression (operand2 expr) state)))
      ((eq? '% (operator expr)) (remainder (Mvalue-expression (operand1 expr) state) (Mvalue-expression (operand2 expr) state)))
      (else (error 'UnknownOperator)))))

(define operator car)
(define operand1 cadr)
(define operand2 caddr)

; Takes care of decrlaring variables
; This function is only called when a new variable is introduced
; Calls addValToLayer which will add the var and value to the appropriate layer
(define Mstate-declare
  (lambda (declaration state)
    (cond
      ((null? state) 'StateUnDeclared) ;should never really be reached but here for safety
      ((null? (hasValue declaration)) (cons (Mstate-addValToLayer (variableName declaration) '() (firstState state) lambdavv) (popState state)))
      ((member*? (variableName declaration) state) (error 'RedefiningError)) 
      (else (cons (Mstate-addValToLayer (variableName declaration) (Mvalue-expression (variableValue declaration) state) (firstState state) lambdavv) (popState state))))))

(define hasValue cdr)
(define variableValue cadr)
(define variableName car)
(define addNewStateLayer (lambda (var value state return) (cons (Mstate-addValToLayer var value (firstState state) lambdavv) (popState state))))


; Check if the variable is part of the state.
; If it is part of the state, remove its value
; Add the new value to the state
; Mstate because adds value for a variable to the state
(define Mstate-assign
  (lambda (assignment state)
    (cond
      ((null? state) 'stateWasNull)
      ((member*? (car assignment) state) (Mstate-add (assignmentVarToSave assignment) (Mboolean-return (assignmentValueToSave assignment) state) (Mstate-remove (assignmentVarToSave assignment) state)))
      (else (error 'VariableNotDeclaredYet)))))

(define assignmentVarToSave car)
(define assignmentValueToSave cadr)

; If statement: evaluates the if statement, and changes the state using the second
; statement if true, otherwise does nothing if there is no else statement, or
; changes the state using the else statement
; Mstate because it changes the state according to the conditional statement
(define Mstate-if
  (lambda (ifstmt state break whileContinue whileBreak throw)
    (cond
      ((null? ifstmt) state)
      ((eq? (fixtf (Mboolean-return (conditionalStatement ifstmt) state)) 'true)(statement-cc (cons (statement1 ifstmt) '()) state break whileContinue whileBreak throw)) ; need to cons with '() so that statement can evaluate firststatement (caar)
      ((eq? (Mboolean-return (conditionalStatement ifstmt) state) 'false)(statement-cc (cons (statement2 ifstmt) '()) state break whileContinue whileBreak throw)) ; need to cons with '() so that statement can evaluate firststatement (caar)
      ((Mboolean-return (conditionalStatement ifstmt) state) (statement-cc (cons (statement1 ifstmt) '()) state break whileContinue whileBreak throw))
      ((null? (cddr ifstmt)) state) ; checks if there is an 'else' statement
      (else (statement-cc (cons (statement2 ifstmt) '()) state break whileContinue whileBreak throw)))))

; Mstate-while-cc
; CC helper function for Mstate-while
; takes the correct break and continue continuation functions and passes them to statement-cc to break to appropriate point
(define Mstate-while-cc
  (lambda (Mstate-whileloop state break whileContinue whileBreak throw)
    (cond
      ((null? Mstate-whileloop) state)
      ((Mboolean-return (conditionalStatement Mstate-whileloop) state) (Mstate-while-cc Mstate-whileloop (statement-cc (cons (statement1 Mstate-whileloop) '()) state break whileContinue whileBreak throw) break whileContinue whileBreak throw))
      (else state))))

; Mstate-while statement: loops if while statement is true, updating the state or
; (second line checks if #t instead of true for while loop), otherwise return
; state
; Mstate because changes variable state as long as while condition is true
(define Mstate-while
  (lambda (Mstate-whileloop state break whileContinue whileBreak throw)
    (cond
      ((null? Mstate-whileloop) state)
      ((Mboolean-return (conditionalStatement Mstate-whileloop) state) (Mstate-while Mstate-whileloop
                    (call/cc
                     (lambda (nestedWhileContinue)
                       (Mstate-while-cc Mstate-whileloop state break nestedWhileContinue whileBreak throw))
                     )
                     break whileContinue whileBreak throw))
      (else state))))

; Adds the variable and the variable's value to the top most layer
; Mstate because adds value with associated variable to state
(define Mstate-addValToLayer
  (lambda (var value state return)
    (cond
      ((null? var) 'variableNameWasNull)
      ((null? state) 'stateWasNullException)
      ((not (member*? var state)) (return (list (cons var (stateNames state)) (cons value (stateValues state)))))
      ((eq? (stateFirstName state) var) (return (list (variablesOfState state) (cons value (stateRestOfValues state)))))
      (else (Mstate-addValToLayer var value (cons (stateRestOfVars state) (cons (stateRestOfValues state) '())) (lambda (v) (return (list (cons (stateFirstName state) (stateNames v)) (cons (stateFirstValue state) (stateValues v))))))))))

(define stateNames car)
(define stateValues cadr)
(define stateFirstName caar)
(define stateFirstValue caadr)
(define stateRestOfValues cdadr)
(define stateRestOfVars cdar)

(define lambdavv (lambda (v) v))

; This function finds which layer the variable is located in
; When the layer is found, it calls addValToLayer to add the var and val to the correct layer
(define Mstate-add
  (lambda (var value state)
    (cond
      ((null? state) 'stateIsNull)
      ((member*? var (firstStateVariables state)) (cons (Mstate-addValToLayer var value (firstState state) lambdavv) (popState state)))
      (else (cons (firstState state) (Mstate-add var value (popState state)))))))

; Removes the variable value from the state and adds an empty list as a place holder
(define Mstate-removeValFromLayer
  (lambda (var state)
    (cond
      ((null? (car state)) state)
      ((eq? (caar state) var) (list (car state) (cons '() (cdadr state))))
      ((null? (cdar state)) state)
      (else (list (append (cons (firstVarName state) '()) (car (Mstate-removeValFromLayer var (append (cons (cdar state) '()) (cons (cdadr state) '()))))) (append (cons (caadr state) '()) (cadr (Mstate-removeValFromLayer var (append (cons (cdar state) '()) (cons (cdadr state) '()))))))))))

(define firstVarName caar)

; Finds the correct layer the variable is in
; When it finds the layer it calls removeValFromLayer to remove the val from layer
(define Mstate-remove
  (lambda (var state)
    (cond
      ((null? state) (error 'varIsNotInState))
      ((member*? var (firstStateVariables state)) (cons (Mstate-removeValFromLayer var (firstState state)) (popState state)))
      (else (cons (firstState state) (Mstate-remove var (popState state)))))))

; Gets the value of the variable from the layer or throws error if it does not have one
; Mstate because returns the state of a variable
(define Mvalue-getValFromLayer
  (lambda (var state)
    (cond
      ((null? state) (error 'itemDoesNotExist)) 
      ((eq? (caar state) var) (caadr state))
      ((null? (cdr state)) (error 'VariableNotAssignedValue))
      (else (Mvalue-getValFromLayer var (list (cdar state) (cdadr state))))))) ; call recursively removing item from first and second lists of state

; Finds the correct layer that the variable is in and calls getValFromLayer
(define Mvalue-getVal
  (lambda (var state)
    (cond
      ((null? state) (error 'itemDoesNotExist))
      ((member*? var (firstStateVariables state)) (Mvalue-getValFromLayer var (firstState state)))
      (else (Mvalue-getVal var (popState state))))))

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
(define firstBoolExpression cadr)
(define secondBoolExpression caddr)
(define returnValue cadar)
(define restOfParseTree cdr)
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

(interpret "tests/0.txt")
(interpret "tests/1.txt")
(interpret "tests/2.txt")
(interpret "tests/3.txt")
(interpret "tests/4.txt")
;(interpret "tests/5.txt")
(interpret "tests/6.txt")
(interpret "tests/7.txt")
(interpret "tests/8.txt")
(interpret "tests/9.txt")
(interpret "tests/10.txt")
;(interpret "tests/11.txt")
;(interpret "tests/12.txt")
;(interpret "tests/13.txt")
(interpret "tests/14.txt")
(interpret "tests/15.txt")
(interpret "tests/16.txt")
;(interpret "tests/17.txt")
(interpret "tests/18.txt")
;(interpret "tests/19.txt")
;(interpret "tests/20.txt")