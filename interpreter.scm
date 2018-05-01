; =======
; EECS 345 Interpreter Part 4
; Alex Marshall, awm48
; Nathan Walls, nfw10
; Anna Burkhart, alb171
#lang racket
(require "classParser.scm")

; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file class-name)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-statement-list (parser file) (newenvironment) class-name return
                                  breakOutsideLoopError continueOutsideLoopError
                                  uncaughtExceptionThrownError))))))

(define breakOutsideLoopError
  (lambda (env) (myerror "Break used outside loop")))

(define continueOutsideLoopError
  (lambda (env) (myerror "Continue used outside of loop")))

(define uncaughtExceptionThrownError
  (lambda (v env) (myerror "Uncaught exception thrown")))

; interprets a list of statements.  The environment from each statement is used for the next ones.
;Useful for debugging and not interpretting main
(define interpret-statement-list
  (lambda (statement-list environment class-name return break continue throw)
    (if (null? statement-list)
        (evaluate-main-class class-name environment return break continue throw)
        ;environment ;keep this for debugging
        (interpret-statement-list (rest-of-statement-list statement-list) (interpret-statement (first-statement statement-list) environment return break continue throw) class-name return break continue throw))))

(define rest-of-statement-list cdr)
(define first-statement car)

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return throw))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment return break continue throw))
      ;((eq? 'static-var (statement-type statement)) (interpret-static-var (statement-without-static-var statement) environment throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment throw))
      ;((and (eq? '= (statement-type statement)) (list? (cadr statement))) (interpret-assign-with-instance-name (statement
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw))
      ((eq? 'function (statement-type statement)) (interpret-function (statement-without-func statement) environment return break continue throw))
      ((eq? 'static-function (statement-type statement)) (interpret-static-function (statement-without-static-func statement) environment return break continue throw))
      ;((eq? 'abstract-function (statement-type statement)) (interpret-abstract-function (statement-without-abstract-func statement) environment return break continue throw))
      ;((and (eq? 'funcall (statement-type statement)) (list? (function-name (statement-without-funcall statement)))) (interpret-funcall-result-environment (interpret-dot (cadr (function-name (statement-without-funcall statement))) (caddr (function-name (statement-without-funcall statement))) environment throw) (add-parameters-to-environment (get-parameters (lookup (function-name (statement-without-funcall statement)) environment)) (parameters (statement-without-funcall statement)) (push-frame environment) throw)
                                                                                       ;return
                                                                                       ;break continue throw))
      ((and (eq? 'funcall (statement-type statement)) (list? (function-name (statement-without-funcall statement)))) (update (cadar (statement-without-funcall statement)) (mycar (interpret-funcall-result-environment (cadr (get-funcall-closure (car (statement-without-funcall statement)) environment)) (add-parameters-to-environment (car (get-funcall-closure (car (statement-without-funcall statement)) environment)) (parameters (statement-without-funcall statement)) (push-frame (append (lookup (cadar (statement-without-funcall statement)) environment) environment)) throw)
                                                                                       return
                                                                                       break continue throw)) environment))
      ((eq? 'funcall (statement-type statement)) (interpret-funcall-result-environment (statement-list-from-function (lookup (function-name (statement-without-funcall statement)) environment)) (add-parameters-to-environment (get-parameters (lookup (function-name (statement-without-funcall statement)) environment)) (parameters (statement-without-funcall statement)) (push-frame environment) throw)
                                                                                       return
                                                                                       break continue throw))
      ((eq? 'new (statement-type statement)) (interpret-new-object (statement-without-new statement) environment return break continue throw))
      ;((eq? 'dot (statement-type statement)) (interpret-dot (statement-without-dot statement) environment throw))
      ((eq? 'class (statement-type statement)) (interpret-class (statement-without-class statement) environment))
      (else (myerror "Unknown statement:" (statement-type statement))))))

(define mycar
  (lambda (l)
    list (car l)))


(define interpret-dot
  (lambda (instance-name field-to-lookup environment throw)
    (cond
      ((null? instance-name) (myerror "instance name was null"))
      ((null? field-to-lookup) (myerror "field-to-lookup was null"))
      ((eq? instance-name 'this) (eval-expression field-to-lookup (pop-frame environment) throw))
      ((eq? instance-name 'super) (eval-expression field-to-lookup (pop-frame (pop-frame environment)) throw))
      (else (eval-expression field-to-lookup (list (lookup instance-name environment)) throw)))))

(define statement-type car)
(define statement-without-func cdr)
(define statement-without-funcall statement-without-func)
(define statement-without-static-var statement-without-funcall)
(define statement-without-static-func statement-without-static-var)
(define statement-without-abstract-func statement-without-static-func)
(define statement-without-new statement-without-abstract-func)
(define statement-without-dot statement-without-new)
(define statement-without-class statement-without-dot)
(define get-parameters car)
(define statement-list-from-function cadr)

; Interprets a var that is being declared as a class object
; Its value is an environment storing any functions or instance fields
(define interpret-new-object
  (lambda (statement environment return break continue throw)
    (cond
      ((null? statement) (myerror "Statement doesn't exist"))
      ((null? (lookup (cadadr statement) environment)) (myerror "Class doesn't exist"))
      (else (insert (car statement) (make-statelayer-from-instance-fields (get-closure-of-class (lookup (get-new-class-name statement) environment)) (newenvironment) return break continue throw) environment)))))

(define get-new-class-name cadadr) ;returns 'A from '(a (new A))
(define get-closure-of-class cadr) ;returns the closure of the class without what the class extends. Might need to include that later on

; Interprets 'class and adds it to the environment
(define interpret-class
  (lambda (class-closure environment)
    (cond
      ((null? class-closure) (myerror "No class closure"))
      (else (insert (get-class-name class-closure) (get-rest-of-class-closure class-closure) environment)))))

(define get-class-name car)
(define get-rest-of-class-closure cdr)

; M-environment function returns the environment that is the result of calling function
(define interpret-funcall-result-environment
 (lambda (statement-list environment return break continue throw)
  (cond
    ((null? statement-list) environment)
    (else (if (list? (call/cc
                      (lambda (breakreturn)
                        (interpret-statement (first-statement statement-list) environment breakreturn break continue throw))))
                  (interpret-funcall-result-environment (rest-of-statement-list statement-list) (call/cc
                                                                              (lambda (breakreturn)
                                                                                (interpret-statement (first-statement statement-list) environment breakreturn break continue throw)))
                                                        return break continue throw)
                  environment)))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return throw)
    ;(return environment))) ;used for debugging purposes
    (return (eval-expression (get-expr statement) environment throw))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
#|(define interpret-declare
  (lambda (statement environment return break continue throw)
    (if (exists-declare-value? statement)
        (if (and (list? (get-declare-value statement) (eq? (car (get-declare-value statement))) 'new))
            (interpret-new-object (statement-without-new statement) environment return break continue throw)
            (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment throw) environment))
        (insert (get-declare-var statement) 'novalue environment))))|#

(define interpret-declare
  (lambda (statement environment return break continue throw)
    (cond
      ((exists-declare-value? statement) (if (list? (get-declare-value statement))
                                             (interpret-new-object (statement-without-new statement) environment return break continue throw)
                                             (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment throw) environment)))
      (else (insert (get-declare-var statement) 'novalue environment)))))
                                         

; Updates the environment to add a new binding for a variable
(define interpret-assign
  (lambda (statement environment throw)
    (cond
      ((list? (get-assign-lhs statement)) (cond ; need to come up with a way to keep on doing dots, and know how many environments to pop to assign to correct place
                                            ((eq? (assign-dot-prefix (cadr statement)) 'this) (update (car (cddadr statement)) (eval-expression (caddr statement) environment throw) (pop-frame environment)))
                                            ((eq? (assign-dot-prefix (cadr statement)) 'super) 1)
                                            (else (myerror "NEED TO IMPLEMENT INTERPRET DOT LMAO"))))
      (else (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment throw) environment)))))

(define assign-dot-prefix cadr)

  
; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment throw) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment throw)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-block-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

; Used for interpreting the block statments of while loops
(define interpret-block-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-block-statement-list (rest-of-statement-list statement-list) (interpret-statement (first-statement statement-list) environment return break continue throw) return break continue throw))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment throw) environment)))

; Interpret a try-catch-finally block
; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-throw-catch-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; M-environment similar to interpret statement but returns the environment instead of interpreting main at the end
(define interpret-throw-catch-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-throw-catch-statement-list (rest-of-statement-list statement-list) (interpret-statement (first-statement statement-list) environment return break continue throw) return break continue throw))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
; We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; To interpret a function and add it to the environment
(define interpret-function
  (lambda (statement environment return break continue throw)
    (cond
      ((null? (func-body statement)) environment) ;checks if the function body is empty
      (else (insert (func-name statement) (func-body statement) environment))))) ;checks if there are any parameters

(define func-name car)
(define func-body cdr)
(define statement-list-of-function cadr)

; To interpret a static-function and add it to the environment
; Currently it is the same as interpret-function but will be updated later
(define interpret-static-function
  (lambda (statement environment return break continue throw)
    (cond
      ((null? (func-body statement)) environment) ;checks if the function body is empty
      (else (insert (func-name statement) (func-body statement) environment))))) ;checks if there are any parameters

; Evaluates the function 'main
(define evaluate-main-class
  (lambda (class-name environment return break continue throw)
    (cond
      ((not (exists? class-name environment)) (myerror "Undefined class")) ;this should check if main is associated with a statement list in the env
      (else (interpret-statement-list (statement-list-of-function (find-function-in-closure (cadr (lookup class-name environment)) 'main))
                                      (make-statelayer-from-instance-fields (cadr (lookup class-name environment))(push-frame environment) return break continue throw)
                                      class-name return break continue throw)))))


(define find-function-in-closure
  (lambda (class-closure func-name)
    (cond
        ((null? class-closure) (myerror "Function does not exist"))
        ((eq? func-name (cadar class-closure)) (cddar class-closure))
        (else (find-function-in-closure (cdr class-closure) func-name)))))


(define make-statelayer-from-instance-fields
  (lambda (class-closure environment return break continue throw)
    (cond
      ((null? class-closure) environment)
      ((list? (car class-closure)) (make-statelayer-from-instance-fields (cdr class-closure) (interpret-statement (car class-closure) environment return break continue throw) return break continue throw)))))

; evaluates a funcall. Funcall here is for example (amethod 1 2 3) or (bmethod)
(define interpret-funcall
  (lambda (funcall environment throw)
    (call/cc
     (lambda (func-return)
       (cond
         ((list? (car funcall)) (interpret-function-statement-list (cadr (get-funcall-closure (car funcall) environment)) (add-parameters-to-environment (car (get-funcall-closure (car funcall) environment)) (parameters funcall) (push-frame environment) throw) func-return breakOutsideLoopError continueOutsideLoopError throw)) ;checks if the funcall is a dot function
         ((not (exists? (function-name funcall) environment)) (myerror "Function does not exist")) ;checks if the function exists
         ((null? (parameters funcall)) (interpret-function-statement-list (statement-list-of-function (lookup (function-name funcall) environment)) (push-frame (pop-frame environment)) func-return breakOutsideLoopError continueOutsideLoopError throw)) ; checks if there are parameters
         (else (interpret-function-statement-list (statement-list-of-function (lookup (function-name funcall) environment)) (add-parameters-to-environment (func-name (lookup (function-name funcall) environment)) (parameters funcall) (push-frame environment) throw) func-return breakOutsideLoopError continueOutsideLoopError throw)))))))
  
(define function-name car)
(define parameters cdr)
(define first car)
(define rest-of cdr)

(define get-funcall-closure
  (lambda (dot-funcall environment)
    (lookup (caddr dot-funcall) (lookup (cadr dot-funcall) environment)))) ; NOTE outer lookup broke on .add bc lookup returned frame instead of environment

; adds the given parameters to the givene environment
(define add-parameters-to-environment
  (lambda (param-names param-values environment throw)
    (cond
      ((null? param-names) environment)
      ((not (eq? (length param-names) (length param-values))) (myerror "Mismatching parameters and arguments"))
      ((list? param-names) (add-parameters-to-environment (parameters param-names) (parameters param-values) (insert (first param-names) (eval-expression (first param-values) (pop-frame environment) throw) environment) throw))
      (else (insert param-names (eval-expression param-values (pop-frame environment)) environment)))))

; The same as interpret-statement-list except at the end it returns the environment
(define interpret-function-statement-list
  (lambda (statement-list environment return break continue throw)
    (cond 
        ((null? statement-list) (pop-frame environment)) 
        (else (interpret-function-statement-list (rest-of statement-list) (interpret-statement (first statement-list) environment return break continue throw) return break continue throw)))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))


(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (get-finally-statement finally-statement))))))

(define get-finally-statement cadr)

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment throw)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment throw)))
      ((eq? (operator expr) 'dot) (interpret-dot (cadr expr) (caddr expr) environment throw))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment throw) environment throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? 'funcall (operator expr)) (interpret-funcall (cdr expr) environment throw))
      ;((eq? 'dot (operator expr)) (interpret-dot (cadr expr) (caddr expr) environment throw))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

;------------------------
; Tests
;------------------------
;(interpret "tests/0.txt" 'A) ;55
;(interpret "tests/1.txt" 'A) ;15
;(interpret "tests/2.txt" 'A) ;12
;(interpret "tests/3.txt" 'A) ;125
(interpret "tests/4.txt" 'A) ;36
;(interpret "tests/5.txt" 'A) ;54
;(interpret "tests/6.txt" 'A) ;110
;(interpret "tests/7.txt" 'C) ;26
;(interpret "tests/8.txt" 'Square) ;117
;(interpret "tests/9.txt" 'Square) ;32
;(interpret "tests/10.txt" 'List) ;15
;(interpret "tests/11.txt" 'List) ;123456
;(interpret "tests/12.txt" 'List) ;5285
;(interpret "tests/13.txt" 'C) ;-716