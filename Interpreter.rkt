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
; Main Interpreter
