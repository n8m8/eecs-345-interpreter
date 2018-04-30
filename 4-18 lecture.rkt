#lang racket

(define foldleft
  (lambda (f i lis)
    (if (null? lis)
        i
        (foldleft f (f i (car lis)) (cdr lis))))) ; uses tail recursion

(define foldright
  (lambda (f i lis)
    (if (null? lis)
        i
        (f (car lis) (foldright f i (cdr lis))))))