#lang racket

(define-syntax-rule (for/filter ([e L]) E1)
  (for/list ([e L]
             #:when E1)
    e))

(define-syntax for/filter/map
  (syntax-rules (where)
    [(for/filter/map ([e L] where E1) E2 E3 ...)
     (for/list ([e L] #:when E1)
       E2
       E3 ...)]))

(provide for/filter for/filter/map)