#lang racket

; perform-exponential-reproduction : [List-of Integer] -> Integer
(define (perform-exponential-reproduction loi amount)
  (define hash (make-hash))
  (for/sum ([val loi])
    (total-reproductions val amount hash)))

(define (part1 loi)
  (perform-exponential-reproduction loi 80))
(define (part2 loi)
  (perform-exponential-reproduction loi 256))

; total-reproductions: Natural Natural [Hash (cons Natural Natural) Natural] -> Natural
(define (total-reproductions n steps hash)
  (define repros (hash-ref hash (cons n steps) #f))
  (cond [(number? repros) repros]
        [(zero? steps) 1]
        [else (let ((total (for/sum ([num (if (zero? n) (list 6 8) (list (sub1 n)))])
                     (total-reproductions num (sub1 steps) hash))))
        (hash-set! hash (cons n steps) total)
        total)]))

(define INPUT '(1 1 1 2 1 5 1 1 2 1 4 1 4 1 1 1 1 1 1 4 1 1 1 1 4 1 1 5 1 3 1 2 1 1 1 2 1 1 1 4 1 1 3 1 5 1 1 1 1 3 5 5 2 1 1 1 2 1 1 1 1 1 1 1 1 5 4 1 1 1 1 1 3 1 1 2 4 4 1 1 1 1 1 1 3 1 1 1 1 5 1 3 1 5 1 2 1 1 5 1 1 1 5 3 3 1 4 1 3 1 3 1 1 1 1 3 1 4 1 1 1 1 1 2 1 1 1 4 2 1 1 5 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1 1 1 1 5 1 1 1 1 3 1 1 1 1 1 3 4 1 2 1 3 2 1 1 2 1 1 1 1 4 1 1 1 1 4 1 1 1 1 1 2 1 1 4 1 1 1 5 3 2 2 1 1 3 1 5 1 5 1 1 1 1 1 5 1 4 1 2 1 1 1 1 2 1 3 1 1 1 1 1 1 2 1 1 1 3 1 4 3 1 4 1 3 2 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 2 1 5 1 1 1 1 2 1 1 1 3 5 1 1 1 1 5 1 1 2 1 2 4 2 2 1 1 1 5 2 1 1 5 1 1 1 1 5 1 1 1 2 1))