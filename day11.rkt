#lang racket

; increase-by-one : [List-of [List-of Integer]] -> [List-of [List-of Integer]]
(define (increase-by-one loloi)
  (for/list ([row loloi])
    (for/list ([i row])
      (add1 i))))

; total-flashes: [List-of [List-of Integer]] -> Nat
(define (total-flashes loloi)
  (for*/sum ([row loloi]
             [i row]
             #:when (> i 9))
    1))

; safe-list-ref : [List-of X] Nat X -> X
(define (safe-list-ref lox nat fault)
  (if (or  (< nat 0) (>= nat (length lox)))
      fault
      (list-ref lox nat)))


; update-due-to-flashes: [List-of [List-of Integer]] -> [List-of [List-of Integer]]
(define (update-due-to-flashes loloi)
  (define inner-len (length (first loloi)))
  (for/list ([row (length loloi)])
    (for/list ([column inner-len])
      (define value (list-ref (list-ref loloi row) column))
      (if (or (zero? value) (> value 9))
          0
          (+ value
             (length (filter (λ (n) (> n 9))
                             (list (safe-list-ref (safe-list-ref loloi
                                                                 (add1 row)
                                                                 '())
                                                  column
                                                  0)
                                   (safe-list-ref (safe-list-ref loloi
                                                                 (sub1 row)
                                                                 '())
                                                  column
                                                  0)
                                   (safe-list-ref (safe-list-ref loloi
                                                                 row
                                                                 '())
                                                  (sub1 column)
                                                  0)
                                   (safe-list-ref (safe-list-ref loloi
                                                                 row
                                                                 '())
                                                  (add1 column)
                                                  0)
                                   (safe-list-ref (safe-list-ref loloi
                                                                 (sub1 row)
                                                                 '())
                                                  (sub1 column)
                                                  0)
                                   (safe-list-ref (safe-list-ref loloi
                                                                 (sub1 row)
                                                                 '())
                                                  (add1 column)
                                                  0)
                                   (safe-list-ref (safe-list-ref loloi
                                                                 (add1 row)
                                                                 '())
                                                  (add1 column)
                                                  0)
                                   (safe-list-ref (safe-list-ref loloi
                                                                 (add1 row)
                                                                 '())
                                                  (sub1 column)
                                                  0)))))))))

; perform-flashes-for-step: [List-of [List-of Integer]] -> [List-of [List-of Integer]]
(define (perform-flashes-for-step loloi)
  (define total-flash (total-flashes loloi))
  (if (zero? total-flash)
      (values 0 loloi)
      (let-values ([(flashes fin) (perform-flashes-for-step (update-due-to-flashes loloi))])
        (values (+ total-flash flashes) fin))))

; perform-n-steps : [List-of [List-of Integer]] Nat -> Nat
(define (perform-n-steps loloi n)
  (define increased (increase-by-one loloi))
  (cond [(zero? n) 0]
        [else
         (let-values ([(flashes fin) (perform-flashes-for-step increased)])
           (+ flashes (perform-n-steps fin (sub1 n))))]))

; perform-steps-until-all : [List-of [List-of Integer]] -> Nat
(define (perform-steps-until-all loloi)
  (define-values (flashes fin) (perform-flashes-for-step (increase-by-one loloi)))
  (if (= flashes 100)
      1
      (add1 (perform-steps-until-all fin))))

(define (part1 loloi) (perform-n-steps loloi 100))
(define part2 perform-steps-until-all)

(define test-input '(
(5 4 8 3 1 4 3 2 2 3)
(2 7 4 5 8 5 4 7 1 1)
(5 2 6 4 5 5 6 1 7 3)
(6 1 4 1 3 3 6 1 4 6)
(6 3 5 7 3 8 5 4 7 8)
(4 1 6 7 5 2 4 6 4 5)
(2 1 7 6 8 4 1 7 2 1)
(6 8 8 2 8 8 1 1 3 4)
(4 8 4 6 8 4 8 5 5 4)
(5 2 8 3 7 5 1 5 2 6)
))

(define INPUT '(
(2 5 2 4 2 5 5 3 3 1)
(1 1 3 5 6 2 5 8 8 1)
(2 8 3 8 3 5 3 8 6 3)
(1 6 6 2 3 1 2 3 6 5)
(6 8 4 7 8 3 5 8 2 5)
(2 1 8 5 6 8 4 3 6 7)
(6 8 7 4 2 1 2 8 3 1)
(5 3 8 7 2 4 7 8 1 1)
(2 2 5 5 4 8 2 8 7 5)
(8 5 2 8 5 5 7 1 3 1)
))