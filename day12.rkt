#lang racket
; find-path-to: String String [List-of (list String String)] [List-of String] [[List-of String] -> Bool] -> Nat
(define (find-path-to from end neighbors seen should-stop)
  (cond [(equal? from end) 1]
        [(should-stop seen from) 0]
        [else
         (define new-set (if (equal? from (string-downcase from))
                             (cons from seen)
                             seen))
         (for/sum ([neighbor (get-neighbors from neighbors)])
           (find-path-to neighbor end neighbors new-set should-stop))]))

(define/match (get-neighbors n loss)
  [(_ (cons (list n1 n2) rest))
     (cond [(equal? n n1) (cons n2 (get-neighbors n rest))]
           [(equal? n n2) (cons n1 (get-neighbors n rest))]
           [else (get-neighbors n rest)])]
  [(_ _) '()])

(define (part1 lolss) (find-path-to "start" "end" lolss '() (λ (seen n) (list? (member n seen)))))
(define (part2 lolss) (find-path-to "start" "end" lolss '()
                                    (compose not should-allow-node)))
; should-allow-node : [List-of String] String -> Boolean
(define (should-allow-node los s)
  (define has-been-visited (list? (member s los)))
  (define (any-duplicates? los)
    (and (cons? los)
         (or (list? (member (first los) (rest los)))
             (any-duplicates? (rest los)))))
  (and (not (and (string=? s "start") has-been-visited))
       (not (and has-been-visited (any-duplicates? los)))))

(define INPUT '(
("end" "MY")
("MY" "xc")
("ho" "NF")
("start" "ho")
("NF" "xc")
("NF" "yf")
("end" "yf")
("xc" "TP")
("MY" "qo")
("yf" "TP")
("dc" "NF")
("dc" "xc")
("start" "dc")
("yf" "MY")
("MY" "ho")
("EM" "uh")
("xc" "yf")
("ho" "dc")
("uh" "NF")
("yf" "ho")
("end" "uh")
("start" "NF")
))

(define test-input '(
("fs" "end")
("he" "DX")
("fs" "he")
("start" "DX")
("pj" "DX")
("end" "zg")
("zg" "sl")
("zg" "pj")
("pj" "he")
("RW" "he")
("fs" "DX")
("pj" "RW")
("zg" "RW")
("start" "pj")
("he" "WI")
("zg" "he")
("pj" "fs")
("start" "RW")
))