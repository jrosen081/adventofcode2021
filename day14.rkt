#lang racket

; A Count is a [Hash Str Nat]

; update-cache : {X} [Hash X Nat] [Hash X Nat] -> [Hash X Nat]
(define (update-cache h1 h2)
  (for/fold ([prev h1])
            ([key-val (hash->list h2)])
    (begin
      (define key (car key-val))
      (define val (cdr key-val))
      (hash-update prev
                   key
                   (lambda (x) (+ x val))
                   0))))

; expand-current: [List-of String] [Hash-of Str Str] [Hash-of (list Nat Str Str) Count] Nat -> Count
(define (expand-current los mapping cache amount)
  (if (zero? amount)
      (get-counts los (make-immutable-hash))
      (match los
        [(list-rest f s more)
         (define cache-hit (hash-ref cache (list amount f s) #f))
         (cond [cache-hit
                (define rest-expanded (expand-current (cons s more)
                                                      mapping
                                                      cache
                                                      amount))
                (update-cache cache-hit rest-expanded)]
               [else
                (define middle (hash-ref mapping (string-append f s)))
                (define stepped (hash-update
                                 (expand-current (list f middle)
                                                 mapping
                                                 cache
                                                 (sub1 amount))
                                 middle
                                 sub1))
                (define stepped-2 (hash-update
                                   (expand-current (list middle s)
                                                   mapping
                                                   cache
                                                   (sub1 amount))
                                   s
                                   sub1))
                (define rest-expanded (expand-current (cons s more)
                                                      mapping
                                                      cache
                                                      amount))
                (hash-set! cache
                           (list amount f s)
                           (update-cache stepped stepped-2))
                
                (define new-union
                  (update-cache rest-expanded
                                (update-cache stepped stepped-2)))
                new-union])]
        [_ (get-counts los (make-immutable-hash))])))

; get-counts: [List-of String] [Hash Str Nat] -> [Hash Str Nat]
(define (get-counts los counts)
  (for/fold ([count counts])
            ([f los])
    (hash-update count f add1 0)))

; difference-with-steps : [List-of String] [Hash Str Nat] Nat -> Nat
(define (difference-with-steps los hash n)
  (define counts (hash->list (expand-current los hash (make-hash) n)))
  (define max (argmax cdr counts))
  (define min (argmin cdr counts))
  (- (cdr max) (cdr min)))

(define (part1 los hash) (difference-with-steps los hash 10))
(define (part2 los hash)
  (difference-with-steps los hash 40))

(define test-inputs '("N" "N" "C" "B"))
(define test-hash (make-immutable-hash (list
                                        (cons "CH" "B")
                                        (cons "HH" "N")
                                        (cons "CB" "H")
                                        (cons "NH" "C")
                                        (cons "HB" "C")
                                        (cons "HC" "B")
                                        (cons "HN" "C")
                                        (cons "NN" "C")
                                        (cons "BH" "H")
                                        (cons "NC" "B")
                                        (cons "NB" "B")
                                        (cons "BN" "B")
                                        (cons "BB" "N")
                                        (cons "BC" "B")
                                        (cons "CC" "N")
                                        (cons "CN" "C")
                                        )))

(define REAL-HASH (make-immutable-hash (list
                                        (cons "SN" "H")
                                        (cons "KP" "O")
                                        (cons "CP" "V")
                                        (cons "FN" "P")
                                        (cons "FV" "S")
                                        (cons "HO" "S")
                                        (cons "NS" "N")
                                        (cons "OP" "C")
                                        (cons "HC" "S")
                                        (cons "NP" "B")
                                        (cons "CF" "V")
                                        (cons "NN" "O")
                                        (cons "OS" "F")
                                        (cons "VO" "V")
                                        (cons "HK" "N")
                                        (cons "SV" "V")
                                        (cons "VC" "V")
                                        (cons "PH" "K")
                                        (cons "NH" "O")
                                        (cons "SB" "N")
                                        (cons "KS" "V")
                                        (cons "CB" "H")
                                        (cons "SS" "P")
                                        (cons "SP" "H")
                                        (cons "VN" "K")
                                        (cons "VP" "O")
                                        (cons "SK" "V")
                                        (cons "VF" "C")
                                        (cons "VV" "B")
                                        (cons "SF" "K")
                                        (cons "HH" "K")
                                        (cons "PV" "V")
                                        (cons "SO" "H")
                                        (cons "NK" "P")
                                        (cons "NO" "C")
                                        (cons "ON" "S")
                                        (cons "PB" "K")
                                        (cons "VS" "H")
                                        (cons "SC" "P")
                                        (cons "HS" "P")
                                        (cons "BS" "P")
                                        (cons "CS" "P")
                                        (cons "VB" "V")
                                        (cons "BP" "K")
                                        (cons "FH" "O")
                                        (cons "OF" "F")
                                        (cons "HF" "F")
                                        (cons "FS" "C")
                                        (cons "BN" "O")
                                        (cons "NC" "F")
                                        (cons "FC" "B")
                                        (cons "CV" "V")
                                        (cons "HN" "C")
                                        (cons "KF" "K")
                                        (cons "OO" "P")
                                        (cons "CC" "S")
                                        (cons "FF" "C")
                                        (cons "BC" "P")
                                        (cons "PP" "F")
                                        (cons "KO" "V")
                                        (cons "PC" "B")
                                        (cons "HB" "H")
                                        (cons "OB" "N")
                                        (cons "OV" "S")
                                        (cons "KH" "B")
                                        (cons "BO" "B")
                                        (cons "HV" "P")
                                        (cons "BV" "K")
                                        (cons "PS" "F")
                                        (cons "CH" "C")
                                        (cons "SH" "H")
                                        (cons "OK" "V")
                                        (cons "NB" "K")
                                        (cons "BF" "S")
                                        (cons "CO" "O")
                                        (cons "NV" "H")
                                        (cons "FB" "K")
                                        (cons "FO" "C")
                                        (cons "CK" "P")
                                        (cons "BH" "B")
                                        (cons "OH" "F")
                                        (cons "KB" "N")
                                        (cons "OC" "K")
                                        (cons "KK" "O")
                                        (cons "CN" "H")
                                        (cons "FP" "K")
                                        (cons "VH" "K")
                                        (cons "VK" "P")
                                        (cons "HP" "S")
                                        (cons "FK" "F")
                                        (cons "BK" "H")
                                        (cons "KV" "V")
                                        (cons "BB" "O")
                                        (cons "KC" "F")
                                        (cons "KN" "C")
                                        (cons "PO" "P")
                                        (cons "NF" "P")
                                        (cons "PN" "S")
                                        (cons "PF" "S")
                                        (cons "PK" "O")
                                        )))

(define INPUT '("N" "B" "O" "K" "H" "V" "H" "O" "S" "V" "K" "S" "S" "B" "S" "V" "V" "B" "C" "S"))