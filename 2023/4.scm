(import (core))

(define (parse line)
  (define (parse-nums ss)
    (filter values
            (map string->number
                 (string-split " " (string-strip ss)))))
  (let ([groups (string-split
                 "|" (cadr (string-split ":" line)))])
    (cons* (parse-nums (car groups))
           (parse-nums (cadr groups)))))

(define (win-size card)
  (let ([A (car card)] [B (cdr card)])
    (length (filter (λ (a) (memq a B)) A))))

(define input (map parse (readlines "4.input")))

;; part 1
(pretty-print
 (fold-left
  + 0
  (map (λ (card)
         (let ([score (win-size card)])
           (if (zero? score) 0 (expt 2 (- score 1)))))
       input)))

;; part 2

(let ([cards (make-vector (length input) 1)])
  (let scan ([n 0])
    (when (< n (vector-length cards))
      (let* ([count (vector-ref cards n)]
             [wins (win-size (list-ref input n))])
        (for-each
         (λ (k) (let ([at (+ 1 k n)])
                  (vector-set! cards at
                               (+ (vector-ref cards at) count))))
         (iota wins))
        (scan (+ 1 n)))))
  (pretty-print (fold-left + 0 (vector->list cards))))
