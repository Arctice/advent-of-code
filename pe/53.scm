#!chezscheme
(import (scheme) (pe))

;; There are exactly ten ways of selecting three from five, 12345:

;;            123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

;; In combinatorics, we use the notation, ^5C[3] = 10.

;; In general,

;;    ^nC[r] =    n!    ,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
;;             r!(n−r)!

;; It is not until n = 23, that a value exceeds one-million: ^23C[10] =
;; 1144066.

;; How many, not necessarily distinct, values of  ^nC[r], for 1 ≤ n ≤ 100,
;; are greater than one-million?

(define (fact n) (fold-left * 1 (list-tail (iota (+ n 1)) 1)))
(define (binom n r)
  (let ([a (min r (- n r))]
        [b (max r (- n r))])
    (div (fold-left * 1 (list-tail (iota (+ n 1))
                                   (+ 1 b)))
         (fact a))))

(define (problem-53)
  (fold-left
   + 0 (map (λ (n) (length (filter (λ (r) (< 1000000 (binom n r))) (iota n))))
            (iota 101))))


(define answer-53 'e3b21256183cf7c2c7a66be163579d37)
