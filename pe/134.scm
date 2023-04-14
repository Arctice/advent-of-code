#!chezscheme
(import (scheme) (pe))

;; Consider the consecutive primes p[1] = 19 and p[2] = 23. It can be
;; verified that 1219 is the smallest number such that the last digits are
;; formed by p[1] whilst also being divisible by p[2].

;; In fact, with the exception of p[1] = 3 and p[2] = 5, for every pair of
;; consecutive primes, p[2] > p[1], there exist values of n for which the
;; last digits are formed by p[1] and n is divisible by p[2]. Let S be the
;; smallest of these values of n.

;; Find ∑ S for every pair of consecutive primes with 5 ≤ p[1] ≤ 1000000.

;; d = 10^digits(p1)
;; k = the prefix of S
;; S = p1 + d * k
;; S = 0 mod p2
;; p1 + d * k = 0 mod p2
;; k = -p1 * d⁻¹ mod p2
;; https://en.wikipedia.org/wiki/Modular_multiplicative_inverse#Using_Euler's_theorem
;; n⁻¹ mod m = n^(φ(m) - 1) mod m
;; when m is prime, φ(m) = m - 1:
;; n⁻¹ mod m = n^(m-2) mod m
;; k = (-p1 * d ^ (p2 - 2)) mod p2


(define (solve p1 p2)
  (let* ([d (exact (ceiling (log p1 10)))]
         [d (expt 10 d)]
         [n-inverse (expt-mod d (- p2 2) p2)]
         [k (mod (* (- p1) n-inverse) p2)])
    (+ (* d k) p1)))

(define primes (cddr (sieve 1000100)))
(define (problem-134)
  (fold-left
   + 0
   (map (λ (p1 p2)
          (if (< p1 1000000) (solve p1 p2) 0))
        (list-head primes (- (length primes) 1))
        (cdr primes))))


(define answer-134 'f12b07460d2586ea47b4d305ae0b0539)

