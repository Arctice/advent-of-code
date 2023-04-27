#!chezscheme
(import (scheme) (pe))

;; Consider the isosceles triangle with base length, b = 16, and legs, L =
;; 17.

;; By using the Pythagorean theorem it can be seen that the height of the
;; triangle, h = √(17^2 − 8^2) = 15, which is one less than the base length.

;; With b = 272 and L = 305, we get h = 273, which is one more than the base
;; length, and this is the second smallest isosceles triangle with the
;; property that h = b ± 1.

;; Find ∑ L for the twelve smallest isosceles triangles for which h = b ± 1
;; and b, L are positive integers.


;; b² + (2b±1)² = L²
;; 5b² ± 4b + 1 = L²

;; these seem to be:
;; the solutions (b, 2b+1 and L) are primitive pythagorean triples
;; the ratio of successive values of b converges on φ^6
;; OEIS A195500 - Pythagorean approximations to 2

(define (problem-138)
  (define (denominator n)
    (let next ([a 8] [b 136] [c 2448] [n n])
      (if (zero? n) a
          (next b c (- (* 17 (+ b c)) a) (- n 1)))))
  (define (nominator n)
    ;; Fib(3n+1)*Fib(3n+2)
    (let next ([a 15] [b 273] [c 4895] [n n])
      (if (zero? n) a
          (next b c (- (* 17 (+ b c)) a) (- n 1)))))
 (fold-left
  + 0
  (map (λ (n) (let ([a (nominator n)]
                    [b (denominator n)])
                (sqrt (+ (* a a) (* b b)))))
       (iota 12))))

(define answer-138 'f7524f4d0d6d042c0f92a0d6469aff85)
