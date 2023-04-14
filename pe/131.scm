#!chezscheme
(import (scheme) (pe))

;; There are some prime values, p, for which there exists a positive integer,
;; n, such that the expression n^3 + n^2p is a perfect cube.

;; For example, when p = 19, 8^3 + 8^2×19 = 12^3.

;; What is perhaps most surprising is that for each prime with this property
;; the value of n is unique, and there are only four such primes below
;; one-hundred.

;; How many primes below one million have this remarkable property?

;; (n³ + n²p) = c³
;; n²(n + p) = c³
;; n+p and n are relatively prime
;; therefore c is a cube iff n + p and n are both cubes
;; unproven: all such p are probably the differences between consecutive cubes

(define solution 0)

(define (problem-131)
  (call/1cc
   (λ (break)
     (for-each
      (λ (a)
        (let* ([b (+ a 1)]
               [n (* a a a)]
               [np (* b b b)]
               [d (- np n)])
          (when (prime? d)
            ;; (printf "p ~s ∛n ~s\n" d a)
            (if (< d 1000000)
                (set! solution (+ 1 solution))
                (break)))))
      (cdr (iota 10000)))))
  solution)

(define answer-131 'f7e6c85504ce6e82442c770f7c8606f0)
