#!chezscheme
(import (scheme) (pe))

;; A number consisting entirely of ones is called a repunit. We shall define
;; R(k) to be a repunit of length k; for example, R(6) = 111111.

;; Given that n is a positive integer and GCD(n, 10) = 1, it can be shown
;; that there always exists a value, k, for which R(k) is divisible by n, and
;; let A(n) be the least such value of k; for example, A(7) = 6 and A(41) =
;; 5.

;; You are given that for all primes, p > 5, that p − 1 is divisible by A(p).
;; For example, when p = 41, A(41) = 5, and 40 is divisible by 5.

;; However, there are rare composite values for which this is also true; the
;; first five examples being 91, 259, 451, 481, and 703.

;; Find the sum of the first twenty-five composite values of n for which
;; GCD(n, 10) = 1 and n − 1 is divisible by A(n).

(define (A n)
  (let ([l (mod n 10)])
    (let next ([d 0] [sum n])
      (if (zero? sum) d
          (let* ([first (mod sum 10)]
                 [rotate
                  (let next ([r 0])
                    (if (= 1 (mod (+ first (* l r)) 10))
                        r (next (+ r 1))))])
            (next (+ d 1) (div (+ sum (* rotate n)) 10)))))))

(define solution
  (let scan ([n 1] [count 0])
    (cond
     [(= count 25) 0]
     [(and (= 1 (gcd n 10))
           (not (prime? n))
           (zero? (mod (- n 1) (A n))))
      (+ n (scan (+ n 1) (+ count 1)))]
     [else (scan (+ n 1) count)])))


(define answer '20594ea0ef7a2f4cf40d19a9b82a0beb)
(verify solution answer)
