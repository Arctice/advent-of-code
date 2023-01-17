#!chezscheme
(import (scheme) (pe))

;; A number consisting entirely of ones is called a repunit. We shall define
;; R(k) to be a repunit of length k.

;; For example, R(10) = 1111111111 = 11×41×271×9091, and the sum of these
;; prime factors is 9414.

;; Find the sum of the first forty prime factors of R(10^9).


;; apparently a repunit is a (10^n - 1) / 9, so:
(define (repunit-mod k m)
  (mod (/ (- (expt-mod 10 k m) 1) 9) m))


(define solution
  (let ([k 1000000000])
    (time
     (let scan ([d 7] [factors '()])
       (cond
        [(= (length factors) 40)
         (fold-left + 0 factors)]
        [(not (prime? d)) (scan (+ 2 d) factors)]
        [(not (zero? (repunit-mod k d))) (scan (+ 2 d) factors)]
        [else (scan (+ 2 d) (cons d factors))])))))



(define answer '5df3a36faa173a393a04a022b2d5d49d)
(verify solution answer)
