#!chezscheme
(import (scheme) (pe))

;; A number consisting entirely of ones is called a repunit. We shall define
;; R(k) to be a repunit of length k; for example, R(6) = 111111.

;; Given that n is a positive integer and GCD(n, 10) = 1, it can be shown
;; that there always exists a value, k, for which R(k) is divisible by n, and
;; let A(n) be the least such value of k; for example, A(7) = 6 and A(41) =
;; 5.

;; The least value of n for which A(n) first exceeds ten is 17.

;; Find the least value of n for which A(n) first exceeds one-million.

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
  (time
   (find
    (λ (n) (if (or (even? n) (zero? (mod n 5))) #f
               (< 1000000 (A n))))
    (map (λ (n) (+ n 1000000))
         (iota 1000)))))


(define answer '82cd979a2b79600137aea54fa0bd944b)
(verify solution answer)