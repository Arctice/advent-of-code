#!chezscheme
(import (scheme) (pe))

;; It can be seen that the number, 125874, and its double, 251748, contain
;; exactly the same digits, but in a different order.

;; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
;; contain the same digits.


(define (digits n)
  (if (< 0 n) (cons (mod n 10) (digits (div n 10))) '()))

(define (anagram? a b)
  (equal? (sort < (digits a)) (sort < (digits b))))

(define solution
  (find
   (λ (n) (for-all (λ (m) (anagram? n (* n m))) '(2 3 4 5 6)))
   (list-tail (iota 1000000) 2)))

(define answer 'a420384997c8a1a93d5a84046117c2aa)
(verify solution answer)
