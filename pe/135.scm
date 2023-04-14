#!chezscheme
(import (scheme) (pe))

;; Problem 135

;; Given the positive integers, x, y, and z, are consecutive terms of an
;; arithmetic progression, the least value of the positive integer, n, for
;; which the equation, x^2 − y^2 − z^2 = n, has exactly two solutions is n =
;; 27:

;;                 34^2 − 27^2 − 20^2 = 12^2 − 9^2 − 6^2 = 27

;; It turns out that n = 1155 is the least value which has exactly ten
;; solutions.

;; How many values of n less than one million have exactly ten distinct
;; solutions?

;; x2 - (x-s)2 - (x-2s)2
;; x2 - 6xs + 5s2 + n = 0
;; Δ = b2 - 4ac = 16s2 - 4n
;; x = (6s ± √Δ) / 2 = 3s ± √(Δ/4)
;; d = √(Δ/4)
;; two solutions if (x' - step - step) is positive
;; 0 < 3s - d - 2s, or simply d < s

;; this method relies on the observation that solutions only exist when d is an
;; integer, and that:
;; d² = 4s² - n
;; n = 4s² - d²
;; since n is a difference of two perfect squares, we can efficiently iterate
;; over all s and all d where 0 < 4s² - d² < 1000000, and count every n we find.

;; the naive approach, iterating over s for a given n would run into many values
;; of d that aren't integers and provide no solutions for that n, while this one
;; counts most of the pairs it finds, outside of the edges where they fall out
;; of range.

(define (solve maximum)
  (let ([counts (make-bytevector maximum 0)])
    (let next-s ([s 1])
      (let next-d ([d (flonum->fixnum
                       (flfloor (sqrt (max 0. (- (* 4 s s) maximum)))))]
                   [prev-n 0])
        (let ([n (fx- (fx* 4 s s) (fx* d d))])
          (cond
           [(<= maximum n)
            (next-d (+ 1 d) n)]
           [(<= n 0)
            (if (not (<= maximum prev-n))
                (next-s (+ 1 s)))]
           [else
            (let ([two-solutions (and (< d s) (not (zero? d)))])
              (bytevector-u8-set! counts n
                             (+ (bytevector-u8-ref counts n)
                                (if two-solutions 2 1))))
            (next-d (+ 1 d) n)]))))
    counts))

(define (problem-135)
  (let* ([maximum 1000000]
         [counts (solve maximum)])
    (length (filter
             (λ (n) (= 10 (bytevector-u8-ref counts n)))
             (cdr (iota maximum))))))

(define answer-135 'c457d7ae48d08a6b84bc0b1b9bd7d474)

;; Problem 136

;;    The positive integers, x, y, and z, are consecutive terms of an arithmetic
;;    progression. Given that n is a positive integer, the equation, x^2 − y^2 −
;;    z^2 = n, has exactly one solution when n = 20:

;;                              13^2 − 10^2 − 7^2 = 20

;;    In fact there are twenty-five values of n below one hundred for which the
;;    equation has a unique solution.

;;    How many values of n less than fifty million have exactly one solution?

(define (problem-136)
  (let* ([maximum 50000000]
         [counts (solve maximum)]
         [c 0])
    (do ([n 1 (+ 1 n)]
         [c 0 (+ c (if (= 1 (bytevector-u8-ref counts n)) 1 0))])
        ((<= maximum n) c))))

(define answer-136 '91db9e8e6cb2dbf9c07a6e0429697336)

