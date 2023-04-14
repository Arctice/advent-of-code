#!chezscheme
(import (scheme) (pe))

;; It is easily proved that no equilateral triangle exists with integral
;; length sides and integral area. However, the almost equilateral triangle
;; 5-5-6 has an area of 12 square units.

;; We shall define an almost equilateral triangle to be a triangle for which
;; two sides are equal and the third differs by no more than one unit.

;; Find the sum of the perimeters of all almost equilateral triangles with
;; integral side lengths and area and whose perimeters do not exceed one
;; billion (1,000,000,000).

;; √(a² - ((a+1)/2)²) * (a+1) * 0.5

(define (integer-triangle a c)
  (let ([s (* (- (* a a) (* c c 1/4)) (+ a 1) (+ a 1) 1/4)])
    (square? s)))

(define (square? x)
  (let-values ([[s r] (exact-integer-sqrt x)]) (zero? r)))

(define (problem-94)
  (let next ([a 3] [count 0])
    (let ([int-area
           (or (integer-triangle a (+ a 1))
               (integer-triangle a (- a 1)))]
          [perimeter (* 3 a)])
      (cond
       [(< 1000000000 perimeter) count]
       [int-area
        ;; (printf "~s-~s-~s: ~s\n" a a (+ 1 a) perimeter)
        (next
         (if (< 1000 a)
             (let ([next (inexact->exact (round (* a 3.73)))])
               (if (even? next) (- next 1) next))
             (+ a 2))
         (+ count
            (+ perimeter 
               (if (integer-triangle a (+ a 1)) 1 -1))))]
       [else (next (+ a 2) count)]))))

(define answer-94 '3218c6bb59f2539ec39ad4bf37c10913)
