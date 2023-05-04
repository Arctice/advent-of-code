#!chezscheme
(import (scheme) (pe))

;; Let (a, b, c) represent the three sides of a right angle triangle with
;; integral length sides. It is possible to place four such triangles
;; together to form a square with length c.

;; For example, (3, 4, 5) triangles can be placed together to form a 5 by 5
;; square with a 1 by 1 hole in the middle and it can be seen that the 5 by 5
;; square can be tiled with twenty-five 1 by 1 squares.

;; However, if (5, 12, 13) triangles were used then the hole would measure 7
;; by 7 and these could not be used to tile the 13 by 13 square.

;; Given that the perimeter of the right triangle is less than one-hundred
;; million, how many Pythagorean triangles would allow such a tiling to take
;; place?


;; a² + b² = c²
;; d = (b - a)
;; c mod d = 0

;; it turns out that if we only consider primitive pythagorean triples, where
;; (a, b, c) are coprime, d can only be equal to 1 (proof?)
;; finding these unique triangles should be much easier and the other cases can
;; be generated from there by multiplying the primitive triangles up to the
;; maximum perimeter
;; max c = 41421357
;; relevant oeis:
;; A001653 - Numbers k such that 2*k^2 - 1 is a square. 

(define (square? x)
  (let-values ([[s r] (exact-integer-sqrt x)]) (zero? r)))

(define (problem-139)
  (define primitive-c
    (let next ([c 5])
      (cond [(square? (- (* c c 2) 1))
             (cons c (next (+ c 1)))]
            [(< c 41421358) (next (+ c 1))]
            [else '()])))
  (define perimeters
    (map (λ (c)
           (let ([a (exact (floor (sqrt (* c c 0.5))))])
             (+ a a 1 c)))
         primitive-c))
  (define max-factors
    (map (λ (p) (floor (/ 100000000 p)))
         perimeters))
  (fold-left + 0 max-factors))

(define answer-139 '1c343ba00e6d17d7239bf45869ffed0c)

