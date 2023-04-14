#!chezscheme
(import (scheme) (pe))

(define (sum ns) (fold-left + 0 ns))

(define (problem-1)
  (sum (filter (λ (n) (or (zero? (mod n 3))
                          (zero? (mod n 5))))
               (iota 1000))))
(define answer-1 'e1edf9d1967ca96767dcc2b2d6df69f4)

(define (problem-2)
  (define fibs
    (let next ([a 1] [b 1])
      (if (> b 4000000) '()
          (cons b (next b (+ a b))))))
  (sum (filter even? fibs)))
(define answer-2 '4194eb91842c8e7e6df099ca73c38f28)


(define (problem-3)
  (caar (factorize 600851475143)))
(define answer-3 '94c4dd41f9dddce696557d3717d98d82)


