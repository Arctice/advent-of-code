#!chezscheme
(import (core))

(define input (slurp "6.input"))

(define (first-substring-without-duplicates n)
  (let ([seen (make-vector 26 0)]
        [last-duplicate 0])
    (define (char-id c) (- (char->integer c) 97))
    (exists
     (λ (i k)
       (let ([previous (vector-ref seen k)])
         (set! last-duplicate (max last-duplicate previous))
         (vector-set! seen k i)
         (and (<= n (- i last-duplicate)) i)))
     (iota (string-length input))
     (map char-id (string->list input)))))

(printf "part 1: ~s\n" (+ 1 (first-substring-without-duplicates 4)))
(printf "part 2: ~s\n" (+ 1 (first-substring-without-duplicates 14)))
