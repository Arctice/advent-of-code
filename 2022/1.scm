#!chezscheme
(import (scheme) (core))

(define input
  (let ([seq (map string->number
                  (readlines "1.input"))])
    (let loop ([rest seq]
               [group '()])
      (cond [(null? rest) '()]
            [(eq? #f (car rest))
             (cons group (loop (cdr rest) '()))]
            [(loop (cdr rest)
                   (cons (car rest) group))]))))

(define (sum xs) (fold-left + 0 xs))
(define sums (map sum input))

(printf "part 1: ~s\n"
        (fold-left max (car sums) (cdr sums)))

(printf "part 2: ~s\n"
        (sum (list-head (sort > sums) 3)))


