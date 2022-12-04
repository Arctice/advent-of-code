#!chezscheme
(import (scheme) (core))

(define input
  (let ([lines (readlines "4.input")])
    (define (parse-assignment ss)
      (apply cons* (map string->number
                        (string-split "-" ss))))
    (map (λ (line)
           (apply cons* (map parse-assignment
                             (string-split "," line))))
         lines)))

(define (fully-overlaps a b)
  (and (<= (car a) (car b))
       (<= (cdr b) (cdr a))))

(define (part-1)
  (length
   (filter (λ (s) (or (fully-overlaps (car s) (cdr s))
                      (fully-overlaps (cdr s) (car s))))
           input)))

(define (left-overlaps a b)
  (and (<= (car a) (car b))
       (<= (car b) (cdr a))))

(define (part-2)
  (length
   (filter (λ (s) (or (left-overlaps (car s) (cdr s))
                      (left-overlaps (cdr s) (car s))))
           input)))

(printf "~s\n" (part-1))

(printf "~s\n" (part-2))
