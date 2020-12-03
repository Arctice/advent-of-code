#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))

(define lines
  (let ([in (open-input-file "3.input")])
    (let loop ([ss (get-line in)])
      (if (eof-object? ss) '()
          (cons ss (loop (get-line in)))))))

(define grid (map string->list lines))
(define grid-height (length grid))

(define (tree? x y)
  (define grid-width (length (head grid)))
  (eq? #\#(list-ref (list-ref grid y) (mod x grid-width))))

;; part 1
(pretty-print
 (length (filter (λ y -> (tree? (* y 3) y))
                 (iota grid-height))))


;; part 2
(define (slope x y)
  (length (filter (λ i -> (tree? (* i x) (* i y)))
                  (iota (ceiling (/ grid-height y))))))

(pretty-print
 (foldl1 * (map (partial apply slope)
                '((1 1) (3 1) (5 1) (7 1) (1 2)))))
