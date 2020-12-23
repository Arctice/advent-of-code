#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))

;; (define cups '(3 8 9 1 2 5 4 6 7))
(define cups '(2 1 5 6 9 4 7 8 3))

(define xcups (append cups (list-tail (iota 1000001) 10)))
(define last-cup (apply max xcups))

(define cups-table
  (let ([cups-table (make-vector (inc (length xcups)))])
    (let loop ([label (head xcups)] [rest (tail xcups)])
      (let ([next (head rest)])
        (vector-set! cups-table label next)
        (if (null? (cdr rest))
            (vector-set! cups-table next (head xcups))
            (loop next (tail rest)))))
    cups-table))

(define (deref label) (vector-ref cups-table label))

(define (move! alpha)
  (let* ([cut (deref alpha)] [cut* (deref cut)] [cut** (deref cut*)]
         [beta (deref cut**)]
         [destination
          (let next ([i alpha])
            (let ([i (+ 1 (mod (- i 2) last-cup))])
              (if (or (= i cut) (= i cut*) (= i cut**))
                  (next i) i)))])
    (vector-set! cups-table alpha beta)
    (vector-set! cups-table cut** (deref destination))
    (vector-set! cups-table destination cut)
    beta))

;; 163035127721
(time (repeat move! (head cups) 10000000))
(pretty-print (* (deref 1) (deref (deref 1))))
