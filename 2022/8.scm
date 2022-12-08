#!chezscheme
(import (scheme) (core))

(define input (map (λ (line) (map (λ (c) (string->number (string c)))
                                  (string->list line)))
                   (readlines "8.input")))

(define (visible-trees view)
  (let next ([view view] [i 0] [peak -1])
    (if (null? view) '()
        (let* ([tree (car view)]
               [rest (next (cdr view) (1+ i) (max peak tree))])
          (if (< peak tree) (cons i rest) rest)))))

(define (transpose grid) (apply map list grid))
(let* ([rows input] [columns (transpose input)]
       [seen (make-hashtable equal-hash equal?)])
  (define (seen! x y) (hashtable-set! seen (cons* x y) #t))
  (for-each (λ (row y)
              (for-each (λ (x) (seen! x y))
                        (visible-trees row))
              (for-each (λ (x) (seen! (- (length row) 1 x) y))
                        (visible-trees (reverse row))))
            rows (enumerate rows))
  (for-each (λ (column x)
              (for-each (λ (y) (seen! x y))
                        (visible-trees column))
              (for-each (λ (y) (seen! x (- (length column) 1 y)))
                        (visible-trees (reverse column))))
            columns (enumerate columns))
  (printf "part 1: ~s\n" (hashtable-size seen)))

(define (view-distances view)
  (let scan ([trees view] [indices (enumerate view)] [peaks '()])
    (if (null? trees) '()
        (let* ([tree (car trees)] [index (car indices)]
               [view-end (memp (λ (peak) (<= tree (cdr peak))) peaks)]
               [peaks (cons (cons* index tree) (or view-end '()))]
               [view-distance (- index (if view-end (caar view-end) 0))])
          (cons view-distance (scan (cdr trees) (cdr indices) peaks))))))

(let* ([rows input] [columns (transpose input)]
       [scenicity (make-hashtable equal-hash equal?)])
  (define (view! x y distance)
    (hashtable-update! scenicity (cons* x y) (λ (v) (* v distance)) 1))
  (for-each
   (λ (row y)
     (for-each (λ (x distance) (view! x y distance))
               (enumerate row) (view-distances row))
     (for-each (λ (x distance) (view! x y distance))
               (reverse (enumerate row)) (view-distances (reverse row))))
   rows (enumerate rows))
  (for-each
   (λ (column x)
     (for-each (λ (y distance) (view! x y distance))
               (enumerate column) (view-distances column))
     (for-each (λ (y distance) (view! x y distance))
               (reverse (enumerate column)) (view-distances (reverse column))))
   columns (enumerate columns))
  (printf "part 2: ~s\n" (apply max (vector->list (hashtable-values scenicity)))))
