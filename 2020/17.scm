#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))

;; (define input '("#####..."
;;                 ".#..##.."
;;                 "##.##.##"
;;                 "...####."
;;                 "#.#...##"
;;                 ".##...#."
;;                 ".#.#.###"
;;                 "#.#.#..#"))

(define input '(".#."
                "..#"
                "###"))

(define (make-grid initial)
  (let ([width (string-length (head initial))]
        [grid (make-hashtable equal-hash equal?)])
    (map (λ y ->
            (map (λ x ->
                    (hashtable-set!
                     grid (list x y 0)
                     (eq? #\# (string-ref (list-ref initial y) x))))
                 (iota width)))
         (enumerate initial))
    grid))


(define grid (make-grid input))

(define (nearby-active-count grid k)
  (let ([s 0])
    (for-each
     (λ y ->
        (for-each
         (λ x ->
            (for-each
             (λ z -> (when (hashtable-ref
                            grid (map + k (list x y z)) #f)
                       (set! s (inc s))))
             '(-1 0 1)))
         '(-1 0 1)))
     '(-1 0 1))
    s))

(define (tick grid)
  (let ([next (hashtable-copy grid)])
    (vector-for-each
     (λ k ->
        (case (hashtable-ref grid k #f)) (list k (nearby-active-count grid k)))
     (hashtable-keys grid))
    next))

(set! grid (tick grid))

(pretty-print (filter (λ k -> (hashtable-ref grid k #f))
                      (vector->list (hashtable-keys grid))))
