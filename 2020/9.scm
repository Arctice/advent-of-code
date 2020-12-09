#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (chezscheme) (core))

(define xmas (list->vector (map string->number (readlines "9.input"))))

(define (invalid? i)
  (let ([n (vector-ref xmas i)])
    (let xloop ([x (- i 25)])
      (let ([yv (- n (vector-ref xmas x))])
        (cond
         [(= x i) n]
         [(< yv 1) (xloop (inc x))]
         [else
          (let yloop ([y (inc x)])
            (cond
             [(= y i) (xloop (inc x))]
             [(= yv (vector-ref xmas y)) #f]
             [else (yloop (inc y))]))])))))

(define invalid (exists invalid? (list-tail (iota (vector-length xmas)) 25)))

(define secret
  (let loop ([a 0] [b 0] [check 0])
    (cond [(> check invalid) (loop (inc a) b (- check (vector-ref xmas a)))]
          [(< check invalid) (loop a (inc b) (+ check (vector-ref xmas b)))]
          [else (let ([seq (list-slice (vector->list xmas) a b)])
                  (+ (apply min seq) (apply max seq)))])))

(pretty-print invalid)
(pretty-print secret)

