#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (chezscheme) (core))

;; (define adapters '(16 10 15 5 1 11 7 19 6 12 4))
;; (define adapters '(28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3))
(define adapters (map string->number (readlines "10.input")))

(set! adapters (sort < (append adapters (list 0 (+ 3 (apply max adapters))))))


(define (part-1 xs)
  (let* ([xs (cons 0 (append xs (list (+ 3 (apply max xs)))))]
         [pairs (zip (tail xs) (list-head xs (dec (length xs))))]
         [differences (map (partial apply -) pairs)])
    (* (count (partial = 3) differences)
       (count (partial = 1) differences))))

(pretty-print (part-1 adapters))


(define (connects? a b) (<= (abs (- a b)) 3))

(define (arrangements adapters)
  (define cache (make-eq-hashtable))
  (define (descend x xs)
    (cond
     [(null? xs) 1]
     [(hashtable-contains? cache x)
      (hashtable-ref cache x '())]
     [else ((λ result -> (begin (hashtable-set! cache x result) result))
            (let choices ([rest xs])
              (cond [(null? rest) 0]
                    [(connects? (head rest) x)
                     (+ (descend (head rest) (tail rest))
                        (choices (tail rest)))]
                    [else 0])))]))
  (descend (head adapters) (tail adapters)))

;; no memoization, faster for small cases
(define (arrangements* adapters)
  (define (ascend x count xs)
    (if (null? xs) count
        (let ([y (head xs)]
              [ys (tail xs)])
          (let update ([y y] [ys ys])
            (when (connects? x (head y))
              (set-cdr! y (+ count (tail y)))
              (when (not (null? ys))
                (update (head ys) (tail ys)))))
          (ascend (head y) (tail y) ys))))
  (ascend (head adapters) 1
          (map (λ y -> (pair y 0)) (tail adapters))))

(pretty-print (arrangements adapters))
(pretty-print (arrangements* adapters))
