#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (chezscheme) (core))

(define seats
  (let* ([rows (map (λ row -> (string-append "S" row "S"))
                    (readlines "11.input"))]
         [width (string-length (head rows))]
         [sentinel-row (list (make-string width #\S))]
         [seats (append sentinel-row rows sentinel-row)])
    (list->vector (map (compose list->vector string->list) seats))))

(define directions '((-1 . -1) (0 . -1) (1 . -1)
                     (-1 . 0)           (1 . 0)
                     (-1 . 1)  (0 . 1)  (1 . 1)))

(define (peek seats x y) (vector-ref (vector-ref seats y) x))


(define (part-1-rule seats x y)
  (define (adjacent-occupied)
    (count (λ p -> (eq? #\# (peek seats (+ x (head p)) (+ y (tail p)))))
           directions))
  (case (peek seats x y)
    [#\. #\.]  [#\S #\S]
    [#\# (if (<= 4 (adjacent-occupied)) #\L #\#)]
    [#\L (if (zero? (adjacent-occupied)) #\# #\L)]))


(define (line-of-sight x y)
  (define (cast-los x y dir)
    (let* ([x (+ x (head dir))] [y (+ y (tail dir))]
           [next (peek seats x y)])
      (if (eq? next #\.) (cast-los x y dir)
          (pair x y))))
  (if (eq? #\L (peek seats x y))
      (map (λ dir -> (cast-los x y dir)) directions)
      #f))

(define line-of-sight-cache
  ((λ results -> (list->vector (map list->vector results)))
   (map (λ y -> (map (λ x -> (line-of-sight x y))
                     (iota (vector-length (vector-ref seats 0)))))
        (iota (vector-length seats)))))

(define (part-2-rule seats x y)
  (define (visible)
    (count (λ p -> (eq? #\# (peek seats (head p) (tail p))))
           (vector-ref (vector-ref line-of-sight-cache y) x)))
  (case (peek seats x y)
    [#\. #\.] [#\S #\S]
    [#\# (if (<= 5 (visible)) #\L #\#)]
    [#\L (if (zero? (visible)) #\# #\L)]))


(define (reseat rule seats)
  (let ([next (vector-map vector-copy seats)])
    (do ([y 0 (inc y)])
        [(= y (vector-length seats))]
      (let ([row (vector-ref next y)])
        (do ([x 0 (inc x)])
            [(= x (vector-length row))]
          (vector-set! row x (rule seats x y)))))
    next))

(define (count-occupied seats)
  (let ([sigma 0])
    (vector-for-each
     (λ row -> (vector-for-each
                (λ c -> (when (eq? c #\#) (set! sigma (inc sigma))))
                row))
     seats)
    sigma))

(define (simulate rule seats)
  (let loop ([next (reseat rule seats)] [previous-count 0])
    (let ([count (count-occupied next)])
      (if (= count previous-count) next
          (loop (reseat rule next) count)))))


(let ([p1 (time (simulate part-1-rule seats))]
      [p2 (time (simulate part-2-rule seats))])
  (printf "~s \n" (count-occupied p1))  ;; 2183
  (printf "~s \n" (count-occupied p2)))  ;; 1990
