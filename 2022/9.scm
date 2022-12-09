#!chezscheme
(import (scheme) (core))

(define-values (vec2 vec2-x vec2-y) (values cons* car cdr))
(define (vec2+ a b) (vec2 (+ (vec2-x a) (vec2-x b)) (+ (vec2-y a) (vec2-y b))))
(define (vec2- a b) (vec2 (- (vec2-x a) (vec2-x b)) (- (vec2-y a) (vec2-y b))))

(define input
  (map (λ (line) (let ([vs (string-split " " line)])
                   (cons* (string->symbol (car vs))
                          (string->number (cadr vs)))))
       (readlines "9.input")))

(define (direction d)
  (case d [U (vec2 0 -1)] [D (vec2 0 1)] [L (vec2 -1 0)] [R (vec2 1 0)]))

(define (sign v) (cond [(negative? v) -1] [(zero? v) 0] [else 1]))
(define (rope-pull head tail)
  (let ([d (vec2- head tail)])
    (if (or (< 1 (abs (vec2-x d))) (< 1 (abs (vec2-y d))))
        (vec2 (sign (vec2-x d)) (sign (vec2-y d)))
        (vec2 0 0))))

(define (solve tail-length)
  (let ([rope (make-list tail-length (vec2 0 0))]
        [seen (make-hashtable equal-hash equal?)])
    (define (pull-rope move)
      (let next ([rope rope] [move move])
        (set-car! rope (vec2+ (car rope) move))
        (if (null? (cdr rope)) (car rope)
            (next (cdr rope)
                  (rope-pull (car rope) (cadr rope))))))

    (hashtable-set! seen (vec2 0 0) #t)
    (for-each
     (λ (d steps) (for-each
                   (λ (step) (let ([tail (pull-rope (direction d))])
                               (hashtable-set! seen tail #t)))
                   (iota steps)))
     (map car input) (map cdr input))
    (vector-length (hashtable-keys seen))))

(printf "part 1: ~s\n" (solve 2))
(printf "part 2: ~s\n" (solve 10))
