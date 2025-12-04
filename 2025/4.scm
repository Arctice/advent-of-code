(import (core) (vec2))

(define directions
  (map (λ (p) (apply vec2 p)) '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))))
(define (adjacent at)
  (map (λ (side) (vec2+ at side)) directions))

(define rolls (make-hashtable equal-hash equal?))
(let ([lines (map string->list (readlines "4.input"))])
  (for-each (λ (row y)
              (for-each (λ (tile x)
                          (hashtable-set! rolls (vec2 x y) (eq? tile #\@)))
                        row (enumerate row)))
            lines (enumerate lines)))

(define (roll? at) (hashtable-ref rolls at #f))
(define (remaining) (filter roll? (vector->list (hashtable-keys rolls))))
(define (removable? p) (> 4 (length (filter roll? (adjacent p)))))
(define (remove p) (hashtable-set! rolls p #f))

(printf "part 1: ~s\n" (length (filter removable? (remaining))))

(let next ([queue (remaining)] [removed 0])
  (let ([removable (filter removable? queue)])
    (for-each remove removable)
    (if (null? removable)
        (printf "part 2: ~s\n" removed)
        (next (remaining) (+ removed (length removable))))))
