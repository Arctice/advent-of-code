(import (core))

(define grid (readlines "6.input"))

(define width (string-length (car grid)))

(define (grid-ref x y)
  (if (and (< -1 x width) (< -1 y width))
      (string-ref (list-ref grid y) x) #\?))

(define guard-start
  (let ([at
         (find (λ (n) (char=? #\^ (grid-ref (mod n width) (div n width))))
               (iota (* width width)))])
    (cons* (mod at width) (div at width))))
(define initial-facing '(0 . -1))

(define (turn dir)
  (case dir
    [(0 . -1) '(1 . 0)]
    [(1 . 0) '(0 . 1)]
    [(0 . 1) '(-1 . 0)]
    [(-1 . 0) '(0 . -1)]))

(define (step pos dir)
  (let* ([forward (cons* (+ (car pos) (car dir))
                         (+ (cdr pos) (cdr dir)))]
         [look (grid-ref (car forward) (cdr forward))])
    (if (char=? #\# look)
        (values pos (turn dir))
        (values forward dir))))

(define (stop? pos)
  (char=? #\? (grid-ref (car pos) (cdr pos))))

(define visited (make-hashtable equal-hash equal?))
(let walk ([pos guard-start] [dir initial-facing])
  (let-values ([(pos dir) (step pos dir)])
    (unless (stop? pos)
      (hashtable-set! visited pos #t)
      (walk pos dir))))

(printf "part 1: ~s\n" (hashtable-size visited))

(let ([obstacle-candidates
       (filter (λ (p) (not (equal? p guard-start)))
               (vector->list (hashtable-keys visited)))]
      [cyclic '()])
  (define (cyclic? obstacle)
    (string-set! (list-ref grid (cdr obstacle)) (car obstacle) #\#)
    (let ([visited (make-hashtable equal-hash equal?)])
      (let walk ([pos guard-start] [dir initial-facing])
        (let*-values ([(pos dir) (step pos dir)]
                      [(state) (cons* pos dir)])
          (cond [(stop? pos)]
                [(hashtable-contains? visited state)
                 (set! cyclic (cons obstacle cyclic))]
                [else (hashtable-set! visited state #t)
                      (walk pos dir)]))))
    (string-set! (list-ref grid (cdr obstacle)) (car obstacle) #\.))
  (for-each cyclic? obstacle-candidates)
  (printf "part 2: ~s\n" (length cyclic)))
