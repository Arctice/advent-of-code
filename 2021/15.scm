(import (core))

(define-values (grid height width)
  (let* ([lines (readlines "15.input")]
         [height (length lines)]
         [width (string-length (car lines))]
         [parse-line
          (λ (l) (map (λ (c) (- (char->integer c) (char->integer #\0)))
                      (string->list l)))]
         [flat (fold-left append '() (map parse-line lines))])
    (values (list->vector flat) height width)))

(define grid-at
  (case-lambda
   [(v) (grid-at (car v) (cdr v))]
   [(x y) (vector-ref grid (fx+ x (fx* y width)))]))


(define (A* start next step-cost end?)
  (let ([queue `(((,start) . 0))]
        [visited
         (make-hashtable (λ (v) (+ (car v) (* 5 width (cdr v))))
                         equal?)])
    (define (compare-priority a b)
      (< (cdr a) (cdr b)))
    (hashtable-set! visited start 0)
    (let search ([queue queue])
      (if (end? (caaar queue)) (car queue)
          (let* ([best (car queue)]
                 [path (car best)] [cost (cdr best)]
                 [new-paths
                  (map
                   (λ (step)
                     (let ([cost (+ cost (step-cost step))]
                           [previous-best
                            (hashtable-ref visited step (greatest-fixnum))])
                       (if (>= cost previous-best) #f
                           (begin
                             (hashtable-set! visited step cost)
                             (cons* (cons step path) cost)))))
                   (next (car path)))]
                 [new-paths (filter values new-paths)]
                 [new-paths (sort compare-priority new-paths)])
            (search (merge compare-priority new-paths (cdr queue))))))))

(define directions '((0 . -1) (-1 . 0) (1 . 0) (0 . 1)))

;; part 1

(let* ([start '(0 . 0)]
       [end (cons* (- width 1) (- height 1))]
       [found
        (A* '(0 . 0)
            (λ (pos)
              (filter
               (λ (pos) (let ([x (car pos)] [y (cdr pos)])
                          (and (< -1 x width) (< -1 y height))))
               (map (λ (dir) (cons* (+ (car pos) (car dir))
                                    (+ (cdr pos) (cdr dir))))
                    directions)))
            grid-at (λ (pos) (equal? pos end)))]
       [path (reverse (car found))])
  (pretty-print (fold-left + 0 (map grid-at (cdr path)))))


;; part 2

(let* ([start '(0 . 0)]
       [width* (* width 5)]
       [height* (* height 5)]
       [end* (cons* (- width* 1) (- height* 1))]
       [grid-at*
        (λ (pos)
          (let* ([x (car pos)] [y (cdr pos)]
                 [x-tile (div x width)] [y-tile (div y height)]
                 [x* (mod x width)] [y* (mod y height)]
                 [risk (grid-at x* y*)]
                 [total-risk (+ risk x-tile y-tile)])
            (+ (mod (- total-risk 1) 9) 1)))]
       [found
        (A* '(0 . 0)
            (λ (pos)
              (filter
               (λ (pos) (let ([x (car pos)] [y (cdr pos)])
                          (and (< -1 x width*) (< -1 y height*))))
               (map (λ (dir)
                      (cons* (+ (car pos) (car dir))
                             (+ (cdr pos) (cdr dir))))
                    directions)))
            grid-at* (λ (pos) (equal? pos end*)))]
       [path (reverse (car found))])
  (pretty-print (fold-left + 0 (map grid-at* (cdr path)))))

