(import (core))

(define input (readlines "4.input"))


(define (rows lines) (map string->list lines))
(define (columns lines)
  (map (λ (x) (map (λ (line) (string-ref line x)) lines))
       (iota (string-length (car lines)))))

(define (grid-ref lines x y)
  (let ([max (string-length (car lines))])
    (if (and (< -1 x max) (< -1 y max))
        (string-ref (list-ref lines y) x)
        #\.)))

(define (major-diagonals lines)
  (let* ([width (length lines)]
         [xs (map (λ (x) (+ 1 (- x width)))
                  (iota (- (* 2 width) 1)))])
    (map (λ (x)
           (map (λ (y) (let ([x (+ x y)])
                         (grid-ref lines x y)))
                (iota width)))
         xs)))

(define (minor-diagonals lines)
  (let* ([width (length lines)])
    (map (λ (n)
           (map (λ (x) (let ([y (- n x)])
                         (grid-ref lines x y)))
                (iota width)))
         (iota (- (* width 2) 1)))))

(define all-views
  (let ([primary
         (append (rows input)
                 (columns input)
                 (major-diagonals input)
                 (minor-diagonals input))])
    (map list->string
         (append primary (map reverse primary)))))

(define (part-1 input)
  (let* ([xmas-locations
          (λ (ss)
            (filter
             (λ (i) (string=? "XMAS" (substring ss i (+ i 4))))
             (iota (- (string-length ss) 3))))]
         [all-xmas-locations
          (fold-left append '() (map xmas-locations all-views))])
    (length all-xmas-locations)))


(define (part-2 input)
  (let ([width (length input)])
    (define (x-mas? x y)
      (let ([c (grid-ref input x y)])
        (if (not (char=? c #\A)) #f
            (let ([ms? (λ (ac) (or (string=? ac "MS") (string=? ac "SM")))])
              (and (ms? (string (grid-ref input (- x 1) (- y 1))
                                (grid-ref input (+ x 1) (+ y 1))))
                   (ms? (string (grid-ref input (- x 1) (+ y 1))
                                (grid-ref input (+ x 1) (- y 1)))))))))
    (fold-left
     + 0
     (map (λ (y) (length (filter (λ (x) (x-mas? x y)) (iota width))))
          (iota width)))))

(printf "part 1: ~s\n" (part-1 input))
(printf "part 2: ~s\n" (part-2 input))
