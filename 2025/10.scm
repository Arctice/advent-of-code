(optimize-level 3)
(import (core))

(define (parse line)
  (let* ([line (string-split " " line)]
         [buttons (list-head (cdr line) (- (length line) 2))]
         [buttons
          (map (λ (s)
                 (let* ([s (substring s 1 (- (string-length s) 1))]
                        [xs (string-split "," s)])
                   (map string->number xs)))
               buttons)]
         [joltage (car (last-pair line))]
         [joltage (substring joltage 1 (- (string-length joltage) 1))]
         [joltage (map string->number (string-split "," joltage))])
    (list buttons joltage)))

(define (reduce-machine buttons joltage)
  (let* ([m (length joltage)]
         [n (+ 1 (length buttons))]
         [M (list->vector (map (λ (row) (make-vector n 0)) (iota m)))])
    (define (ref i j) (vector-ref (vector-ref M i) j))
    (define (mset! i j x) (vector-set! (vector-ref M i) j x))
    (define (swap-rows! i j)
      (let ([a (vector-ref M i)] [b (vector-ref M j)])
        (vector-set! M j a) (vector-set! M i b)))
    (for-each (λ (i x) (mset! i (- n 1) x)) (iota m) joltage)
    (for-each
     (λ (column button)
       (for-each (λ (row) (mset! row column 1))
                 button))
     (enumerate buttons) buttons)
    (let loop ([h 0] [k 0])
      (when (and (< h m) (< k n))
        (let ([i-max
               (car (sort
                     (λ (a b) (> (abs (ref a k)) (abs (ref b k))))
                     (list-tail (iota m) h)))])
          (if (zero? (ref i-max k))
              (loop h (+ 1 k))
              (begin
                (swap-rows! h i-max)
                (for-each
                 (λ (i)
                   (let ([f (/ (ref i k) (ref h k))])
                     (mset! i k 0)
                     (for-each
                      (λ (j) (mset! i j (- (ref i j) (* (ref h j) f))))
                      (list-tail (iota n) (+ k 1)))))
                 (list-tail (iota m) (+ 1 h)))
                (loop (+ 1 h) (+ 1 k)))))))
    M))

;; returns indices
(define (free-variables buttons M)
  (let* ([width (vector-length (vector-ref M 0))]
         [fixed
          (map (λ (row)
                 (let ([vars (memp (λ (x) (not (zero? x)))
                                   (vector->list row))])
                   (and vars (- width (length vars)))))
               (vector->list M))])
    (filter (λ (i) (not (memq i fixed)))
                  (enumerate buttons))))

(define (insert-free-var-rows matrix variables)
  (let* ([width (vector-length (vector-ref matrix 0))]
         [free-var-rows
          (map (λ (i x) (let ([v (make-vector width 0)])
                          (vector-set! v x 1)
                          (vector-set! v (- width 1) 0)
                          v))
               (enumerate variables) variables)])
    (list->vector
     (list-head
      (let next ([i 0] [matrix (vector->list matrix)] [free free-var-rows])
        (cond
         [(null? free) matrix]
         [(null? matrix) free]
         [(zero? (vector-ref (car matrix) i))
          (cons (car free)
                (next (+ i 1) matrix (cdr free)))]
         [(cons (car matrix)
                (next (+ 1 i) (cdr matrix) free))]))
      (- width 1)))))


(define (solve matrix variables bindings)
  (let* ([width (- (vector-length (vector-ref matrix 0)) 1)]
         [variable (make-vector width)])
    (for-each
     (λ (i x) (vector-set! (vector-ref matrix i) width x))
     (list-head variables (min (length variables) (length bindings)))
     (list-head bindings (min (length variables) (length bindings))))
    (for-each
     (λ (i)
       (let* ([n (numerator (vector-ref (vector-ref matrix i) i))]
              [d (denominator (vector-ref (vector-ref matrix i) i))]
              [backsubs
               (let rest ([j (+ i 1)])
                 (if (not (< j width)) 0
                     (+ (rest (+ j 1))
                        (* (vector-ref (vector-ref matrix i) j)
                           (vector-ref variable j)))))]
              [value (- (vector-ref (vector-ref matrix i) width) backsubs)])
         (vector-set! variable i (/ (* d value) n))))
     (reverse (iota width)))
    (and (for-all (λ (x) (and (integer? x) (nonnegative? x)))
                  (vector->list variable))
         (fold-left + 0 (vector->list variable)))))


(define (best-solution matrix free)
  (let* ([zero-solve (solve matrix free '()) ]
         [best '()] [best-value zero-solve])
    (if (null? free) best-value
        (let ([press-limit 150])
          (let heuristic-search ([k 0])
            (let* ([bindings
                    (map (λ (e) (mod (fxdiv k (expt press-limit e))
                                     press-limit))
                         (enumerate free))]
                   [solution (solve matrix free bindings)])
              (when (and solution
                         (or (not best-value) (< solution best-value)))
                (set! best bindings) (set! best-value solution)))
            (if (< k (expt press-limit (length free)))
                (heuristic-search (+ 1 k))
                best-value))))))

(define lines (map parse (readlines "10.input")))
(define total-presses 0)

(time
 (for-each
  (λ (machine i)
    (let* ([buttons (car machine)]
           [joltage (cadr machine)]
           [matrix (reduce-machine buttons joltage)]
           [free (free-variables buttons matrix)]
           [matrix (insert-free-var-rows matrix free)])
      (if (> 3 (length free))
          (set! total-presses (+ total-presses (best-solution matrix free)))
          (begin
            (printf "machine ~s\n" i)
            (printf "free vars: ~s\n" free)
            (set! total-presses (+ total-presses (best-solution matrix free)))))
      (printf "total: ~s \n" total-presses)))
  lines (enumerate lines)))

;; explanation based on first AoC example input;

(define line "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")
(define machine (parse line))
(define buttons (car machine))
(define joltage (cadr machine))
;; lights ignored for part 2
(pretty-print buttons)
;; (3) (1 3) (2) (2 3) (0 2) (0 1)
;;  a    b    c    d     e     f
(pretty-print joltage)
;; (3 5 4 7)

;; the relations between the machine joltages:
;;   {3, 5, 4, 7}
;; a           1
;; b     1     1
;; c        1
;; d        1  1
;; e  1     1
;; f  1  1

;; we can turn this into a system of equations:
;; (each letter represents the number of times that button is pressed)
;; 3 = e + f
;; 5 = b + f
;; 4 = c + d + e
;; 7 = a + b + e

;; which we can further represent as a matrix:
;; 0 0 0 0 1 1 | 3
;; 0 1 0 0 0 1 | 5
;; 0 0 1 1 0 1 | 4
;; 1 1 0 0 1 0 | 7
;; this representation makes it easy to manipulate the equations
;; (you can add rows to one another to simplify them)

(define matrix (reduce-machine buttons joltage))
;; reduce-machine turns the parsed machine buttons and joltage
;; into a matrix as above, and uses gaussian reduction to simplify
;; it into a row echelon form, where the first non-zero value
;; in each row is to the right of the previous row's first value
(vector-for-each pretty-print matrix)
;; (1 1 0 1 0 0 7)
;; (0 1 0 0 0 1 5)
;; (0 0 1 1 1 0 4)
;; (0 0 0 0 1 1 3)
;;  a b c d e f
;; 
;; in this form, the value of each variable (button) can be obtained
;; by subtracting the rows below it as many times as necessary until
;; only the leftmost 1 and rightmost result value remain
;;
;; in some cases, there are less matrix rows than the number of buttons,
;; which means there are multiple solutions; in this case, there is
;; no row where the first non-zero value corresponds to buttons d and f.
;; this means the program still needs to search for all possible values of
;; those free variables to find the minimum solution
;;
;; (the benefit of this representation is that by having simplified the equations,
;;  the state search space is greatly reduced compared to something like a graph
;;  search across individual button presses)
;; 

(define free (free-variables buttons matrix))
(pretty-print free)
;; (3 5) - the indices of the free variables
(define matrix (insert-free-var-rows matrix free))
(vector-for-each pretty-print matrix)
;; inserting rows for the free variables simplifies solver's backsubstitution code
;; #(1 1 0 1 0 0 7)
;; #(0 1 0 0 0 1 5)
;; #(0 0 1 1 1 0 4)
;; #(0 0 0 1 0 0 ?)
;; #(0 0 0 0 1 1 3)
;; #(0 0 0 0 0 1 ?)
;; the ?s are initially 0 and the solver directly mutates
;; those entries to calculate each candidate's results

;; from here on, all the program needs to do is try out all possible
;; combinations of the free variables, and find the one that is valid
;; and requires the least button presses.
;; I initially planned on wrapping this in some kind of meta search
;; strategy that narrows down valid solution ranges,
;; (many randomly selected combinations result in negative button presses)
;; but in practice it turned out that for the examples in the AoC data
;; there are only ever 3 free variables at most, and bounding the search
;; to a maximum of 200 button presses seemed to work fine for every example.
(best-solution matrix free) ;; 10
;; >99% of the runtime is spent checking the few cases in the input code
;; that had 3 free variables, due to the search space expanding to 200^3 states.
;; this took about 5 minutes, though after limiting to 150 the program
;; only takes ~100s and still returns the correct result

