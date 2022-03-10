(import (core))

(define (parse lines)
  (define (parse-line line)
    (let ([input (substring line 0 58)]
          [output (substring line 61 (string-length line))])
      (append (string-split " " input)
              (string-split " " output))))
  (map parse-line lines))

(define input (parse (readlines "8.input")))

(define (empty-matrix)
  (list->vector (map (λ _ (make-vector 7 #f)) (iota 7))))
(define (identity-matrix)
  (list->vector (map (λ _ (make-vector 7 #t)) (iota 7))))
(define (row-matrix row)
  (let ([m (empty-matrix)])
    (vector-set! m row (make-vector 7 #t)) m))
(define (column-matrix column)
  (let ([m (empty-matrix)])
    (for-each (λ (row) (vector-set! (vector-ref m row) column #t))
              (iota 7)) m))

(define (matrix-invert m)
  (let ([u (empty-matrix)])
    (for-each
     (λ (row) (for-each
               (λ (column)
                 (let ([a (vector-ref (vector-ref m row) column)])
                   (vector-set! (vector-ref u row) column (not a))))
               (iota 7)))
     (iota 7))
    u))
(define (matrix-union m n)
  (let ([u (empty-matrix)])
    (for-each
     (λ (row) (for-each
               (λ (column)
                 (let ([a (vector-ref (vector-ref m row) column)]
                       [b (vector-ref (vector-ref n row) column)])
                   (vector-set! (vector-ref u row) column
                                (or a b))))
               (iota 7)))
     (iota 7))
    u))
(define (matrix-intersection m n)
  (let ([u (empty-matrix)])
    (for-each
     (λ (row) (for-each
               (λ (column)
                 (let ([a (vector-ref (vector-ref m row) column)]
                       [b (vector-ref (vector-ref n row) column)])
                   (vector-set! (vector-ref u row) column
                                (and a b))))
               (iota 7))) (iota 7))
    u))

(define (matrix-column m i)
  (map (λ (r) (vector-ref (vector-ref m r) i))
       (iota 7)))

(define (print-matrix m)
  (printf " ")
  (for-each (λ (n) (printf " ~s" n)) (iota 7))
  (printf "\n")
  (for-each
   (λ (row) (printf "~s" (list-ref '(a b c d e f g) row))
      (for-each
       (λ (column)
         (printf " ~c"
                 (if (vector-ref (vector-ref m row) column)
                     #\* #\.)))
       (iota 7))
      (printf "\n"))
   (iota 7)))

(define (wire-matrix wires)
  (fold-left matrix-union (empty-matrix)
             (map column-matrix wires)))

(define (word-matrix word)
  (let ([characters
         (map (λ (c) (- (char->integer c)
                        (char->integer #\a)))
              (string->list word))])
    (fold-left matrix-union (empty-matrix)
               (map row-matrix characters))))

(define display 
  '((0 1 2   4 5 6) ;; 0
    (    2     5  ) ;; 1
    (0   2 3 4   6) ;; 2
    (0   2 3   5 6) ;; 3
    (  1 2 3   5  ) ;; 4
    (0 1   3   5 6) ;; 5
    (0 1   3 4 5 6) ;; 6
    (0   2     5  ) ;; 7
    (0 1 2 3 4 5 6) ;; 8
    (0 1 2 3   5 6))) ;; 9

(define (find-constraints facts)
  (let ([singles (make-eqv-hashtable)])
    (for-each
     (λ (n) (let* ([row (vector->list (vector-ref facts n))]
                   [column (matrix-column facts n)])
              (when (= 1 (length (filter values row)))
                (let ([wire (- 7 (length (memq #t row)))])
                  (hashtable-set! singles n wire)))
              (when (= 1 (length (filter values column)))
                (let ([character (- 7 (length (memq #t column)))])
                  (hashtable-set! singles character n)))))
     (iota 7))
    (map (λ (c) (cons* (integer->char (+ c (char->integer #\a)))
                       (hashtable-ref singles c #f)))
         (vector->list (hashtable-keys singles)))))

(define (propagate-constraints facts)
  (fold-left
   (λ (facts constraint)
     (let* ([character (string (car constraint))]
            [wire (list (cdr constraint))]
            [character (word-matrix character)]
            [wire (wire-matrix wire)]
            [clear (matrix-invert (matrix-union character wire))]
            [update (matrix-union (matrix-intersection character wire)
                                  clear)])
       (matrix-intersection facts update)))
   facts (find-constraints facts)))

(define (solve words)
  (define (combine word)
    (let* ([a (word-matrix word)]
           [possible-wirings
            (filter (λ (wires) (= (string-length word) (length wires)))
                    display)]
           [options
            (map (λ (wires)
                   (let* ([b (wire-matrix wires)]
                          [unaffected (matrix-invert (matrix-union a b))]
                          [possible (matrix-intersection a b)]
                          [contribution (matrix-union unaffected possible)])
                     contribution))
                 possible-wirings)]
           [update (fold-left matrix-union (empty-matrix) options)])
      update))
  (find-constraints
   (propagate-constraints
    (fold-left matrix-intersection
               (identity-matrix) (map combine words)))))

(define (translate character-map word)
  (- (length display)
     (length (member (sort < (map (λ (c) (cdr (assoc c character-map)))
                                  (string->list word)))
                     display))))

(let* ([outputs
        (map (λ (line)
               (let* ([character-map (solve line)]
                      [output (list-tail line (- (length line) 4))]
                      [translation
                       (map (λ (word) (translate character-map word))
                            output)])
                 translation))
             input)]
       [unique-counts
        (length (filter (λ (x) (memq x '(1 4 7 8)))
                        (fold-left append '() outputs)))]
       [output-values
        (map string->number
             (map (λ (xs) (fold-left string-append ""
                                     (map number->string xs)))
                  outputs))])
  (printf "~s\n" unique-counts)
  (printf "~s\n" (fold-left + 0 output-values)))
