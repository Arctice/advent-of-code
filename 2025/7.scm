(import (core))

(define input (readlines "7.input"))

(define (parse lines)
  (let* ([width (string-length (car lines))]
         [S (- width (length (memq #\S (string->list (car lines)))))])
    (values
     S
     (filter
      (λ (l) (not (null? l)))
      (map (λ (line)
             (filter values
                     (map (λ (index char) (and (char=? char #\^) index))
                          (enumerate line) line)))
           (map string->list (cdr lines)))))))

(define-values (start splitter-positions) (parse input))

(define (solve)
  (let ([beams (make-eq-hashtable)]
        [count 0])
    (hashtable-set! beams start 1)
    (let scan ([splitters splitter-positions])
      (if (null? splitters)
          (cons* count (fold-left + 0 (vector->list (hashtable-values beams))))
          (begin
            (for-each
             (λ (at)
               (let ([beam (hashtable-ref beams at 0)])
                 (when (< 0 beam) (set! count (+ 1 count)))
                 (hashtable-update! beams (- at 1) (λ (x) (+ x beam)) 0)
                 (hashtable-update! beams (+ at 1) (λ (x) (+ x beam)) 0)
                 (hashtable-delete! beams at)))
             (car splitters))
            (scan (cdr splitters)))))))

(printf "~s\n" (solve))


