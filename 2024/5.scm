(import (core))

(define rules (make-hashtable + =))

(define sequences
  (let* ([lines (readlines "5.input")]
         [csv (cdr (member "" lines))]
         [pairs (list-head lines (- (length lines) (length csv) 1))]
         [csv (map (λ (l) (map string->number (string-split "," l))) csv)])
    (for-each
     (λ (pair)
       (let* ([pair (map string->number (string-split "|" pair))]
              [from (car pair)] [to (cadr pair)])
         (hashtable-update! rules from (λ (l) (cons to l)) '())))
     pairs)
    csv))

(let* ([part-1 0] [part-2 0])
  (for-each
   (λ (sequence)
     (let* ([weight
             (λ (key) (length (filter (λ (x) (member x sequence))
                                      (hashtable-ref rules key '()))))]
            [sorted (sort (λ (a b) (> (weight a) (weight b))) sequence)]
            [original-correct (equal? sequence sorted)]
            [middle-value (list-ref sorted (/ (- (length sorted) 1) 2))])
       (if original-correct
           (set! part-1 (+ part-1 middle-value))
           (set! part-2 (+ part-2 middle-value)))))
   sequences)
  (printf "part 1: ~s\n" part-1)
  (printf "part 2: ~s\n" part-2))
