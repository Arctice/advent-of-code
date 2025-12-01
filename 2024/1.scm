(import (core))

(define (parse path)
  (let* ([lines (readlines path)]
         [parse-line (λ (line) (map string->number (string-split "   " line)))]
         [pairs (map parse-line lines)])
    (list (map car pairs) (map cadr pairs))))

(define (part-1 A B)
  (let* ([A (sort < A)]
         [B (sort < B)])
    (fold-left + 0 (map abs (map - A B)))))

(define (part-2 A B)
  (let ([B-counts (make-hashtable + =)])
    (for-each (λ (x) (hashtable-update! B-counts x 1+ 0)) B)
    (fold-left + 0 (map (λ (x) (* x (hashtable-ref B-counts x 0))) A))))

(define input (parse "1.input"))
(printf "part 1: ~s\n" (apply part-1 input))
(printf "part 2: ~s\n" (apply part-2 input))
