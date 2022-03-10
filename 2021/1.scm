(define-syntax λ (identifier-syntax lambda))

(define (readlines path)
  (call-with-input-file path
    (λ (f) (let line () (if (port-eof? f) '()
                            (cons (get-line f) (line)))))))

(define (drop l n)
  (list-head l (- (length l) n)))

(define input (map string->number (readlines "1.input")))

;; part 1
(pretty-print
 (fold-left + 0 (map (λ (a b) (if (< a b) 1 0))
                     (drop input 1) (cdr input))))

;; part 2
(let* ([windows (map + (drop input 2) (cdr (drop input 1)) (cddr input))]
       [diffs (map < (drop windows 1) (cdr windows))]
       [count (length (filter (λ (x) x) diffs))])
  (pretty-print count))
