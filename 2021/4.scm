(import (core))

(define (parse-cards lines)
  (let* ([lines
          (map (λ (line)
                 (filter (λ (x) x)
                         (map string->number
                              (string-split " " line))))
               lines)]
         [groups
          (let next ([lines lines])
            (if (null? lines) '()
                (cons (list-head lines 5)
                      (next (list-tail lines 5)))))])
    groups))

(define (score card numbers)
  (let ([marks (vector-map (λ _ (make-vector 5 0))
                           (make-vector 5))])
    (define (called lucky-number)
      (for-each
       (λ (row) (for-each
                 (λ (column)
                   (if (= lucky-number
                          (list-ref (list-ref card row) column))
                       (vector-set! (vector-ref marks row) column 1)))
                 (iota 5)))
       (iota 5)))
    (define (won?)
      (or (exists
           (λ (row)
             (= 5 (fold-left
                   + 0 (vector->list (vector-ref marks row)))))
           (iota 5))
          (exists
           (λ (column)
             (= 5 (fold-left
                   + 0 (map (λ (row) (vector-ref (vector-ref marks row)
                                                 column))
                            (iota 5)))))
           (iota 5))))
    (define (winning-score winning-number)
      (* winning-number
       (fold-left
        (λ (sum row)
          (+ sum (fold-left
                  (λ (sum column)
                    (+ sum
                       (let ([mark (vector-ref (vector-ref marks row) column)]
                             [card (list-ref (list-ref card row) column)])
                         (* (fxxor 1 mark) card))))
                  0 (iota 5))))
        0 (iota 5))))
    (let next ([numbers numbers]
               [turn 0])
      (let ([number (car numbers)])
        (called number)
        (cond [(won?) (values turn (winning-score number))]
              [(null? (cdr numbers)) 0]
              [else (next (cdr numbers)
                          (+ 1 turn))])))))

(let* ([input
        (filter (λ (line) (not (string=? "" line)))
                (readlines "4.input"))]
       [called-numbers (map string->number
                            (string-split "," (car input)))]
       [cards (parse-cards (cdr input))]
       [scores
        (map (λ (card)
               (let-values ([(turn score)
                             (score card called-numbers)])
                 (cons* turn score)))
             cards)]
       [scores (sort (λ (a b) (< (car a) (car b)))
                     scores)])
  (pretty-print (map cons* cards scores))
  )
