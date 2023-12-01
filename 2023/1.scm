(import (core))

(define (part-1 lines)
  (fold-left
   + 0
   (map (λ (line)
          (let ([digits (filter char-numeric? (string->list line))])
            (string->number
             (string (car digits) (car (last-pair digits))))))
        lines)))

(define words
  '(("one" . 1) ("two" . 2) ("three" . 3) ("four" . 4) ("five" . 5)
    ("six" . 6) ("seven" . 7) ("eight" . 8) ("nine" . 9)))

(define (match-digits ss)
  (let ([results '()])
    (let scan ([i 0] [matching '()])
      (cond
       [(= i (string-length ss)) (reverse results)]
       [(char-numeric? (string-ref ss i))
        (set! results (cons (string->number (string (string-ref ss i)))
                            results))
        (scan (+ 1 i) '())]
       [(let* ([valid?
                (λ (match) (char=? (string-ref ss i)
                                   (string-ref (cadr match) (car match))))]
               [completed?
                (λ (match) (= (+ 1 (car match)) (string-length (cadr match))))]
               [new-matches (map (λ (word) (cons 0 word)) words)]
               [matching
                (map (λ (match)
                       (cond [(not (valid? match)) #f]
                             [(completed? match)
                              (set! results (cons (cddr match) results)) #f]
                             [else (cons (+ 1 (car match)) (cdr match))]))
                     (append matching new-matches))])
          (scan (+ 1 i) (filter values matching)))]))))

(define (part-2 lines)
  (fold-left
   + 0
   (map (λ (line)
          (let ([digits (match-digits line)])
            (+ (* 10 (car digits)) (car (last-pair digits)))))
        lines)))


(define input (readlines "1.input"))
(printf "~s\n" (part-1 input))
(time (printf "~s\n" (part-2 input)))
