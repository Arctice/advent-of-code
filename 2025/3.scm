(import (core))

(define (solve bank count)
  (let* ([left (- count 1)]
         [head (list-head bank (- (length bank) left))]
         [first (apply max head)] [rest (cdr (memq first bank))])
    (+ (* first (expt 10 left))
       (if (zero? left) 0 (solve rest left)))))

(let* ([input (readlines "3.input")]
       [banks (map (λ (bank) (map (λ (c) (string->number (string c)))
                                  (string->list bank)))
                   input)])
  (printf "part 1: ~s\n" (fold-left + 0 (map (λ (bank) (solve bank 2)) banks)))
  (printf "part 1: ~s\n" (fold-left + 0 (map (λ (bank) (solve bank 12)) banks))))

