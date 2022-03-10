(import (core))

(define input
  (map string->number
       (string-split "," (car (readlines "7.input")))))

;; part 1
(let* ([crabs (sort < input)]
       [partition (fxdiv (length crabs) 2)]
       [towards (list-ref crabs partition)]
       [diffs (map (λ (p) (abs (- towards p))) crabs)]
       [cost (fold-left + 0 diffs)])
  (pretty-print cost))


(define (cost n) (/ (* n (+ n 1)) 2))

(let* ([crabs input]
       [estimate (exact (round (/ (fold-left + 0 crabs)
                                  (length crabs))))])
  (for-each
   (λ (i) (let* ([optimum (+ estimate -1 i)]
                 [distances (map (λ (c) (abs (- optimum c))) crabs)]
                 [total-cost (fold-left + 0 (map cost distances))])
            (printf "~s ~s\n" optimum total-cost)))
   (iota 3)))

