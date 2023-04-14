#!chezscheme
(import (scheme) (pe))

(define (time-seconds time) (+ (time-second time)
                               (/ (time-nanosecond time) 1e9)))

(for-each
 (λ (i)
   (let ([path (format "~s.scm" i)]
         [problem (string->symbol (format "problem-~s" i))]
         [answer (string->symbol (format "answer-~s" i))])
     (when (file-exists? path) (load path))
     (when (top-level-bound? problem)
       (let* ([cost (make-cost-center)]
              [solve (top-level-value problem)]
              [solution (with-cost-center #t cost solve)])
         (printf "~s: " i)
         (printf "(~,1fs) " (time-seconds (cost-center-time cost)))
         (verify solution (top-level-value answer))))))
 (cdr (iota 500)))
