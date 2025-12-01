(import (core))

(define (parse path)
  (define (parse-line line)
    (map string->number (string-split " " line)))
  (map parse-line (readlines path)))

(define (part-1-safe report)
  (let ([diffs (map - (list-tail report 1)
                    (list-head report (- (length report) 1)))])
    (or (for-all (λ (d) (<= 1 d 3)) diffs)
        (for-all (λ (d) (<= -3 d -1)) diffs))))

(define (part-2-safe report)
  (or (part-1-safe report)
      (exists
       (λ (cut)
         (part-1-safe
          (append (list-head report cut)
                  (list-tail report (+ 1 cut)))))
       (enumerate report))))

(define input (parse "2.input"))

(printf "~s\n" (length (filter part-1-safe input)))
(printf "~s\n" (length (filter part-2-safe input)))
