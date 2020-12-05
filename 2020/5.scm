#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))

(define lines
  (let ([in (open-input-file "5.input")])
    (let loop ([ss (get-line in)])
      (if (eof-object? ss) '()
          (cons ss (loop (get-line in)))))))


(define (find-row instructions)
  (let next ([i 0] [lo 0] [hi 128])
    (if (= i 7) lo
        (let ([m (floor (/ (+ lo hi) 2))]
              [choice (string-ref instructions i)])
          (if (eq? choice #\F)
              (next (inc i) lo m)
              (next (inc i) m hi))))))

(define (get-column instructions)
  (case instructions
    ["LLL" 0] ["LLR" 1] ["LRL" 2] ["LRR" 3]
    ["RLL" 4] ["RLR" 5] ["RRL" 6] ["RRR" 7]))


;; part 1
(define (seat-id pass)
  (+ (* 8 (find-row (substring pass 0 7)))
     (get-column (substring pass 7 10))))

(printf "~s\n" (apply max (map seat-id lines)))

;; part 2
(define seats (sort < (map seat-id lines)))

(pretty-print
 (let search ([seat (cadr seats)]
              [rest (cddr seats)])
   (let ([next-id (head rest)])
     (if (< 1 (- next-id seat))
         (inc seat)
         (search (head rest) (tail rest))))))
