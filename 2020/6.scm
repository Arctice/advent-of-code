#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))

(define groups
  (map (λ group -> (string-split (string-strip group) "\n"))
       (string-split (slurp "6.input") "\n\n")))

(define (collect-answers group)
  (let ([counts (make-eq-hashtable)])
    (for-each (λ line ->
                 (for-each (λ c -> (hashtable-update! counts c inc 0))
                           (string->list line)))
              group)
    (hashtable-values counts)))

;; part 1
(define (answers-union group)
  (vector-length (collect-answers group)))

(printf "~s\n" (sum (map answers-union groups)))

;; part 2
(define (answers-intersection group)
  (let ([size (length group)])
    (length (filter (λ x -> (= x size))
                    (vector->list (collect-answers group))))))

(printf "~s\n" (sum (map answers-intersection groups)))

