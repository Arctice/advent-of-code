#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))


(define (parse ss)
  (let* ([ss (string-split ss ": ")]
         [pwd (cadr ss)]
         [policy (head ss)]
         [c (string-ref policy (dec (string-length policy)))]
         [range (string-split (head (string-split policy " ")) "-")]
         [range (map string->number range)])
    (list (head range) (cadr range) c pwd)))


(define (valid? min max c pwd)
  (let ([count (length (filter (partial eq? c)
                               (string->list pwd)))])
    (and (<= min count) (<= count max))))

(define (valid2? a b c pwd)
  (let ([a (eq? c (string-ref pwd (dec a)))]
        [b (eq? c (string-ref pwd (dec b)))])
    (or (and (not a) b)
        (and a (not b)))))


(define xs (map parse (readlines "2.input")))


;; part 1
(printf "~s\n" (length (filter (partial apply valid?) xs)))

;; part 2
(printf "~s\n" (length (filter (partial apply valid2?) xs)))

