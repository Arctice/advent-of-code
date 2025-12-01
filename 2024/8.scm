(import (core))

(define grid (readlines "8.input"))

(define width (string-length (car grid)))
(assert (= width (length grid)))

(define antennas (make-hashtable equal-hash equal?))

(for-each
 (λ (row y)
   (for-each
    (λ (c x)
      (unless (char=? #\. c)
        (hashtable-update!
         antennas c (λ (l) (cons (list x y) l)) '())))
    (string->list row) (iota width)))
 grid (enumerate grid))

(define antinodes (make-hashtable equal-hash equal?))
(define (inbounds? pos)
  (let ([x (car pos)] [y (cadr pos)])
    (and (< -1 x width) (< -1 y width))))
(define (add-antinode! pos)
  (when (inbounds? pos)
    (hashtable-set! antinodes pos #t)))

(vector-for-each
 (λ (positions)
   (for-each
    (λ (A)
      (for-each
       (λ (B) (unless (equal? A B)
                (let ([d (map - A B)])
                  (add-antinode! (map + A d))
                  (add-antinode! (map - B d)))))
       positions))
    positions))
 (hashtable-values antennas))

(printf "part-1: ~s\n" (hashtable-size antinodes))

(vector-for-each
 (λ (positions)
   (for-each
    (λ (A)
      (for-each
       (λ (B)
         (unless (equal? A B)
           (let ([d (map - A B)])
             (let scan ([A* A])
               (when (inbounds? A*)
                 (add-antinode! A*)
                 (scan (map + A* d))))
             (let scan ([B* B])
               (when (inbounds? B*)
                 (add-antinode! B*)
                 (scan (map - B* d))))
             )))
       positions))
    positions))
 (hashtable-values antennas))

(printf "part-2: ~s\n" (hashtable-size antinodes))
