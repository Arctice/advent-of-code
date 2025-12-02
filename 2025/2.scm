(import (core))

(define (divides? d n) (= 0 (fxremainder n d)))
(define (range a b) (map (λ (x) (+ x a)) (iota (- b a))))

(define (repeating-pattern-numbers-in-range a b)
  (define found (make-eq-hashtable))
  (define (search digits min max)
    (for-each
     (λ (pattern-length)
       (let ([iter-min (div min (expt 10 (- digits pattern-length)))]
             [iter-max (div max (expt 10 (- digits pattern-length)))])
         (for-each
          (λ (pattern)
            (let* ([repeats (div digits pattern-length)]
                   [num (make-list repeats (number->string pattern))]
                   [num (string->number (apply string-append num))])
              (when (<= min num max)
                (hashtable-set! found num #t))))
          (range iter-min (+ 1 iter-max)))))
     (filter (λ (pattern-length) (divides? pattern-length digits))
             (cdr (iota digits)))))
  (let ([min-digits (flonum->fixnum (+ 1. (log a 10)))]
        [max-digits (flonum->fixnum (+ 1. (log b 10)))])
    (let next ([digits min-digits])
      (search digits
              (max a (expt 10 (- digits 1)))
              (min b (- (expt 10 digits) 1)))
      (if (< digits max-digits) (next (+ 1 digits))))
    (vector->list (hashtable-keys found))))

(define (parse line)
  (map (λ (range) (map string->number (string-split "-" range)))
       (string-split "," line)))
(define ranges (parse (string-strip (slurp "2.input"))))

(define found-numbers
  (apply append
         (map (λ (range) (apply repeating-pattern-numbers-in-range range))
              ranges)))
(define (part-1-number? n)
  (let* ([s (number->string n)] [l (string-length s)])
    (string=? (substring s 0 (div l 2))
              (substring s (div l 2) l))))
(printf "part 1: ~s\n" (fold-left + 0 (filter part-1-number? found-numbers)))
(printf "part 2: ~s\n" (fold-left + 0 found-numbers))
