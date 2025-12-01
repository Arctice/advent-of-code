(import (core))

(define input
  (let ([entries (make-hashtable + =)])
    (for-each
     (λ (line) (let* ([parse (string-split ": " line)]
                      [key (string->number (car parse))]
                      [xs (string-split " " (cadr parse))]
                      [xs (map string->number xs)])
                 (hashtable-set! entries key xs)))
     (readlines "7.input"))
    entries))

(define (apply-ops-1 n xs)
  (let* ([positions (- (length xs) 1)])
    (fold-left
     (λ (total next index)
       ((if (bitwise-bit-set? n index) * +) total next))
     (car xs) (cdr xs) (iota positions))))

(define (try-1 result xs)
  (let* ([positions (- (length xs) 1)]
         [combinations (expt 2 positions)])
    (filter (λ (n) (= result (apply-ops-1 n xs)))
            (iota combinations))))

(let* ([keys (vector->list (hashtable-keys input))]
       [solvable? (λ (x) (not (null? (try-1 x (hashtable-ref input x (void))))))])
  (printf "part-1: ~s\n" (fold-left + 0 (filter solvable? keys))))


;; okay now do it ternary

(define (apply-ops-2 n xs)
  (let* ([positions (- (length xs) 1)])
    (define (ternary-ref n i)
      (mod (div n (expt 3 i)) 3))
    (define (|| a b)
      (string->number (string-append (number->string a) (number->string b))))
    (fold-left
     (λ (total next index)
       ((case (ternary-ref n index)
          [0 +] [1 *] [2 ||])
        total next))
     (car xs) (cdr xs) (iota positions))))

(define (try-2 result xs)
  (let* ([positions (- (length xs) 1)]
         [combinations (expt 3 positions)])
    (filter (λ (n) (= result (apply-ops-2 n xs)))
            (iota combinations))))

(let* ([keys (vector->list (hashtable-keys input))]
       [solvable? (λ (x) (not (null? (try-2 x (hashtable-ref input x (void))))))])
  (printf "part-2: ~s\n" (fold-left + 0 (filter solvable? keys))))

