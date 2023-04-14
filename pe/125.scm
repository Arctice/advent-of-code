#!chezscheme
(import (scheme) (pe))

;; The palindromic number 595 is interesting because it can be written as the
;; sum of consecutive squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.

;; There are exactly eleven palindromes below one-thousand that can be
;; written as consecutive square sums, and the sum of these palindromes is
;; 4164. Note that 1 = 0^2 + 1^2 has not been included as this problem is
;; concerned with the squares of positive integers.

;; Find the sum of all the numbers less than 10^8 that are both palindromic
;; and can be written as the sum of consecutive squares.


(define (sum-of-squares? n)
  (define (square n) (* n n))
  (let scan ([lower 1] [upper 2] [sq-sum 5])
    (cond [(= lower upper) #f]
          [(= sq-sum n) #t]
          [(< sq-sum n)
           (scan lower (+ 1 upper) (+ sq-sum (square (+ 1 upper))))]
          [else (scan (+ 1 lower) upper (- sq-sum (square lower)))])))

(define (generate-palindromes d)
  (let* ([tail (div d 2)]
         [middle? (odd? (mod d 2))]
         [pad-format (format "~~~s,,,'0@s" tail)]
         [tails (map (λ (n) (format pad-format n))
                     (cdr (iota (expt 10 tail))))])
    (define (palindromize a)
      (let ([b (list->string (reverse (string->list a)))])
        (if (not middle?) (string-append a b)
            (map (λ (k) (string-append a (number->string k) b))
                 (iota 10)))))
    (filter (λ (n) (< (expt 10 (- d 1)) n))
            (map string->number
                 (let ([palindromes (map palindromize tails)])
                   (if (not middle?) palindromes
                       (fold-left append '() palindromes)))))))


(define palindromes
  (apply append (cdr (iota 10))
         (map generate-palindromes (cddr (iota 9)))))

(define matching (filter sum-of-squares? palindromes))

(define (problem-125) (fold-left + 0 matching))

(define answer-125 '1b5635e8ab723e01570ca783129493dd)
