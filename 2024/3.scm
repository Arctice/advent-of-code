(import (core))

(define (scan-number ss i)
  (let next ([i i] [num ""])
    (let ([c (string-ref ss i)])
      (if (not (char-numeric? c)) num
          (next (+ i 1) (string-append num (string c)))))))

(define (scan-mul ss i)
  (call/cc
   (λ (fail)
     (let*-values
         ([(i)
           (if (string=? "mul(" (substring ss i (+ i 4)))
               (values (+ i 4))
               (fail (cons* (+ i 1) #f)))]
          [(i a)
           (let* ([xs (scan-number ss i)]
                  [len (string-length xs)])
             (if (= 0 len) (fail (cons* i #f))
                 (values (+ i len) (string->number xs))))]
          [(i)
           (if (char=? #\, (string-ref ss i))
               (values (+ i 1))
               (fail (cons* i #f)))]
          [(i b)
           (let* ([xs (scan-number ss i)]
                  [len (string-length xs)])
             (if (= 0 len) (fail (cons* i #f))
                 (values (+ i len) (string->number xs))))]
          [(i)
           (if (char=? #\) (string-ref ss i))
               (values (+ i 1))
               (fail (cons* i #f)))])
       (cons* i (cons* a b))))))

(define (part-1-parse ss)
  (let scan ([i 0])
    (if (>= (+ 7 i) (string-length ss)) '()
        (let* ([mul (scan-mul ss i)]
               [i (car mul)] [mul (cdr mul)])
          (if mul (cons mul (scan i)) (scan i))))))

(define (part-2-parse ss)
  (let scan ([i 0] [do #t])
    (if (>= (+ 7 i) (string-length ss)) '()
        (cond
         [(string=? "do()" (substring ss i (+ i 4)))
          (scan (+ i 4) #t)]
         [(string=? "don't()" (substring ss i (+ i 7)))
          (scan (+ i 7) #f)]
         [(not do) (scan (+ i 1) do)]
         [else
          (let* ([mul (scan-mul ss i)]
                 [i (car mul)] [mul (cdr mul)])
            (if mul
                (cons mul (scan i do))
                (scan i do)))]))))


(define input (slurp "3.input"))

(printf "~s\n"
        (fold-left
         + 0
         (map (λ (mul) (* (car mul) (cdr mul)))
              (part-1-parse input))))

(printf "~s\n"
        (fold-left
         + 0
         (map (λ (mul) (* (car mul) (cdr mul)))
              (part-2-parse input))))
