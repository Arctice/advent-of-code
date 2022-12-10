#!chezscheme
(import (scheme) (core))

(define input
  (map (λ (line) (let ([line (string-split " " line)])
                   (cons (string->symbol (car line))
                         (map string->number (cdr line)))))
       (readlines "10.input")))

(define (cpu)
  (let run ([register 1] [tape input] [cycle 1])
    (cons
     (cons* cycle register)
     (if (null? tape) '()
         (let ([next (car tape)] [rest (cdr tape)] [cycle (+ 1 cycle)])
           (record-case
            next
            [noop () (run register rest cycle)]
            [addx (n) (run register (cons `(addx* ,n) rest) cycle)]
            [addx* (n) (run (+ n register) rest cycle)]))))))

(let* ([output
        (filter (λ (x) (= 20 (mod (car x) 40))) (cpu))]
       [signal-strengths
        (map (λ (x) (* (car x) (cdr x))) output)])
  (printf "part 1: ~s\n" (fold-left + 0 signal-strengths)))


(define (crt register-values)
  (for-each
   (λ (cycle sprite)
     (let* ([pixel (mod (- cycle 1) 40)]
            [activated (<= (abs (- pixel sprite)) 1)])
       (printf "~c" (if activated #\| #\space))
       (when (= pixel 39) (printf "\n"))))
   (map car register-values)
   (map cdr register-values)))

(crt (cpu))
