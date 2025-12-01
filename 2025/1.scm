(import (core))

(define (parse line)
  (let* ([num (string->number
               (substring line 1 (string-length line)))])
    (cons* (string-ref line 0) num)))

(define input (map parse (readlines "1.input")))

(let safe ([dial 50] [instructions input]
           [password-1 0] [password-2 0])
  (let* ([clicks (car instructions)]
         [direction (case (car clicks) [#\R 1] [#\L -1])]
         [next-dial (+ dial (* direction (cdr clicks)))]
         [passed-zero (and (not (zero? dial)) (<= next-dial 0))]
         [extra-passes (div (abs next-dial) 100)]
         [dial (mod next-dial 100)]
         [password-1 (+ password-1 (if (zero? dial) 1 0))]
         [password-2 (+ password-2 (if passed-zero 1 0) extra-passes)])
    (if (null? (cdr instructions))
        (printf "~s ~s\n" password-1 password-2)
        (safe dial (cdr instructions) password-1 password-2))))
