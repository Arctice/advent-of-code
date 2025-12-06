(import (core))

(define height 4)

(define input (readlines "6.input"))

(define (clean-line line)
  (filter (λ (s) (not (zero? (string-length s))))
          (string-split " " line)))
(define cleaned (map clean-line input))

(define numbers
  (map (λ (line) (map string->number line))
       (list-head cleaned height)))
(define operations
  (map string->symbol (list-ref cleaned height)))

(define (calculate-column i)
  (let ([nums (map (λ (k) (list-ref (list-ref numbers k) i))
                   (iota height))]
        [op (list-ref operations i)])
    (eval (append (list op) nums))))

(printf "part 1: ~s\n"
        (fold-left + 0 (map (λ (i) (calculate-column i))
                            (enumerate operations))))

(define (vertical-number c)
  (let ([digits (map (λ (r) (string-ref (list-ref input r) c))
                     (iota height))])
    (string->number (string-strip (list->string digits)))))

(let* ([width (string-length (car input))]
       [numbers (map vertical-number (iota width))]
       [number-groups
        (let scan ([rest numbers] [group '()])
          (cond
           [(null? rest) (list group)]
           [(car rest) (scan (cdr rest) (cons (car rest) group))]
           [else (cons group (scan (cdr rest) '()))]))]
       [results
        (map (λ (op numbers) (eval (cons op numbers)))
             operations number-groups)])
  (printf "part 2: ~s\n" (fold-left + 0 results)))

