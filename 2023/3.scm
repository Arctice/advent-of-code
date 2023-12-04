(import (core))

(define schematic (readlines "3.input"))

(define (part-numbers row)
  (let scan ([n 0] [s ""])
    (cond
     [(= n (string-length row))
      (if (= 0 (string-length s)) '()
          (list (cons* (- n (string-length s)) n s)))]
     [(char-numeric? (string-ref row n))
      (scan (+ 1 n) (string-append s (string (string-ref row n))))]
     [(< 0 (string-length s))
      (cons (cons* (- n (string-length s)) n s)
            (scan (+ 1 n) ""))]
     [else (scan (+ 1 n) s)])))

(define (part-symbols schematic)
  (let ([symbols '()])
    (for-each
     (λ (y row)
       (for-each
        (λ (x c)
          (when (not (or (char-numeric? c) (char=? c #\.)))
            (set! symbols (cons (cons* y x c) symbols))))
        (iota (string-length row)) (string->list row)))
     (enumerate schematic) schematic)
    (reverse symbols)))

(define part-layout (map part-numbers schematic))
(define symbol-list (part-symbols schematic))

(define part-numbers (make-hashtable equal-hash equal?))

(define (adjacent-parts y x)
  (let ([parts (make-hashtable equal-hash equal?)])
    (for-each
     (λ (y)
       (for-each
        (λ (part)
          (let ([part-a (car part)] [part-b (cadr part)])
            (when (and (< part-a (+ x 2)) (< (- x 1) part-b))
              (hashtable-set! parts (cons y part) #t))))
        (list-ref part-layout y)))
     (list (- y 1) y (+ 1 y)))
    (vector->list (hashtable-keys parts))))

;; part 1
(for-each
 (λ (symbol)
   (let ([y (car symbol)] [x (cadr symbol)])
     (for-each (λ (part) (hashtable-set! part-numbers part #t))
               (adjacent-parts y x))))
 symbol-list)

(printf "~s\n" (fold-left
                + 0 (map string->number
                         (map cdddr (vector->list
                                     (hashtable-keys part-numbers))))))

;; part 2
(define (gear? symbol) (char=? #\* (cddr symbol)))


(pretty-print
 (fold-left
  + 0
  (map
   (λ (symbol)
     (let* ([y (car symbol)] [x (cadr symbol)]
            [parts (adjacent-parts y x)])
       (if (not (= 2 (length parts))) 0
           (apply * (map string->number (map cdddr parts))))))
   (filter gear? symbol-list))))
