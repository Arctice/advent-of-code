(define-syntax λ (identifier-syntax lambda))

(define (slurp path)
  (call-with-input-file path get-string-all))

(define input
  (map (λ (c) (string->number (string c)))
       (filter (λ (c) (char-numeric? c))
               (string->list (slurp "6.input")))))

;; part 1, naive
(let run ([fish (sort < input)]
          [steps 80])
  (if (zero? steps)
      (printf "~s\n" (length fish))
      (let ([spawns (length (filter zero? fish))]
            [fish (map (λ (c) (if (zero? c) 6 (- c 1)))
                       fish)])
        (run (append (sort < fish)
                     (make-list spawns 8))
             (- steps 1)))))

;; part 2, radix
(let ([fish (make-vector 9 0)]
      [steps 256])
  (for-each (λ (n) (vector-set! fish n (+ 1 (vector-ref fish n))))
            input)
  (for-each
   (λ (_) (let ([spawns (vector-ref fish 0)])
            (for-each (λ (i) (vector-set! fish i (vector-ref fish (+ 1 i))))
                      (iota 8))
            (vector-set! fish 6 (+ spawns (vector-ref fish 6)))
            (vector-set! fish 8 spawns)))
   (iota steps))
  (printf "~s\n" (fold-left + 0 (vector->list fish))))
