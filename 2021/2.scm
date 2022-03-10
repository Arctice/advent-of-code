(define-syntax λ (identifier-syntax lambda))

(define (readlines path)
  (call-with-input-file path
    (λ (f) (let line () (if (port-eof? f) '()
                            (cons (get-line f) (line)))))))
(define (drop l n)
  (list-head l (- (length l) n)))

(define input
  (map (λ (line)
         (let* ([line (string->list line)]
                [value (memq #\space line)]
                [word (list-head line (- (length line) (length value)))])
           (cons* (list->string word)
                  (string->number (list->string (cdr value))))))
       (readlines "2.input")))

;; part 1
(let run ([pos 0] [depth 0] [course input])
  (if (null? course) (printf "~s\n" (* pos depth))
      (let* ([val (cdar course)]
             [move (case (caar course)
                     ["forward" `(,val . 0)]
                     ["down" `(0 . ,val)]
                     ["up" `(0 . ,(- val))])])
        (run (+ pos (car move))
             (+ depth (cdr move))
             (cdr course)))))

