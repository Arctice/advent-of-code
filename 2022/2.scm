#!chezscheme
(import (scheme) (core))

(define (loses-to hand)
  (case hand [rock 'paper] [paper 'scissors] [scissors 'rock]))

(define (hand-value hand)
  (case hand [rock 1] [paper 2] [scissors 3]))

(define (score-p1 line)
  (let* ([opponent
          (case (car line)
            [A 'rock] [B 'paper] [C 'scissors])]
         [hand
          (case (cdr line)
            [X 'rock] [Y 'paper] [Z 'scissors])]
         [won (eq? hand (loses-to opponent))]
         [drew (eq? hand opponent)])
    (+ (hand-value hand)
       (cond [won 6] [drew 3] [else 0]))))

(define (score-p2 line)
  (let* ([opponent
          (case (car line)
            [A 'rock] [B 'paper] [C 'scissors])]
         [result
          (case (cdr line)
            [X 'loss] [Y 'draw] [Z 'win])]
         [wins (loses-to opponent)]
         [loses (loses-to wins)]
         [hand
          (case result
            [win wins] [draw opponent] [loss loses])])
    (+ (hand-value hand)
       (case result [loss 0] [draw 3] [win 6]))))


(define input
  (let ([lines (readlines "2.input")])
    (map (λ (line)
           (cons* (string->symbol (substring line 0 1))
                  (string->symbol (substring line 2 3))))
         lines)))

(printf "~s\n" (fold-left + 0 (map score-p1 input)))

(printf "~s\n" (fold-left + 0 (map score-p2 input)))

