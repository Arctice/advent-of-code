(import (core))

(define (parse path)
  (define (next port)
    (define (next-token)
      (let next-char ([chars '()])
        (let ([c (lookahead-char port)])
          (cond [(eof-object? c) #f]
                [(or (char-numeric? c) (char-alphabetic? c))
                 (next-char (cons (get-char port) chars))]
                [(char-whitespace? c)
                 (get-char port)
                 (if (null? chars) (next-char chars)
                     (list->string (reverse chars)))]
                [else (if (null? chars) (string (get-char port))
                          (list->string (reverse chars)))]))))
    (define (next-game)
      (next-token) (next-token)
      (let next-round ([group '()])
        (let ([token (next-token)])
          (cond
           [(not token) (list group)]
           [(string->number token)
            => (λ (count) (let ([color (string->symbol (next-token))])
                            (next-round (cons (cons* color count) group))))]
           [(string=? "," token) (next-round group)]
           [(string=? ";" token) (cons group (next-round '()))]
           [(string=? "Game" token) (list group)]))))
    (next-token)
    (let games ()
      (if (port-eof? port) '()
          (cons (next-game) (games)))))
  (call-with-input-file path next))

(define (round-get round color)
  (let ([have (assq color round)]) (if have (cdr have) 0)))

(define (part-1-valid? game)
  (define (round-valid? round)
    (and (>= 12 (round-get round 'red))
         (>= 13 (round-get round 'green))
         (>= 14 (round-get round 'blue))))
  (for-all round-valid? game))

(define (part-1 games)
  (let ([valid-id-sum 0])
    (for-each
     (λ (id game) (when (part-1-valid? game)
                    (set! valid-id-sum (+ (+ 1 id) valid-id-sum))))
     (enumerate input-games) input-games)
    valid-id-sum))

(define (part-2 games)
  (define (max-color game color)
    (apply max (map (λ (round) (round-get round color)) game)))
  (define (bag-size game)
    (* (max-color game 'red) (max-color game 'green) (max-color game 'blue)))
  (fold-left + 0 (map bag-size games)))

(define input-games (parse "2.input"))

(printf "~s\n" (part-1 input-games))
(printf "~s\n" (part-2 input-games))

