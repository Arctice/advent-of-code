#!chezscheme
(import (scheme) (core))

(define input (map string->list (readlines "3.input")))

(define (priority c)
  (mod (- (char->integer c) 96) 58))

(define (part-1)
  (define (misplaced-item bag)
    (let* ([l (length bag)]
           [fst (list-head bag (div l 2))]
           [snd (list-tail bag (div l 2))]
           [duplicate (find (λ (c) (memq c snd)) fst)])
      (priority duplicate)))
  (fold-left + 0 (map misplaced-item input)))

(printf "~s\n" (part-1))

(define groups
  (let next ([rest input])
    (if (null? rest) '()
        (cons (list-head rest 3)
              (next (list-tail rest 3))))))

(define (bag->bitset bag)
  (apply bitwise-ior
         (map (λ (item) (fxsll 1 (priority item))) bag)))

(define (part-2)
  (define (group-badge group)
    (let* ([sets (map bag->bitset group)]
           [overlap (apply bitwise-and sets)])
      (bitwise-first-bit-set overlap)))
  (fold-left + 0 (map group-badge groups)))

(printf "~s\n" (part-2))
