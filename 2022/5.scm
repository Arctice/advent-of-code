#!chezscheme
(import (scheme) (core))

(define (parse-stack lines)
  (let* ([bucket-indices
          (map (λ (ss) (string->number (string-strip ss)))
               (string-split "   " (car (last-pair lines))))]
         [size (apply max bucket-indices)]
         [stack-lines (list-head lines (- (length lines) 1))])
    (list->vector
     (map (λ (k) (let ([offset (+ 1 (* 4 k))])
                   (filter (λ (c) (not (char-whitespace? c)))
                           (map (λ (line) (string-ref line offset))
                                stack-lines))))
          (iota size)))))

(define-record-type move (fields count from to))
(define (parse-instructions lines)
  (map
   (λ (line)
     (let* ([line (substring line 5 (string-length line))]
            [line (string-split " from " line)]
            [move (map string->number
                       (string-split " to " (cadr line)))]
            [crates (string->number (car line))]
            [from (car move)] [to (cadr move)])
       (make-move crates (- from 1) (- to 1))))
   lines))

(define (parse-input file)
  (let* ([lines (readlines file)]
         [instructions (cdr (memq "" lines))]
         [stack (list-head lines (- (length lines)
                                    (length instructions) 1))])
    (values (parse-stack stack)
            (parse-instructions instructions))))


(define (execute-move! stack move crane-type)
  (let* ([from (vector-ref stack (move-from move))]
         [grabbed (list-head from (move-count move))]
         [remaining (list-tail from (move-count move))]
         [to (vector-ref stack (move-to move))]
         [arrangement (if (eq? crane-type 9000)
                          (reverse grabbed) grabbed)]
         [new-stack (append arrangement to)])
    (vector-set! stack (move-from move) remaining)
    (vector-set! stack (move-to move) new-stack)))

(define (part-1)
  (define-values (stack moves) (parse-input "5.input"))
  (for-each (λ (move) (execute-move! stack move 9000)) moves)
  (printf "~s\n" (list->string (map car (vector->list stack)))))

(define (part-2)
  (define-values (stack moves) (parse-input "5.input"))
  (for-each (λ (move) (execute-move! stack move 9001)) moves)
  (printf "~s\n" (list->string (map car (vector->list stack)))))


(part-1)
(part-2)
