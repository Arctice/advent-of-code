#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))

(define input
  (map (λ line -> (read (open-input-string (string-append "(" line ")"))))
       (readlines "18.input")))


(define (aoc-eval e)
  (cond [(or (symbol? e) (number? e)) e]
        [(< 3 (length e))
         (aoc-eval (cons (list-head e 3) (list-tail e 3)))]
        [else
         (eval (map aoc-eval (list (cadr e) (car e) (caddr e))))]))

;; part 1

(pretty-print (sum (map aoc-eval input)))

;; part 2

(define (aoc->rpn e)
  (define (gen e rpn)
    (cond [(null? e) rpn]
          [(number? (head e))
           (gen (tail e) (append rpn (list (head e))))]
          [(list? (head e))
           (gen (tail e) (append rpn (gen (head e) '())))]
          [(eq? '+ (head e)) (gen (tail e) (cons '+ rpn))]
          [(eq? '* (head e)) (append '(*) rpn (gen (tail e) '()))]))
  (gen e '()))

(define (rpn-eval e)
  (define (next e)
    (cond [(symbol? (head e))
           (let*-values ([[op] (head e)]
                         [[a e] (next (tail e))]
                         [[b rest] (next e)])
             (values (eval (list op a b)) rest))]
          [(number? (head e)) (values (head e) (tail e))]))
  (let-values ([[v _] (next e)]) v))

(pretty-print (sum (map (compose rpn-eval aoc->rpn) input)))

