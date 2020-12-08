#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (chezscheme) (core))

(define (parse-instruction line)
  (apply (λ op x -> (pair (string->symbol op)
                          (string->number x)))
   (string-split line " ")))

(define (execute code)
  (define seen (make-vector (vector-length code) #f))
  (define halt (void))

  (define (next acc ip)
    (if (vector-ref seen ip)
        (halt (list 'infinite-loop acc)))
    (vector-set! seen ip #t)
    (let* ([inst (vector-ref code ip)]
           [op (head inst)] [v (tail inst)])
      (case op [nop (next acc (inc ip))]
            [jmp (next acc (+ ip v))]
            [acc (next (+ acc v) (inc ip))]
            [halt (halt (list 'halt acc))])))

  (call/cc (lambda (here) (set! halt here)
                   (next 0 0))))

(define tape
  (list->vector
   (append! (map parse-instruction (readlines "8.input"))
            '((halt . 0)))))


;; part 1
(pretty-print (execute tape))


;; part 2
(pretty-print
 (find
  (λ result -> (eq? 'halt (head result)))
  (map (λ i ->
          (let* ([tape* (vector-copy tape)]
                 [inst (vector-ref tape* i)]
                 [op (head inst)]
                 [swapped (case op [nop 'jmp] [jmp 'nop])])
            (vector-set! tape* i (pair swapped (tail inst)))
            (execute tape*)))
       (filter (λ i -> (member (head (vector-ref tape i)) '(nop jmp)))
               (iota (vector-length tape))))))
