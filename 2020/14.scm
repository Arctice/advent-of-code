#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))

(define (parse line)
  (define (bitmask ss)
    (append
     (map (λ ss -> (string->number (string-append "#b" ss)))
          (list
           (string-replace ss "X" "0")
           (string-replace ss "X" "1")
           (string-replace  (string-replace ss "0" "1")
                            "X" "0")))
     (list
      (filter (λ i -> (eq? (string-ref ss (- 35 i)) #\X))
              (iota (string-length ss))))))
  (if (string=? "mask" (substring line 0 4))
      (cons 'mask
            (bitmask
             (list-ref (string-split line " ") 2)))
      (let* ([ss (substring line 4 (string-length line))]
             [addr (string->number
                    (list-ref (string-split ss "]") 0))]
             [value (string->number
                     (list-ref (string-split ss " ") 2))])
        (list 'mem addr value))))

(define input-program (map parse (readlines "14.input")))

;; part 1
(define (part-1)
  (let ([memory (make-eq-hashtable)]
        [mask-or 0]
        [mask-and 0])
    (for-each
     (λ instr ->
        (case (head instr)
          [mask (set! mask-or (cadr instr))
                (set! mask-and (caddr instr))]
          [mem (let* ([addr (cadr instr)]
                      [val (bitwise-and mask-and (caddr instr))]
                      [val (bitwise-ior mask-or val)])
                 (hashtable-set!
                  memory addr
                  val))]))
     input-program)
    (sum (vector->list (hashtable-values memory)))))

(pretty-print (part-1))


;; part 2

(define (bit-variants n)
  (map (λ z -> (map (λ d -> (bitwise-bit-set? z d))
                    (iota n)))
       (iota (expt 2 n))))

(define (part-2)
  (let ([memory (make-eq-hashtable)]
        [mask-or 0]
        [mask-and 0]
        [mask-flux '()])
    (for-each
     (λ instr ->
        (case (head instr)
          [mask (set! mask-or (list-ref instr 1))
                (set! mask-and (list-ref instr 3))
                (set! mask-flux (list-ref instr 4))]
          [mem
           (let* ([addr (cadr instr)]
                  [addr (bitwise-ior mask-or (bitwise-and mask-and addr))]
                  [val (caddr instr)])
             (for-each
              (λ variant ->
                 (let* ([layout
                         (filter (λ bit -> (list-ref variant bit))
                                 (enumerate variant))]
                        [masks
                         (map (λ bit -> (expt 2 (list-ref mask-flux bit)))
                              layout)])
                   (hashtable-set!
                    memory (fold-left bitwise-ior addr masks) val)))
              (bit-variants (length mask-flux)))
             )]))
     input-program)
    (sum (vector->list (hashtable-values memory)))))

(pretty-print (part-2))
