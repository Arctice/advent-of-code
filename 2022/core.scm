#!chezscheme

(library (core)
  (export
   λ
   string-split
   slurp readlines)
  (import (scheme))

  (define-syntax λ (identifier-syntax lambda))

  (define (slurp path)
    (call-with-input-file path get-string-all))

  (define (readlines path)
    (define (next f) (if (port-eof? f) '()
                         (cons (get-line f) (next f))))
    (call-with-input-file path next))

  (define (string-split separator ss)
    (letrec* ([ss-len (string-length ss)]
              [sep-len (string-length separator)]
              [last-possible-match (fx- ss-len sep-len)]
              [try-match
               (λ (first) (let for ([i 0])
                            (if (char=? (string-ref ss (fx+ first i))
                                        (string-ref separator i))
                                (let ([i (+ 1 i)])
                                  (if (fx= i sep-len) #t (for i)))
                                #f)))]
              [find-match
               (λ (index) (cond [(fx> index last-possible-match) #f]
                                [(try-match index) index]
                                [else (find-match (+ 1 index))]))]
              [next-split
               (λ (start) (let ([match (find-match start)])
                            (if match (cons (substring ss start match)
                                            (next-split (fx+ match sep-len)))
                                (list (substring ss start ss-len)))))])
      (if (fx= sep-len 0) (error 'string-split "empty separator"))
      (next-split 0)))

  )
