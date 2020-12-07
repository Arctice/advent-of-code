#!chezscheme

(library (core)
  (export
   head tail pair
   inc dec
   foldl1
   max-key sum
   interpose
   string-join string-strip string-split string-replace
   to-string
   slurp readlines
   list-slice
   zip
   make-dict dict-set! dict-lookup dict-update!
   id
   -> λ compose partial
   memoized
   )
  (import (scheme))


  (define head car)
  (define tail cdr)
  (define pair cons*)


  (define inc add1)
  (define dec sub1)

  (define (id v) v)

  (define (foldl1 fn seq) (fold-left fn (head seq) (tail seq)))


  (define (max-key key . vals)
    (define max-key*
      (lambda (mx mx-key key vals)
        (if (null? vals) mx
            (let* ([next (head vals)]
                   [next-key (key next)])
              (if (< mx-key next-key)
                  (max-key* next next-key key (tail vals))
                  (max-key* mx mx-key key (tail vals)))))))
    (if (null? vals) #f
        (let* ([first (head vals)]
               [first-key (key first)])
          (max-key* first first-key key (tail vals)))))

  (define (sum seq) (fold-left + 0 seq))


  (define (interpose sep l)
    (cond
     [(null? l) '()]
     [(null? (cdr l)) l]
     [else (cons (car l) (cons sep (interpose sep (cdr l))))]))


  (define string-join
    (case-lambda
     ([strs] (apply string-append strs))
     ([sep strs] (apply string-append (interpose sep strs)))))

  (define (string-strip ss)
    (let* ([len (string-length ss)]
           [first (let loop ([i 0])
                    (cond [(= i len) i]
                          [(char-whitespace? (string-ref ss i))
                           (loop (inc i))]
                          [else i]))]
           [last (let loop ([i len])
                   (cond [(= i first) i]
                         [(char-whitespace? (string-ref ss (- i 1)))
                          (loop (dec i))]
                         [else i]))])
      (substring ss first last)))

  (define (string-split ss separator)
    (letrec* ([ss-len (string-length ss)]
              [sep-len (string-length separator)]
              [last-possible-match (fx- ss-len sep-len)]
              [try-match
               (λ first -> (let for ([i 0])
                             (if (char=? (string-ref ss (fx+ first i))
                                         (string-ref separator i))
                                 (let ([i (inc i)])
                                   (if (fx= i sep-len) #t (for i)))
                                 #f)))]
              [find-match
               (λ index -> (cond [(fx> index last-possible-match) #f]
                                 [(try-match index) index]
                                 [else (find-match (inc index))]))]
              [next-split
               (λ start -> (let ([match (find-match start)])
                             (if match (cons (substring ss start match)
                                             (next-split (fx+ match sep-len)))
                                 (list (substring ss start ss-len)))))]
              )
      (if (fx= sep-len 0) (error 'string-split "empty separator"))
      (next-split 0)))

  (define (string-replace ss old new)
    (string-join new (string-split ss old)))

  (define (to-string obj)
    (let ([out (open-output-string)])
      (display obj out)
      (get-output-string out)))

  (define (slurp path)
    (with-input-from-file path
      (λ -> (get-string-all (current-input-port)))))

  (define (readlines path)
    (call-with-input-file path
      (λ in -> (let loop () (if (port-eof? in) '()
                                (cons (get-line in) (loop)))))))


  (define (list-slice list start end)
    (let* ([len (length list)]
           [left (max 0 (min start len))]
           [right (max 0 (- (min end len) left))])
      (list-head (list-tail list left) right)))


  (define (zip . args) (apply map list args))


  (define (make-dict) (make-hashtable equal-hash equal?))
  (define (dict-set! dict key val) (hashtable-set! dict key val))
  (define dict-lookup
    (case-lambda
     ([dict key] (hashtable-ref dict key #f))
     ([dict key default] (hashtable-ref dict key default))))
  (define dict-update! hashtable-update!)


  (define-syntax -> (syntax-rules ()))
  (define-syntax λ
    (syntax-rules (->)
      [(_ -> . body) (lambda () . body)]
      [(_ arg1 arg2 ... -> body) (lambda (arg1 arg2 ...) body)]
      ))

  (define (compose fn . fns)
    (if (null? fns) fn
        (let ([g (apply compose fns)])
          (lambda args (call-with-values (λ -> (apply g args)) fn)))))

  (define-syntax partial
    (syntax-rules ()
      ([_ f a1 a2 ...] (lambda args (apply f a1 a2 ... args)))))

  (define (memoized fn)
    (let ([cache (make-dict)])
      (lambda args
        (let ([prev (dict-lookup cache args)])
          (or prev
              (let ([result (apply fn args)])
                (dict-set! cache args result)
                result))))))
  
  )
