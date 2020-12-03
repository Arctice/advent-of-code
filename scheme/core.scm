#!chezscheme

(library (core)
  (export
   head tail pair
   inc dec
   foldl1
   max-key sum
   interpose
   string-join string-strip string-split
   to-string
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
    (define (strip-tail ss len first last i)
      (cond
       [(>= i len) (substring ss first last)]
       [(char-whitespace? (string-ref ss i)) (strip-tail ss len first last (inc i))]
       [else (strip-tail ss len first (inc i) (inc i))]))
    (define (strip-head ss len first)
      (cond
       [(>= first len) ""]
       [(char-whitespace? (string-ref ss first)) (strip-head ss len (inc first))]
       [else (strip-tail ss len first first first)]))
    (strip-head ss (string-length ss) 0))

  (define (string-split ss separator)
    (let ([sep-len (string-length separator)])
      (let loop ([word ""] [ss ss])
        (cond [(< (string-length ss) sep-len)
               (list (string-append word ss))]
              [(string=? separator (substring ss 0 sep-len))
               (cons word (loop "" (substring ss sep-len (string-length ss))))]
              [else (loop (string-append word (substring ss 0 1))
                          (substring ss 1 (string-length ss)))]))))

  (define (to-string obj)
    (let ([out (open-output-string)])
      (display obj out)
      (get-output-string out)))

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
