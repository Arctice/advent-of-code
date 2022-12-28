#!chezscheme

(library (pe)
  (export λ
          verify
          divides?
          factorize
          divisors
          prime?
          sieve
          permutations
          )
  (import (scheme))
  (define-syntax λ (identifier-syntax lambda))

  (define (md5-check val)
    (call-with-values
        (lambda ()
          (open-process-ports
           (string-append " " (format "echo -n ~s | md5sum"
                                      (format "~s" val)))))
      (lambda (fd0 fd1 fd2 pid)
        (let loop ([spin #f])
          (if spin
              (get-string-n (transcoded-port fd1 (native-transcoder)) 32)
              (loop (input-port-ready? fd1)))))))

  (define (verify solution answer)
    (printf "~x ~s\n"
            (if (equal? (md5-check solution)
                        (format "~x" answer))
                "OK!" "WRONG!")
            solution))


  (define (divides? d n) (= 0 (fxremainder n d)))

  (define (consecutive-counts l)
    (define (recur current count l)
      (cond [(null? l) (list (cons* current count))]
            [(equal? current (car l))
             (recur current (+ 1 count) (cdr l))]
            [else
             (cons (cons* current count)
                   (recur (car l) 1 (cdr l)))]))
    (if (null? l) '() (recur (car l) 1 (cdr l))))

  (define (factorize n)
    (define (next-divisor d) (if (= d 2) 3 (+ 2 d)))
    (consecutive-counts
     (let loop ([d 2] [n n] [factors '()])
       (cond [(< n 2) factors]
             [(divides? d n) (loop d (/ n d) (cons d factors))]
             [(> (* d d) n) (cons n factors)]
             [else (loop (next-divisor d) n factors)]))))

  (define (divisors n)
    (fold-left
     (λ (divisors f)
       (append
        (let* ([exp (cdr f)] [factor (car f)]
               [exponents (reverse (list-tail (iota (+ 1 exp)) 1))])
          (apply
           append
           (map (λ (exponent)
                  (map (λ (divisor) (* divisor (expt factor exponent)))
                       divisors))
                exponents)))
        divisors))
     '(1) (factorize n)))

  (define (prime? n)
    (or (= n 2)
        (and (not (divides? 2 n))
             (let loop ([d 3])
               (cond [(< n (* d d)) #t]
                     [(divides? d n) #f]
                     [else (loop (+ 2 d))])))))

  (define (sieve size)
    (define (sieve xs table)
      (if (null? xs) '()
          (let* ([x (car xs)] [xs (cdr xs)]
                 [facts (hashtable-ref table x #f)])
            (if facts
                (begin
                  (hashtable-delete! table x)
                  (for-each
                   (λ (f) (hashtable-update!
                           table (+ f x)
                           (λ (facts) (cons f facts)) (list)))
                   facts)
                  (sieve xs table))
                (begin
                  (hashtable-set! table (* x x) (list x))
                  (cons x (sieve xs table)))
                ))))
    (sieve (list-tail (iota size) 2)
           (make-hashtable + =)))

  (define (permutations set)
    (define (list-cut* l i)
      (append! (list-head l i)
               (list-tail l (+ 1 i))))
    (if (null? set) '(())
        (fold-left
         append! '()
         (map (λ (i) (let ([selection (list-ref set i)]
                           [rest (list-cut* set i)])
                       (map (λ (p) (cons selection p))
                            (permutations rest))))
              (enumerate set)))))


  )

;; (define (fermat-test n)
;;   (define (try-divisor a) (= (expt-mod a n n) a))
;;   (try-divisor (+ 1 (random (max 1 (- n 1))))))
