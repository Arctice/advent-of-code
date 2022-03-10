(import (core))

(define input
  (map
   (λ (line)
     (let* ([line (string->list line)]
            [pivot (- (length line)
                      (length (memq #\- line)))])
       (cons*
        (string->symbol (list->string (list-head line pivot)))
        (string->symbol (list->string (cdr (list-tail line pivot)))))))
   (readlines "12.input")))

(define graph
    (fold-left
     (λ (graph A B)
       (when (not (eq? B 'start))
         (hashtable-update! graph A (λ (vs) (cons B vs)) '()))
       (when (not (eq? A 'start))
         (hashtable-update! graph B (λ (vs) (cons A vs)) '()))
       graph)
     (make-hashtable symbol-hash symbol=?)
     (map car input) (map cdr input)))

(define (big-cave? node)
  (let ([node (symbol->string node)])
    (for-all (λ (i) (char-upper-case? (string-ref node i)))
             (iota (string-length node)))))

(define (valid-1 path)
  (or (big-cave? (car path))
      (let scan ([rest path] [visited '()])
        (let ([at (car rest)])
          (cond
           [(null? (cdr rest)) #t]
           [(memq at visited) #f]
           [else (scan (cdr rest) (cons at visited))])))))

(define (valid-2 path)
  (or (big-cave? (car path))
      (let scan ([rest path] [visited '()] [double #f])
        (let ([at (car rest)])
          (cond
           [(null? (cdr rest)) #t]
           [(memq at visited)
            (if double #f (scan (cdr rest) visited #t))]
           [else (scan (cdr rest) (cons at visited) double)])))))

(define (all-paths valid?)
  (let search ([path '(start)])
    (let ([node (car path)])
      (if (eq? node 'end) (list path)
          (let* ([path (if (big-cave? (car path))
                           (cdr path) path)]
                 [paths (map (λ (next) (cons next path))
                             (symbol-hashtable-ref graph node #f))]
                 [valid (filter valid? paths)])
            (apply append! (map search valid)))))))

(pretty-print (length (time (all-paths valid-1))))
(pretty-print (length (time (all-paths valid-2))))
