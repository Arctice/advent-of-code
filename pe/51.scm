#!chezscheme
(import (scheme) (pe))

;; By replacing the 1^st digit of the 2-digit number *3, it turns out that
;; six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all
;; prime.

;; By replacing the 3^rd and 4^th digits of 56**3 with the same digit, this
;; 5-digit number is the first example having seven primes among the ten
;; generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663,
;; 56773, and 56993. Consequently 56003, being the first member of this
;; family, is the smallest prime with this property.

;; Find the smallest prime which, by replacing part of the number (not
;; necessarily adjacent digits) with the same digit, is part of an eight
;; prime value family.


(define (digits n)
  (if (< 0 n) (append (digits (div n 10))
                      (list (mod n 10))) '()))

(define (trie-add! trie xs)
  (define (add! node xs)
    (cond
     [(null? (cdr xs)) (cons (car xs) node)]
     [(assq (car xs) node)
      => (λ (next) (set-cdr! next (add! (cdr next) (cdr xs)))
            node)]
     [else (cons (cons* (car xs) (add! '() (cdr xs)))
                 node)]))
  (let ([length-node (assq (length xs) trie)])
    (if length-node
        (begin (set-cdr! length-node (add! (cdr length-node) xs))
               trie)
        (cons (cons* (length xs) (add! '() xs)) trie))))

(define (trie-contains? trie xs)
  (define (next node xs)
    (cond
     [(null? (cdr xs)) (if (memq (car xs) node) #t #f)]
     [(assq (car xs) node) => (λ (node) (next (cdr node) (cdr xs)))]
     [else #f]))
  (let ([length-node (assq (length xs) trie)])
    (and length-node (next (cdr length-node) xs))))


(define (trie-find-similar trie xs)
  (define (next node xs old new)
    (cond
     [(null? (cdr xs))
      (map list (filter (λ (n) (= n (car xs))) node))]
     [(not new)
      (let ([sub-results
             (map (λ (sub-node)
                    (let ([digit (car sub-node)])
                      (map
                       (λ (result) (cons digit result))
                       (if (= digit (car xs))
                           (next (cdr sub-node) (cdr xs)
                                 old new)
                           (next (cdr sub-node) (cdr xs)
                                 (car xs) digit)))))
                  node)])
        (fold-left append '() sub-results))]
     [else
      (let ([match
             (λ (x)
               (let ([node (assq x node)])
                 (if (not node) '()
                     (map (λ (result) (cons x result))
                          (next (cdr node) (cdr xs) old new)))))])
        (append (match (car xs))
                (if (= (car xs) old)
                    (match new) '())))]))

  (let ([length-node (assq (length xs) trie)])
    (if (not length-node) '()
        (next (cdr length-node) xs #f #f))))

(define (similar-prime-family trie prime)
  (let* ([prime (digits prime)]
         [groups (make-hashtable equal-hash equal?)])
    (for-each
     (λ (near)
       (let ([key (filter (λ (i) (not (= (list-ref prime i)
                                         (list-ref near i))))
                          (enumerate near))])
         (hashtable-update! groups key (λ (group) (cons near group)) '())))
     (trie-find-similar trie prime))
    (cons prime
          (fold-left
           (λ (best next)
             (if (< (length best) (length next)) next best))
           '() (vector->list (hashtable-values groups))))))



(define (problem-51)
  (let* ([primes (sieve 1000000)]
         [trie
          (fold-left (λ (trie prime) (trie-add! trie (digits prime)))
                     '() primes)])
    (let ([result #f])
      (fold-left
       (λ (best n)
         (let* ([family (similar-prime-family trie n)]
                [size (length family)])
           (when (< best size)
             ;; (printf "~s ~s ~s\n" size n family)
             (set! result n))
           (max best size)))
       0 primes)
      result)))


(define answer-51 'e2a8daa5eb919905dadd795593084c22)
