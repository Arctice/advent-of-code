#!chezscheme
(import (scheme) (pe))

;; (...)

;; We shall define m(k) to be the minimum number of multiplications to
;; compute n^k; for example m(15) = 5.

;; For 1 ≤ k ≤ 200, find ∑ m(k).

(define (dfs state next end? depth)
  (cond [(end? state) state]
        [(zero? depth) #f]
        [else (find (λ (node) (dfs node next end? (- depth 1)))
                    (next state))]))

(define (possible-chains state)
  (let* ([sums (map (λ (a) (map (λ (b) (+ a b)) state)) state)]
         [sums (apply append! sums)]
         [highest (car state)]
         [useful? (λ (mul) (<  highest mul))]
         [sorted (sort < (filter useful? sums))]
         [unique
          (fold-left (λ (set n) (if (= n (car set)) set
                                    (cons n set)))
                     (list (car sorted)) (cdr sorted))])
    (map (λ (next) (cons next state)) unique)))

(define chain-length (make-hashtable + =))
(define (found-all? max)
  (λ (state)
    (let* ([n (car state)]
           [new (and (not (hashtable-ref chain-length n #f))
                     (<= n max))])
      (and new
           (hashtable-set! chain-length n (- (length state) 1))
           (for-all (λ (n) (hashtable-ref chain-length n #f))
                    (cdr (iota (+ 1 max))))))))

(define solution
  (let ([max-n 200])
    (time
     (exists (λ (max-depth) (dfs '(1) possible-chains
                                 (found-all? max-n) max-depth))
             (iota 20)))

    (fold-left + 0 (map (λ (n) (hashtable-ref chain-length n #f))
                        (cdr (iota (+ 1 max-n)))))))

(define answer 'b710915795b9e9c02cf10d6d2bdb688c)
(verify solution answer)
