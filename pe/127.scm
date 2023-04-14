#!chezscheme
(import (scheme) (pe))

;; The radical of n, rad(n), is the product of distinct prime factors of n.
;; For example, 504 = 2^3 × 3^2 × 7, so rad(504) = 2 × 3 × 7 = 42.

;; We shall define the triplet of positive integers (a, b, c) to be an
;; abc-hit if:

;;  1. GCD(a, b) = GCD(a, c) = GCD(b, c) = 1
;;  2. a < b
;;  3. a + b = c
;;  4. rad(abc) < c

;; For example, (5, 27, 32) is an abc-hit, because:

;;  1. GCD(5, 27) = GCD(5, 32) = GCD(27, 32) = 1
;;  2. 5 < 27
;;  3. 5 + 27 = 32
;;  4. rad(4320) = 30 < 32

;; It turns out that abc-hits are quite rare and there are only thirty-one
;; abc-hits for c < 1000, with ∑c = 12523.

;; Find ∑c for c < 120000.

(define radical
  (let* ([radical (λ (n) (fold-left * 1 (map car (factorize n))))]
         [cache (list->fxvector (map radical (iota 120001)))])
    (λ (n) (fxvector-ref cache n))))

(define (scan max)
  (let ([s 0])
    (let search ([c 3])
      (let ([c* (radical c)])
        (when (not (= c c*))
          (let inner ([a 1] [b (- c 1)])
            (when (< (* 2 a) c)
              (let ([b* (radical b)])
                (when (> c (* b* c*))
                  (let ([a* (radical a)])
                    (when (and (> c (* a* b* c*))
                               (= 1 (gcd a* b* c*)))
                      (set! s (+ s c))))))
              (inner (1+ a) (1- b))))))
      (when (< c max) (search (+ c 1))))
    s))

(define (problem-127) (scan 120000))

(define answer-127 'c6b1ae935b33c90a2c320b5f6ef3e4ba)
