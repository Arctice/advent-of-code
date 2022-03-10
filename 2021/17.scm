(import (core))
(optimize-level 3)

;; (define target '((20 . 30) . (-10 . -5)))
(define target '((85 . 145) . (-163 . -108)))

(define (within-target? x y)
  (let ([xt (car target)] [yt (cdr target)])
    (and (fx<= (car xt) x (cdr xt))
         (fx<= (car yt) y (cdr yt)))))

(define (trajectory vel-x vel-y)
  (let step ([x 0] [y 0]
             [vel-x vel-x] [vel-y vel-y]
             [highest 0])
    (let ([x (fx+ x vel-x)] [y (fx+ y vel-y)]
          [vel-x (if (zero? vel-x) vel-x (- vel-x 1))]
          [vel-y (fx- vel-y 1)])
      (cond [(within-target? x y) highest]
            [(or (< y (cadr target)) (> x (cdar target))) #f]
            [else (step x y vel-x vel-y (max y highest))]))))

(let* ([trajectories
        (map (λ (x) (map (λ (y) (trajectory x (- y 180)))
                         (iota 2000)))
             (iota 200))]
       [trajectories
        (filter values (fold-left append '() trajectories))])
  (printf "part 1 ~s\n" (apply max trajectories))
  (printf "part 2 ~s\n" (length trajectories)))
