(define-syntax λ (identifier-syntax lambda))

(define (readlines path)
  (call-with-input-file path
    (λ (f) (let line () (if (port-eof? f) '()
                            (cons (get-line f) (line)))))))

(define input (map (λ (line) (string->number line 2))
                   (readlines "3.input")))

(define width (fold-left max 0 (map fxlength input)))

(define (count-column pos nums)
  (fold-left + 0 (map (λ (n) (fxand 1 (fxsrl n pos))) nums)))

;; part 1
(let* ([counts (map (λ (i) (count-column i input)) (iota width))]
       [best (map (λ (n) (if (< n (/ (length input) 2)) 0 1)) counts)]
       [gamma (apply + (map fxsll best (enumerate best)))]
       [epsilon (fxxor gamma (- (expt 2 width) 1))])
  (printf "~s\n" (* gamma epsilon)))

;; part 2
(let* ([a
        (fold-left
         (λ (xs i)
           (if (= 1 (length xs)) xs
               (let* ([ones (count-column i xs)]
                      [best (if (< ones (/ (length xs) 2)) 0 1)])
                 (filter (λ (x) (= best (fxand 1 (fxsrl x i)))) xs))))
         input (reverse (iota width)))]
       [b
        (fold-left
         (λ (xs i)
           (if (= 1 (length xs)) xs
               (let* ([ones (count-column i xs)]
                      [best (if (< ones (/ (length xs) 2)) 0 1)])
                 (filter (λ (x) (not (= best (fxand 1 (fxsrl x i))))) xs))))
         input (reverse (iota width)))])
  (printf "~s\n" (* (car a) (car b))))


