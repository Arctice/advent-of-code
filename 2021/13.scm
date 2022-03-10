(import (core))
(define-values (dots folds)
  (let* ([input (slurp "13.input")]
         [folds (- (string-length input)
                   (length (memq #\f (string->list input))))]
         [dots
          (map (λ (line) (apply cons* (map string->number
                                           (string-split "," line))))
           (string-split "\n" (substring input 0 (- folds 2))))]
         [folds
          (map
           (λ (line) (let ([d (substring line 11 12)]
                           [num (substring line 13 (string-length line))])
                       (cons* (string->symbol d) (string->number num))))
           (cdr (reverse
                 (string-split "\n" (substring input folds
                                               (string-length input))))))])
    (values dots (reverse folds))))

(define (fold-transform fold)
  (let ([d (car fold)] [v (cdr fold)])
    (λ (dot) (let ([x (car dot)] [y (cdr dot)])
               (case d
                 [y (cons* x (- v (abs (- v y))))]
                 [x (cons* (- v (abs (- v x))) y)])))))

;; part 1
(let* ([fold (fold-transform (car folds))]
       [positions (make-hashtable equal-hash equal?)])
  (for-each (λ (dot) (hashtable-set! positions (fold dot) #t)) dots)
  (printf "~s\n" (hashtable-size positions)))

;; part 2
(let* ([folds (map fold-transform folds)]
       [fold (λ (dot) (fold-left (λ (d f) (f d)) dot folds))]
       [positions (make-hashtable equal-hash equal?)])
  (for-each (λ (dot) (hashtable-set! positions (fold dot) #t)) dots)
  (for-each
   (λ (y) (for-each
           (λ (x) (let ([dot? (hashtable-contains? positions (cons* x y))])
                    (printf "~x" (if dot? "." " "))))
           (iota 60))
      (printf "\n"))
   (iota 12)))

