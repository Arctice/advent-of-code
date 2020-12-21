#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))

(define (parse line)
  (let* ([line (string-split line " (contains ")]
         [ingredients (string-split (head line) " ")]
         [allergens (string-split (head (string-split (cadr line) ")")) ", ")])
    (pair allergens ingredients)))

(define food (map parse (readlines "21.input")))


(define (set-intersection a b) (filter (λ x -> (member x b)) a))
(define (set-difference a b) (remp (λ x -> (member x b)) a))

(define (allergen-constraints)
  (let ([constraints (make-hashtable string-hash string=?)])
    (map (λ food ->
            (let ([ingredients (tail food)])
              (map (λ allergen -> (hashtable-update!
                                   constraints allergen
                                   (partial set-intersection ingredients)
                                   ingredients))
                   (head food))))
         food)
    (vector->list (vector-map pair (hashtable-keys constraints)
                              (hashtable-values constraints)))))

(define (solve-constraints allergens)
  (if (null? allergens) '()
      (let* ([a (find (λ a -> (= 2 (length a))) allergens)]
             [rest (remq a allergens)])
        (cons a (solve-constraints
                 (map (λ b -> (set-difference b a)) rest))))))


(let* ([allergens (sort (λ a b -> (string<? (head a) (head b))) 
                        (solve-constraints (allergen-constraints)))]
       [bad-food (map cadr allergens)])
  ;; part 1
  (pretty-print
   (sum (map (λ food -> (length (set-difference (tail food) bad-food))) food)))

  ;; part 2
  (printf "~a\n" (string-join "," bad-food)))
