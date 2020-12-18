#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))

(define (parse-rule line)
  (apply
   (λ name vals ->
      (pair name
            (map (λ range -> (apply pair (map string->number
                                              (string-split range "-"))))
                 (string-split vals " or "))))
   (string-split line ": ")))

(define (parse-inputs in)
  (define (csv->ints line)
    (map string->number (string-split line ",")))
  (apply
   (λ rules your others ->
      (values (map parse-rule rules)
              (csv->ints (cadr your))
              (map csv->ints (tail others))))
   (map (λ line -> (string-split (string-strip line) "\n"))
        (string-split in "\n\n"))))


(define-values (rules your-ticket other-tickets)
  (parse-inputs (slurp "16.input")))


(define (matches range x)
  (and (<= (head range) x) (<= x (tail range))))
(define (possible-value x)
  (exists (λ rule -> (exists (λ range -> (matches range x)) (tail rule)))
          rules))

(define (part-1-errors)
  (sum (foldl1 append (map (partial filter (compose not possible-value))
                           other-tickets))))

(pretty-print (part-1-errors))


;; part 2

(define valid-tickets (filter (partial for-all possible-value) other-tickets))

(define (valid-positions ranges)
  (define (valid-value x)
    (exists (λ range -> (matches range x)) ranges))
  (let ([valid (map (partial for-all valid-value)
                    (apply map list your-ticket valid-tickets))])
    (filter (λ i -> (list-ref valid i))
            (enumerate your-ticket))))

(define (solve-constraints sets)
  (let ([determined (find (λ set -> (= (length set) 1))
                          (filter list? sets))])
    (if (not determined) sets
        (let ([choice (head determined)])
          (solve-constraints
           (map (λ set ->
                   (cond [(eq? set determined) choice]
                         [(list? set) (remove choice set)]
                         [else set]))
                sets))))))

(define (field-order)
  (solve-constraints
   (map valid-positions (map tail rules))))

(define (part-2)
  (let ([fields (field-order)])
    (map (λ i -> (list-ref your-ticket i))
         (list-head (field-order) 6))))

(pretty-print (foldl1 * (part-2)))
