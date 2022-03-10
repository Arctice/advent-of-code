(import (core))

(define (build-dict alist)
  (let ([dict (make-hashtable string-hash string=?)])
    (for-each (λ (k v) (hashtable-set! dict k v))
              (map car alist) (map cdr alist))
    dict))

(define (parse-rule line)
  (cons* (substring line 0 2) (string-ref line 6)))

(define-values (base-formula rules)
  (let ([lines (readlines "14.input")])
    (values (car lines)
            (build-dict (map parse-rule (cddr lines))))))

;; part 1
(define (expand-polymer polymer)
  (let* ([positions (iota (- (string-length polymer) 1))]
         [pairs (map (λ (i) (substring polymer i (+ i 2))) positions)]
         [insertions (map (λ (pair) (hashtable-ref rules pair #f)) pairs)])
    (list->string
     (reverse
      (fold-left
       (λ (polymer pair c)
         (append! (list (string-ref pair 1))
                  (if c (list c) '())
                  polymer))
       (list (string-ref polymer 0)) pairs insertions)))))

(define (character-counts polymer)
  (let ([counts (make-hashtable char->integer char=?)])
    (for-each
     (λ (char) (hashtable-update! counts char fx1+ 0))
     (string->list polymer))
    (sort (λ (a b) (< (cdr a) (cdr b)))
          (map (λ (x) (cons x (hashtable-ref counts x #f)))
               (vector->list (hashtable-keys counts))))))

(let* ([steps 10]
       [polymer (fold-left (λ (polymer _) (expand-polymer polymer))
                           base-formula (iota steps))]
       [counts (character-counts polymer)])
  (pretty-print (- (cdar (last-pair counts))
                   (cdar counts))))

;; part 2
(define (better-expand-polymer polymer steps)
  (let ([pairs (make-hashtable string-hash string=?)]
        [counts (make-hashtable char->integer char=?)])
    (for-each
     (λ (i) (let ([pair (substring polymer i (+ i 2))])
              (hashtable-update! pairs pair fx1+ 0)))
     (iota (- (string-length polymer) 1)))
    (for-each (λ (char) (hashtable-update! counts char fx1+ 0))
              (string->list polymer))
    (fold-left
     (λ (pairs i)
       (let ([new-pairs (make-hashtable string-hash string=?)])
         (for-each
          (λ (pair)
            (let* ([count (hashtable-ref pairs pair #f)]
                   [char (hashtable-ref rules pair #f)]
                   [A (string (string-ref pair 0) char)]
                   [B (string char (string-ref pair 1))]
                   [add-count (λ (n) (+ n count))])
              (hashtable-update! new-pairs A add-count 0)
              (hashtable-update! new-pairs B add-count 0)
              (hashtable-update! counts char add-count 0)))
          (vector->list (hashtable-keys pairs)))
         new-pairs))
     pairs (iota steps))
    counts))

(let* ([steps 40]
       [counts (better-expand-polymer base-formula steps)]
       [counts (sort (λ (a b) (< (cdr a) (cdr b)))
                     (map (λ (x) (cons x (hashtable-ref counts x #f)))
                          (vector->list (hashtable-keys counts))))])
  (pretty-print (- (cdar (last-pair counts))
                   (cdar counts))))
