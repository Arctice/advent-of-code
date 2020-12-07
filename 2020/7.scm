#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (chezscheme) (core))

(define (parse-rule line)
  (let* ([line (string-split
                (substring line 0 (dec (string-length line)))
                " contain ")]
         [name (head line)]
         [contents
          (if (string=? (cadr line) "no other bags") '()
              (string-split (cadr line) ", "))]
         [contents
          (map (λ child ->
                  (let* ([amount (head (string-split child " "))]
                         [name-start (inc (string-length amount))]
                         [name (substring child name-start
                                          (string-length child))]
                         [suffix (if (string=? "1" amount) "s" "")])
                    (pair (string->number amount)
                          (string-append name suffix))))
               contents)])
    (list name contents)))


;; preprocessing
(define name-mapping
  (let ([names (make-hashtable string-hash string=?)])
    (λ name -> (if (hashtable-contains? names name)
                   (hashtable-ref names name 0)
                   (let ([id (hashtable-size names)])
                     (hashtable-set! names name id) id)))))

(define rules (make-eq-hashtable))
(define reverse-rules (make-eq-hashtable))

(define (add-rule name content)
  (let ([parent-id (name-mapping name)]
        [content
         (map (λ v -> (pair (head v) (name-mapping (tail v))))
              content)])
    (hashtable-set! rules parent-id content)
    (for-each (λ child ->
                 (let ([child-id (tail child)]
                       [count (head child)])
                   (hashtable-update!
                    reverse-rules child-id
                    (λ containers -> (cons (pair count parent-id)
                                           containers))
                    '())))
              content)))

(map (partial apply add-rule) (map parse-rule (readlines "7.input")))


;; part 1
(define (parents-of item) (hashtable-ref reverse-rules item '()))

(define (recursive-parents item)
  (define seen (make-vector (name-mapping "sentinel") #f))
  (define (first-seen v)
    (if (vector-ref seen v) #f (vector-set! seen v #t)))
  (let search ([pending (list item)])
    (if (null? pending)
        (length (filter id (vector->list seen)))
        (let* ([next (head pending)]
               [children (map tail (parents-of next))]
               [new (filter first-seen children)])
          (search (append! new (tail pending)))))))

(pretty-print (recursive-parents (name-mapping "shiny gold bags")))


;; part 2
(define (contents-of item)
  (map (λ v -> (pair (* (head item) (head v)) (tail v)))
       (hashtable-ref rules (tail item) '())))

(define (recursive-contents item)
  (let search ([pending (list item)]
               [bag-count 0])
    (if (null? pending) bag-count
        (let* ([next (head pending)]
               [children (contents-of next)]
               [new-bags (sum (map head children))])
          (search (append! children (tail pending))
                  (+ bag-count new-bags))))))

(pretty-print (recursive-contents (pair 1 (name-mapping "shiny gold bags"))))

