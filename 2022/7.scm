#!chezscheme
(import (core))

(define input
  (map (λ (line)
         (let ([tokens (string-split " " line)])
           (if (equal? "$" (car tokens))
               (cons (string->symbol (cadr tokens)) (cddr tokens))
               (cons (or (string->number (car tokens))
                         (string->symbol (car tokens)))
                     (cdr tokens)))))
       (readlines "7.input")))

(define-record-type dir (fields parent (mutable children)))
(define (get-file dir name)
  (cdr (assoc name (dir-children dir))))
(define (add-file! dir name f)
  (dir-children-set!
   dir (cons (cons* name f) (dir-children dir))))

(define (build-dir-tree input)
  (define root (make-dir #f '()))
  (let next ([cd #f] [lines input])
    (if (null? lines) root
        (let ([line (car lines)] [lines (cdr lines)])
          (cond
           [(eq? 'cd (car line))
            (let ([name (cadr line)])
              (set! cd (case name
                         ["/" root]
                         [".." (dir-parent cd)]
                         [else (get-file cd name)])))]
           [(eq? 'dir (car line))
            (add-file! cd (cadr line) (make-dir cd '()))]
           [(number? (car line))
            (let ([size (car line)] [name (cadr line)])
              (add-file! cd name (cons* size name)))])
          (next cd lines)))))

(define (directory-sizes tree)
  (define all-sizes '())
  (define (traverse file)
    (if (not (dir? file)) (car file)
        (let* ([sizes (map (λ (link) (traverse (cdr link)))
                           (dir-children file))]
               [total (fold-left + 0 sizes)])
          (set! all-sizes (cons total all-sizes)) total)))
  (traverse tree) all-sizes)

(define (part-1 dir-sizes)
  (fold-left + 0 (filter (λ (size) (<= size 100000)) dir-sizes)))

(define (part-2 dir-sizes)
  (let* ([free-space (- 70000000 (apply max dir-sizes))]
         [needed (- 30000000 free-space)])
    (apply min (filter (λ (size) (<= needed size)) dir-sizes))))

(let* ([sizes (directory-sizes (build-dir-tree input))])
  (printf "part 1: ~s\n" (part-1 sizes))
  (printf "part 2: ~s\n" (part-2 sizes)))
