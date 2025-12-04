#!chezscheme

(library (vec2)
  (export vec2 vec2? vec2-x vec2-y vec2+ vec2- vec2* vec2-div vec2=?
          vec2-id vec2-hash)
  (import (scheme))

  (define (vec2 a b) (fxvector a b))
  (define (vec2? v) (and (fxvector? v) (= 2 (fxvector-length v))))

  (define (vec2-x v) (fxvector-ref v 0))
  (define (vec2-y v) (fxvector-ref v 1))

  (define (vec2+ a b) (vec2 (fx+ (vec2-x a) (vec2-x b))
                            (fx+ (vec2-y a) (vec2-y b))))
  (define (vec2- a b) (vec2 (fx- (vec2-x a) (vec2-x b))
                            (fx- (vec2-y a) (vec2-y b))))
  (define (vec2* v c) (vec2 (fx* (vec2-x v) c) (fx* (vec2-y v) c)))
  (define (vec2-div v c) (vec2 (fxdiv (vec2-x v) c) (fxdiv (vec2-y v) c)))
  (define (vec2=? a b) (and (fx= (vec2-x a) (vec2-x b))
                            (fx= (vec2-y a) (vec2-y b))))

  (define (vec2-id v)
    (fx+ (fxsll (fx+ 32768 (vec2-x v)) 16) (vec2-y v)))

  (define (vec2-hash seed v)
    (let* ([m #x5bd1e995] [r 24]
           [hash
            (lambda (h x)
              (let* ([k (bitwise-and (* x m) #xffffffff)]
                     [k (bitwise-xor k (bitwise-arithmetic-shift-right k r))]
                     [h (bitwise-xor (* h m) (* k m))])
                (bitwise-and h #xffffffff)))]
           [h (hash (hash seed (vec2-x v)) (vec2-y v))]
           [h (bitwise-xor h (bitwise-arithmetic-shift-right h 13))]
           [h (bitwise-and (* h m) #xffffffff)])
      (bitwise-xor h (bitwise-arithmetic-shift-right h 15)))))

(library (vec2f)
  (export vec2f vec2f? vec2f-x vec2f-y vec2f+ vec2f-
          vec2f* vec2f=? vec2->vec2f vec2f->vec2
          vec2f-length-squared vec2f-normalize vec2f-dot)
  (import (scheme) (vec2))
  (define (fl->round-fx x) (flonum->fixnum (flround x)))

  (define-values (vec2f-record vec2f vec2f-x vec2f-y vec2f?)
    (let () (define-record-type vec2f (fields x y))
         (values (record-type-descriptor vec2f)
                 make-vec2f vec2f-x vec2f-y vec2f?)))

  (define (vec2f+ a b) (vec2f (fl+ (vec2f-x a) (vec2f-x b))
                              (fl+ (vec2f-y a) (vec2f-y b))))
  (define (vec2f- a b) (vec2f (fl- (vec2f-x a) (vec2f-x b))
                              (fl- (vec2f-y a) (vec2f-y b))))
  (define (vec2f* v c) (vec2f (fl* (vec2f-x v) c) (fl* (vec2f-y v) c)))
  (define (vec2f=? a b) (and (fl= (vec2f-x a) (vec2f-x b))
                             (fl= (vec2f-y a) (vec2f-y b))))

  (define (vec2->vec2f v) (vec2f (fixnum->flonum (vec2-x v))
                                 (fixnum->flonum (vec2-y v))))
  (define (vec2f->vec2 v) (vec2 (fl->round-fx (vec2f-x v))
                                (fl->round-fx (vec2f-y v))))
  (define (vec2f-length-squared v) (fl+ (fl* (vec2f-x v) (vec2f-x v))
                                        (fl* (vec2f-y v) (vec2f-y v))))
  (define (vec2f-normalize v)
    (vec2f* v (/ 1. (sqrt (vec2f-length-squared v)))))
  (define (vec2f-dot a b) (fl+ (fl* (vec2f-x a) (vec2f-x b))
                               (fl* (vec2f-y a) (vec2f-y b))))

  (record-type-equal-procedure vec2f-record
                               (lambda (a b eql?) (vec2f=? a b))))
