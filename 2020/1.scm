#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))

(define xs  (map string->number (readlines "1.input")))
(define ys (make-bytevector 2020 0))
(for-each (lambda (x) (bytevector-u8-set! ys x 1)) xs)

(for-each
 (lambda (x)
   (let ([y (- 2020 x)])
     (when (= 1 (bytevector-u8-ref ys y))
       (display (* x y)) (newline))))
 xs)

(time
 (let loop ([x (car xs)] [rest (cdr xs)])
   (for-each
    (lambda (y)
      (let ([z (fx- 2020 x y)])
        (when (and
               (fx< 0 z)
               (= 1 (bytevector-u8-ref ys z)))
          (display (fx* x y z)) (newline)))
      )
    rest)
   (when (not (null? (cdr rest)))
     (loop (car rest) (cdr rest))))
 )

