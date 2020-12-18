#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))

;; 0,3,1,6,7,5

(define (z n)
  (let ([positions (make-vector (max 100 n) #f)])
    (vector-set! positions 0 0)
    (vector-set! positions 3 1)
    (vector-set! positions 1 2)
    (vector-set! positions 6 3)
    (vector-set! positions 7 4)
    (let loop ([prev 5] [i 6])
      (let* ([rpos (vector-ref positions prev)]
             [distance (if rpos (- i rpos 1) 0)])
        (vector-set! positions prev (dec i))

        (if (= i n)
            distance
            (loop distance (inc i)))))))

(pretty-print (z 2019))
(pretty-print (z 29999999))
