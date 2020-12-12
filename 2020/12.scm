#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))

(define (parse line)
  (let* ([instruction (string-ref line 0)]
         [value (string->number (substring line 1 (string-length line)))]
         [value (case instruction
                  [[#\L #\R] (/ value 90)]
                  [else value])])
    (pair instruction value)))

(define instructions (map parse (readlines "12.input")))
;; (define instructions (map parse (list "F10" "N3" "F7" "R90" "F11")))

;; part 1
(define (move-ship instruction facing ship-x ship-y)
  (let ([action (head instruction)]
        [value (tail instruction)])
    (case action
      [#\N (values facing ship-x (- ship-y value))]
      [#\S (values facing ship-x (+ ship-y value))]
      [#\E (values facing (+ ship-x value) ship-y)]
      [#\W (values facing (- ship-x value) ship-y)]
      [#\L (values (mod (+ facing 4 (- value)) 4) ship-x ship-y)]
      [#\R (values (mod (+ facing 4 value)     4) ship-x ship-y)]
      [#\F (move-ship
            (pair (case facing [0 #\E] [1 #\S] [2 #\W] [3 #\N]) value)
            facing ship-x ship-y)])))

(define (execute-part-1 instructions)
  (let move ([facing 0]
             [ship-x 0]
             [ship-y 0]
             [next (head instructions)]
             [rest (tail instructions)])
    (call-with-values (λ -> (move-ship next facing ship-x ship-y))
      (λ facing ship-x ship-y ->
         (if (null? rest)
             (list ship-x ship-y)
             (move facing ship-x ship-y (head rest) (tail rest)))
         ))))

(define (manhattan-distance x y) (+ (abs x) (abs y)))

(apply
 (λ ship-x ship-y ->
    (let ([distance (manhattan-distance ship-x ship-y)])
      (printf "~s\n" distance)))
 (execute-part-1 instructions))


;; part 2

(define (turn-waypoint value x y)
  (define (turn-once x y) (list (- y) x))
  (fold-left (λ position _ -> (apply (λ x y -> (turn-once x y))
                       position))
             (list x y) (iota value)))

(define (move-waypoint instruction wp-x wp-y)
  (let ([action (head instruction)]
        [value (tail instruction)])
    (case action
      [#\N (values wp-x (- wp-y value))]
      [#\S (values wp-x (+ wp-y value))]
      [#\E (values (+ wp-x value) wp-y)]
      [#\W (values (- wp-x value) wp-y)]
      [#\L (apply values (turn-waypoint (- 4 value) wp-x wp-y))]
      [#\R (apply values (turn-waypoint value wp-x wp-y))])))

(define (execute-part-2 instructions)
  (let ([ship-x 0] [ship-y 0]
        [wp-x 10] [wp-y -1])
    (for-each
     (λ instruction ->
        (let ([action (head instruction)]
              [value (tail instruction)])
          (if (eq? #\F action)
              (begin (set! ship-x (+ ship-x (* wp-x value)))
                     (set! ship-y (+ ship-y (* wp-y value))))
              (call-with-values
                  (λ -> (move-waypoint instruction wp-x wp-y))
                (λ x y -> (begin (set! wp-x x) (set! wp-y y)))))))
     instructions)
    (list ship-x ship-y)))

;; part 2
(apply
 (λ ship-x ship-y ->
    (let ([distance (manhattan-distance ship-x ship-y)])
      (printf "~s\n" distance)))
 (execute-part-2 instructions))


