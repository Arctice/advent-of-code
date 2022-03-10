(import (core))

(define (to-bitfield hex)
  (define (hex-value digit)
    (string->number (string digit) 16))
  (set! hex (string-append hex "0"))
  (let ([field (make-bytevector
                (fxdiv (+ (string-length hex) 1) 2) 0)])
    (for-each
     (λ (i) (let ([byte (fxdiv i 2)] [lower (fxmod i 2)]
                  [value (string->number (substring hex i (+ i 1)) 16)])
              (when value
                (bytevector-u8-set!
                 field byte
                 (fxior (fxsll value (if (zero? lower) 4 0))
                        (bytevector-u8-ref field byte))))))
     (iota (string-length hex)))
    field))

(define (bitfield-read field start end)
  (let* ([first-byte (fxdiv start 8)] [last-byte (fxdiv end 8)]
         [start-offset (fxmod start 8)] [end-offset (fxmod end 8)]
         [first (fxand (bytevector-u8-ref field first-byte)
                       (fxsrl #xFF start-offset))])
    (cond
     [(= first-byte last-byte)
      (fxsrl first (- 8 end-offset))]
     [(= 1 (- last-byte first-byte))
      (let* ([last (bytevector-u8-ref field last-byte)])
        (fxsrl (fxior (fxsll first 8) last)
               (- 8 end-offset)))]
     [else
      (let* ([width (- last-byte first-byte)]
             [xs (map
                  (λ (n)
                    (bitwise-arithmetic-shift-left
                     (bitwise-and
                      (if (not (zero? n)) #xFF
                          (fxsrl #xff start-offset))
                      (bytevector-u8-ref field (+ n first-byte)))
                     (* 8 (- width n))))
                  (iota (+ 1 width)))]
             [val (fold-left bitwise-ior 0 xs)])
        (bitwise-arithmetic-shift-right val (- 8 end-offset)))])))

(define (read-next-packet msg at)
  (let* ([first-bit at]
         [version (bitfield-read msg at (+ at 3))]
         [id (bitfield-read msg (+ at 3) (+ at 6))])
    (case id
      [4 (let ([offset (+ at 6)])
           (let read-literal ([position 0] [value 0])
             (let* ([block (+ offset (* position 5))]
                    [continue (bitfield-read msg block (+ 1 block))]
                    [digit (bitfield-read msg (+ 1 block) (+ 5 block))]
                    [last-bit (+ block 5)]
                    [value (fxior (fxsll value 4) digit)])
               (if (zero? continue)
                   (values (- last-bit first-bit)
                           (cons* (cons* version id) value))
                   (read-literal (+ 1 position) value)))))]
      [else
       (let ([length-type (bitfield-read msg (+ at 6) (+ at 7))]
             [at (+ at 7)]
             [op (cons* version id)])
         (case length-type
           [0 (let* ([size (bitfield-read msg at (+ at 15))]
                     [at (+ at 15)]
                     [subpackets
                      (let subpacket ([offset 0])
                        (let-values ([(advance packet)
                                      (read-next-packet msg (+ at offset))])
                          (let ([offset (+ offset advance)])
                            (cons packet
                                  (if (>= offset size) '()
                                      (subpacket offset))))))])
                (values (- (+ at size) first-bit)
                        (cons op subpackets)))]
           [1 (let* ([count (bitfield-read msg at (+ at 11))]
                     [at (+ at 11)]
                     [last-bit 0]
                     [subpackets
                      (let subpacket ([offset 0] [n 0])
                        (if (>= n count) '()
                            (let-values ([(advance packet)
                                          (read-next-packet msg (+ at offset))])
                              (let ([offset (+ offset advance)])
                                (set! last-bit (+ at offset))
                                (cons packet (subpacket offset (+ 1 n)))))))])
                (values (- last-bit first-bit)
                        (cons* op subpackets)))]))])))


;; part 1
(define (version-sum message)
  (define (version-sum packet)
    (let* ([version (caar packet)]
           [id (cdar packet)])
      (+ version
         (case id [4 0]
               [else (fold-left
                      + 0 (map version-sum (cdr packet)))]))))
  (let-values ([(size data) (read-next-packet (to-bitfield message) 0)])
    (version-sum data)))

(assert (= 6 (version-sum "D2FE28")))
(assert (= 9 (version-sum "38006F45291200")))
(assert (= 14 (version-sum "EE00D40C823060")))
(assert (= 16 (version-sum "8A004A801A8002F478")))
(assert (= 12 (version-sum "620080001611562C8802118E34")))
(assert (= 23 (version-sum "C0015000016115A2E0802F182340")))
(assert (= 31 (version-sum "A0016C880162017C3686B18A3D4780")))

(define input (slurp "16.input"))
(printf "~s\n" (version-sum input))

;; part 2
(define (eval node)
  (let* ([header (car node)]
         [version (car header)] [id (cdr header)]
         [values (cdr node)])
    (case id
      [0 (apply + (map eval values))]
      [1 (apply * (map eval values))]
      [2 (apply min (map eval values))]
      [3 (apply max (map eval values))]
      [4 values]
      [5 (let ([a (eval (car values))] [b (eval (cadr values))])
           (if (> a b) 1 0))]
      [6 (let ([a (eval (car values))] [b (eval (cadr values))])
           (if (< a b) 1 0))]
      [7 (let ([a (eval (car values))] [b (eval (cadr values))])
           (if (= a b) 1 0))])))

(define (eval-message msg)
  (let-values ([(_ data) (read-next-packet (to-bitfield msg) 0)])
    (time (eval data))))

(assert (= 3 (eval-message "C200B40A82")))
(assert (= 54 (eval-message "04005AC33890")))
(assert (= 7 (eval-message "880086C3E88112")))
(assert (= 9 (eval-message "CE00C43D881120")))
(assert (= 1 (eval-message "D8005AC2A8F0")))
(assert (= 0 (eval-message "F600BC2D8F")))
(assert (= 0 (eval-message "9C005AC2F8F0")))
(assert (= 1 (eval-message "9C0141080250320F1802104A08")))

(printf "~s\n" (eval-message input))
