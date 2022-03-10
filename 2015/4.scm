
(define libssl-md5
  (begin (load-shared-object "libcrypto.so")
         (foreign-procedure "MD5" (string unsigned-32 u8*) uptr)))

(define (md5 ss)
  (let ([digest (make-bytevector 16)])
    (libssl-md5 ss (string-length ss) digest)
    (bytevector->u8-list digest)))

(define (hex-digits xs)
  (fold-right append! '()
              (map (lambda (x) (list (fxsrl x 4) (fxand x 15))) xs)))

(define (zero-prefix-md5 word k)
  (let search ([n 0])
    (let* ([ss (string-append word (format "~s" n))]
           [md5 (list-head (md5 ss) (fxdiv (+ 1 k) 2))]
           [hex (hex-digits md5)])
      (if (for-all zero? (list-head hex k)) n
          (search (+ n 1))))))

(define word "bgvyzdsv")
(printf "~s\n" (zero-prefix-md5 word 5))
(printf "~s\n" (zero-prefix-md5 word 6))
