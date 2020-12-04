#!/usr/bin/env -S scheme --libdirs "../scheme/" --script
(import (scheme) (core))

(define (parse-passport ss)
  (let* ([ss (string-replace (string-strip ss) "\n" " ")]
         [fields (string-split ss " ")])
    (map (λ entry -> (apply (λ name data -> (pair (string->symbol name) data))
                            (string-split entry ":")))
         fields)))

(define passports
  (let ([input (slurp "4.input")])
    (map parse-passport (string-split input "\n\n"))))


;; part1
(define (valid-fields? passport)
  (for-all (λ required -> (assoc required passport))
          '(byr iyr eyr hgt hcl ecl pid)))

(define valid-passports (filter valid-fields? passports))
(pretty-print (length valid-passports))


;; part2
(define (strict-passport? fields)
  (define (validate field)
    (let ([name (head field)]
          [v (tail field)])
      (case name
        ['byr (let ([x (string->number v)]) (and (>= x 1920) (<= x 2002)))]
        ['iyr (let ([x (string->number v)]) (and (>= x 2010) (<= x 2020)))]
        ['eyr (let ([x (string->number v)]) (and (>= x 2020) (<= x 2030)))]
        ['hgt (let* ([len (string-length v)]
                     [suffix (substring v (- len 2) len)]
                     [x (string->number (substring v 0 (- len 2)))])
                (case suffix
                  ["cm" (and x (>= x 150) (<= x 193))]
                  ["in" (and x (>= x 59) (<= x 76))]
                  [else #f]))]
        ['hcl (and (= (string-length v) 7)
                   (eq? (string-ref v 0) #\#)
                   (string->number (substring v 1 7) 16))]
        ['ecl (member v '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))]
        ['pid (and (= 9 (string-length v)) (string->number v))]
        ['cid #t])))
  (for-all validate fields))

(define strict-passports (filter strict-passport? valid-passports))
(pretty-print (length strict-passports))
