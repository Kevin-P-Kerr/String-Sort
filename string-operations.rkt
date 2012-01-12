#lang racket 
(require "list-operations.rkt")

(define (manip-string s)
  (local [(define (aux n)
            (if (or (= n (string-length s))
                    (string=? (string #\space) (string (string-ref s n)))) 
                    (substring s 0 n)  
                    (aux (+ n 1))))]
    (aux 0)))

(define (string-to-stringlist s)
  (local [(define (aux astring n)
            (if (or (= n 
                       (string-length astring))
                    (> n
                       (string-length astring)))'()
                    (let [(b (string-length (manip-string (substring astring n))))]
                      (cons (manip-string (substring astring n))
                            (aux astring (+ n (+ b 1)))))))]
    (aux s 0)))

(define (convert-list alist)
(local [(define (convert-listelement-string n)
  (string-append (number->string n)))]
  (map convert-listelement-string alist)))

(define (string-to-numlist s)
  (let [(alist (string-to-stringlist s))]
     (map string->number alist)))

(define (string-great-sort s)
  (let [(alist (string-to-numlist s))]
    (convert-list (greatest-sort alist))))

(define (string-least-sort s)
  (let [(alist (string-to-numlist s))]
    (convert-list (least-sort alist))))


(provide string-great-sort string-least-sort)

