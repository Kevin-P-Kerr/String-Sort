#lang racket

(define (filter predicate sequence) 
   (cond ((null? sequence) '()) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence))))) 

(define (filter-less-car alist)
  (let [(a (car alist))]
    (local [(define (lessa? n)
      (< n a))]
     (filter lessa? alist))))

(define (filter-greater-car alist)
  (let [(a (car alist))]
    (local [(define (greata? n)
              (> n a))]
      (filter greata? alist))))


(define (find-least alist)
  (local [(define (find-least-prep xlist blist)
          (if (null? xlist) blist
              (find-least-prep (filter-less-car xlist)(car xlist))))]
    (find-least-prep alist alist)))

(define (find-greatest alist)
  (local [(define (find-greatest-prep xlist blist)
            (if (null? xlist) blist
                (find-greatest-prep (filter-greater-car xlist)(car xlist))))]
    (find-greatest-prep alist alist)))

(define (enumerate low-range high-range)
  (if (> low-range high-range)'()
      (cons low-range (enumerate (+ 1 low-range) high-range))))

(define (reverse-enumerate high-range low-range)
  (if (< high-range low-range)'()
      (cons high-range (reverse-enumerate (- high-range 1) low-range))))

(define (check n alist)
        (cond ((null? alist) alist)
              ((= n (car alist)) alist)
              (else (check n (cdr alist)))))

(define (in? n alist)
  (if (null? (check n alist)) '#f '#t))

(define (least-sort alist)
  (let [(a (find-least alist))
        (b (find-greatest alist))]
    (local [(define blist (enumerate a b))
            (define (in-helper n)
              (in? n alist))]
      (filter in-helper blist))))


(define (greatest-sort alist)
  (let [(a (find-greatest alist))
        (b (find-least alist))]
    (local [(define blist (reverse-enumerate a b))
            (define (in-helper n)
              (in? n alist))]
      (filter in-helper blist))))

(provide greatest-sort)

