;; alternate definitions for greatest sort and least sort

(define (greatest-sort alist)
	(define (swap-pass alist)
		(if (= 1 (length alist)) alist
			(let ((fst (car alist)) (scnd (cadr alist)) (rest (cddr alist)))
			(if (< fst scnd) (cons scnd (swap-pass (cons fst rest)))
			    (cons fst (swap-pass (cons scnd rest)))))))
	(letrec ([loop (lambda (counter value)
				(if (= counter 1) value
					(loop (- counter 1) (swap-pass value))))])
		(loop (length alist) alist)))

(define (least-sort alist)
	(define (swap-pass alist) 
		(if (= 1 (length alist)) alist
			(let ((fst (car alist)) (scnd (cadr alist)) (rest (cddr alist)))
		(if (> fst scnd) (cons scnd (swap-pass (cons fst rest)))
		   (cons fst (swap-pass (cons scnd rest)))))))
	(letrec ([loop (lambda (counter value)
			(if (= counter 1) value
				(loop (- counter 1) (swap-pass value))))])
	(loop (length alist) alist)))

(provide greatest-sort least-sort)


