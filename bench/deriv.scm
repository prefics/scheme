(define (deriv-aux a) (list '/ (deriv a) a))
 
(define (deriv a)
  (cond
    ((not (pair? a))
     (cond ((eq? a 'x) 1) (else 0)))
    ((eq? (car a) '+)
     (cons '+ (map deriv (cdr a))))
    ((eq? (car a) '-)
     (cons '- (map deriv
                      (cdr a))))
    ((eq? (car a) '*)
     (list '*
           a
           (cons '+ (map deriv-aux (cdr a)))))
    ((eq? (car a) '/)
     (list '-
           (list '/
                 (deriv (cadr a))
                 (caddr a))
           (list '/
                 (cadr a)
                 (list '*
                       (caddr a)
                       (caddr a)
                       (deriv (caddr a))))))
    (else 'error)))
 
(define (benchmark)
  (let loop ((n 50000))
    (if (zero? n)
	n
	(begin
	  (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
	  (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
	  (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
	  (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
	  (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
	  (loop (- n 1))))))
 
