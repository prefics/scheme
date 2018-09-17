;;; dderiv.scm -- dderiv benchmark from Gabriel

(define pg-alist '())
(define (put sym d what)
  (set! pg-alist (cons (cons sym what) pg-alist)))
(define (get sym d)
  (cdr (assq sym pg-alist)))

(define (dderiv-aux a)
  (list '/ (dderiv a) a))
 
(define (f+dderiv a)
  (cons '+ (map dderiv a)))
 
(define (f-dderiv a)
  (cons '- (map dderiv a)))
 
(define (*dderiv a)
  (list '* (cons '* a)
        (cons '+ (map dderiv-aux a))))
 
(define (/dderiv a)
  (list '-
        (list '/
              (dderiv (car a))
              (cadr a))
        (list '/
              (car a)
              (list '*
                    (cadr a)
                    (cadr a)
                    (dderiv (cadr a))))))
 
(put '+ 'dderiv f+dderiv)    ; install procedure on the property list
 
(put '- 'dderiv f-dderiv)    ; install procedure on the property list
 
(put '* 'dderiv *dderiv)    ; install procedure on the property list
 
(put '/ 'dderiv /dderiv)    ; install procedure on the property list
 
(define (dderiv a)
  (cond
    ((not (pair? a))
     (cond ((eq? a 'x) 1) (else 0)))
    (else (let ((dderiv (get (car a) 'dderiv)))
         (cond (dderiv (dderiv (cdr a)))
               (else 'error))))))
 
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
 
