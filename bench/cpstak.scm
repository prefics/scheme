;;; cpstak.scm -- cpstak benchmark from Gabriel

(define (cpstak x y z)
  (letrec ((tak (lambda (x y z k)
		  (if (not (< y x))
		      (k z)
		      (tak (- x 1)
			   y
			   z
			   (lambda (v1)
			     (tak (- y 1)
				  z
				  x
				  (lambda (v2)
				    (tak (- z 1)
					 x
					 y
					 (lambda (v3)
					   (tak v1 v2 v3 k)))))))))))
    (tak x y z (lambda (a) a))))

(define (benchmark)
  (let loop ((n 32))
    (if (zero? n)
	n
	(begin
	  (cpstak 18 12 6)
	  (loop (- n 1))))))

 
