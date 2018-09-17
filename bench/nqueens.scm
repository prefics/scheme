;;; nqueens.scm -- nqueen benchmark from Gabriel suite

(define trace? #f)

(define (nqueens n)
  (letrec ((one-to
	    (lambda (n)
	      (let loop ((i n) (l '()))
		(if (= i 0) l (loop (- i 1) (cons i l))))))
	   
	   (try-it
	    (lambda (x y z)
	       (if (null? x)
		   (if (null? y)
		       (begin
			 (if trace? (begin (write z) (newline)))
			 1)
		       0)
		   (+ (if (ok? (car x) 1 z)
			  (try-it (append (cdr x) y) '() (cons (car x) z))
			  0)
		      (try-it (cdr x) (cons (car x) y) z)))))

	   (ok?
	    (lambda (row dist placed)
	      (if (null? placed)
		  #t
		  (and (not (= (car placed) (+ row dist)))
		       (not (= (car placed) (- row dist)))
		       (ok? row (+ dist 1) (cdr placed)))))))
    (try-it (one-to n) '() '())))

(define (with-benchmark thunk)
  (let ((write-time
	 (lambda (t)
	   (display (+ (* 1000000 (bvec-ref (car t) 0)) (cdr t)))
	   (newline))))		      
    (let ((start (time)))
      (thunk)
      (let ((end (time)))
	(write-time (time- end start))))))

(define (benchmark)
  (with-benchmark
   (lambda ()
     (let loop ((n 500) (v 0))
       (if (zero? n)
	   v
	   (loop (- n 1) (nqueens 8)))))))
