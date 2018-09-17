;;; ctak.scm -- ctak benchmark from Gabriel suite

(define (ctak x y z)
  (call-with-current-continuation
   (lambda (k)
     (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (cond ((not (< y x))  ;xy
         (k z))
        (else (call-with-current-continuation
               (ctak-aux
                k
                (call-with-current-continuation
                 (lambda (k)
                   (ctak-aux k
                             (- x 1)
                             y
                             z)))
                (call-with-current-continuation
                 (lambda (k)
                   (ctak-aux k
                             (- y 1)
                             z
                             x)))
                (call-with-current-continuation
                 (lambda (k)
                   (ctak-aux k
                             (- z 1)
                             x
                             y))))))))

(define (benchmark)
  (let loop ((n 8) (v 0))
    (if (zero? n)
	v
	(loop (- n 1)
	      (ctak 18 12 6)))))
