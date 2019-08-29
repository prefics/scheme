
(define (dotimes n proc) (if (> n 0) (begin (proc) (dotimes (- n 1) proc))))

(define (bench-class-of)
  (dotimes 1000000 (lambda () (class-of "hello") (class-of 3) (class-of 2.0))))

