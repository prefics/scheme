;;; random.scm -- random number generation

(define random
  (let ((last 1))
    (lambda (n)
      (let ((new (modulo (* last 1277) 131072)))
        (set! last new)
        (modulo new n)))))

