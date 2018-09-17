;;; number.scm -- numerical routine 

(define (number? obj) (or (%fixnum? obj) (%real? obj)))
(define (complex? o) #f)
(define (real? o) (%real? o))
(define (rational? o) #f)
(define (integer? o) (%fixnum? o))

(define (exact? o) (integer? o))
(define (inexact? o) (real? o))

; (define (= z1 z2 ...))
; (define (< x1 x2 ...))
; (define (> x1 x2 ...))
; (define (<= x1 x2 ...))
; (define (>= x1 x2 ...))

(define (zero? z) (= z 0))
(define (positive? x) (>= x 0))
(define (negative? x) (< x 0))
(define (odd? n) (= (remainder n 2) 1))
(define (even? n) (= (remainder n 2) 0))
;;; MIN & MAX allows more than one argument
(define (max x1 x2) (if (< x1 x2) x2 x1))
(define (min x1 x2) (if (< x1 x2) x1 x2))

; (define (+ z1 ...))
; (define (* z1 ...))
; (define (- z1 z2))
; (define (/ z1 z2 ...))

(define (abs x) (if (negative? x) (- 0 x) x))
; (define (quotient n1 n2))
; (define (remainder n1 n2))
; (define (modulo n1 n2))

(define (gcd/2 a b)
  (if (zero? b)
      (if (and (inexact? b) (exact? a))
          (exact->inexact (abs a))
          (abs a))
      (gcd/2 b (remainder a b))))

(define (gcd . n)
  (if (null? n)
      0
      (let repeat ((a (car n))
                   (b (cdr n)))
        (if (null? b)
            a
            (repeat (gcd/2 a (car b)) (cdr b))))))

(define (lcm/2 a b) (/ (abs (* a b)) (gcd/2 a b)))

(define (lcm . n)
  (if (null? n)
      1
      (let repeat ((a (car n)) (b (cdr n)))
        (if (null? b)
            a
            (repeat (lcm/2 a (car b)) (cdr b))))))

(define (numerator q) (%numerator q))
(define (denominator q) (%denominator q))

(define (floor x)
  (cond ((%real? x) (%floor x))
        ((fixnum? x) x)
        (else (error "FLOOR expects a number, got ~a" x))))

(define (ceiling x)
  (cond ((%real? x) (%ceiling x))
        ((fixnum? x) x)
        (else (error "CEILING expects a number, got ~a" x))))

(define (truncate x)
  (cond ((%real? x) (%truncate x))
        ((%fixnum? x) x)
        (else (error "TRUNCATE expects a number, got ~a" x))))

(define (round x)
  (cond ((%real? x) (%round x))
        ((%fixnum? x) (%round x))
        (else (error "ROUND expects a number, got ~a" x))))

(define (rationalize x y) (error "Unimplemented RATIONALIZE"))

(define (exp z)
  (cond ((%real? z) (%exp z))
        ((%fixnum? z) (%exp (%fixnum->real z)))
        (else (error "EXP expects a number, got ~a" z))))

(define (log z)
  (cond ((%real? z) (%log z))
        ((%fixnum? z) (%log (%fixnum->real z)))
        (else (error "LOG expects a number, got ~a" z))))

(define (sin z)
  (cond ((%real? z) (%sin z))
        ((%fixnum? z) (%sin (%fixnum->real z)))
        (else (error "SIN expects a number, got ~a" z))))

(define (cos z)
  (cond ((%real? z) (%cos z))
        ((%fixnum? z) (%cos (%fixnum->real z)))
        (else (error "COS expects a number, got ~a" z))))

(define (tan z)
  (cond ((%real? z) (%tan z))
        ((%fixnum? z) (%tan (%fixnum->real z)))
        (else (error "TAN expects a number, got ~a" z))))

(define (asin z)
  (cond ((%real? z) (%asin z))
        ((%fixnum? z) (%asin (%fixnum->real z)))
        (else (error "ASIN expects a number, got ~a" z))))

(define (acos z)
  (cond ((%real? z) (%acos z))
        ((%fixnum? z) (%acos (%fixnum->real z)))
        (else (error "ACOS expects a number, got ~a" z))))

;(define (atan z) (%atan z))
;(define (atan y x))

(define (sqrt z)
(cond ((%real? z) (%sqrt z))
      ((%fixnum? z) (%sqrt (%fixnum->real z)))
      (else (error "SQRT expects a number, got ~a" z))))

(define (expt z1 z2)
  (cond ((%real? z1)
         (cond ((%real? z2) (%expt z1 z2))
               ((%fixnum? z2) (%expt z1 (%fixnum->real z2)))
               (else (error "EXPT expects a number as 2nd arg, got ~a" z2))))
        ((%fixnum? z1)
         (cond ((%real? z2) (%expt (%fixnum->real z1) z2))
               ((%fixnum? z2) (%expt (%fixnum->real z1) (%fixnum->real z2)))
               (else (error "EXPT expects a number as 2nd arg, got ~a" z2))))
        (else (error "EXPT expects two numbers as arguments, got ~a and ~a"
                     z1 z2))))
        
(define (make-rectangular x1 x2) (error "Unimplemented MAKE-RECTANGULAR"))
(define (make-polar x1 x2) (error "Unimplemented MAKE-POLAR"))
(define (real-part z) (error "Unimplemented REAL-PART"))
(define (imag-part z) (error "Unimplemented IMAG-PART"))
(define (magnitude z) (error "Unimplemented MAGNITUDE"))
(define (angle z) (error "Unimplemented ANGLE"))

(define (exact->inexact z)
  (cond ((%fixnum? z) (%fixnum->real z))
        ((%real? z) z)
        (else (error "EXACT->INEXACT expects a number, got ~a" z))))

;   (cond ((%fixnum? z) (%fixnum->real z))
;         ((%rational? z) (/ (%fixnum->real (numerator z)) 
;                            (%fixnum->real (denominator z))))
;         (else z)))

(define (inexact->exact z)
  (if (%real? z) (%floor z) z))

