;;; extensions.scm -- some extensions of R5RS

;;; ## Some list procedures

(define (split lst)
  (let lp ((lst lst)
           (left '())
           (right '()))
    (if (null? lst)
        (cons left right)
        (lp (cdr lst)
            (cons (car lst) right)
            left))))

(define (merge l1 l2 less?)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else (let ((e1 (car l1))
                    (e2 (car l2)))
                (if (less? e1 e2)
                    (cons e1 (merge (cdr l1) l2 less?))
                    (cons e2 (merge l1 (cdr l2) less?)))))))

(define (sort lst less?)
  (cond ((null? lst) lst)
        ((null? (cdr lst)) lst)
        (else
         (let ((splited (split lst)))
           (merge (sort (car splited) less?)
                  (sort (cdr splited) less?)
                  less?)))))

(define (delq element lst)
  (filter (lambda (e) (eq? e element)) lst))

(define (delv element lst)
  (filter (lambda (e) (eqv? e element)) lst))

(define (delete element lst)
  (filter (lambda (e) (equal? e element)) lst))

(define (posq element lst)
  (let find ((lst lst)
             (i 0))
    (cond ((null? lst) #f)
          ((eq? (car lst) element) i)
          (else (find (cdr lst) (+ i 1))))))

(define (posv element lst)
  (let find ((lst lst)
             (i 0))
    (cond ((null? lst) #f)
          ((eqv? (car lst) element) i)
          (else (find (cdr lst) (+ i 1))))))

(define (position element lst)
  (let find ((lst lst)
             (i 0))
    (cond ((null? lst) #f)
          ((equal? (car lst) element) i)
          (else (find (cdr lst) (+ i 1))))))

(define (reduce proc lst)
  (cond ((null? lst) (error "cannot reduce an empty list"))
        ((null? (cdr lst)) (car lst))
        (else
         (let ((head (car lst))
               (rest (reduce proc (cdr lst))))
           (proc head rest)))))

(define (filter pred? lst)
  (cond ((null? lst) lst)
        ((pred? (car lst)) (filter pred? (cdr lst)))
        (else (cons (car lst) (filter pred? (cdr lst))))))

(define (all? pred? lst)
  (if (null? lst)
      #t
      (and (pred? (car lst))
           (all? pred? (cdr lst)))))

(define (any? pred? lst)
  (if (null? lst)
      #f
      (or (pred? (car lst)) (any? pred? (cdr lst)))))

(define (make-list n e)
  (if (<= n 0)
      '()
      (cons e (make-list (- n 1) e))))

(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

(define (fold-left proc init lst)
  (if (null? lst)
      init
      (fold-left proc (proc (car lst) init) (cdr lst))))

(define (fold-right proc init lst)
  (if (null? lst)
      init
      (proc (car lst) (fold-right proc init (cdr lst)))))
;;; ## Some Vector procedures

(define (order vec start end)
  (if (>= start end)
      start
      (let ((e1 (vector-ref vec start))
	    (e2 (vector-ref vec end)))
;	(display (list 'order start end)) (newline)
	(if (string<=? e1 e2)
	    (order vec start (- end 1))
	    (begin
	      (vector-set! vec start e2)
	      (vector-set! vec end e1)
	      (order-reverse vec (+ start 1) end))))))

(define (vector-sort! vec . less?)
  (let do-sort ((start 0)
		(end (- (vector-length vec) 1)))
;    (display (list 'do-sort start end)) (newline)
    (if (>= start end)
	vec
	(let ((middle (order vec start end)))
	  (do-sort start (- middle 1))
	  (do-sort (+ middle 1) end)))))


(define (subvector vector start end)
  (let ((result (make-vector (- end start) #f)))
    (let lp ((i start)
	     (ii 0))
      (if (< i end)
	  (let ((e (vector-ref vector i)))
	    (vector-set! result ii e)
	    (lp (+ i 1) (+ ii 1)))
	  result))))

(define (vector-extend vector to)
  (let ((result (make-vector to #f)))
    (let lp ((i 0))
      (if (< i (vector-length vector))
	  (let ((e (vector-ref vector i)))
	    (vector-set! result i e)
	    (lp (+ i 1)))
	  result))))

;;; ## Some String procedures

(define (string-prefix? prefix str)
  (let loop ((i 0))
    (if (< i (string-length prefix))
	(and (< i (string-length str))
	     (char=? (string-ref prefix i)
		     (string-ref str i))
	     (loop (+ i 1)))
	#t)))

(define (string-position str char)
  (let find ((i 0))
    (if (< i (string-length str))
	(let ((ch (string-ref str i)))
	  (if (char=? ch char)
	      i
	      (find (+ i 1))))
	#f)))

(define (string-split string ch)
  (let loop ((start 0)
             (i 0))
    (if (< i (string-length string))
        (if (char=? (string-ref string i) ch)
            (cons (substring string start i) (loop (+ i 1) (+ i 1)))
            (loop start (+ i 1)))
        (list (substring string start i)))))

(define (string-join sep strings)
  (cond ((null? strings) "")
	((null? (cdr strings)) (car strings))
	(else (string-append (car strings)
			     (if (string? sep)
				 sep
				 (string sep))
			     (string-join sep (cdr strings))))))

;;; ## Random numbers

(define (random n) (modulo (%random) n))

;;; Copied from programmingpraxis.com in standard prelude

;; (define rand #f)
;; (define randint #f)

;; (letrec ((two31 #x80000000)
;; 	 (a (make-vector 56 -1))
;; 	 (fptr #f)
;; 	 (mod-diff (lambda (x y) (modulo (- x y) two31))) ; generic version
;; 	 (flip-cycle (lambda ()
;; 		       (let lp ((ii 1) (jj 32))
;; 			 (if (<= jj 55)
;; 			     (begin
;; 			       (vector-set! a ii (mod-diff (vector-ref a ii) (vector-ref a jj)))
;; 			       (lp (+ ii 1) (+ jj 32)))))
;; 		       (let lp ((ii 25) (jj 1))
;; 			 (if (<= ii 55)
;; 			     (begin
;; 				 (vector-set! a ii (mod-diff (vector-ref a ii) (vector-ref a jj)))
;; 				 (lp (+ ii 1) (+ jj 1)))))
;; 		       (set! fptr 54) (vector-ref a 55)))
;; 	 (init-rand (lambda (seed)
;; 			   (let* ((seed (mod-diff seed 0))
;; 				  (prev seed)
;; 				  (next 1))
;; 			     (vector-set! a 55 prev)
;; 			     (let lp ((i 21))
;; 			       (if (zero? i)
;; 				   i
;; 				   (begin
;; 				     (vector-set! a i next)
;; 				     (set! next (mod-diff prev next))
;; 				     (set! seed (+ (quotient seed 2)
;; 						   (if (odd? seed) #x40000000 0)))
;; 				     (set! next (mod-diff next seed))
;; 				     (set! prev (vector-ref a i))
;; 				     (lp (modulo (+ i 21) 55)))))
;; 			     (flip-cycle)
;; 			     (flip-cycle)
;; 			     (flip-cycle)
;; 			     (flip-cycle)
;; 			     (flip-cycle))))
;; 	 (next-rand (lambda ()
;; 		      (if (negative? (vector-ref a fptr)) (flip-cycle)
;; 			  (let ((next (vector-ref a fptr))) (set! fptr (- fptr 1)) next))))
;; 	 (unif-rand (lambda (m)
;; 		      (let ((t (- two31 (modulo two31 m))))
;; 			(let loop ((r (next-rand)))
;; 			  (if (<= t r) (loop (next-rand)) (modulo r m)))))))
;;   (init-rand 19380110) ; happy birthday donald e knuth
;;   (set! rand (lambda seed
;; 	       (cond ((null? seed) (/ (next-rand) two31))
;; 		     ((eq? (car seed) 'get) (cons fptr (vector->list a)))
;; 		     ((eq? (car seed) 'set) (set! fptr (caadr seed))
;; 		      (set! a (list->vector (cdadr seed))))
;; 		     (else (/ (init-rand (modulo (numerator
;; 						  (inexact->exact (car seed))) two31)) two31)))))
;;   (set! randint (lambda args
;; 		  (cond ((null? (cdr args))
;; 			 (if (< (car args) two31) (unif-rand (car args))
;; 			     (floor (* (next-rand) (car args)))))
;; 			((< (car args) (cadr args))
;; 			 (let ((span (- (cadr args) (car args))))
;; 			   (+ (car args)
;; 			      (if (< span two31) (unif-rand span)
;; 				  (floor (* (next-rand) span))))))
;; 			(else (let ((span (- (car args) (cadr args))))
;; 				(- (car args)
;; 				   (if (< span two31) (unif-rand span)
;; 				       (floor (* (next-rand) span))))))))))
