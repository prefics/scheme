;;; fluids.scm -- fluid variables , WARNING NOT THREAD SAFE !!!!

;;; Description

;; Provide Fluid Variables. 
;; IFC:

;; (MAKE-FLUID value)
;; (FLUID variable)
;; (SET-FLUID! variable value)
;; (LET-FLUID variable value proc)

;;; Code:


(define-record-type <fluid>
  (make-fluid value)
  fluid?
  (value fluid/value set-fluid/value!))

(define (dynamic-env)
  (suspend-cc (lambda (return)
                (resume-cc return
                           (vector-ref return 10)))))

(define (set-dynamic-env! env)
  (suspend-cc (lambda (return)
                (vector-set! return 10 env)
                (resume-cc return #t))))
	 
(define (reset-continuation!)
  (suspend-cc (lambda (return)
		(let ((cont (vector-ref return 8)))
		  (vector-set! cont 0 (unspecific-object))
		  (resume-cc return #t)))))

(define (fluid f)
  (let ((val (assq f (dynamic-env))))
    (if val (cdr val) (fluid/value f))))

(define (set-fluid! f val)
  (let ((probe (assq f (dynamic-env))))
    (if probe 
        (set-cdr! probe val)
        (set-fluid/value! f val))))

(define (let-fluid f val proc)
  (let ((env (dynamic-env)))
    (dynamic-wind
	(lambda () (set-dynamic-env! (cons (cons f val) env)))
	proc
	(lambda () (set-dynamic-env! env)))))

;;; Dynamic stuff

(define (make-dynamic-point enter leave) (vector enter leave))
(define (dynamic-point-enter dp) (vector-ref dp 0))
(define (dynamic-point-leave dp) (vector-ref dp 1))
(define (dynamic-cont)
  (suspend-cc (lambda (return) (resume-cc return (vector-ref return 11)))))

(define (set-dynamic-cont! cont)
  (suspend-cc (lambda (return)
                (vector-set! return 11 cont)
                (resume-cc return #t))))
	 
;;; copied from http://paste.lisp.org/display/49837

(define dynamic-wind
  (lambda (before thunk after)
    (before)
    (set-dynamic-cont! (cons (make-dynamic-point before after) (dynamic-cont)))
    (let ((result (thunk)))
      (set-dynamic-cont! (cdr (dynamic-cont)))
      (after)
      result)))

(define call/wc
  (lambda (proc)
    (let ((winds (dynamic-cont)))
      (suspend-cc (lambda (k)
		    (proc (lambda (val)
			    (let ((here (dynamic-cont)))
			      (if (not (eq? winds here))
				  (dynamic-unwind winds
						  (- (length here) 
						     (length winds))))
			      ;(set-dynamic-cont! winds)
			      (resume-cc k val)))))))))
  
(define (dynamic-unwind winds n)
  (cond 
   ((eq? (dynamic-cont) winds) 'done)
   ((< n 0)
    (dynamic-unwind (cdr winds) (+ n 1))
    ((dynamic-point-enter (car winds)))
    (set-dynamic-cont! winds))
   (else
    (let* ((dc (dynamic-cont))
	   (after (dynamic-point-leave (car dc))))
      (set-dynamic-cont! (cdr dc))
      (after)
      (dynamic-unwind winds (- n 1))))))

;; (define (call/wc proc)
;;   (let* ((here (dynamic-cont))
;; 	 (result (suspend-cc (lambda (k)
;; 			       (proc (lambda (val)
;; 				       (resume-cc k val)))))))
;;     (unwind-rewind! (dynamic-cont) here)
;;     (set-dynamic-cont! here)
;;     result))

(define call-with-current-continuation call/wc)
		
;; (define (unwind-rewind! from to)
;;   (if (null? from)
;;       (rewind! from to)
;;       (let ((point (car from)))
;; 	(if (memq point to)
;; 	    (rewind! point to)
;; 	    (begin
;; 	      ((dynamic-point-leave point))
;; 	      (unwind-rewind! (cdr from) to))))))

;; (define (rewind! point to)
;;   (if (null? to)
;;       'done
;;       (let ((p (car to)))
;; 	(if (eq? point p)
;; 	    'done
;; 	    (begin
;; 	      ((dynamic-point-enter to))
;; 	      (rewind! point (cdr to)))))))

(define (call/cc proc)
  (suspend-cc (lambda (k) 
		(proc (lambda (val)
			(resume-cc k val))))))

;; (define (dynamic-wind before thunk after)
;;   (let ((dp (make-dynamic-point before after)))
;;     (set-dynamic-cont! (cons dp (dynamic-cont)))
;;     (before)
;;     (let ((return-val (thunk)))
;;       (after)
;;       (set-dynamic-cont! (cdr (dynamic-cont)))
;;       return-val)))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize (?var ?val) . ?body)
     (let ((old ?var)
	   (new ?val))
       (dynamic-wind 
	   (lambda () (set! ?var new))
	   (lambda () . ?body)
	   (lambda () (set! ?var old)))))))

(define (tst-dyn)
  (let ((v 1))
    (display "val is ") (display v) (newline)
    (parameterize (v 2)
      (display "val is ") (display v) (newline)
      (call/wc (lambda (k)
		 (display "val is ") (display v) (newline)
		 (parameterize (v 3)
		   (display "val is ") (display v) (newline)
		   (k 1234))))
      (display "val is ") (display v) (newline))
    (display "val is ") (display v) (newline)))
