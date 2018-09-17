;;; hashtable.scm -- Hashtable support

(define-record-type :hashtable
  (really-make-hashtable hash-proc size buckets)
  hashtable?
  (hash-proc hashtable/hash-proc set-hashtable/hash-proc!)
  (size      hashtable/size      set-hashtable/size!)
  (buckets   hashtable/buckets   set-hashtable/buckets!))

(define $empty-bucket "empty bucket")
;(define $default-hashtable-size 213)
(define $default-hashtable-size 753)
(define (default-hash obj) (object-hash obj))

(define (make-hashtable . hash-proc)
  (let ((hash (if (null? hash-proc) default-hash (car hash-proc))))
    (really-make-hashtable hash
			   $default-hashtable-size
			   (make-vector $default-hashtable-size $empty-bucket))))

(define (hash-ref hashtable key)
  (let* ((hash-proc (hashtable/hash-proc hashtable))
	 (size (hashtable/size hashtable))
	 (bucket (modulo (hash-proc key) size))
	 (buckets (hashtable/buckets hashtable)))
    (let loop ((b bucket))
      (let ((key/val (vector-ref buckets b)))
	(if (eq? key/val $empty-bucket)
	    (let ((b* (modulo (+ b 1) size)))
	      (if (= bucket b*)
		  #f
		  (loop b*)))
	    (if (eq? (car key/val) key)
		(cdr key/val)
		(let ((b* (modulo (+ b 1) size)))
		  (if (= bucket b*)
		      #f
		      (loop (+ b 1))))))))))

(define (hash-set! hashtable key val)
  (let* ((hash-proc (hashtable/hash-proc hashtable))
	 (size (hashtable/size hashtable))
	 (bucket (modulo (hash-proc key) size))
	 (buckets (hashtable/buckets hashtable)))
    (let loop ((b bucket))
      (let ((key/val (vector-ref buckets b)))
	(if (eq? key/val $empty-bucket)
	    (vector-set! buckets b (cons key val))
	    (let ((b* (modulo (+ b 1) size)))
	    (if (= bucket b*)
		(error "HASHTABLE full" hashtable)
		(loop b*))))))))

; (define bench
;   (lambda (times)
;     (let ((h (make-hashtable object-hash)))
;       (loop (for i from 0 upto 128)
; 	    (do (hash-set! h i 3)))
;       (loop (for i from 0 upto 128)
; 	    (do (loop (for j from 0 upto 100)
; 		      (do (hash-ref h i))))))))

; (define (bench-alist)
;   (let ((alist (loop (for i from 0 upto 128)
; 		     (collect (cons i 3)))))
;     (loop (for i from 0 upto 128)
; 	  (do (loop (for j from 0 upto 100)
; 		    (do (assq j alist)))))))
