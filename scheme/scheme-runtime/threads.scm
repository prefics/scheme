;;; threads.scm -- cooperative thread implementation

;; Author: Pierre De Pascale
;; Date: 27 Jan 2004

;;;Commentary:

;;;Code:

;;;
;; = Spinlocks

;;;
;; Spinlock datastructure. A spinlock has a name for information
;; purposes. When locking a spinlock and the spinlock is already locked,
;; spinning is used to wait until locking is successful.
(define-record-type <spinlock>
  (%make-spinlock name value)
  spinlock?
  (name spinlock-name set-spinlock-name!)
  (value spinlock-value set-spinlock-value!))

;;;
;; Creates a spinlock with `name`.
(define (make-spinlock name)
  (if (not (string? name))
      (error "bad argument to MAKE-SPINLOCK: name should be a string, but was ~a" name))
  (%make-spinlock name (cons #t #t)))

;;;
;; Try to lock `spinlock`. If successful `#t` is returned. Otherwise `#f` is returned.
(define (spinlock-try-lock! spinlock)
  (if (not (spinlock? spinlock))
      (error "SPINLOCK-TRY-LOCK: wrong argument, need a spinlock, got: ~a" spinlock))
  (%test+set! (spinlock-value spinlock) #t #f))

;;;
;; Lock `spinlock`. Spinning is used until the locking succeeds.
(define (spinlock-lock! spinlock)
  (if (not (spinlock? spinlock))
      (error "SPINLOCK-LOCK: wrong argument, need a spinlock, got: ~a" spinlock))
  (let spin ((value (spinlock-try-lock! spinlock)))
    (if value
	spinlock
	(spin (spinlock-try-lock! spinlock)))))

;;;
;; Unlock `spinlock` independently if it was locked or not.
(define (spinlock-unlock! spinlock)
  (if (not (spinlock? spinlock))
      (error "SPINLOCK-LOCK: wrong argument, need a spinlock, got: ~a" spinlock))
  (let ((value (spinlock-value spinlock)))
    (set-cdr! value #t)))

;;;
;; Returns `#t` if `spinlock` is locked. Otherwise `#f` is returned.
(define (spinlock-locked? spinlock)
  (if (not (spinlock? spinlock))
      (error "SPINLOCK-LOCKED?: wrong argument, need a spinlock, got ~a" spinlock))
  (cdr (spinlock-value spinlock)))

;;;
;; Executes `thunk` while holding `spinlock` lock. The value returned
;; is what value `thunk` returns.
(define (with-spinlock-held spinlock thunk)
  (dynamic-wind
      (lambda () (spinlock-lock! spinlock))
      thunk
      (lambda () (spinlock-unlock! spinlock))))


;;;
;; == Threads support

(define-record-type <thread>
  (really-make-thread name status value joiner continuation)
  thread?
  (name thread-name set-thread-name!)
  (status thread-status set-thread-status!)
  (value thread-value set-thread-value!)
  (joiner thread-joiner set-thread-joiner!)
  (continuation thread-continuation set-thread-continuation!))

(define (make-thread proc . opt)
  (let ((proc* (let ((env (dynamic-env)))
		 (lambda _
		   (let ((value #f))
		     (set-dynamic-env! env)
		     (set-dynamic-cont! '())
		     (reset-continuation!)
		     (condition-case
		      (set! value (proc))
		      (<condition> (lambda (c) (set! value c))))
		     (thread-terminate! *thread* value)))))
	(name (if (pair? opt) (car opt) "Unnamed thread")))
    (really-make-thread name $status/ready (unspecific-object) '() proc*)))

(define (make-empty-queue)
  (let ((queue (cons '() '())))
    queue))

(define (enqueue! elem queue)
  (let ((last (cdr queue)))
    (set-cdr! queue (cons elem '()))
    (if (pair? last)
        (set-cdr! last (cdr queue))
        (set-car! queue (cdr queue)))
    queue))

(define (dequeue! queue)
  (if (queue-empty? queue)
      (error "queue empty")
      (let ((front (car queue)))
        (set-car! queue (cdr front))
        (if (null? (car queue)) (set-cdr! queue '()))
        (car front))))

(define (queue-empty? queue) (null? (queue-front queue)))

(define (queue-front queue) (car queue))

(define *scheduler-lock* (make-spinlock "Scheduler's lock"))

(define *threads* (make-empty-queue))
(define *read-waiting* '())
(define *write-waiting* '())
(define *exit-waiting* '())
(define *timeouts* '())

(define (thread-block-read channel)
  (spinlock-lock! *scheduler-lock*)
  (call/cc (lambda (proc)
             (set-thread-continuation! *thread* proc)
             (set-thread-status! *thread* $status/suspended)
             (set! *read-waiting* (cons (cons channel *thread*) *read-waiting*))
             (enqueue! *thread* *threads*)
             ;; (display (map thread-name (car *threads*)))
             ;; (display (list "Block Read" (thread-name *thread*))) (newline)
             (schedule-thread!))))

(define (thread-block-write channel)
  (spinlock-lock! *scheduler-lock*)
  (call/cc (lambda (proc)
             (set-thread-continuation! *thread* proc)
             (set-thread-status! *thread* $status/suspended)
             (set! *write-waiting* (cons (cons channel *thread*) *write-waiting*))
             (enqueue! *thread* *threads*)
             ;; (display (map thread-name (car *threads*)))
             ;; (display (list "Block Write" (thread-name *thread*))) (newline)
             (schedule-thread!))))

(define (time+ delta)
  (let ((t (posix-time)))
    (bvec-set! t 3 (+ delta (bvec-ref t 3)))
    t))

(define (add-timeout! time proc)
  (letrec ((insert
	    (lambda (list)
	      (cond ((null? list) (cons (cons time proc) '()))
		    ((< time (caar list))
		     (cons (cons time proc) list))
		    (else (cons (car list) (insert (cdr list))))))))
    (set! *timeouts* (insert *timeouts*))
    (display "timeout inserted")))

;;; We shouldn't simply call the THUNK for timeouts as it is done in the event
;; thread. It should push the closure on the thread continuation and let the
;; THUNK run when the thread is next scheduled. But you need to make sure the
;; thread will run and is not currently blocked.
(define (expire-timeouts! time)
  (set! *timeouts*
	(let lp ((timeout-entries *timeouts*))
	  (cond ((null? timeout-entries) timeout-entries)
		((>= time (caar timeout-entries))
		 ((cdar timeout-entries))
		 (lp (cdr timeout-entries)))
		(else timeout-entries)))))

(define (first-timeout) (if (pair? *timeouts*) (caar *timeouts*) #f))

(define (scan-thread pred? proc l)
  (if (null? l)
      l
      (let* ((entry (car l))
	     (channel (car entry))
	     (thread (cdr entry)))
	(if (pred? channel)
	    (begin
              ;; (display (list "Unblock " (thread-name thread))) (newline)
              (proc thread)
              ;; (display (map thread-name (cons *thread* (car *threads*))))
              (scan-thread pred? proc (cdr l)))
	    (cons entry (scan-thread pred? proc (cdr l)))))))

(define *idle-hooks* '())

(define (add-idle-hook! hook)
  (set! *idle-hooks* (cons hook *idle-hooks*)))

(define (run-idle-hooks!)
  (for-each (lambda (hook) (hook)) *idle-hooks*))

(define *e* "No error")

(define (event-i/o-thread)
  (expire-timeouts! (posix-time))
  ;; Note: the following MAP calls cannot be reduced by eta reduction to
  ;; map car *read-waiting*, because it won't compile at bootstrapping time
  ;; At that time, the compiler sees CAR as primitive and cannot perform
  ;; the eta expansion (or inverse).
  (spinlock-lock! *scheduler-lock*)
  (let* ((reads (map (lambda (e) (car e)) *read-waiting*))
	 (writes (map (lambda (e) (car e)) *write-waiting*))
	 (oks (posix-select reads writes (or (first-timeout) 100))))
;    (display "! reads=") (display reads) (display "!")
;    (display "! writes=") (display writes) (display "!")
;    (display "! oks=") (display oks) (display "!")
    (cond ((number? oks) (set! *e* oks))
	  ((eq? oks #f) oks)
	  (else
	   (begin
             ;; (display (list "ready to unblock" (map thread-name (car *threads*)))) (newline)
	     (set! *read-waiting*
		   (scan-thread (lambda (channel) (memq channel oks))
                                mark-thread-ready
				*read-waiting*))
	     (set! *write-waiting*
		   (scan-thread (lambda (channel) (memq channel oks))
                                mark-thread-ready
				*write-waiting*)))))
    (run-idle-hooks!)
    (spinlock-unlock! *scheduler-lock*)
    (thread-yield!)
    (event-i/o-thread)))

(define (start-multiprocessing)
  (set-dynamic-env! '())
  (set-dynamic-cont! '())
  (set-trap! trap/timer thread-yield-if-possible)
  (call/cc
   (lambda (rest)
     (let ((thread (or (thread-named "Init")
                       (really-make-thread "Init" $status/ready (unspecific-object) '() rest))))
       (set-thread-continuation! thread rest)
       (spinlock-lock! *scheduler-lock*)
       (if (queue-empty? *threads*)
	   (enqueue! (really-make-thread "Event I/O"
					 $status/ready
					 (unspecific-object)
					 '()
					 (lambda (k) (event-i/o-thread)))
		     *threads*))
       (set! *thread* thread)
       (enqueue! thread *threads*)
       (schedule-thread!)))))

(define $status/ready 0)
(define $status/dead 1)
(define $status/suspended 2)

(define *thread* #f)

(define (threads)
  (cons (thread-name *thread*) (map thread-name (car *threads*))))

(define $slice 50)

(define (schedule-thread!)
  ;;(display (list "available:" (map thread-name (car *threads*)))) (newline)
  (if (queue-empty? *threads*)
      (let loop () (display "No more threads to run!!!") (loop))
      (let ((thread (dequeue! *threads*)))
        ;;(display "scheduling:") (display (thread-name thread)) (display (thread-status thread))
        ;;(newline)
	(cond ((eq? (thread-status thread) $status/ready)
	       (set! *thread* thread)
	       (spinlock-unlock! *scheduler-lock*)
               (%set-timer 0 $slice)
	       ((thread-continuation thread) 'continue))
              ((eq? (thread-status thread) $status/suspended)
               ;;(display (list "skiping " (thread-name thread))) (newline)
               (enqueue! thread *threads*)
               (schedule-thread!))
              (else
               ;; (display (list "dead thread ?!?" (thread-name thread) (thread-status thread)))
               ;; (newline)
               (schedule-thread!))))))

(define (current-thread) *thread*)

(define (mark-thread-ready thread)
  (set-thread-status! thread $status/ready)
  ;; (enqueue! thread *threads*)
  )

(define (really-thread-yield proc)
  (set-thread-continuation! *thread* proc)
  (enqueue! *thread* *threads*)
  (schedule-thread!))

(define (thread-yield-if-possible)
  (if (spinlock-try-lock! *scheduler-lock*)
      (call/cc really-thread-yield)))

(define (thread-yield!)
  (spinlock-lock! *scheduler-lock*)
  (call/cc really-thread-yield))

(define (thread-start! thread)
  (spinlock-lock! *scheduler-lock*)
  (enqueue! thread *threads*)
  (spinlock-unlock! *scheduler-lock*))

(define (thread-suspend! thread)
  (spinlock-lock! *scheduler-lock*)
  (set-thread-status! thread $status/suspended)
  (if (eq? thread (current-thread))
      (call/cc (lambda (resume)
                 (set-thread-continuation! thread resume)
                 (enqueue! thread *threads*)
                 (schedule-thread!)))
      (spinlock-unlock! *scheduler-lock*))))

(define (thread-resume! thread)
  (spinlock-lock! *scheduler-lock*)
  (if (eq? (thread-status thread) $status/suspended)
      (set-thread-status! thread $status/ready))
  (spinlock-unlock! *scheduler-lock*))

(define (thread-named name)
  (spinlock-lock! *scheduler-lock*)
  (let loop ((threads (cons *thread* (car *threads*))))
    (cond ((null? threads)
           (spinlock-unlock! *scheduler-lock*)
           #f)
          ((equal? name (thread-name (car threads)))
           (spinlock-unlock! *scheduler-lock*)
           (car threads))
          (else (loop (cdr threads))))))

(define (thread-terminate! thread value)
  (spinlock-lock! *scheduler-lock*)
  (if (eq? (thread-status thread) $status/dead)
      (spinlock-unlock! *scheduler-lock*)
      (begin
        ;; (display (list 'thread-terminate (thread-name (current-thread))
        ;;                (thread-name thread) value)) (newline)
	(set-thread-value! thread value)
	(for-each (lambda (t)
                    ;; (display (list 'resuming-joined (thread-name t))) (newline)
                    (set-thread-status! t $status/ready)
                    ;; (enqueue! t *threads*)
                    )
                  (thread-joiner thread))
	(set-thread-status! thread $status/dead)
        ;; (display (list 'tr-at-end (threads))) (newline)
	(if (eq? thread (current-thread))
	    (schedule-thread!)
            (spinlock-unlock! *scheduler-lock*)))))

(define (thread-join! thread)
  (spinlock-lock! *scheduler-lock*)
  ;; (display (list 'thread-join (thread-name (current-thread))
  ;;                (thread-name thread))) (newline)
  (if (eq? (thread-status thread) $status/dead)
      (spinlock-unlock! *scheduler-lock*)
      (let ((myself (current-thread)))
	(set-thread-joiner! thread (cons myself (thread-joiner thread)))
        (call/cc
         (lambda (k)
           (set-thread-continuation! (current-thread) k)
           (set-thread-status! (current-thread) $status/suspended)
           (enqueue! myself *threads*)
           (schedule-thread!))))))

(define (thread-interrupt! thread thunk)
  (spinlock-lock! *scheduler-lock*)
  (if (eq? thread (current-thread))
      (begin
        (spinlock-unlock! *scheduler-lock*)
        (thunk))
      (begin
        (let ((old (thread-continuation thread)))
	  (set-thread-continuation! thread (lambda (v)
					     (thunk)
					     (old)))
          (spinlock-unlock! *scheduler-lock*)))))
