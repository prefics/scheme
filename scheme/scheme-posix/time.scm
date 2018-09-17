;;; time.scm -- time & date management procedures

(define-record-type <date>
  (make-date seconds minute hour month-day month year tz-name tz-secs
	     summer? week-day year-day)
  date?
  (seconds   date-seconds   set-date-seconds!)
  (minute    date-minute    set-date-minute!)
  (hour      date-hour      set-date-hour!)
  (month-day date-month-day set-date-month-day!)
  (month     date-month     set-date-month!)
  (year      date-year      set-date-year!)
  (tz-name   date-tz-name   set-date-tz-name!)
  (tz-secs   date-tz-secs   set-date-tz-secs!)
  (summer?   date-summer?   set-date-summer?)
  (week-day  date-week-day  set-date-week-day!)
  (year-day  date-year-day  set-date-year-day!))

(define (time+ticks))
(define (ticks/sec))

(define (date . time)
  (let* ((time (if (null? time) (posix-time) (car time)))
	 (date (posix-localtime (car time)
				(make-date #f #f #f #f #f #f #f #f #f #f #f))))
    (set-date-year! date (+ 1900 (date-year date)))
    date))


;;;
;; = Date handling
(define *day-names* '#("<unknown>" "Monday" "Tuesday" "Wednesday" "Thirsday" "Friday" "Saturday" "Sunday"))

(define (date->string date)
  (string-append
   (vector-ref *day-names* (date-week-day date)) " "
   (number->string (date-month-day date)) "/"
   (number->string (date-month date)) "/"
   (number->string (date-year date)) " "
   (number->string (date-hour date)) ":"
   (number->string (date-minute date)) ":"
   (number->string (date-seconds date))))

(define (format-date fmt date) #f)

(define (fill-in-date! date) #f)

; (define (display-date date)
;    (display (date/week-day date)) (display " ")
;    (display (date/month-day date)) (display "/")
;    (display (date/month date)) (display "/")
;    (display (date/year date)) (display " ")
;    (display (date/hour date)) (display ":")
;    (display (date/minute date)) (display ":")
;    (display (date/seconds date)) (newline))

;;;
;; = Time Function

(define $milliseconds 1000000)

(define (time . date) (posix-time))

(define (time-seconds t) (car t))
(define (time-milliseconds t) (cdr t))

(define (make-time seconds milli) (cons seconds milli))
;(define (time seconds milli) (cons seconds milli))

(define (seconds->time seconds) (make-time seconds 0))
(define (time->seconds time) (time-seconds time))

(define (time- t1 t2)
  (if (>= (cdr t1) (cdr t2))
      (cons (- (car t1) (car t2))
            (- (cdr t1) (cdr t2)))
      (cons (- (- (car t1) 1) (car t2))
            (- (+ (cdr t1) $milliseconds)
               (cdr t2)))))

(define (time+ t1 t2)
  (let ((milli (+ (cdr t1) (cdr t2))))
    (if (>= milli $milliseconds)
        (cons (+ 1 (+ (car t1) (car t2)))
              (- milli $milliseconds))
        (cons (+ (car t1) (car t2))
              (+ (cdr t1) (cdr t2))))))

(define (time=? t1 t2)
  (and (= (car t1) (car t2))
       (= (cdr t1) (cdr t2))))

(define (make-time-comparator less?)
  (lambda (t1 t2)
    (or (less? (car t1) (car t2))
        (and (= (car t1) (car t2))
             (less? (cdr t1) (cdr t2))))))

(define time<? (make-time-comparator (lambda (t1 t2) (< t1 t2))))
(define time<=? (make-time-comparator (lambda (t1 t2) (<= t1 t2))))
(define time>=? (make-time-comparator (lambda (t1 t2) (>= t1 t2))))
(define time>? (make-time-comparator (lambda (t1 t2) (> t1 t2))))
(define (time/=? t1 t2) (not (time=? t1 t2)))

(define (time->number time) (+ (* $milliseconds (time-seconds time))
                               (time-milliseconds time)))

(define (number->time time)
  (make-time (quotient time $milliseconds)
             (modulo time $milliseconds)))
