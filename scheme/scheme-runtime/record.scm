;;; record.scm -- define records system for Schemy

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type (?type ?inherit ...)
       (?constructor ?args ...)
       ?pred
       (?field ?getter . ?setter) ...)
     (begin
       ;;(define-record-type* '?type (list ?inherit ...) '(?field ...))
       (define ?type (make-record-type '?type (list ?inherit ...) '(?field ...)))
       (define ?pred (make-record-pred ?type))
       (define ?constructor (lambda (?args ...)
			      (let ((rec (make-record ?type)))
				(record-set! rec '?args ?args) ...
				(initialize rec)
                                rec)))
       (let ((type ?type))
	 (define-record-field type ?field ?getter . ?setter) ...)))
    ((define-record-type ?type
       (?constructor ?args ...)
       ?pred
       (?field ?getter . ?setter) ...)
     (begin
       ;;(define-record-type* '?type (list <top>) '(?field ...))
       (define ?type (make-record-type '?type (list <top>) '(?field ...)))
       (define ?pred (make-record-pred ?type))
       (define ?constructor (lambda (?args ...)
			      (let ((rec (make-record ?type)))
				(record-set! rec '?args ?args) ...
                                rec)))
       (let ((type ?type))
	 (define-record-field type ?field ?getter . ?setter) ...)))))

(define (define-record-type* name parents fields)
  (let* ((type (make-record-type name parents fields))
	 (cm (or (current-module-name) (error "No current module setted")))
	 (binding (lookup-ref name cm))
	 (value (and binding (ref/value binding))))
    (display ";; redefining type") (newline)
    (if (record-type? value)
	(if (not (record-type=? type value))
	    ;; Ok the type is different, so set it
	    (begin
	      (set-ref/value! binding type)
	      (set-record-type-methods! type (record-type-methods value))))
	;; OK the type is new, so just bind it
	(bind-ref! name type (current-module)))))

(define (record-type=? t1 t2)
  (and
   (eq? (record-type-name t1) (record-type-name t2))
   (equal? (record-type-supers t1) (record-type-supers t2))
   (equal? (record-type-slots t1) (record-type-slots t2))))

;; (define-syntax define-record-field
;;   (syntax-rules ()
;;     ((define-record-field ?field ?getter)
;;      (define ?getter (lambda (obj) (record-ref obj '?field))))
;;     ((define-record-field ?field ?getter ?setter)
;;      (begin (define ?getter (lambda (obj) (record-ref obj '?field)))
;; 	    (define ?setter (lambda (obj val)
;;  			      (record-set! obj '?field val)))))))

(define-syntax define-record-field
  (syntax-rules ()
    ((define-record-field ?type ?field ?getter)
     (begin
       (define-generic ?getter (obj))
       (define-method ?getter ((obj ?type)) (record-ref obj '?field))))
    ((define-record-field ?type ?field ?getter ?setter)
     (begin
       (define-generic ?getter (obj))
       (define-method ?getter ((obj ?type)) (record-ref obj '?field))
       (define-generic ?setter (obj value))
       (define-method ?setter ((obj ?type) value)
	 (record-set! obj '?field value))))))
     ;; (begin (define ?getter (lambda (obj) (record-ref obj '?field)))
     ;; 	    (define ?setter (lambda (obj val)
     ;; 			      (record-set! obj '?field val)))))))

(define-syntax define-abstact-type
  (syntax-rules ()
    ((define-abstract-type ?name ?pred)
     (begin
       (define ?name (make-record-type '?type (list <top>) '()))
       (define ?pred (lambda (obj) (instance? obj ?name)))))
    ((define-abstact-type (?name ?parents ...) ?pred)
     (begin
       (define ?name (make-record-type '?type (list ?parents ...) '()))
       (define ?pred (lambda (obj) (instance? obj ?name)))))))

;;; Type hierarchy

(define (all? pred? lst)
  (or (null? lst)
      (and (pred? (car lst)) (all? pred? (cdr lst)))))

(define (any? pred? lst)
  (if (null? lst)
      #f
      (or (pred? (car lst)) (any? pred? (cdr lst)))))

(define (record-type-name rt) (stob-ref rt 0))
(define (record-type-supers rt) (stob-ref rt 1))
(define (record-type-direct-slots rt) (stob-ref rt 2))
(define (record-type-slots rt) (stob-ref rt 3))
(define (record-type-methods rt) (stob-ref rt 4))
(define (set-record-type-methods! rt methods) (stob-set! rt 4 methods))
(define (record-type-ancestors rt) (stob-ref rt 5))
(define (record-type-subclasses rt) (stob-ref rt 6))

(define <<type>> (make-stob 7 #f))
    
(define <<class>>
  (let ((rec (make-stob 7 #f)))
    (set-stob-class! rec rec)
    (stob-set! rec 0 '<<class>>)
    (stob-set! rec 1 (list <<type>>))
    (stob-set! rec 2 '(name parent direct fields methods ancestors subclasses))
    (stob-set! rec 3 '(name parent direct fields methods ancestors subclasses))
    (stob-set! rec 4 '())
    (stob-set! rec 5 #f)
    (stob-set! rec 6 '())
    rec))

(define <top>
  (let ((class (make-stob 7 #f)))
    (set-stob-class! class <<class>>)
    (stob-set! class 0 '<top>)
    (stob-set! class 1 '())
    (stob-set! class 2 '())
    (stob-set! class 3 '())
    (stob-set! class 4 '())
    (stob-set! class 5 (list class))
    (stob-set! class 6 '())
    class))

(set-stob-class! <<type>> <<class>>)
(stob-set! <<type>> 0 '<<type>>)
(stob-set! <<type>> 1 (list <top>))
(stob-set! <<type>> 2 '())
(stob-set! <<type>> 3 '())
(stob-set! <<type>> 4 '())
(stob-set! <<type>> 5 (list <<type>> <top>))
(stob-set! <<type>> 6 '())

(stob-set! <<class>> 5 (list <<class>> <<type>> <top>))

(define (add-subclass! parent child)
  (stob-set! parent 6 (cons child (stob-ref parent 6))))

(add-subclass! <<type>> <<class>>)
(add-subclass! <top> <<type>>)

(define (make-record type)
  (let ((size (length (record-type-slots type))))
    (make-stob size type)))

(define (record? obj) (stob? obj))

(define (record-type obj)
  (if (record? obj)
      (stob-class obj)
      (error "RECORD-TYPE: object ~a not a record" obj)))

(define (compute-all-fields rec)
  (if (null? rec)
      rec
      (append (record-type-direct-slots (car rec))
              (compute-all-fields (append (cdr rec)
                                          (record-type-supers (car rec)))))))

(define (remove-duplicates lst)
  (if (null? lst)
      lst
      (let ((head (car lst)))
        (if (memq head (cdr lst))
            (remove-duplicates (cdr lst))
            (cons head (remove-duplicates (cdr lst)))))))

;; copied from goo reference manual, Annex A

(define (record-type-ordered-ancestors c)
  (let ((parents (record-type-supers c)))
    (let merge-lists ((partial-cpl (list c))
		      (remaining-lists (cons parents
					     (map record-type-ancestors parents))))
      (if (all? (lambda (l) (null? l)) remaining-lists)
	  (reverse partial-cpl)
	  (let* ((candidate (lambda (c)
			      (let ((tail? (lambda (l) (if (null? l) #f
							   (memq c (cdr l))))))
				(and (not (any? tail? remaining-lists)) c))))
		 (candidate-at-head (lambda (l) (and (not (null? l))
						     (candidate (car l)))))
		 (next (any? candidate-at-head remaining-lists)))
	    (if next
		(let ((del-next (lambda (l)
				  (cond ((null? l) l)
					((eq? (car l) next) (cdr l))
					(else l)))))
		  (merge-lists (cons next partial-cpl)
			       (map del-next remaining-lists)))
		(error "inconsistent precedence graph for ~a" c)))))))

(define (make-record-type name inherits fields)
  (let ((rec (make-stob 7 <<class>>)))
    (stob-set! rec 0 name)
    (stob-set! rec 1 inherits)
    (stob-set! rec 2 fields)
    (stob-set! rec 3 (remove-duplicates (compute-all-fields (list rec))))
    (stob-set! rec 4 '())
    (stob-set! rec 5 (record-type-ordered-ancestors rec))
    (stob-set! rec 6 '())
    (for-each (lambda (p) (add-subclass! p rec)) inherits)
    rec))

(define (record-type? obj)
  (and (record? obj) (eq? (record-type obj) <<class>>)))

(define (make-record-pred type) (lambda (obj) (instance? obj type)))

;; (define (record-ref obj field)
;;   (let* ((type (stob-class obj))
;; 	 (offset (field-pos field (stob-ref type 3))))
;;     (if offset
;;         (stob-ref obj offset)
;;         (error "field does not exists ~s in ~a" field obj))))

;; (define (record-set! obj field val)
;;   (let* ((type (stob-class obj))
;; 	 (offset (field-pos field (stob-ref type 3))))
;;     (if offset
;;         (stob-set! obj offset val)
;;         (error "Field does not exists ~s in ~s" field obj))))

(define (record-ref obj field)
  (%record-ref obj field))

(define (record-set! obj field val)
  (%record-set! obj field val))

(define (field-pos field fields)
  (if (null? fields)
      #f
      (if (eq? field (car fields))
	  0
          (let ((p (field-pos field (cdr fields))))
            (if p (+ 1 p) p)))))

;;; type system

(define <<union>> (make-record-type '<<union>> (list <<type>>) '(tys)))
(define (make-type-union tys)
  (let ((r (make-record <<union>>)))
    (record-set! r 'tys tys)
    r))

(define type-union? (make-record-pred <<union>>))
(define (union-types obj) (record-ref obj 'tys))

(define <<singleton>> (make-record-type '<<singleton>> (list <<type>>) '(object)))
(define (make-type-singleton object)
  (let ((r (make-record <<singleton>>)))
    (record-set! r 'object object)
    r))
(define type-singleton? (make-record-pred <<singleton>>))
(define (type-object t) (record-ref t 'object))

(define <<subclass>> (make-record-type '<<subclass>> (list <<type>>) '(class)))
(define (make-type-subclass class)
  (let ((r (make-record <<subclass>>)))
    (record-set! r 'class class)
    r))
(define type-subclass? (make-record-pred <<subclass>>))
(define (type-class t) (record-ref t 'class))

(define (t? type) (t+ (t= #f) type))
(define (t= obj) (make-type-singleton obj))
(define (t< class) (make-type-subclass class))
(define (t+ . types) (make-type-union types))

;;; RECORD-CASE SYNTAX ========================================================

(define-syntax rcc
  (syntax-rules (else)
    ((rcc ?var (else . ?body))
     (begin  . ?body))
    ((rcc ?var)
     (error "record-case fall through"))
    ((rcc ?var (?type ((?field-var ?field-name) ...) . ?body) ?clauses ...)
     (if (eq? (record-type ?var) ?type)
	 (let ((var ?var))
	   (let ((?field-var (record-ref var '?field-name)) ...) . ?body))
         (rcc ?var ?clauses ...)))
    ((rcc ?var (?type (?fields ...) . ?body) ?clauses ...)
     (if (eq? (record-type ?var) ?type)
         (let ((var ?var))
	   (let ((?fields (record-ref var '?fields)) ...) . ?body))
         (rcc ?var ?clauses ...)))))

(define-syntax record-case
  (syntax-rules ()
    ((record-case ?rec ?clauses ...)
     (let ((val ?rec))
       (rcc val ?clauses ...)))))

;;; INITIAL RECORD TYPES ======================================================

(define (make-initial-type name supers direct-slots slots methods)
  (let ((type (make-stob 7 #f)))
    (set-stob-class! type <<class>>)
    (stob-set! type 0 name)
    (stob-set! type 1 supers)
    (stob-set! type 2 direct-slots)
    (stob-set! type 3 slots)
    (stob-set! type 4 methods)
    (if (not (and (pair? supers) (eq? (car supers) <top>)))
	(error "An initial type should inherit from <top> only"))
    (stob-set! type 5 (list type <top>))
    (for-each (lambda (p) (add-subclass! p type)) supers)
    (stob-set! type 6 '())
    type))

(define <nil> (make-initial-type '<nil> (list <top>) '() '() '()))
(define <undefined> (make-initial-type '<undefined> (list <top>) '() '() '()))
(define <unbound> (make-initial-type '<unbound> (list <top>) '() '() '()))
(define <eof> (make-initial-type '<eof> (list <top>) '() '() '()))
(define <bool> (make-initial-type '<bool> (list <top>) '() '() '()))
(define <number> (make-initial-type '<number> (list <top>) '() '() '()))
(define <string> (make-initial-type '<string> (list <top>) '() '() '()))
(define <char> (make-initial-type '<char> (list <top>) '() '() '()))
(define <channel> (make-initial-type '<channel> (list <top>) '() '() '()))
(define <vector> (make-initial-type '<vector> (list <top>) '() '() '()))
(define <byte-vector> (make-initial-type '<byte-vector> (list <top>) '() '() '()))
(define <pair> (make-initial-type '<pair> (list <top>) '() '() '()))
(define <closure> (make-initial-type '<closure> (list <top>) '() '() '()))
(define <symbol> (make-initial-type '<symbol> (list <top>) '() '() '()))
(define <ref> (make-initial-type '<ref> (list <top>) '() '() '()))

(define <integer>
  (let ((class (make-stob 7 <<class>>)))
    (stob-set! class 0 '<integer>)
    (stob-set! class 1 (list <number>))
    (stob-set! class 2 (list))
    (stob-set! class 3 (list))
    (stob-set! class 4 '())
    (stob-set! class 5 (list class <number> <top>))
    (stob-set! class 6 '())
    (add-subclass! <number> class)
    class))

(define <real>
  (let ((class (make-stob 7 <<class>>)))
    (stob-set! class 0 '<real>)
    (stob-set! class 1 (list <number>))
    (stob-set! class 2 (list))
    (stob-set! class 3 (list))
    (stob-set! class 4 '())
    (stob-set! class 5 (list class <number> <top>))
    (stob-set! class 6 '())
    (add-subclass! <number> class)
    class))

(define (class-of obj)
  (cond ((stob? obj) (stob-class obj))
        ((%fixnum? obj) <integer>)
        ((%real? obj) <real>)
        ((string? obj) <string>)
        ((char? obj) <char>)
        ((channel? obj) <channel>)
        ((vector? obj) <vector>)
        ((bvec? obj) <byte-vector>)
	((null? obj) <nil>)
        ((pair? obj) <pair>)
	((generic? obj) <generic>)
        ((procedure? obj) <closure>)
        ((symbol? obj) <symbol>)
        ((ref? obj) <ref>)
        ((eq? obj #f) <bool>)
        ((eq? obj #t) <bool>)
        ((eq? obj $eof) <eof>)
	((eq? obj (unbound-object)) <unbound>)
	((eq? obj (unspecific-object)) <undefined>)
        (else (error "Unknown class of object ~a" obj))))

(define (instance? obj type)
  (cond
   ((record-type? type) (subtype? (class-of obj) type))
   ((type-singleton? type) (eq? obj (type-object type)))
   ((type-union? type) (any? (lambda (t) (instance? obj t))
			     (union-types type)))
   ((type-subclass? type) (and (record-type? obj)
			       (subtype? obj (type-class type))))
   (else #f)))

(define (subclass? c1 c2)
  (if (and (record-type? c1) (record-type? c2))
      (and (memq c2 (record-type-ancestors c1)) #t)
      (error "wrong argument to subclass, not classes ~a and ~a" c1 c2)))

(define (subtype-class? t1 t2)
  (cond ((record-type? t2) (subclass? t1 t2))
	((type-singleton? t2) #f)
	((type-subclass? t2) (and (eq? t1 <<class>>)
				  (eq? (type-class t2) <<class>>)))
	(else #f)))

(define (subtype-singleton? t1 t2)
  (cond ((record-type? t2) (instance? (type-object t1) t2))
	((type-singleton? t2) (eq? (type-object t1) t2))
	((type-subclass? t2) (and (instance? (type-object t1) <<class>>)
				  (subclass? (type-object t1) (type-class t2))))
	(else #f)))

(define (subtype-subclass? t1 t2)
  (cond
   ((type-singleton? t2) #f)
   ((type-subclass? t2) (subclass? (type-class t1) (type-class t2)))
   (else #f)))

(define (subtype-union? t1 t2)
  (cond
   ((type-union? t2) (all? (lambda (t) (subtype? t t2)) (union-types t1)))
   (else (all? (lambda (t) (subtype? t t2)) (union-types t1)))))

(define (subtype? t1 t2)
  (cond
   ((record-type? t1) (subtype-class? t1 t2))
   ((type-singleton? t1) (subtype-singleton? t1 t2))
   ((type-union? t1) (subtype-union? t1 t2))
   ((type-subclass? t1) (subtype-subclass? t1 t2))
   ((type-union? t1) (subtype-union? t1 t2))
   (else (error "bad arguments to subtype? ã ~a" t1 t2))))

;;; Generic

(define-syntax define-generic
  (syntax-rules ()
    ((define-generic ?name (?args ...))
     (define ?name (make-generic '?name '(?args ...))))))

(define-syntax defg
  (syntax-rules ()
    ((defg ?name) (?args ...))
    (define ?name (make-generic '?name '(?args ...)))))

(define (apply-meths meths args)
  (apply (car meths) (cons (lambda args (if (null? (cdr meths))
					    (error "no more next methods")
					    (apply-meths (cdr meths) args)))
			   args)))

(define (make-generic name formals)
  (let ((argmax (length formals))
	(cache (make-empty-cache 0))
	(methods '()))
    (lambda args
      (let ((entry (lookup-cache 0 argmax args cache)))
	(if entry
	    (apply-meths entry args)
	    (let* ((meths (applicable-methods methods args))
		   (sorted (sorted-applicable-methods meths)))
	      (if (null? sorted)
		  (begin
		    (debug-generic name args cache methods meths sorted)
		    (error "no applicable method for ~a with ~a" name args
			   cache methods))
		  (begin
		    (add-cache! 0 argmax args cache sorted)
		    (apply-meths sorted args)))))))))

(define (generic? obj)
  (and (procedure? obj) (eq? (procedure-ref obj 1)
			     (procedure-ref make-generic 1))))

(define (generic-name gen)
  (let ((env (procedure-ref gen 0)))
    (and (vector? env)
	 (let ((env* (vector-ref env 0)))
	   (and (vector? env*)
		(vector-ref env* 1))))))

(define (generic-args gen)
  (let ((env (procedure-ref gen 0)))
    (and (vector? env)
	 (let ((env* (vector-ref env 0)))
	   (and (vector? env*)
		(vector-ref env* 2))))))

(define (debug-generic name args cache methods meths sorted)
  (display "DEBUG GENERIC") (newline)
  (display (list name args cache meths sorted)) (newline)
  (for-each (lambda (m) (display (method-specs m)) (newline)) methods)
  (display (method-applicable? (car methods) args)) (newline)
  (display (instance? (car args) (car (method-specs (car methods))))) (newline)
  (display (class-of (car args))) (display (car (method-specs (car methods))))
  (display (eq? (class-of (car args)) (car (method-specs (car methods)))))
  (newline)
)


(define (set-generic-methods! generic methods)
  (vector-set! (procedure-ref generic 0) 1 methods))
(define (generic-methods generic)
  (vector-ref (procedure-ref generic 0) 1))
(define (set-generic-cache! generic cache)
  (vector-set! (procedure-ref generic 0) 2 cache))
(define (generic-cache generic)
  (vector-ref (procedure-ref generic 0) 2))

(define (method-applicable? meth args)
  (let applicable? ((types (method-specs meth))
		    (args args))
    (if (null? types)
	#t
	(and (pair? args)
	     (instance? (car args) (car types))
	     (applicable? (cdr types) (cdr args))))))

(define (method-more-specific? m1 m2)
  (let specific? ((s1 (method-specs m1))
                  (s2 (method-specs m2)))
    (if (null? s1)
        (if (null? s2)
	    #t
	    (error "incongruent methods ~a and ~a" m1 m2))
        (and (subtype? (car s1) (car s2))
             (specific? (cdr s1) (cdr s2))))))

(define (sorted-applicable-methods meths)
  (if (null? meths)
      '()
      (let sort ((meths (cdr meths))
                 (smallest (car meths))
                 (unordered '()))
        (if (null? meths)
            (cons smallest (sorted-applicable-methods unordered))
            (if (method-more-specific? smallest (car meths))
                (sort (cdr meths)
                      smallest
                      (cons (car meths) unordered))
                (sort (cdr meths)
                      (car meths)
                      (cons smallest unordered)))))))

(define (applicable-methods meths args)
  (if (null? meths)
      meths
      (let ((meth (car meths)))
        (if (method-applicable? meth args)
            (cons meth (applicable-methods (cdr meths) args))
            (applicable-methods (cdr meths) args)))))

(define (method-congruents? method generic)
  (and (= (length (method-specs method))
          (length (generic-args generic)))))
            
(define (generic-add-method! generic method)
  (if (method-congruents? method generic)
      (begin
        (set-generic-methods! generic
                              (add-method method
                                          (generic-methods generic)))
        (flush-generic-cache! generic)
        generic)
      (error "Method ~a is not compatible with generic ~a"
             method generic)))

(define (same-method? m1 m2)
  (let check ((s1 (method-specs m1))
              (s2 (method-specs m2)))
    (if (null? s1)
        #t
        (and (eq? (car s1) (car s2))
             (check (cdr s1) (cdr s2))))))

(define (add-method method methods)
  (cond ((null? methods) (list method))
        ((same-method? method (car methods))
         (cons method (cdr methods)))
        (else (cons (car methods)
                    (add-method method (cdr methods))))))

(define (flush-generic-cache! generic)
  (set-generic-cache! generic (make-empty-cache 0)))
			
  ;; (let ((specs (if (null? specs) (method-specs method) (car specs))))
  ;;   (set-generic-methods!
  ;;    generic
  ;;    (let lp ((meths (generic-methods generic)))
  ;;      (cond ((null? meths) '())
  ;; 	     ((equal? specs (method-specs (car meths)))
  ;; 	      (cons method (cdr meths)))
  ;; 	     (else (cons (car meths) (lp (cdr meths)))))))))

(define (method-specs meth) (vector-ref (procedure-ref meth 0) 1))

;; Note: if you modify this syntax definition, you should also change
;; the same definition in bootstrap.scm
(define-syntax define-method
  (lambda (e r c)
    (let ((%generic-add-method! (r 'generic-add-method!))
    	  (%let (r 'let))
    	  (%list (r 'list))
          (%lambda (r 'lambda))
	  (%top (r '<top>)))
      (let* ((generic (cadr e))
             (args (caddr e))
             (body (cdddr e))
    	     (specs (let map* ((args args))
		      (cond ((null? args) args)
			    ((pair? args)
			     (let ((arg (car args)))
			       (if (pair? arg)
				   (cons (cadr arg) (map* (cdr args)))
				   (cons %top (map* (cdr args))))))
			    (else '()))))
	     (arglist (cons 'next-method
			    (let map* ((args args))
			      (cond ((null? args) args)
				    ((pair? args)
				     (let ((arg (car args)))
				       (if (pair? arg)
					   (cons (car arg) (map* (cdr args)))
					   (cons arg (map* (cdr args))))))
				    (else args))))))
	`(,%generic-add-method! ,generic
				(let ((specs (,%list ,@specs)))
				  (,%lambda ,arglist ,@body))
				)))))

(define-syntax defm
  (syntax-rules ()
    ((defm ?name ?args . ?code)
     (define-method ?name ?args . ?code))))

(define <generic>
  (let ((class (make-stob 7 <<class>>)))
    (stob-set! class 0 '<generic>)
    (stob-set! class 1 (list class <top>))
    (stob-set! class 2 (list 'name 'args))
    (stob-set! class 3 (list 'name 'args))
    (stob-set! class 4 '())
    (stob-set! class 5 (list class <top>))
    (stob-set! class 6 '())
    (add-subclass! <top> class)
    class))

;;; Cache mechanism

(define (make-cache pos singletons entries) (vector 'cache pos singletons entries))
(define (cache-pos cache) (vector-ref cache 1))
(define (cache-singletons cache) (vector-ref cache 2))
(define (set-cache-singletons! cache singletons) (vector-set! cache 2 singletons))
(define (cache-classes cache) (vector-ref cache 3))
(define (set-cache-classes! cache classes) (vector-set! cache 3 classes))

(define (make-empty-cache n)
  (make-cache n (vector) (vector)))

(define (lookup-cache n max args cache)
  (cond ((= n max) cache)
	((< n (cache-pos cache))
	 (lookup-cache (+ n 1) max (cdr args) cache))
	(else
	 (or (lookup-entries n max (car args) (cdr args) (cache-singletons cache))
	     (lookup-entries n max (class-of (car args)) (cdr args) (cache-classes cache))))))

(define (lookup-entries n max arg args entries)
  (let lp ((i 0))
    (if (< i (vector-length entries))
        (let ((entry (vector-ref entries i)))
          (if (eq? arg entry)
              (lookup-cache (+ n 1) max args (vector-ref entries (+ i 1)))
              (lp (+ i 2))))
        #f)))

(define (spec-is-singleton? arg meths n)
  (and (pair? meths)
       (instance? (list-ref (method-specs (car meths)) n) <<singleton>>)))
      
(define (add-cache! n max args cache meths)
  ;;(display (list 'adding-cache n args cache meths)) (newline)
  (if (= n max)
      meths
      (let ((pos (cache-pos cache)))
	(if (< n pos)
	    (add-cache! (+ n 1) max (cdr args) cache meths)
	    (let ((arg (car args)))
	      (if (spec-is-singleton? arg meths n)
		  (let ((entry (add-cache-entry n max arg (cdr args) meths (cache-singletons cache))))
		    (set-cache-singletons! cache entry))
		  (let ((entry (add-cache-entry n max (class-of arg) (cdr args) meths (cache-classes cache))))
		    (set-cache-classes! cache entry)))
	      cache)))))
  
(define (add-cache-entry n max arg args meths entries)
  ;;(display (list 'add-cache-entry n args args)) (newline)
  (let lp ((i 0))
    (if (< i (vector-length entries))
        (let ((entry (vector-ref entries i)))
          (if (eq? arg entry)
              (begin
		(add-cache! (+ n 1) max args (vector-ref entries (+ i 1)) meths)
		entries)
              (lp (+ i 1))))
        (cache-extend entries arg
                       (add-cache! (+ n 1) max args (make-empty-cache (+ n 1)) meths)))))

(define (cache-extend vec key value)
  ;;(display (list 'cache-extend vec key value)) (newline)
  (let ((vec* (make-vector (+ (vector-length vec) 2) #f)))
    (let lp ((i 0))
      (if (< i (vector-length vec))
          (let ((v (vector-ref vec i)))
            (vector-set! vec* i v)
            (lp (+ i 1)))
          (begin
            (vector-set! vec* i key)
            (vector-set! vec* (+ i 1) value)
            vec*)))))

;;; Instance creation

(define-generic make (instance))
(define-method make ((class <<type>>)) (error "Don't know how to make a ~a" class))
(define-method make ((class <<class>>) . inits)
  ;; This is the default method for how to make instances
  ;; 1. we allocate the instance
  ;; 2. we initialize the provided slots
  ;; 3. we call initialize on the instance
  ;; 4. we return the new instance
  (let ((instance (make-record class)))
    (let init ((inits inits))
      (if (null? inits)
	  instance
	  (let ((field (car inits))
		(value (cadr inits)))
	    (record-set! instance field value)
	    (init (cddr inits)))))
    (apply initialize (cons instance inits))
;    (display "made instance") (display instance) (newline)
    instance))
  
(define-generic initialize (instance))
(define-method initialize ((instance <top>) . inits) instance)
(define-method initialize ((instance <<class>>) . inits)
  (record-set! instance 'fields
	       (remove-duplicates (compute-all-fields (list instance))))
  (record-set! instance 'ancestors
	       (record-type-ordered-ancestors instance))
  (record-set! instance 'subclasses '()))

(define (key-ref props key default)
  (cond ((null? props) default)
        ((null? (cdr props)) (error "bad property list ~a, odd length" props))
        (else (if (eq? key (car props))
                  (cadr props)
                  (key-ref (cddr props) key default)))))

(define-syntax let-keys
  (syntax-rules ()
    ((let-keys ?props ((?name ?init) ...) . ?code)
     (let* ((props ?props)
            (?name (key-ref props '?name ?init)) ...)
       . ?code))))
