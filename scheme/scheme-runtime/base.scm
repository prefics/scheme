;; base.scm -- standard pairs procedure as defined in rnrs, along with syntax !!!

;;; NUMERICAL

; (define-syntax +
;   (syntax-rules ()
;     (+ eta-+)
;     ((+ a) a)
;     ((+ a b) (+ a b))
;     ((+ a b bs ...) (+ a (+ b bs ...)))))

; (define-syntax -
;   (syntax-rules ()
;     (- eta--)
;     ((- a) (- 0 a))
;     ((- a b) (- a b))))

; (define-syntax *
;   (syntax-rules ()
;     (* eta-*)
;     ((* a b) (* a b))
;     ((* a b bs ...) (* a (* b bs ...)))))

;;;
;; Predicate returning #t if the `obj` is a number
(define (number? obj) (%fixnum? obj))

;;;
;; Make a pair containing the two arguments `h` and `t`
(define (cons h t) (make-pair h t))

;;;
;; This fonction is the composition of `car` and `car`
(define (caar   x) (car (car x)))
(define (cadr   x) (car (cdr x)))
(define (cdar   x) (cdr (car x)))
(define (cddr   x) (cdr (cdr x)))

(define (caaar  x) (car (car (car x))))
(define (caadr  x) (car (car (cdr x))))
(define (cadar  x) (car (cdr (car x))))
(define (caddr  x) (car (cdr (cdr x))))
(define (cdaar  x) (cdr (car (car x))))
(define (cdadr  x) (cdr (car (cdr x))))
(define (cddar  x) (cdr (cdr (car x))))
(define (cdddr  x) (cdr (cdr (cdr x))))

(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

;;;
;; List predicate returning #t if `l` is a proper list, i.e does not
;; contain cycle and is terminated by `'()`
(define (list? l)
  (let loop ((slow l)
             (fast l))
    (if (null? fast)
        #t
        (and (pair? fast)
             (if (null? (cdr fast))
                 #t
                 (and (pair? (cdr fast))
                      (not (eq? slow (cdr fast)))
                      (loop (cdr slow) (cddr fast))))))))

;;;
;; Constructs a list of all the arguments of the procedure.
(define (list . l) l)

;;;
;; Computes the length of `l`. Note that it works only on proper list
(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

;;;
;; Append all the lists (arguments) into one big list. Note that it
;; only works if the lists are proper, otherwise this procedure never
;; returns.
(define (append . l)
  (if (null? l)
      l
      (let loop ((l l))
        (if (null? (cdr l))
            (car l)
            (let loop2 ((l2 (car l)))
              (if (null? l2)
                  (loop (cdr l))
                  (cons (car l2) (loop2 (cdr l2)))))))))

;;;
;; Reverse the list `l`, i.e returns a list whose elements are the
;; same as the one from `l` but in reverse order.
(define (reverse l)
  (let loop ((l l)
             (rest '()))
    (if (null? l)
        rest
        (loop (cdr l) (cons (car l) rest)))))

;;;
;; Return the `k` pair of the list `l`. Note that this only works if
;; the length of `l` ist bigger than `k`. Otherwise a
;; `primitive-error` is raised.
(define (list-tail l k)
  (if (zero? k)
      l
      (list-tail (cdr l) (- k 1))))

;;;
;; Return the `k` element of the list `l`. The indexes start from
;; 0. Note that this only works if the length of the list is at least
;; `k`. Otherwise a `primitive-error` is raised.
(define (list-ref l k)
  (if (zero? k)
      (car l)
      (list-ref (cdr l) (- k 1))))

;;;
;; Return the pair whose car-cell is equal, in the meaning of `eq?`,
;; to `obj` if `obj` is a member of the list. Otherwise #f is
;; returned.
(define (memq obj l)
  (if (null? l)
      #f
      (if (eq? obj (car l))
          l
          (memq obj (cdr l)))))

;;;
;; Return the pair whose car-cell is equal, in the meaning of `eqv?`,
;; to `obj` if `obj` is a member of the list. Otherwise #f is
;; returned.
(define (memv obj l)
  (if (null? l)
      #f
      (if (eqv? obj (car l))
          l
          (memv obj (cdr l)))))

;;;
;; Return the pair whose car-cell is equal, in the meaning of `equal?`,
;; to `obj` if `obj` is a member of the list. Otherwise #f is
;; returned.
(define (member obj l)
  (if (null? l)
      #f
      (if (equal? obj (car l))
          l
          (member obj (cdr l)))))

;; (define (assq obj alist)
;;   (if (null? alist)
;;       #f
;;       (if (eq? obj (car (car alist)))
;;           (car alist)
;;           (assq obj (cdr alist)))))

;;;
;; Given an association list, returns the entry whose key is equal to
;; `obj`, equal in the sense of `eq?`.
(define (assq obj alist)
  (%assq obj alist))

;;;
;; Given an association list, returns the entry whose key is equal to
;; `obj`, equal in the sense of `eqv?`.
(define (assv obj alist)
  (if (null? alist)
      #f
      (if (eqv? obj (car (car alist)))
          (car alist)
          (assv obj (cdr alist)))))

;;;
;; Given an association list, returns the entry whose key is equal to
;; `obj`, equal in the sense of `equal?`.
(define (assoc obj alist)
  (if (null? alist)
      #f
      (if (equal? obj (car (car alist)))
          (car alist)
          (assoc obj (cdr alist)))))

;;;
;; == String procedures
;;
;; Strings are basically a sequence of characters indexed by numbers
;; starting at 0.

;;;
;; Constructs a string of length `k` containing only the characters
;; `char`. If `char` is not given, it defaults to the `#\space`
;; character.
(define (make-string k . char)
  (if (null? char)
      (make-string k #\space)
      (if (char? (car char))
    	  (make-string k (car char))
	  (error "make-string: initializer is not a char"))))

;;;
;; Constructs a string whose characters are `chars`.
(define (string . chars)
  (list->string chars))

;;;
;; Return if the two strings `string1` and `string2` contains the same
;; sequence of characters. If any of the argument `string1` or
;; `string2` are not strings, a `primitive-error` is signalled.
(define (string=? string1 string2)
  (let ((length1 (string-length string1))
        (length2 (string-length string2)))
    (if (= length1 length2)
        (let check ((i 0))
          (if (= i length1)
              #t
              (if (char=? (string-ref string1 i)
                          (string-ref string2 i))
                  (check (+ i 1))
                  #f)))
        #f)))

;;;
;; Return if the two strings `string1` and `string2` contains the same
;; sequence of characters without regards to case sensitivity. If any
;; of the argument `string1` or `string2` are not strings, a
;; `primitive-error` is signalled.
(define (string-ci=? string1 string2)
  (let ((length1 (string-length string1))
        (length2 (string-length string2)))
    (if (= length1 length2)
        (let check ((i 0))
          (if (= i length1)
              #t
              (if (char-ci=? (string-ref string1 i)
                             (string-ref string2 i))
                  (check (+ i 1))
                  #f)))
        #f)))

;;;
;; Return if `string1` is lexically smaller than `string2`. If any of
;; the argument `string1` or `string2` are not strings, a
;; `primitive-error` is signalled.
(define (string<? string1 string2)
  (let ((length1 (string-length string1))
        (length2 (string-length string2)))
    (let check ((i 0))
      (cond ((and (= i length1) (= i length2)) #f)
            ((= i length1) #t)
            ((= i length2) #f)
            (else
             (if (char=? (string-ref string1 i)
                         (string-ref string2 i))
                 (check (+ i 1))
                 (char<? (string-ref string1 i)
                         (string-ref string2 i))))))))

;;;
;; Return if `string1` is lexically greater than `string2`. If any of
;; the argument `string1` or `string2` are not strings, a
;; `primitive-error` is signalled.
(define (string>? string1 string2)
  (let ((length1 (string-length string1))
        (length2 (string-length string2)))
    (let check ((i 0))
      (cond ((and (= i length1) (= i length2)) #f)
            ((= i length1) #f)
            ((= i length2) #t)
            (else
             (if (char=? (string-ref string1 i)
                         (string-ref string2 i))
                 (check (+ i 1))
                 (char>? (string-ref string1 i)
                         (string-ref string2 i))))))))

;;;
;; Return if `string1` is lexically smaller or equal to `string2`. If
;; any of the argument `string1` or `string2` are not strings, a
;; `primitive-error` is signalled.
(define (string<=? string1 string2)
  (let ((length1 (string-length string1))
        (length2 (string-length string2)))
    (let check ((i 0))
      (cond ((and (= i length1) (= i length2)) #t)
            ((= i length1) #t)
            ((= i length2) #f)
            (else
             (if (char=? (string-ref string1 i)
                         (string-ref string2 i))
                 (check (+ i 1))
                 (char<? (string-ref string1 i)
                         (string-ref string2 i))))))))

;;;
;; Return if `string1` is lexically greater or equal to `string2`. If
;; any of the argument `string1` or `string2` are not strings, a
;; `primitive-error` is signalled.
(define (string>=? string1 string2)
  (let ((length1 (string-length string1))
        (length2 (string-length string2)))
    (let check ((i 0))
      (cond ((and (= i length1) (= i length2)) #t)
            ((= i length1) #f)
            ((= i length2) #t)
            (else
             (if (char=? (string-ref string1 i)
                         (string-ref string2 i))
                 (check (+ i 1))
                 (char>? (string-ref string1 i)
                         (string-ref string2 i))))))))

;;;
;; Return if `string1` is lexically smaller than `string2` without
;; regards to the case of the character. If any of the argument
;; `string1` or `string2` are not strings, a `primitive-error` is
;; signalled.
(define (string-ci<? string1 string2)
  (let ((length1 (string-length string1))
        (length2 (string-length string2)))
    (let check ((i 0))
      (cond ((and (= i length1) (= i length2)) #f)
            ((= i length1) #t)
            ((= i length2) #f)
            (else
             (if (char-ci=? (string-ref string1 i)
                            (string-ref string2 i))
                 (check (+ i 1))
                 (char-ci<? (string-ref string1 i)
                            (string-ref string2 i))))))))

;;;
;; Return if `string1` is greater than `string2` without regards to
;; the case of the character. If any of the argument `string1` or
;; `string2` are not strings, a `primitive-error` is signalled.
(define (string-ci>? string1 string2)
  (let ((length1 (string-length string1))
        (length2 (string-length string2)))
    (let check ((i 0))
      (cond ((and (= i length1) (= i length2)) #f)
            ((= i length1) #f)
            ((= i length2) #t)
            (else
             (if (char-ci=? (string-ref string1 i)
                            (string-ref string2 i))
                 (check (+ i 1))
                 (char-ci>? (string-ref string1 i)
                            (string-ref string2 i))))))))

;;;
;; Return if `string1` is lexically smaller or equal to `string2`
;; without regards to the case of the character. If any of the
;; argument `string1` or `string2` are not strings, a
;; `primitive-error` is signalled.
(define (string-ci<=? string1 string2)
  (let ((length1 (string-length string1))
        (length2 (string-length string2)))
    (let check ((i 0))
      (cond ((and (= i length1) (= i length2)) #t)
            ((= i length1) #t)
            ((= i length2) #f)
            (else
             (if (char-ci=? (string-ref string1 i)
                            (string-ref string2 i))
                 (check (+ i 1))
                 (char-ci<? (string-ref string1 i)
                            (string-ref string2 i))))))))

;;;
;; Return if `string1` is lexically greater or equal to `string2`
;; without regards to the case of the character. If any of the
;; argument `string1` or `string2` are not strings, a
;; `primitive-error` is signalled.
(define (string-ci>=? string1 string2)
  (let ((length1 (string-length string1))
        (length2 (string-length string2)))
    (let check ((i 0))
      (cond ((and (= i length1) (= i length2)) #t)
            ((= i length1) #f)
            ((= i length2) #t)
            (else
             (if (char-ci=? (string-ref string1 i)
                            (string-ref string2 i))
                 (check (+ i 1))
                 (char-ci>? (string-ref string1 i)
                            (string-ref string2 i))))))))

;;;
;; computes the substring of `string` between the index `start` and
;; `end`. Note that `start` and `end` should already be ordered and
;; `string` should be a string.
(define (substring string start end)
  (if (>= start 0)
      (if (<= end (string-length string))
          (let ((new-string (make-string (- end start) #\1)))
            (let copy ((i start))
              (if (< i end)
                  (begin
                    (string-set! new-string (- i start) (string-ref string i))
                    (copy (+ i 1)))
                  new-string)))
	  (error "SUBSTRING: END value longer than string" string start end))
      (error "SUBSTRING: START value before the beginning" string start end)))
      

;;;
;; Convert the number is string representation and return the integer
;; having the same value. The base of the represenation can be
;; specified with the optional argument `base`. The default value is 10. 
(define (string->number str . base)
  (let* ((base (if (null? base) 10 (car base)))
         (negative? (char=? #\- (string-ref str 0))))
    (let loop ((i (if negative? 1 0))
               (n 0))
      (if (< i (string-length str))
          (let ((c (string-ref str i)))
            (if (char-numeric? c)
                (loop (+ i 1) (+ (* base n)
                                 (- (char->integer c) (char->integer #\0))))
                n))
          n))))

;;;
;; Return the concatenation of `strings`.
(define (string-append . strings)
  (let ((length (let loop ((s strings))
                  (if (null? s)
                      0
                      (+ (string-length (car s))
                         (loop (cdr s)))))))
    (if (= length 0)
        (make-string 0 #\1)
	(let ((new-string (make-string length #\1)))
	  (let loop ((current (car strings))
		     (rest (cdr strings))
		     (i 0)
		     (j 0)
		     (upto (string-length (car strings))))
	    (if (< i length)
		(if (< j upto)
		    (begin
		      (string-set! new-string i (string-ref current j))
		      (loop current rest (+ i 1) (+ j 1) upto))
		    (loop (car rest)
			  (cdr rest) i 0
			  (string-length (car rest))))))
	  new-string))))

;;;
;; Return the list of the characters contained in `string`.
(define (string->list string)
  (let ((length (string-length string)))
    (let loop ((i 0))
      (if (< i length)
          (cons (string-ref string i)
                (loop (+ i 1)))
          '()))))

;;;
;; Return the string containing all the characters in the list `l` in
;; the same order. Note that a `primitive-error` is signalled if the
;; list is not made of characters.
(define (list->string l)
  (let ((new-string (make-string (length l) #\1)))
    (let loop ((l l)
               (i 0))
      (if (null? l)
          new-string
          (begin
            (string-set! new-string i (car l))
            (loop (cdr l) (+ i 1)))))))

;;;
;; Return a copy of the string `string` passed as argument.
(define (string-copy string)
  (let ((length (string-length string)))
    (let ((new-string (make-string length #\1)))
      (let copy ((i 0))
        (if (< i length)
            (begin
              (string-set! new-string i (string-ref string i))
              (copy (+ i 1)))
            new-string)))))

;;;
;; Fill `string` with the character `char`. Note that it is
;; destructive operation for `string`.
(define (string-fill! string char)
  (let ((length (string-length string)))
    (let fill ((i 0))
      (if (< i length)
          (begin
            (string-set! string i char)
            (fill (+ i 1)))))))

;;;
;; Convert the integer `number` to a string. If `number` is not a
;; number the string ##NaN is returned.
(define $digits "0123456789abcdefghijklmnopqrstuvwxyz")

(define (number->string number . base)
  (let ((base (if (null? base)
                  10
                  (car base))))
    (cond ((%fixnum? number)
           (list->string
            (let ((digits
                   (reverse
                    (let loop ((n (if (< number 0) (- 0 number) number)))
                      (if (< n base)
                          (list (string-ref $digits n))
                          (let ((this (string-ref $digits (modulo n base)))
                                (rest (loop (quotient n base))))
                          (cons this rest)))))))
              (if (< number 0) (cons #\- digits) digits))))
          ((%real? number) (%real->string number))
          (else (error "Bad argument ~a to NUMBER->STRING, expected a number")))))

;;;
;; == Character procedures
;;
;; Characters are taken from the ISO-8859-1 set of characters and are
;; coded on 8-bit.

;;;
;; Return #t if the characters `char1` and `char2` are the same.
(define (char=? char1 char2)
  (and (char? char1)
       (char? char2)
       (eq? char1 char2)))

;;;
;; Return #t if character `char1` is lexically smaller than
;; `char2`. If either `char1` or `char2` are not characters, a
;; `primitive-error` is signalled.
(define (char<? char1 char2)
  (< (char->integer char1) (char->integer char2)))

;;;
;; Return #t if character `char1` is lexically greater than
;; `char2`. If either `char1` or `char2` are not characters, a
;; `primitive-error` is signalled.
(define (char>? char1 char2)
  (> (char->integer char1) (char->integer char2)))

;;;
;; Return #t if character `char1` is lexically smaller or equal to
;; `char2`. If either `char1` or `char2` are not characters, a
;; `primitive-error` is signalled.
(define (char<=? char1 char2)
  (<= (char->integer char1) (char->integer char2)))

;;;
;; Return #t if character `char1` is lexically greater or equal to
;; `char2`. If either `char1` or `char2` are not characters, a
;; `primitive-error` is signalled.
(define (char>=? char1 char2)
  (>= (char->integer char1) (char->integer char2)))

;;;
;; Return #t if character `char1` and `char2` are the same without
;; regards to their case. If either `char1` or `char2` are not
;; characters, a `primitive-error` is signalled.
(define (char-ci=? char1 char2)
  (char=? (char-downcase char1)
          (char-downcase char2)))

;;;
;; Return #t if character `char1` is lexically smaller than `char2`
;; without regards to their case. If either `char1` or `char2` are not
;; characters, a `primitive-error` is signalled.
(define (char-ci<? char1 char2)
  (char<? (char-downcase char1)
          (char-downcase char2)))

;;;
;; Return #t if character `char1` is lexically greater than `char2`
;; without regards to their case. If either `char1` or `char2` are not
;; characters, a `primitive-error` is signalled.
(define (char-ci>? char1 char2)
  (char>? (char-downcase char1)
          (char-downcase char2)))

;;;
;; Return #t if character `char1` is lexically smaller or equal to
;; `char2` without regards to their case. If either `char1` or `char2`
;; are not characters, a `primitive-error` is signalled.
(define (char-ci<=? char1 char2)
  (char<=? (char-downcase char1)
           (char-downcase char2)))

;;;
;; Return #t if character `char1` is lexically greater or equal to
;; `char2` without regards to their case. If either `char1` or `char2`
;; are not characters, a `primitive-error` is signalled.
(define (char-ci>=? char1 char2)
  (char>=? (char-downcase char1)
           (char-downcase char2)))

;;;
;; Return #t if `char` is an alphabetical character that is in the
;; range #\a to #\z or the range #\A to #\Z. If `char` is not a
;; character, #f is returned.
(define (char-alphabetic? char)
  (and (char? char)
       (or (and (char<=? #\a char)
                (char<=? char #\z))
           (and (char<=? #\A char)
                (char<=? char #\Z)))))

;;;
;; Return #t if `char` is a numeric character that is in the range of
;; #\0 to #\9. If `char` is not a charater, #f is returned.
(define (char-numeric? char)
  (and (char? char)
       (char<=? #\0 char)
       (char<=? char #\9)))

(define $tab (number->char 9))
(define $linefeed (number->char 10))
(define $vtab (number->char 11))
(define $formfeed (number->char 12))
(define $newline (number->char 13))

;;;
;; Return #t if `char` is a white space character. White space
;; characters are #\space, #\tab, #\linefeed, #\formfeed or
;; #\newline. Note that if `char` is not a character, a
;; `primitive-error` is signalled.
(define (char-whitespace? char)
  (or (char=? char #\space)
      (char=? char $tab)
      (char=? char $linefeed)
      (char=? char $formfeed)
      (char=? char $newline)))

;;;
;; Return #t if `letter` is an upper case character, that is in the
;; range #\A to #\Z. If `letter` is not a character, a
;; `primitive-error` is signalled.
(define (char-upper-case? letter)
  (and (char<=? #\A letter)
       (char<=? letter #\Z)))

;;;
;; Return #t if `letter` is an upper case character, that is in the
;; range #\a to #\z. If `letter` is not a character, a
;; `primitive-error` is signalled.
(define (char-lower-case? letter)
  (and (char<=? #\a letter)
       (char<=? letter #\z)))

;;;
;; Return the character code of `char` in the ISO-8859-1 encoding. 
(define (char->integer char)
  (char->number char))

;;;
;; Return the character associated with the character code `n` in the
;; ISO-8859-1 encoding.
(define (integer->char n)
  (number->char n))

;;;
;; Return the the upper case character of `char` if it is a lower case
;; letter. Otherwise return `char`.
(define (char-upcase char)
  (if (char-lower-case? char)
      (let ((delta (- (char->integer #\A) (char->integer #\a))))
        (integer->char (+ delta (char->integer char))))
      char))

;;;
;; Return the the lower case character of `char` if it is an upper case
;; letter. Otherwise return `char`.
(define (char-downcase char)
  (if (char-upper-case? char)
      (let ((delta (- (char->integer #\a) (char->integer #\A))))
        (integer->char (+ delta (char->integer char))))
      char))

;;;
;; The equivalent predicate `eqv?`
(define eqv?
  (lambda (o1 o2)
    (eq? o1 o2)))

;;;
;; Return if `x` and `y` are equal structurally speaking.
(define (equal? x y)
  (cond
   ((eqv? x y) #t)
   ((pair? x) (and (pair? y) (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
   ((string? x) (and (string? y) (string=? x y)))
   ((vector? x)
    (and (vector? y)
         (= (vector-length x) (vector-length y))
         (let loop ((i (- (vector-length x) 1)))
           (if (< i 0)
               #t
               (and (equal? (vector-ref x i) (vector-ref y i))
                    (loop (- i 1)))))))
   ((bvec? x)
    (and (bvec? y) (= x y)))
   (else #f)))

;;;
;; Return the boolean complement of `obj`.
(define (not obj)
  (if obj #f #t))

;;;
;; Return #t if `obj` is a boolean value, that is either #t or #f.
(define (boolean? obj)
  (or (eq? obj #t)
      (eq? obj #f)))

;(define (apply proc . args)
;  (declare (primitive apply))
;  (apply proc args)

;(define (all-car . l)
;  (let loop ((l l))
;    (if (null? l)
;        '()
;        (cons (caar l) (loop (cdr l))))))

;(define (all-cdr . l)
;  (let loop ((l l))
;    (if (null? l)
;        '()
;        (cons (cdar l) (loop (cdr l))))))

;(define (map proc . lists)
;  (let loop ((lists lists))
;    (if (null? (car lists))
;        '()
;        (cons (apply proc (all-car lists))
;              (loop (all-cdr lists))))))

;;;
;; Constucts the list whose elements are composed of the application of the
;; procedure `proc` to the elements of `l`.
(define (map proc l)
  (if (null? l)
      l
      (cons (proc (car l)) (map proc (cdr l)))))

;(define (for-each proc . lists)
;  (let loop ((lists lists))
;    (if (null? (car lists))
;        '()
;        (begin
;          (apply proc (all-car lists))
;          (loop (all-cdr lists))))))

;;;
;; Calls the procedure `proc` to each of the element of `l` in their
;; order of appearence in `l`.
(define (for-each proc l)
  (if (null? l)
      'done
      (begin
        (proc (car l))
        (for-each proc (cdr l)))))

;(define (force promise)
;  #f)

;(define (call-with-current-continuation proc)
;  #f)

;;;
;; == Vector procedures
;;
;; Vectors are a sequence of objects indexed by integer starting from
;; 0. Note that the elements are accessed in O(1) time.

;;;
;; Return the vector containing all the `objs` elements.
(define (vector . objs)
  (list->vector objs))

;;;
;; Compute the list containing the same elements of `vector` in the
;; same order.
(define (vector->list vector)
  (let ((length (vector-length vector)))
    (let loop ((i 0))
      (if (< i length)
          (cons (vector-ref vector i)
                (loop (+ i 1)))
          '()))))

;;;
;; Compute the vector containing the same elements of `l` in the same
;; order.
(define (list->vector l)
  (let ((new-vector (make-vector (length l) 1)))
    (let loop ((i 0)
               (l l))
      (if (null? l)
          new-vector
          (begin
            (vector-set! new-vector i (car l))
            (loop (+ i 1) (cdr l)))))))

;;;
;; Fill the vector `vector` with the element `fill`.
(define (vector-fill! vector fill)
  (let ((length (vector-length vector)))
    (let fill ((i 0))
      (if (< i length)
          (begin
            (vector-set! vector i fill)
            (fill (+ i 1)))))))

;;;
;; == Specific predefined values

;;;
;; Return the unbound object.
(define (unbound-object) 'the-unbound-object)

;;;
;; Return #t if `o` is the unbound object.
(define (unbound-object? o) (eq? o (unbound-object)))

;;;
;; Return the unspecific object.
(define (unspecific-object) 'the-unspecific-object)

;;;
;; Return #t if `o` is the unspecific object.
(define (unspecific-object? o) (eq? o (unspecific-object)))

;;;
;; == Environment procedures 

;;;
;; Return an environment compatible with the revision `n` of the Scheme
;; report.
(define (rnrs-environment n)
  (if (= n 5)
      (lookup-module 'scheme-runtime)
      (error "RnRS ~a environment does not exists" n)))

(define *eval* (lambda (exp env) (error "Eval not available")))

;;;
;; Evaluates the Scheme expression according to the environment `env`.
(define (eval exp env)
  (with-current-module env
    (lambda () (*eval* exp env))))

;;;
;; Sets `proc` as the evaluation procedure to use. `proc` should be a
;; procedure of two arguments. The first is the expression that should
;; be evaluated. The second is the environment in which to evaluate
;; the expression.
(define (set-eval-procedure proc)
  (set! *eval* proc))
