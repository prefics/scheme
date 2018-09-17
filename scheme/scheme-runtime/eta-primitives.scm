;; eta-primitive.scm -- primitives as procedures

(define (eq? a b) (eq? a b))

(define (pair? o)  (pair? o))
(define (car p) (car p))
(define (cdr p) (cdr p))
(define (set-car! p o) (set-car! p o))
(define (set-cdr! p o) (set-cdr! p o))
(define (null? l) (null? l))

(define (string? obj) (string? obj))
(define (string-length string) (string-length string))
(define (string-ref string k) (string-ref string k))
(define (string-set! string k char) (string-set! string k char))

(define (char? obj) (char? obj))

(define (make-vector length . init) 
  (if (null? init) 
      (make-vector length 'undefined) 
      (make-vector length (car init))))
(define (vector? o) (vector? o))
(define (vector-length v) (vector-length v))
(define (vector-ref v i) (vector-ref v i))
(define (vector-set! v i o) (vector-set! v i o))

(define (symbol? o) (symbol? o))
(define (symbol->string o) (symbol->string o))
(define (string->symbol o) (string->symbol o))

(define (make-procedure size) (make-procedure size))
(define (procedure? o) (procedure? o))
(define (procedure-set! p i o) (procedure-set! p i o))
(define (procedure-ref p i) (procedure-ref p i))
(define (procedure-length p) (procedure-length p))
(define (apply proc args) (apply proc args))

(define (quotient a b) (quotient a b))
(define (remainder a b) (remainder a b))
(define (modulo a b) (modulo a b))
(define (< a b) (< a b))
(define (<= a b) (<= a b))
(define (= a b) (= a b))
(define (> a b) (> a b))
(define (>= a b) (>= a b))

(define (+ a b) (+ a b))
(define (- a b) (- a b))
(define (* a b) (* a b))
(define (/ a b) (/ a b))

(define (make-bvec size) (make-bvec size))
(define (bvec? o) (bvec? o))
(define (bvec-ref bvec i) (bvec-ref bvec i))
(define (bvec-set! bvec i o) (bvec-set! bvec i o))
(define (bvec-length bvec) (bvec-length bvec))

(define (eof-object? o) (eof-object? o))

(define (close-socket socket)
  (close-channel socket))

(define (make-channel num)
  (make-channel num))

(define (write-channel channel str/bvec start end)
  (write-channel channel str/bvec start end))

(define (read-channel channel)
  (read-channel channel))

(define (open-input-channel str)
  (open-input-channel str))

(define (open-output-channel str mode)
  (open-output-channel str mode))

(define (channel? obj)
  (channel? obj))

(define (channel-number channel)
  (channel-number channel))

(define (close-channel channel)
  (close-channel channel))

(define (peek-channel channel)
  (peek-channel channel))

(define (bit-or v1 v2)
  (bit-or v1 v2))

(define (bit-and v1 v2)
  (bit-and v1 v2))

(define (bit-xor v1 v2)
  (bit-xor v1 v2))

(define (bit-not v)
  (bit-not v))

(define (bit-ash v1 v2)
  (bit-ash v1 v2))

(define (object-hash string/symbol/keyword)
  (object-hash string/symbol/keyword))

(define (suspend-cc proc) (suspend-cc proc))
(define (resume-cc cont val) (resume-cc cont val))

(define (ref? obj) (ref? obj))
(define (make-ref name module value) (make-ref name module value))
(define (ref-name ref) (ref-name ref))
(define (ref-module ref) (ref-module ref))
(define (ref-value ref) (ref-value ref))
(define (set-ref-name! ref name) (set-ref-name! ref name))
(define (set-ref-module! ref name) (set-ref-module! ref name))
(define (set-ref-value! ref name) (set-ref-value! ref name))

(define (channel-read channel string size)
  (posix-read channel string size))

(define (channel-write channel string size)
  (posix-write channel string size))

(define (posix-select read write timeout)
  (posix-select read write timeout))

(define (native-call code env) (%native-call code env))

(define (%ffi-open lib) (%ffi-open lib))
(define (%ffi-error) (%ffi-error))
(define (%ffi-sym lib name) (%ffi-sym lib name))
(define (%ffi-close lib) (%ffi-close lib))
(define (%ffi-apply proc args) (%ffi-apply proc args))
(define (%ffi-mem-ref mem offset length) (%ffi-mem-ref mem offset length))
(define (%ffi-string-ref mem) (%ffi-string-ref mem))
(define (%ffi-mem-set! dst src offset) (%ffi-mem-set! dst src offset))
(define (%ffi-mirror obj) (%ffi-mirror obj))
(define (%ffi-deref mem offset length) (%ffi-deref mem offset length))
(define (%ffi-malloc size) (%ffi-malloc size))
(define (%ffi-free ptr) (%ffi-free ptr))
(define (%ffi-u8-ref ptr) (%ffi-u8-ref ptr))
(define (%ffi-u16-ref ptr) (%ffi-u16-ref ptr))
(define (%ffi-u32-ref ptr) (%ffi-u32-ref ptr))
(define (%ffi-u64-ref ptr) (%ffi-u64-ref ptr))
(define (%ffi-double-ref ptr) (%ffi-double-ref ptr))

(define (%test+set! pair old new) (%test+set! pair old new))

(define (stob-ref stob i) (stob-ref stob i))
(define (stob-class stob) (stob-class stob))

(define (%assq obj alist) (%assq obj alist))
(define (%record-ref obj field) (%record-ref obj field))
(define (%record-set! obj field val) (%record-set! obj field val))

(define (%host-error) (%host-error))
(define (%error=? e1 e2) (%error=? e1 e2))

(define (%set-timer sec usec) (%set-timer sec usec))

(define (%time) (posix-time))

(define (%posix-getcwd) (posix-getcwd))
(define (%posix-stat fn rec) (posix-stat fn rec))

