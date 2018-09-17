;;; runtime.scm -- Scheme48 runtime module definition

(define-structure scheme-runtime
  (export pair? cons car cdr set-car! set-cdr!
	  caar cadr cdar cddr 
	  caaar caadr cadar caddr cdaar cdadr cddar cdddr
	  caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	  cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	  null? list? list length append reverse list-tail list-ref
	  memq memv member assq assv assoc
	  
	  string? make-string string string-length string-ref string-set!
	  string=? string-ci=? string<? string>? string<=? string>=?
	  string-ci<? string-ci>? string-ci<=? string-ci>=? substring
	  string-append string->list list->string string-copy 
	  string-fill! number->string
	  
	  char? char=? char<? char>? char<=? char>=? 
	  char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
	  char-alphabetic? char-numeric? char-whitespace?
	  char-upper-case? char-lower-case? char->integer integer->char
	  char-upcase char-downcase
	  
	  eqv? equal? not boolean?

	  apply map for-each 
;          force delay call-with-current-continuation
	  
	  call-with-input-file call-with-output-file
	  with-input-from-file with-output-to-file with-output-appended-to-file
	  open-input-file open-output-file
	  close-input-port close-output-port
	  read eof-object? char-ready? peek-char read-char
	  write display newline write-char 
	  load transcript-on transcript-off
          current-input-port current-output-port
	  
	  vector vector->list list->vector vector-fill!

          ;; special values
          unbound-object unbound-object?
          unspecific-object unspecific-object?

          ;;; extensions

          ; record.scm
          (define-record-type :syntax)

          ; fluids.scm
          fluid let-fluid set-fluid! make-fluid fluid?

          ;io.scm
          initialize-i/o!

          ; modules
          *modules* make-module module? module/name set-module/name!
          module/export set-module/export! module/open set-module/open!
          module/doc set-module/doc! module/defined set-module/defined!
          module/syntax set-module/syntax!
          modules module-names 
	  bind-module! unbind-module! lookup-module lookup-ref lookup/create-ref
          bind-ref! unbind-ref!
          make-ref ref/name ref/module ref/value set-ref/name! set-ref/module! set-ref/value! ref?
          bind-syntax! unbind-syntax! lookup-syntax
	  make-syntax syntax? syntax/env syntax/transformer
	  make-primitive primitive? primitive/name primitive/transformer

	  with-current-module current-module current-module-name

	  ; hashtable.scm
	  make-hashtable hash-ref hash-set!

          ; conditions.scm
	  (condition-case :syntax)
	  
	  describe-condition default-handler
	  <condition> condition? make-condition
	  <simple-condition> make-simple-condition simple-condition? condition-message condition-arguments
	  <serious-condition> make-serious-condition serious-condition
	  <error> make-error error?
	  <simple-error> make-simple-error simple-error?
	  
	  <call-error> make-call-error call-error? call-error-function call-error-arguments
	  <arity-error> make-arity-error arity-error?
	  <type-error> make-type-error type-error?
	  
	  <unbound-global-error> make-unbound-global-error unbound-global-error? unbound-global-error-variable
	  <primitive-error> make-primitive-error primitive-error?
	  
	  <restart> make-restart restart-description
	  for-each-handler
	  signal
	  error
	  initialize-conditions!
	  add-signal-hook! remove-signal-hook! with-signal-hook
	  signal-hooks
	  
	  eval set-eval-procedure rnrs-environment
          )
  (open compat)
  (files (scheme-runtime base)
;         (runtime boo)
	 (scheme-runtime s48-record)
         (scheme-runtime fluids)
	 (scheme-runtime conditions)
         (scheme-runtime io)
	 (scheme-runtime hashtable)
	 (scheme-runtime module)))
