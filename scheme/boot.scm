;;; initial image maker

(begin
  (define *stdin* (make-channel 0))
  (define *stdout* (make-channel 1))
  (define *stderr* (make-channel 2))

  (define (message text)
    (write-channel *stdout* text 0 (string-length text)))

  (define $eof 'the-end-of-file-object)

  (define (list . l) l)

;  (define (write-channel n m) (display m))
  (message "Bootstrapping Schemy 1.0...")

  ;;; Module
  ;;
  ;; Predefined module system. It will be replaced later once the real
  ;; module is loaded.

  (define *defs* '())
  (define *syntaxes* 'scheme-runtime)
  (define (lookup-ref name module-name)
;    (write-channel *stdout* "looking ref:")
;    (write-channel *stdout* name)
    (let loop ((refs *defs*))
      (if (null? refs)
          (let ((new (make-ref name module-name #f)))
;            (write-channel *stdout* "making new ref")
;            (write-channel *stdout* new)
            (set! *defs* (make-pair new *defs*))
            new)
          (if (eq? name (ref-name (car refs)))
              (begin
;                (write-channel *stdout* "reusing ref")
;                (write-channel *stdout* (car refs))
                (car refs))
              (loop (cdr refs))))))

  (define (make-syntax transformer env)
    (let ((vec (make-vector 4 #f)))
      (vector-set! vec 0 'syntax)
      (vector-set! vec 1 transformer)
      (vector-set! vec 2 env)
      vec))

  (define bind-syntax!
    (lambda (name syntax module)
      (set! *syntaxes* (make-pair (make-pair name syntax) *syntaxes*))))

  (define (module/syntax module)
    *syntaxes*)

;;; load predefined FASL

  (define *fasls* '("scheme/runtime.fasl"
                    "scheme/module.fasl"
                    "scheme/syntax.fasl"
                    "scheme/compiler.fasl"
                    "scheme/repl.fasl"))

;;; FASL reader

  (define fasl/fixnum     0)
  (define fasl/true       1)
  (define fasl/false      2)
  (define fasl/char       3)
  (define fasl/pair       4)
  (define fasl/string     5)
  (define fasl/vector     6)
  (define fasl/bvec       7)
  (define fasl/nil        8)
  (define fasl/unspecific 9)
  (define fasl/unbound    10)
  (define fasl/symbol     11)
  (define fasl/ref        12)
  (define fasl/eof        14)

  (define type/definition 0)
  (define type/syntax     1)
  (define type/expr       2)

  (define (read-byte channel)
    (let ((byte (read-channel channel)))
      (if (eof-object? byte)
          byte
          (char->number byte))))

  (define read-long
    (lambda (channel)
      (let ((h1 (char->number (read-channel channel)))
	    (h2 (char->number (read-channel channel)))
	    (h3 (char->number (read-channel channel)))
	    (h4 (char->number (read-channel channel))))
	(+ (* (+ (* (+ (* 256 h1)
		       h2)
		    256)
		 h3)
	      256)
	   h4))))

  (define read-counted-string
    (lambda (channel)
      (let ((size (read-long channel)))
        (let ((str (make-string size #\1)))
          (let ((loop #f))
            (set! loop
                  (lambda (i)
                    (if (< i size)
                        (begin
;                         (write-channel *stdout* i)
;                         (write-channel *stdout* "+str+")
                          (string-set! str i (read-channel channel))
                          (loop (+ i 1)))
			str)))
            (loop 0))))))

  (define *fasl-reader* (make-vector 15 (lambda () 'undefined-fasl-reader)))
  (define define-fasl-reader
    (lambda (type body)
      (vector-set! *fasl-reader* type body)
      (message ".")))

  (define-fasl-reader fasl/eof
    (lambda (channel) $eof))

  (define-fasl-reader fasl/unbound
    (lambda (channel) 'the-unbound-object))

  (define-fasl-reader fasl/unspecific
    (lambda (channel) 'the-unspecific-object))

  (define-fasl-reader fasl/fixnum
    (lambda (channel)
;      (write-channel *stdout* "fx")
      (read-long channel)))
  (define-fasl-reader fasl/true
    (lambda (channel)
;      (write-channel *stdout* "#t")
      #t))
  (define-fasl-reader fasl/false
    (lambda (channel)
;      (write-channel *stdout* "#f")
      #f))
  (define-fasl-reader fasl/char
    (lambda (channel)
;      (write-channel *stdout* "char")
      (read-channel channel)))
  (define-fasl-reader fasl/pair
    (lambda (channel)
;      (write-channel *stdout* "pair")
      (let ((head (read-fasl channel)))
        (let ((tail (read-fasl channel)))
      (make-pair head tail)))))
  (define-fasl-reader fasl/string read-counted-string)

  (define-fasl-reader fasl/vector
    (lambda (channel)
;      (write-channel *stdout* "vector")
      (let ((size (read-long channel)))
        (let ((vec (make-vector size #f)))
          (let ((loop #f))
            (set! loop (lambda (index)
                         (if (< index size)
                             (begin
;                               (write-channel *stdout* "+vec+")
                               (vector-set! vec index (read-fasl channel))
                               (loop (+ index 1)))
			     vec)))
            (loop 0))))))
  (define-fasl-reader fasl/bvec
    (lambda (channel)
;      (write-channel *stdout* "bvec")
      (let ((size (read-long channel)))
;        (write-channel *stdout* size)
        (let ((bvec (make-bvec size)))
          (let ((loop #f))
            (set! loop (lambda (index)
                         (if (< index size)
                             (begin
                               (bvec-set! bvec index (read-channel channel))
                               (loop (+ index 1)))
			     bvec)))
            (loop 0))))))
  (define-fasl-reader fasl/nil
    (lambda (channel)
;      (write-channel *stdout* "nil")
      '()))
  (define-fasl-reader fasl/symbol
    (lambda (channel)
;      (write-channel *stdout* "symbol")
      (string->symbol (read-counted-string channel))))
;       (let ((size (read-long channel)))
;         (let ((str (make-string size #\1)))
;           (let ((loop #f))
;             (set! loop
;                   (lambda (i)
;                     (if (< i size)
;                         (begin
;                           (string-set! str i (read-channel channel))
;                           (loop (+ i 1)))
;                         (string->symbol str))))
;             (loop 0))))))

  (define-fasl-reader fasl/ref
    (lambda (channel)
;      (write-channel *stdout* "ref")
      (let ((name (string->symbol (read-counted-string channel))))
	(let ((module (string->symbol (read-counted-string channel))))
	  (lookup-ref name module)))))

  (define read-fasl
    (lambda (channel)
      (let ((byte (read-channel channel)))
        (if (eof-object? byte)
            byte
            (let ((index (char->number byte)))
              (if (< index (vector-length *fasl-reader*))
                  ((vector-ref *fasl-reader* index) channel)
                  (begin
                    (message "ERROR !!!!!!! Unknown fasl code")
                    (write-channel *stdout* byte 0 1))))))))

  (define read-type
    (lambda (channel) (read-byte channel)))

  (define load-execute-fasl
    (lambda (fasl-name)
      (let ((channel (open-input-channel fasl-name)))
	(let loop ((type (read-type channel)))
          (write-channel *stdout* type 0 1)
	  (if (eof-object? type)
	      (close-channel channel)
              (begin
;                (write-channel *stdout* type)
;                (write-channel *stdout* type/definition)
                (if (= type type/definition)
                    (let ((fasl (read-fasl channel)))
                      (let ((proc (make-procedure 2)))
                        (message "d")
                        ;		    (write-channel *stdout* fasl)
                        (procedure-set! proc 0 (make-vector 1 'undefined))
                        (procedure-set! proc 1 fasl)
                        ;		    (write-channel *stdout* proc)
                        (proc)
                        (message ".")
                        (loop (read-type channel))))
                    1)
                (if (= type type/syntax)
                    (let ((symbol (read-fasl channel)))
                      (let ((fasl (read-fasl channel)))
                        (let ((proc (make-procedure 2)))
                          (message "s")
                          ;		    (write-channel *stdout* fasl)
                          (procedure-set! proc 0 (make-vector 1 'undefined))
                          (procedure-set! proc 1 fasl)
                          ;		    (write-channel *stdout* proc)
			  (bind-syntax! symbol
                                        (make-syntax (proc)
                                                     (module/syntax *module*))
                                        *module*)
                          (message ".")
                          (loop (read-type channel)))))
                    1)
                (if (= type type/expr)
                    (let ((fasl (read-fasl channel)))
                      (let ((proc (make-procedure 2)))
                        (message "e")
                        ;		    (write-channel *stdout* fasl)
                        (procedure-set! proc 0 (make-vector 1 'undefined))
                        (procedure-set! proc 1 fasl)
                        ;		    (write-channel *stdout* proc)
                        (proc)
                        (message ".")
                        (loop (read-type channel))))
                    1)))))))

  (define load-all
    ;; Load all fasls file given in FASL-NAMES
    (lambda (fasl-names)
      (if (null? fasl-names)
	  #t
	  (begin
	    (message (car fasl-names))
	    (message "...")
	    (let ((fasl (load-execute-fasl (car fasl-names))))
	      (message "done.")
	      (load-all (cdr fasl-names)))))))

  (define dump-defined-symbol
    (lambda (l)
      (if (null? l)
	  'done
	  (let ((ref (car l)))
	    (write-channel *stdout* (car ref) 0 1)
	    (write-channel *stdout* " --> " 0 1)
	    (write-channel *stdout* (cdr ref) 0 1)
	    (dump-defined-symbol (cdr l))))))

  ;;; module support

  (define *runtime-sig*
    ;; signature of the runtime system. Here are all exported binding
    '(pair? cons car cdr set-car! set-cdr!
	    caar cadr cdar cddr
	    caaar caadr cadar caddr cdaar cdadr cddar cdddr
	    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	    null? list? list length append reverse list-tail list-ref list-set!
	    memq memv member assq assv assoc

	    string? make-string string string-length string-ref string-set!
	    string=? string-ci=? string<? string>? string<=? string>=?
	    string-ci<? string-ci>? string-ci<=? string-ci>=? substring
	    string-append string->list list->string string-copy
	    string-fill! number->string string->number

	    char? char=? char<? char>? char<=? char>=?
	    char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
	    char-alphabetic? char-numeric? char-whitespace?
	    char-upper-case? char-lower-case? char->integer integer->char
	    char-upcase char-downcase

	    eq? eqv? equal? not boolean?

	    apply map for-each
	    ;; force delay
	    call-with-current-continuation dynamic-wind call/cc call/wc

	    vector vector->list list->vector vector-fill!

            ;; special values

            unbound-object unbound-object?
            unspecific-object unspecific-object?

	    ;; primitive

	    make-bvec bvec? bvec-ref bvec-set! bvec-length
	    make-procedure procedure? procedure-ref procedure-set! procedure-length

	    + - quotient remainder modulo * / < <= = > >=
	    symbol? eq? number? pair? car cdr set-car! set-cdr! null?
	    vector? make-vector vector-length vector-ref vector-set!
	    eof-object? char->number number->char symbol->string string->symbol
	    apply

	    bit-or bit-and bit-xor bit-not bit-ash
	    object-hash

	    make-channel close-channel read-channel write-channel

	    (or :syntax)
	    (and :syntax)
	    (if :syntax)
	    (begin :syntax)
	    (let :syntax)
	    (letrec :syntax)
	    (let* :syntax)
	    (cond :syntax)
	    (set! :syntax)
	    (quote :syntax)
	    (lambda :syntax)
	    (define :syntax)
	    (define-syntax :syntax)
	    (case :syntax)

            native-call

            ;; number.scm
            complex? real? rational? integer? exact? inexact?
            zero? positive? negative? odd? even? max min abs gcd lcm
            numerator denominator
            floor ceiling truncate round rationalize
            exp log sin cos tan asin acos atan sqrt expt
            make-rectangular make-polar real-part imag-part magnitude angle
            exact->inexact inexact->exact

	    ;; rules.scm

	    (syntax-rules :syntax)
            (quasiquote :syntax)

	    ;; extensions to basic standard Scheme

            ;; format.scm
            format define-format-directive format-to-string

            ;; loop.scm
            (loop :syntax)
            (define-loop-keyword :syntax)
            (define-for-iterator :syntax)

	    ;; record.scm
	    (define-record-type :syntax) <record> make-record record?
	    record-ref record-set! record-type make-record-type
	    (record-case :syntax)
	    make initialize <top> <<type>> <<class>> class-of
	    <<union>> make-type-union type-union? union-types
	    <<singleton>> make-type-singleton type-singleton? type-object
	    <<type-subclass>>  make-type-subclass type-subclass? type-class
	    t? t= t< t+
	    (define-generic :syntax) (defg :syntax) <generic> generic?
	    generic-name generic-args
	    make-generic generic-methods method-applicable?
	    method-more-specific? sorted-applicable-methods
	    applicable-methods generic-add-method!

	    (define-method :syntax) method-specs
            (defm :syntax)

	    <nil> <undefined> <unbound> <eof> <bool> <number> <integer>
            <collection> <sequence> <list>
            <real> <string> <char> <channel>
	    <vector> <byte-vector> <pair> <closure> <symbol> <ref>
	    record-type-name record-type-supers record-type-direct-slots
	    record-type-slots record-type-methods record-type-ancestors
	    record-type-subclasses

	    instance? subtype? subclass?

            (let-keys :syntax)

	    ;; fluids.scm
	    fluid let-fluid set-fluid! make-fluid fluid?

	    ;; io.scm
	    <port> port-read-char port-peek-char port-read-line port-write-char
	    port-write-string port-close port-name port-position set-port-position!
	    <channel-port> make-channel-port channel-port? channel-port-channel
	    <string-port> output-string

	    open-input-string-port open-output-string-port
	    input-port? output-port? call-with-input-file call-with-output-file
	    with-input-from-string with-output-to-string
	    with-input-from-file with-output-to-file with-output-appended-to-file
	    open-input-file open-output-file
	    with-input-from-port with-output-to-port
	    close-input-port close-output-port
	    channel->input-port channel->output-port
	    port->channel
	    read $eof eof-object? char-ready? peek-char read-char read-line
	    write write-object display newline write-char
	    load transcript-on transcript-off
	    current-input-port current-output-port
	    initialize-i/o! read-error! ascii->char char->ascii
            set-process-alignment-procedure!

	    resource-providers locate-resource
	    add-resource-provider remove-resource-provider
	    open-resource-input-port with-input-from-resource
	    call-with-input-resource

	    ;; hashtable.scm
	    make-hashtable hash-ref hash-set!

	    ;; init.scm

	    (define-init-action :syntax)
	    make-action action?
	    action-name set-action-name!
	    action-depends set-action-depends!
	    action-do set-action-do!

            add-init-action! remove-init-action! lookup-action init-actions
            init-action-exists?
            save-image-file

            with-out-of-memory

	    ;; modules
            modules module-names *modules*
            <module> make-module module? module/name set-module/name!
            module/export set-module/export! module/open set-module/open!
            module/doc set-module/doc! module/defined set-module/defined!
            module/syntax set-module/syntax!
            bind-module! unbind-module! lookup-module
            require add-module-provider delete-module-provider module-providers
            with-current-module current-module current-module-name
            lookup-ref lookup/create-ref
            bind-ref! unbind-ref!
            make-ref ref? ref/name set-ref/name! ref/value set-ref/value!
            ref/module set-ref/module!

            bind-syntax! unbind-syntax! lookup-syntax
            make-syntax syntax? syntax/env syntax/transformer syntax/module
            make-primitive primitive? primitive/name primitive/transformer

	    bound?

	    eval set-eval-procedure
            with-source-location current-source-location

	    ;; conditions.scm
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

            ;; debug.scm
            continuation-val continuation-stack continuation-top
            continuation-env continuation-template continuation-code
            continuation-literals continuation-pc continuation-cont
            continuation-argc continuation-dynamic continuation-trap
            set-continuation-val! set-continuation-stack! set-continuation-top!
            set-continuation-env! set-continuation-template!
            set-continuation-code! set-continuation-literals! set-continuation-pc!
	    set-continuation-cont! set-continuation-argc! set-continuation-dynamic!
	    set-continuation-trap!
            make-frame frame-ref frame-previous frame-env frame-template frame-pc
	    frame-set! set-frame-previous! set-frame-env! set-frame-template!
	    set-frame-pc! frame-code frame-literals frame-debug frame-arguments
	    frame-locals frame-length cc->frame for-each-stack-frame

            make-template template-code template-literals template-debug
            set-template-code! set-template-literals! set-template-debug!
            make-debug debug-name debug-env debug-parameters debug-doc
            debug-file

	    show-backtrace disassemble

	    ;; thread.scm
	    <thread> make-thread thread?
	    thread-name set-thread-name! thread-value set-thread-value!
	    thread-continuation set-thread-continuation! current-thread
	    thread-start! thread-yield! thread-suspend! thread-terminate!
	    thread-join! thread-resume! threads thread-block-read thread-block-write
	    thread-interrupt!
	    add-idle-hook!

	    make-spinlock spinlock? spinlock-name
	    spinlock-try-lock! spinlock-lock! spinlock-unlock! spinlock-locked?
	    with-spinlock-held

	    ;; alien.scm
	    alien-value? alien-value-value alien-value-type
	    malloc free! malloc-array malloc-ptr sizeof
	    basic-type? basic-type-name basic-type-size
	    :bool :void :char :wchar :short :ushort :int :uint :long :ulong
	    :float :double :cstring
	    alien->scheme scheme->alien
	    cstruct-type? cstruct-type-name cstruct-type-fields
	    cstruct-ref cstruct-set! (define-cstruct-type :syntax)
	    pointer-type? pointer-type-type make-pointer pointer-deref
	    (define-pointer-type :syntax)
	    make-array-type array-type? array-type-type array-type-size
	    array-ref array-set!
	    (define-alien-library :syntax)
	    (define-alien-entry :syntax)
	    (define-alien-variable :syntax)
	    alien-error!
	    alien-library-path set-alien-library-path!
	    register-alien-library! register-alien-entry! register-alien-variable!

	    ;; eta-primitives.scm

	    channel-read channel-write %host-error
	    open-input-channel open-output-channel
	    close-channel

	    ;; extensions.scm

	    sort
	    delq delv delete
	    posq posv position
	    reduce filter
	    all? any?
	    make-list
	    sum

	    vector-sort! subvector
	    vector-extend

	    string-prefix?
	    string-position
	    string-split
	    string-join

            random
))

  (define *compiler-sig* '(compile&go compile-only compile syntax-expand
                           assembler make-syntax-env
                           make-define-syntax define-syntax? define-syntax/expr define-syntax/name
                           make-lambda lambda? lambda/formals lambda/body
                           make-if if? if/exp if/then if/else
                           make-set! set!? set!/lhs set!/rhs
                           make-let let? let/bindings let/body
                           make-application application? app/operator app/operands
                           make-literal literal? literal/val
                           make-begin begin? begin/body
                           make-letrec letrec? letrec/bindings letrec/body
                           make-scoped scoped? scoped/uid scoped/name
                           make-primitive-call primitive-call? primitive-call/name
                           primitive-call/args
                           return-continuation write-type write-fasl!
                           type/syntax type/define type/expr type/module
                           read-fasl read-type))


  (define *repl-sig* '(*boot* repl))

  (define *posix-sig*
    '($owner/all $owner/read $owner/write $owner/execute
      $group/all $group/read $group/write $group/execute
      $other/all $other/read $other/write $other/execute

      $access/read $access/write $access/execute $access/exist
      $mode/fifo $mode/char $mode/file $mode/directory $mode/block $mode/link
      $mode/socket

      ;; process.scm
      (run/port :syntax) (run/file :syntax) (run/string :syntax) (run/strings :syntax)
      (run/sexp :syntax) (run/sexps :syntax) (exec-epf :syntax) (& :syntax) (run :syntax)
      exec exec-path exec/env exec-path/env exit call-terminally suspend fork
      fork/pipe fork/pipe+ pipe run/pipe*
      <process> process? make-process process/pid set-process/pid!
      wait wait-any wait-process-group getenv setenv umask set-umask! with-umask*
      signal-process open-pty fork-pty
      (with-umask :syntax) chdir cwd with-cwd* (with-cwd :syntax) pid
      parent-id process-group set-process-group! set-priority! priority nice

      user-login-name user-uid user-effective-uid user-gid user-effective-gid
      user-supplementary-gids set-uid! set-gid! process-times cpu-ticks/sec

      <userinfo> make-user-info user-info? user-info-name user-info-passwd user-info-uid
      user-info-gid user-info-gecos user-info-home-dir user-info-shell
      user-name user-passwd user-gid user-gecos user-home-dir user-shell
      user-info username->uid uid->username

      <groupinfo> make-group-info group-info? group-info-name group-info-passwd
      group-info-gid group-info-members group-info

      <system-info> make-system-info system-info?
      system-info-name system-info-node-name system-info-release
      system-info-version system-info-machine
      system-info system-name system-node-name system-release system-version
      system-machine

      add-resource-path! remove-resource-path!

      ;; file.scm
      create-directory! create-fifo! create-hard-link! create-symlink!
      delete-directory! delete-file! delete-filesys-object!
      read-symlink rename-file! set-file-mode! set-file-owner! set-file-group!
      set-file-time! sync-file! sync-file-system! truncate-file!
      make-file-info
      file-info? file-info-type file-info-device file-info-inode file-info-mode
      file-info-nlinks file-info-uid file-info-gid file-info-size file-info-atime
      file-info-mtime file-info-ctime
      file-info file-directory? file-fifo? file-regular? file-socket?
      file-special? file-symlink? file-not-readable? file-not-writable?
      file-not-executable? file-readable? file-writable? file-executable?
      file-not-exists? file-exists? directory-files
      create-temp-file!

      ;; network.scm
      $domain/unix $domain/inet $socket/datagram $socket/stream $socket/raw
      socket? <socket> make-socket socket-input socket-output socket-family socket-type socket-peer
      socket-connect bind-listen-accept-loop
      $address/any $address/loopback $address/broadcast
      create-socket close-socket bind-socket listen-socket accept-socket connect-socket
      <socket-address> socket-address?
      <unix-socket-address> unix-socket-address? unix-socket-address-path
      <inet-socket-address> inet-socket-address? inet-socket-address-host inet-socket-address-port
      unix-address->socket-address inet-address->socket-address
      $message/out-of-bound $message/peek $message/dont-route
      receive-message receive-message! receive-message/partial receive-message!/partial
      send-message send-message/partial
      $socket/debug $socket/accept-connect $socket/reuse-address $socket/keep-alive
      $socket/dont-route $socket/broadcast $socket/use-loop-back $socket/oob-inline
      $socket/use-privileged $socket/cant-signal $tcp/no-delay $socket/send-buffer
      $socket/receive-buffer $socket/send-low-water $socket/receive-low-water
      $socket/error $socket/type $ip/time-to-live $tcp/max-segment
      $socket/send-timeout $socket/receive-timeout
      $socket/bind-to-device $socket/linger
      $level/socket $level/ip $level/tcp $level/udp
      socket-option set-socket-option!
      <host-info> host-info? host-info-name host-info-aliases host-info-addresses
      <network-info> network-info? network-info-name network-info-aliases network-info-net
      <service-info> service-info? service-info-name service-info-aliases service-info-port service-info-protocol
      <protocol-info> protocol-info? protocol-info-name protocol-info-aliases protocol-info-number
      host-info network-info service-info protcol-info

      ;; file.scm
      file-name-directory? file-name-non-directory? file-name-as-directory
      directory-as-file-name file-name-absolute? file-name-directory
      file-name-nondirectory split-file-name path-list->file-name
      file-name-extension file-name-sans-extension parse-file-name decode-file-name
      replace-extension simplify-file-name resolve-file-name
      expand-file-name absolute-file-name home-dir home-file
      file-contents file-type file-device file-inode file-mode file-nlinks file-uid file-gid
      file-size file-atime file-mtime file-ctime

      ;; time.scm
      make-date date? date-seconds set-date-seconds!
      date-minute set-date-minute date-hour set-date-hour!
      date-month-day set-date-month-day! date-month set-date-month!
      date-year set-date-year! date-tz-name set-date-tz-name!
      date-tz-secs set-date-tz-secs! date-summer? set-date-summer!
      date-week-day set-date-week-day! date-year-day set-date-year-day!
      date
      time time-seconds time-milliseconds make-time
      time- time+ time=? time<? time<=? time>=? time>? time/=?
      time->number number->time seconds->time time->seconds
      ))

  ;; Current loading environment

  (define *module* #f)
  (define (set-module! module)
    (set! *module* module))

  (define (bind-module! name module ref)
    (set-ref-value! ref (make-pair (make-pair name module)
				   (ref-value ref))))

  ;;  (write-channel *stdout* "reading fasl...")

  ;; first load the runtime system FASL file.

  (load-all '("../boot/runtime-base.fasl"
              "../boot/runtime-number.fasl"
              "../boot/runtime-rules.fasl"
              "../boot/runtime-format.fasl"
;              "../boot/runtime-loop.fasl"
	      "../boot/runtime-record.fasl"
;              "../boot/runtime-boo.fasl"
	      "../boot/runtime-fluids.fasl"
	      "../boot/runtime-module.fasl"
	      "../boot/runtime-conditions.fasl"
	      "../boot/runtime-port.fasl"
	      "../boot/runtime-read.fasl"
	      "../boot/runtime-hashtable.fasl"
	      "../boot/runtime-init.fasl"
              "../boot/runtime-debug.fasl"
	      "../boot/runtime-threads.fasl"
	      "../boot/runtime-alien.fasl"
	      "../boot/runtime-extensions.fasl"
              "../boot/runtime-eta-primitives.fasl"))

  ;; Now load the compiler and REPL FASL files

  (let* ((*modules*-ref (lookup-ref '*modules* 'scheme-runtime))
	 (lookup/create-ref* (lookup-ref 'lookup/create-ref 'scheme-runtime))
	 (make-module* (lookup-ref 'make-module 'scheme-runtime))
	 (module/syntax* (lookup-ref 'module/syntax 'scheme-runtime))
         (module/export* (lookup-ref 'module/export 'scheme-runtime))
	 (module/name* (lookup-ref 'module/name 'scheme-runtime))
	 (bind-syntax!* (lookup-ref 'bind-syntax! 'scheme-runtime))
	 (runtime-module ((ref-value make-module*)
			  'scheme-runtime
			  *runtime-sig*
			  '()
			  "Scheme Runtime System 1"
			  *defs*
			  *syntaxes*)))
    (bind-module! 'scheme-runtime runtime-module *modules*-ref)

    (set! module/export* (ref-value module/export*))
    (set! lookup-ref (ref-value lookup/create-ref*))
    (set! bind-syntax! (ref-value bind-syntax!*))
    (set! module/syntax (ref-value module/syntax*))

;     (lambda (name expander)
;       (bind-syntax! name (cons (cons ((cdr module/syntax) *module*) expander)
; 			       ((cdr module/name*) *module*)))))

    ;; load compiler

    (message "Loading compiler...")
    (let ((compiler-module ((ref-value make-module*) 'scheme-compiler
			    *compiler-sig* '(scheme-runtime) "Scheme Compiler"
			    '()
			    'scheme-compiler)))
      (bind-module! 'scheme-compiler compiler-module *modules*-ref)
      (set-module! compiler-module)

      (load-all '("../boot/compiler-syntax.fasl"
                  "../boot/compiler-compiler.fasl"
                  "../boot/compiler-gen.fasl"
                  "../boot/compiler-assembler.fasl"
                  "../boot/compiler-fasl.fasl")))

    ;; Load Posix support

    (message "Loading Posix...")
    (let ((posix-module ((ref-value make-module*) 'scheme-posix
			 *posix-sig* '(scheme-runtime)
			"Scheme Posix support" '() 'scheme-posix)))
      (bind-module! 'scheme-posix posix-module *modules*-ref)
      (set-module! posix-module)

      (load-all '("../boot/posix-file.fasl"
                  "../boot/posix-filename.fasl"
		  "../boot/posix-time.fasl"
		  "../boot/posix-constants.fasl"
		  "../boot/posix-network.fasl"
		  "../boot/posix-process.fasl")))

    ;; load REPL FASL files

    (message "Loading REPL...")
    (let ((repl-module ((ref-value make-module*) 'scheme-repl
			*repl-sig* '(scheme-runtime scheme-compiler scheme-posix)
			"Scheme Read-Eval-Print-Loop" '() 'scheme-repl)))
      (bind-module! 'scheme-repl repl-module *modules*-ref)
      (set-module! repl-module)

      (load-all '("../boot/repl-repl.fasl")))

    (let loop ((syns (ref-value (lookup-ref '*initial-syntax-env* 'scheme-compiler))))
      (if (pair? syns)
          (let ((syn (car syns)))
;            (write-channel *stdout* syn)
            (bind-syntax! (car syn) (cdr syn) runtime-module)
            (loop (cdr syns)))
          'ok))

;    (write-channel *stdout* (ref-value *modules*-ref))

    ;; Install predifined syntaxes in the scheme-runtime module


;     (bind-syntax! 'define-syntax
; 		  (ref-value (lookup-ref '*define-syntax* 'scheme-compiler))
; 		  runtime-module)

;     (bind-syntax! 'lambda
; 		  (ref-value (lookup-ref '*lambda* 'scheme-compiler))
; 		  runtime-module)

;     (bind-syntax! 'let-syntax
; 		  (ref-value (lookup-ref '*let-syntax* 'scheme-compiler))
; 		  runtime-module)

;     (bind-syntax! '%let
; 		  (ref-value (lookup-ref '*let* 'scheme-compiler))
; 		  runtime-module)

;     (bind-syntax! 'begin
; 		  (ref-value (lookup-ref '*begin* 'scheme-compiler))
; 		  runtime-module)

;     (bind-syntax! 'set!
; 		  (ref-value (lookup-ref '*set!* 'scheme-compiler))
; 		  runtime-module)

;     (bind-syntax! 'if
; 		  (ref-value (lookup-ref '*if* 'scheme-compiler))
; 		  runtime-module)

;     (bind-syntax! 'quote
; 		  (ref-value (lookup-ref '*quote* 'scheme-compiler))
; 		  runtime-module)

;     (bind-syntax! 'letrec
; 		  (ref-value (lookup-ref '*letrec* 'scheme-compiler))
; 		  runtime-module)

    ;; Save Initial Image File

    (message "Saving image file...")
					;  (dump-defined-symbol *defs*)
					;   (set-cdr! (lookup-ref '*modules*)
					; 	    (make-pair (make-pair 'nucleus-core
					;                                   ((cdr (lookup-ref 'make-module))
					;                                    'nucleus
					;                                    '(compile/exp welcome)
					;                                    '()
					;                                    "Nucleus module"
					;                                    *defs* *syntaxes*))
					; 		       (cdr (lookup-ref '*modules*))))
					; 					;  (write-channel (make-channel 1) (cdr (lookup-ref '*modules*)))
;    (write-channel *stdout*
;		   (ref-value (lookup-ref '*boot*
;					  'scheme-runtime)))

    (set-ref-value! (lookup-ref '*top* 'scheme-runtime)
                    (ref-value (lookup-ref 'run-repl 'scheme-repl)))
    (save-image "initial.image"
                (ref-value (lookup-ref '*boot*
                                       'scheme-runtime))))
)
