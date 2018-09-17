;;; network.scm -- networking procedures

(define $domain/unix 0)
(define $domain/inet 1)

(define $socket/datagram 0)
(define $socket/stream   1)
(define $socket/raw      2)

(define-record-type <socket>
  (make-socket family type peer input output)
  socket?
  (family  socket-family set-socket-family!)
  (type    socket-type set-socket-type!)
  (peer    socket-peer set-socket-peer!)
  (input   socket-input set-socket-input!)
  (output  socket-output set-socket-output!))

;;; HIGH LEVEL IFC ===========================================================

(define (socket-connect domain type . args)
  #f)
(define (bind-listen-accept-loop domain proc . args)
  #f)
(define (bind-prepare-listen-accept-loop family perpare proc arg)
  #f)

;;; == Low Level Interface 

(define $address/any       0)
(define $address/loopback  1)
(define $address/broadcast 2)

;;;
;; Return a new socket according to `family`, `type` and `protocol`
(define (create-socket family type protocol)
  (let ((channels (posix-socket family type)))
    (cond ((pair? channels) (make-socket family type #f (car channels) (cdr channels)))
	  ((number? channels) (posix-error "Error during socket creation, code=~a" channel))
	  (else (posix-error "Unknown error during socket creation")))))
	   
;;;
;; Close `socket`
(define (close-socket socket)
  (if (socket? socket)
      (close-channel (socket-input socket))))

;;;
;; Bind `socket` to the specific `address`.
(define (bind-socket socket address)
  (posix-bind (socket-input socket) (socket-address-buffer address)))

;;;
;; Listen on `socket` using a specific `backlog`
(define (listen-socket socket backlog)
  (posix-listen (socket-input socket) backlog))

;;;
;; Waits for a client connection to the server socket defined by
;; `socket` and return a connection port.
(define (accept-socket socket)
  (let* ((channel (socket-input socket))
	 (family (socket-family socket))
	 (type (socket-type socket))
	 (result (posix-accept channel)))
    (if (vector? result)
	(make-socket family type (vector-ref result 2) (vector-ref result 0) (vector-ref result 1))
	(if (= result $errno/again)
	    (begin 
	      (thread-block-read channel)
	      (accept-socket socket))
	    (posix-error "Error during call to accept, code=~a" result)))))

;; (define (connect-socket* host port)
;;   (let* ((socket (posix-connect host port)))
;;     (if socket
;; 	(make-socket socket)
;; 	(posix-error "Error during connection to ~a at port ~a" host port))))

;;;
;; Creates a client connection using `socket` to the specified
;; `address`.
(define (connect-socket socket address)
  (let ((channel (posix-connect (socket-input socket)
				(socket-address-buffer address))))
    (cond ((channel? channel) socket)
	  ((= channel $errno/inprogress)
	   (thread-block-write (socket-output socket))
	   (let ((code (socket-option socket $level/socket $socket/error)))
	     (cond ((= code 0) socket)
		   ;; should probably give a better error message, see man
		   ;; pages. At least separate TIMEOUT and NETWORK UNREACHABLE.
		   (else (posix-error "Error connecting to ~a" address)))))
	  (else
	   (posix-error "Error ~a during connection to ~a" channel address)))))

(define-record-type <socket-address>
  (make-socket-address buffer)
  socket-address? 
  (buffer socket-address-buffer set-socket-address-buffer!))

(define-record-type (<unix-socket-address> <socket-address>)
  (make-unix-socket-address path buffer)
  unix-socket-address?
  (path unix-socket-address-path))

(define-record-type (<inet-socket-address> <socket-address>)
  (make-inet-socket-address host port buffer)
  inet-socket-address?
  (host inet-socket-address-host)
  (port inet-socket-address-port))

;;;
;; Creates a socket address for the Unix Domain address denoted by `path`.
(define (unix-address->socket-address path)
  (let ((buffer (posix-unix-address path)))
    (if buffer
	(make-unix-socket-address path buffer)
	(posix-error "unix address error ~a" path))))    

;;;
;; Creates a socket address for the Internet address denoted by `host`
;; and `port`.
(define (inet-address->socket-address host port)
  (let ((buffer (posix-inet-address host port)))
    (if buffer
	(make-inet-socket-address host port buffer)
	(posix-error "inet address error ~a:~a" host port))))
       
;;; I/O ON SOCKETS ===========================================================

(define $message/out-of-bound 1)
(define $message/peek         2)
(define $message/dont-route   4)

(define (receive-message socket length)
  #f)

(define (receivce-message! socket buffer start end flags)
  #f)

(define (receive-message/partial socket length . flags)
  #f)

(define (receive-message!/partial socket buffer start env flags)
  #f)

(define (send-message socket buffer start end flags address)
  #f)

(define (send-message/partial socket string start end flags address)
  #f)

;;; SOCKET OPTIONS ============================================================

(define $socket/debug          0)
(define $socket/accept-connect 1)
(define $socket/reuse-address  2)
(define $socket/keep-alive     3)
(define $socket/dont-route     4)
(define $socket/broadcast      5)
(define $socket/use-loop-back  6)
(define $socket/oob-inline     7)
(define $socket/use-privileged 8)
(define $socket/cant-signal    9)
(define $tcp/no-delay          10)

(define $socket/send-buffer       11)
(define $socket/receive-buffer    12)
(define $socket/send-low-water    13)
(define $socket/receive-low-water 14)
(define $socket/error             15)
(define $socket/type              16)
(define $ip/time-to-live          17)
(define $tcp/max-segment          18)

(define $socket/send-timeout    19)
(define $socket/receive-timeout 20)
(define $socket/bind-to-device  21)
(define $socket/linger          22)

(define $level/socket 0)
(define $level/ip     1)
(define $level/tcp    2)
(define $level/udp    3)

;;;
;; Returns the current value of socket `option` on `socket` on `level`
(define (socket-option socket level option)
  (let ((v (posix-getsockopt (socket-input socket) level option)))
    (cond ((eq? v #f)
	   (posix-error "socket option unsupported ~a ~a" level option))
	  ((< v 0)
	   (posix-error "error ~a getting socket option ~a ~a" v level option))
	  (else v))))

;;;
;; Sets the `option` of `socket` at the specified `level` to `value`.
(define (set-socket-option! socket level option value)
  (let ((errno (posix-setsockopt socket level option value)))
    (if errno
	(posix-error "Error setting socket option ~a ~a ~a"
		     level option value))))	

;;; DATABASE ENTRIES =========================================================

(define-record-type <host-info>
  (make-host-info name aliases addresses)
  host-info?
  (name      host-info-name      set-host-info-name!)
  (aliases   host-info-aliases   set-host-info-aliases!)
  (addresses host-info-addresses set-host-info-addresses!))

(define-record-type <network-info>
  (make-network-info name aliases net)
  network-info?
  (name    network-info-name    set-network-info-name!)
  (aliases network-info-aliases set-network-info-aliases!)
  (net     network-info-net     set-network-info-net!))

(define-record-type <service-info>
  (make-service-info name aliases port protocol)
  service-info?
  (name     service-info-name     set-service-info-name!)
  (aliases  service-info-aliases  set-service-info-aliases!)
  (port     service-info-port     set-service-info-port!)
  (protocol service-info-protocol set-service-info-protocol!))

(define-record-type <protcol-info>
  (make-protocol-info name aliases number)
  protocol-info?
  (name    protocol-info-name    set-protocol-info-name!)
  (aliases protocol-info-aliases set-protocol-info-aliases!)
  (number  protocol-info-number  set-protocol-info-number!))

(define-record-type (<host-error> <error>)
  (make-host-error message)
  host-error?)

(define-record-type (<host-not-found-error> <host-error>)
  (make-host-not-found-error message)
  host-not-found-error?)

(define-record-type (<try-again-error> <host-error>)
  (make-try-again-error message)
  try-again-error?)

(define-record-type (<no-recovery-error> <host-error>)
  (make-no-recovery-error message)
  no-recovery-error?)

(define-record-type (<no-data-error> <host-error>)
  (make-no-data-error message)
  no-data-error?)

(define-record-type (<no-addresses-error> <host-error>)
  (make-no-addresses-error message)
  no-addresses-error?)

(define $herror/host-not-found 0)
(define $herror/try-again      1)
(define $herror/no-recovery    2)
(define $herror/no-data        3)
(define $herror/no-addresses   4)

(define (host-info name/address)
  (let* ((info (make-host-info #f #f #f))
         (info* (posix-host-info info name/address)))
    (if (host-info? info*) 
        info
        (case info*
          (($herror/host-not-found) (signal (make-host-not-found-error name/address)))
          (($herror/try-again)      (signal (make-try-again-error name/address)))
          (($herror/no-recovery)    (signal (make-no-recovery-error name/address)))
          (($herror/no-data)        (signal (make-no-data-error name/address)))
          (($herror/no-addresses)   (signal (make-no-addresses-error name/address)))
          (else (error "HOST-INFO: unknown host error ~a" info*))))))

(define (network-info name/address)
  (let ((info (make-network-info #f #f #f)))
    (posix-network-info info name/address)))

(define (service-info name/number . protocol-name)
  (let ((info (make-service-info #f #f #f #f)))
    (posix-service-info info name/number protocol-name)))

(define (protocol-info name/number)
  (let ((info (make-protocol-info #f #f #f)))
    (posix-protocol-info info name/number)))
             

