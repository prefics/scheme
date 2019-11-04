;;; port.scm -- input/output procedures

;;; Commentary:


;;; Code:

;;; Port definition ===============================================
;;
;; A port is a communication mechanism. It consists of a sequence
;; of characters ended with an EOF-OBJECT. Each port has a DIRECTION
;; indicating if it allows the reading or writing or both operations
;; Ports supports the look ahead of at most one character with the
;; PEEK-CHAR operation.

;; Two basic operations exist for ports: PORT-READ-CHAR and PORT-WRITE-CHAR
;;
;; (port-read-char PORT) => (or <char> <eof>)
;; This procedures returns the next character available in the port
;; If there are no more characters available the EOF-OBJECT is returned
;; Once a port has hit the end all following calls to PORT-READ-CHAR
;; returns the EOF-OBJECT.

;; (port-write-char PORT CHAR) => UNSPECIFIC
;; PORT-WRITE-CHAR outputs CHAR in the specific PORT.

(define-record-type <port>
  (make-port name direction peek)
  port?
  (name      %port-name)
  (direction port-direction)
  (peek      port-peek set-port-peek!))

(define-generic port-read-char (port))
(define-generic port-read-line (port))
(define-generic port-peek-char (port))
(define-generic port-write-char (port char))
(define-generic port-write-string (port string))
(define-generic port-position (port))
(define-generic set-port-position! (port position))
(define-generic port-name (port))

(define-generic port-close (port))

(define $lf (integer->char 10))

;;; Default implementations =======================================

(define-method port-read-line ((port <port>))
  (let loop ((ch (port-read-char port))
             (lst '()))
    (if (eof-object? ch)
        (if (eq? lst '())
            ch
            (list->string (reverse lst)))
        (if (char=? ch $lf)
            (list->string (reverse lst))
            (loop (port-read-char port) (cons ch lst))))))

(define-method port-write-string ((port <port>) str)
  (let loop ((i 0))
    (if (< i (string-length str))
        (begin
          (port-write-char port (string-ref str i))
          (loop (+ i 1))))))

(define-method port-name ((port <port>))
  (%port-name port))

(define-method port-position ((port <port>))
  (error "Port does not support positioning ~a" port))

(define-method set-port-position! ((port <port>) position)
  (error "Port does not support port positioning ~a" port))

;;; Channel Ports =================================================

(define $eintr 4)
(define $eagain 11)

(define-record-type (<channel-port> <port>)
  (make-channel-port name direction peek channel)
  channel-port?
  (channel channel-port-channel))

(define-method port-read-char ((port <channel-port>))
  (let ((peek (port-peek port)))
    (set-port-peek! port #f)
    (if peek
        peek
        (let* ((channel (channel-port-channel port))
               (rc (read-channel channel)))
          (if rc
              rc
              (let ((errno (%host-error)))
                (cond ((%error=? errno $eintr)
                       (port-read-char port))
                      ((%error=? errno $eagain)
                       (thread-block-read channel)
                       (port-read-char port))
                      (else
                       (error "error ~a during READING" errno)))))))))

(define-method port-peek-char ((port <channel-port>))
  (let ((peek (port-peek port)))
    (if peek
        peek
        (let* ((channel (channel-port-channel port))
               (ch (read-channel channel)))
          (if ch
              (begin
                (set-port-peek! port ch)
                ch)
              (let ((errno (%host-error)))
                (cond ((%error=? errno $eintr)
                       (port-peek-char port))
                      ((%error=? errno $eagain)
                       (thread-block-read channel)
                       (port-peek-char port))
                      (else
                       (error "error ~a during READING" errno)))))))))

(define-method port-write-char ((port <channel-port>) char)
  (let* ((channel (channel-port-channel port))
         (rc (write-channel channel char 0 1)))
    (if rc
        rc
        (let ((errno (%host-error)))
          (cond ((%error=? errno $eintr)
                 (port-write-char port char))
                ((%error=? errno $eagain)
                 (thread-block-write channel)
                 (port-write-char port char))
                (else
                 (error "error ~a during WRITE" errno)))))))

(define-method port-write-string ((port <channel-port>) string)
  (let* ((channel (channel-port-channel port))
         (end (string-length string)))
    (let loop ((start 0))
      (if (< start end)
          (let ((rc (write-channel channel string start end)))
            (if rc
                (loop (+ start rc))
                (let ((errno (%host-error)))
                  (cond ((%error=? errno $eintr)
                         (loop start))
                        ((%error=? errno $eagain)
                         (thread-block-write channel)
                         (loop start))
                        (else
                         (error "error ~a during WRITE" errno))))))))))

(define-method port-close ((port <channel-port>))
  (let ((channel (channel-port-channel port)))
    (close-channel channel)))

;; constant definition from /usr/include/unistd.h

(define $seek/begining 0)
(define $seek/current 1)
(define $seek/end 2)

(define-method port-position ((port <channel-port>))
  (let ((channel (channel-port-channel port)))
    (posix-seek channel 0 $seek/current)))

(define-method set-port-position! ((port <channel-port>) pos)
  (let ((channel (channel-port-channel port)))
    (posix-seek channel pos $seek/begining)))

;;; String Ports ==================================================

(define $buffer-size 1024)

(define-record-type (<string-port> <port>)
  (make-string-port name direction peek buffer index len)
  string-port?
  (buffer string-port-buffer set-string-port-buffer!)
  (index  string-port-index set-string-port-index!)
  (len    string-port-len set-string-port-len!))

(define-method port-read-char ((port <string-port>))
  (let ((buffer (string-port-buffer port))
        (index (string-port-index port)))
    (if (>= index (string-port-len port))
        $eof
        (let ((ch (string-ref buffer index)))
          (set-string-port-index! port (+ index 1))
          ch))))

(define-method port-peek-char ((port <string-port>))
  (let ((buffer (string-port-buffer port))
        (index (string-port-index port)))
    (if (>= index (string-port-len port))
        $eof
        (string-ref buffer index))))

(define (string-copy-from-to from to)
  (let loop ((i 0))
    (if (< i (string-length from))
        (if (< i (string-length to))
            (begin
              (string-set! to i (string-ref from i))
              (loop (+ i 1)))
            to)
        to)))

(define-method port-write-char ((port <string-port>) char)
  (let* ((buffer (string-port-buffer port))
         (index (string-port-index port)))
    (cond ((< index (string-port-len port))
           (string-set! buffer index char)
           (set-string-port-index! port (+ index 1)))
          ((< index (string-length buffer))
           (string-set! buffer index char)
           (set-string-port-index! port (+ index 1))
           (set-string-port-len! port (string-port-len port)))
          (else
           (let ((new (make-string (+ (string-length buffer) 
                                      $buffer-size)
                                   #\space)))
             (string-copy-from-to buffer new)
             (set-string-port-buffer! port new)
             (port-write-char port char))))))

(define-method port-close ((port <string-port>))
  (output-string port))

(define-method port-position ((port <string-port>))
  (string-port-index port))

(define-method set-port-position! ((port <string-port>) pos)
  (set-string-port-index! port pos))

(define (output-string port)
  (substring (string-port-buffer port) 0 (string-port-index port)))

(define (open-input-string-port string . pos)
  (let ((pos (if (pair? pos) (car pos) 0)))
    (make-string-port "<input string port>" 'input #f string pos (string-length string))))

(define (open-output-string-port)
  (make-string-port "<output string port>" 'output #f (make-string $buffer-size #\space) 0 0))

(define (with-output-to-string proc)
  (let ((port (open-output-string-port)))
    (with-output-to-port port proc)
    (port-close port)))

(define (with-input-from-string string proc)
  (let ((port (open-input-string-port string)))
    (with-input-from-port port proc)))

;;; General =======================================================

(define $mode/truncate 1)
(define $mode/append 2)

(define with-process-aligned 
  (lambda (proc) (proc)))

(define (set-process-alignment-procedure! proc)
  (set! with-process-aligned proc))

(define (input-port? port)
  (eq? 'input (port-direction port)))

(define (output-port? port)
  (eq? 'output (port-direction port)))

(define (call-with-input-file string proc)
  (let ((port #f))
    (set! port (open-input-file string))
    (dynamic-wind 
        (lambda () 1)
        (lambda () (proc port))
        (lambda () 
          (close-input-port port)
          (set! port #f)))))

(define (call-with-output-file string proc)
  (let ((port #f))
    (set! port (open-output-file string))
    (dynamic-wind 
        (lambda () (set! port (open-output-file string)))
        (lambda () (proc port))
        (lambda () 
          (close-output-port port)
          (set! port #f)))))

(define (with-input-from-file string proc)
  (let ((port #f))
    (set! port (open-input-file string))
    (dynamic-wind 
        (lambda () 1)
        (lambda () (let-fluid $current-input-port$ port proc))
        (lambda () 
          (close-input-port port)
          (set! port #f)))))

(define (with-output-to-file string proc)
  (let ((port #f))
    (set! port (open-output-file string))
    (dynamic-wind 
        (lambda () 1)
        (lambda () (let-fluid $current-output-port$ port proc))
        (lambda () 
          (close-output-port port)
          (set! port #f)))))

(define (with-output-appended-to-file string proc)
  (let ((port #f))
    (set! port (open-output-file string #t))
    (dynamic-wind 
        (lambda () 1)
        (lambda () (let-fluid $current-output-port$ port proc))
        (lambda () 
          (close-output-port port)
          (set! port #f)))))

(define (with-output-to-port port proc)
  (let-fluid $current-output-port$ port proc))

(define (with-input-from-port port proc)
  (let-fluid $current-input-port$ port proc))

(define (open-input-file filename)
  (let ((channel (with-process-aligned
                  (lambda ()
                    (open-input-channel filename)))))
    (cond ((number? channel)
           ;; should differentiate between EACCESS, EEXIST, EISDIR
           ;; ELOOP, EMFILE, ENAMETOOLONG, ENFILE, ENODEV, ENOENT,
           ;; ENOMEM, ENOSPC, ENOTDIR, ENXIO, EOVERFLOW, EPERM,
           ;; EROFS, ETXTBSY, EWOULDBLOCK
           (error "Error opening file ~a for reading" filename))
          (else
           (make-channel-port filename 'input #f channel)))))

(define (open-output-file filename . append?)
  (let* ((mode (if (null? append?) 
                   $mode/truncate 
                   (if (car append?)
                       $mode/append
                       $mode/truncate)))
         (channel (with-process-aligned
                   (lambda ()
                     (open-output-channel filename mode)))))
    (cond ((number? channel)
           ;; should differentiate between EACCESS, EEXIST, EISDIR
           ;; ELOOP, EMFILE, ENAMETOOLONG, ENFILE, ENODEV, ENOENT,
           ;; ENOMEM, ENOSPC, ENOTDIR, ENXIO, EOVERFLOW, EPERM,
           ;; EROFS, ETXTBSY, EWOULDBLOCK
           (error "Error ~a opening file ~a for writing in mode ~a" channel filename mode))
          (else
           (make-channel-port filename 'output #f channel)))))

(define (close-input-port port)
  (port-close port))

(define (close-output-port port)
  (port-close port))

(define (channel->input-port channel)
  (make-channel-port "<unnamed-port>" 'input #f channel))

(define (channel->output-port channel)
  (make-channel-port "<unnamed-port>" 'output #f channel))

;;; ASCII encoding =============================================================

(define (ascii->char n) (integer->char n))
(define (char->ascii ch) (char->integer ch))

(define $current-input-port$ 
  (make-fluid (make-channel-port "<stdin>" 'input #f (make-channel 0))))

(define (current-input-port) (fluid $current-input-port$))

(define $current-output-port$ 
  (make-fluid (make-channel-port "<stdout>" 'output #f (make-channel 1))))

(define (current-output-port) (fluid $current-output-port$))

(define (initialize-i/o!)
  (set-fluid! $current-input-port$ 
              (make-channel-port "<stdin>" 'input #f (make-channel 0)))
  (set-fluid! $current-output-port$ 
              (make-channel-port "<stdout>" 'output #f (make-channel 1))))

;; Resource

(define *resource-providers* '())

(define (resource-providers) (append *resource-providers* '()))

(define (add-resource-provider provider)
  (set! *resource-providers* (cons provider *resource-providers*)))

(define (remove-resource-provider provider)
  (set! *resource-providers*
        (let rem ((rs *resource-providers*))
          (cond ((null? rs) rs)
                ((eq? provider (car rs)) (cdr rs))
                (else (cons (car rs) (rem (cdr rs))))))))

(define (locate-resource name)
  (let find ((rs *resource-providers*))
    (if (null? rs)
        #f
        (or ((car rs) name)
            (find (cdr rs))))))

(define (open-resource-input-port name)
  (let ((resource-name (locate-resource name)))
    (if resource-name
        (open-input-file resource-name)
        (error "resource ~a not found" name))))

(define (with-input-from-resource name thunk)
  (let ((resource-name (locate-resource name)))
    (if resource
        (with-input-from-file resource-name thunk)
        (error "resource ~a not found" name))))

(define (call-with-input-resource name thunk)
  (let ((ip (open-resource-input-port name)))
    (thunk ip)
    (close-input-port ip)))
