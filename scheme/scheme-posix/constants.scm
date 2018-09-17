;;; constants.scm -- Posix conditions used

;;;
;; = POSIX constants definition
;;
;; This part defines some general constants for use when working
;; with the posix interface for Scheme. It basically contains constants for:
;;
;;   * File accessing mode
;;   * Access mode ?!?
;;   * File mode describing file types
;;   * ERRNO constants
;;   * POSIX signal constants

;;;
;; == POSIX error

;;;
;; Posix condition signalled by POSIX system calls

(define-record-type (<posix-error> <error>)
  (make-posix-error errno syscall arguments)
  posix-error?
  (errno posix-error-errno)
  (syscall posix-error-syscall)
  (arguments posix-error-arguments))

(define-method describe-condition ((c <posix-error>))
  "posix call returned an error code")

;;;
;; Signals a POSIX condition. The `errno` specifies the POSIX error number
;; to signal, `syscall` defines the system call that signalled the error and
;; `args` are the argument given when performing the system call.
(define (posix-error errno syscall . args)
  (signal (make-posix-error errno syscall args)))

;;;
;; Same as `posix-error`. See [[scm:posix-error]].
(define (errno-error errno syscall . args)
  (signal (make-posix-error errno syscall args)))

(define-syntax with-errno-handler
  (syntax-rules ()
    ((with-errno-handler (clauses ...) body ...)
     (with-errno-handler* (lambda (errno packet)
                            (case errno
                              clauses ...))
                          (lambda () body ...)))))

(define (with-errno-handler* handler thunk)
  (condition-case
   (thunk)
   (<posix-error> (lambda (error) (handler error)))))

(define-syntax syscall
  (syntax-rules ()
    ((syscall ?exp)
     (let loop ((errno ?exp))
       (if (= $errno/eintr)
           (loop ?exp)
           errno)))))

;;;
;; == File Access Mode Constants

;;;
;; Owner has all rights on the file (read/write/execute).
(define $owner/all     1792)
(define $owner/read    1024)
(define $owner/write   512)
(define $owner/execute 256)

;;;
;; User of the same group have all rights on the file (read/write/execute).
(define $group/all     128)
(define $group/read    64)
(define $group/write   32)
(define $group/execute 16)

;;;
;; Other people have all rights on the file (read/write/execute).
(define $other/all     7)
(define $other/read    4)
(define $other/write   2)
(define $other/execute 1)

;;;
;; == Access mode for access syscall

;;;
;; Access is possible for a read operation
(define $access/read    4)
(define $access/write   2)
(define $access/execute 1)
(define $access/exist   0)

;;;
;; == File mode
;;
;; File mode constants define the different file type supported on the
;; file system.

;;; FIFO file
(define $mode/fifo      1)
;;; Character device file
(define $mode/char      2)
;;; Directory file
(define $mode/directory 4)
;;; Block device file
(define $mode/block     8)
;;; Plain file
(define $mode/file      16)
;;; Link file
(define $mode/link      32)
;;; Socket file
(define $mode/socket    64)

;;;
;; == ERRNO Constants 
;;
;; ERRNO constants follow the definition of the Linux kernel. See file
;; `/usr/include/asm/errno.h` in your Linux kernel source file.

;;; Operation not permitted
(define	$errno/perm		 1)
;;; No such file or directory
(define	$errno/noent		 2)
;;; No such process
(define	$errno/srch		 3)
;;; Interrupted system call
(define	$errno/intr		 4)
;;; I/O error
(define	$errno/io		 5)
;;; No such device or address
(define	$errno/nxio		 6)
;;; Arg list too long
(define	$errno/2big		 7)
;;; Exec format error
(define	$errno/noexec		 8)
;;; Bad file number
(define	$errno/badf		 9)
;;; No child processes
(define	$errno/child		10)
;;; Try again
(define	$errno/again		11)
;;; Out of memory
(define	$errno/nomem		12)
;;; Permission denied
(define	$errno/acces		13)
;;; Bad address
(define	$errno/fault		14)
;;; Block device required
(define	$errno/notblk		15)
;;; Device or resource busy
(define	$errno/busy		16)
;;; File exists
(define	$errno/exist		17)
;;; Cross-device link
(define	$errno/xdev		18)
;;; No such device
(define	$errno/nodev		19)
;;; Not a directory
(define	$errno/notdir		20)
;;; Is a directory
(define	$errno/isdir		21)
;;; Invalid argument
(define	$errno/inval		22)
;;; File table overflow
(define	$errno/nfile		23)
;;; Too many open files
(define	$errno/mfile		24)
;;; Not a typewriter
(define	$errno/notty		25)
;;; Text file busy
(define	$errno/txtbsy		26)
;;; File too large
(define	$errno/fbig		27)
;;; No space left on device
(define	$errno/nospc		28)
;;; Illegal seek
(define	$errno/spipe		29)
;;; Read-only file system
(define	$errno/rofs		30)
;;; Too many links
(define	$errno/mlink		31)
;;; Broken pipe
(define	$errno/pipe		32)
;;; Math argument out of domain of func
(define	$errno/dom		33)
;;; Math result not representable
(define	$errno/range		34)
;;; Resource deadlock would occur
(define	$errno/deadlk		35)
;;; File name too long
(define	$errno/nametoolong	36)
;;; No record locks available
(define	$errno/nolck		37)
;;; Function not implemented
(define	$errno/nosys		38)
;;; Directory not empty
(define	$errno/notempty	        39)
;;; Too many symbolic links encountered
(define	$errno/loop		40)
;;; Operation would block
(define	$errno/wouldblock	$errno/again)
;;; No message of desired type
(define	$errno/nomsg		42)
;;; Identifier removed
(define	$errno/idrm		43)
;;; Channel number out of range
(define	$errno/chrng		44)
;;; Level 2 not synchronized
(define	$errno/l2nsync	        45)
;;; Level 3 halted
(define	$errno/l3hlt		46)
;;; Level 3 reset
(define	$errno/l3rst		47)
;;; Link number out of range
(define	$errno/lnrng		48)
;;; Protocol driver not attached
(define	$errno/unatch		49)
;;; No CSI structure available
(define	$errno/nocsi		50)
;;; Level 2 halted
(define	$errno/l2hlt		51)
;;; Invalid exchange
(define	$errno/bade		52)
;;; Invalid request descriptor
(define	$errno/badr		53)
;;; Exchange full
(define	$errno/xfull		54)
;;; No anode
(define	$errno/noano		55)
;;; Invalid request code
(define	$errno/badrqc		56)
;;; Invalid slot
(define	$errno/badslt		57)
;;; Dead lock ?!?
(define	$errno/deadlock	        $errno/deadlk)

;;; Bad font file format
(define	$errno/bfont		59)
;;; Device not a stream
(define	$errno/nostr		60)
;;; No data available
(define	$errno/nodata		61)
;;; Timer expired
(define	$errno/time		62)
;;; Out of streams resources
(define	$errno/nosr		63)
;;; Machine is not on the network
(define	$errno/nonet		64)
;;; Package not installed
(define	$errno/nopkg		65)
;;; Object is remote
(define	$errno/remote		66)
;;; Link has been severed
(define	$errno/nolink		67)
;;; Advertise error
(define	$errno/adv		68)
;;; Srmount error
(define	$errno/srmnt		69)
;;; Communication error on send
(define	$errno/comm		70)
;;; Protocol error
(define	$errno/proto		71)
;;; Multihop attempted
(define	$errno/multihop	        72)
;;; RFS specific error
(define	$errno/dotdot		73)
;;; Not a data message
(define	$errno/badmsg		74)
;;; Value too large for defined data type
(define	$errno/overflow	        75)

;;; Name not unique on network
(define	$errno/notuniq	        76)
;;; File descriptor in bad state
(define	$errno/badfd		77)
;;; Remote address changed
(define	$errno/remchg		78)
;;; Can not access a needed shared library
(define	$errno/libacc		79)

;;; Accessing a corrupted shared library
(define	$errno/libbad		80)
;;; .lib section in a.out corrupted
(define	$errno/libscn		81)
;;; Attempting to link in too many shared libraries
(define	$errno/libmax		82)
;;; Cannot exec a shared library directly
(define	$errno/libexec	        83)
;;; Illegal byte sequence
(define	$errno/ilseq		84)
;;; Interrupted system call should be restarted
(define	$errno/restart	        85)
;;; Streams pipe error
(define	$errno/strpipe	        86)
;;; Too many users
(define	$errno/users		87)
;;; Socket operation on non-socket
(define	$errno/notsock	        88)
;;; Destination address required
(define	$errno/destaddrreq	89)
;;; Message too long
(define	$errno/msgsize	        90)
;;; Protocol wrong type for socket
(define	$errno/prototype	91)
;;; Protocol not available
(define	$errno/noprotoopt	92)
;;; Protocol not supported
(define	$errno/protonosupport	93)
;;; Socket type not supported
(define	$errno/socktnosupport	94)
;;; Operation not supported on transport endpoint
(define	$errno/opnotsupp	95)
;;; Protocol family not supported
(define	$errno/pfnosupport	96)
;;; Address family not supported by protocol
(define	$errno/afnosupport	97)
;;; Address already in use
(define	$errno/addrinuse	98)
;;; Cannot assign requested address
(define	$errno/addrnotavail	99)
;;; Network is down
(define	$errno/netdown	        100)
;;; Network is unreachable
(define	$errno/netunreach	101)
;;; Network dropped connection because of reset
(define	$errno/netreset	        102)
;;; Software caused connection abort
(define	$errno/connaborted	103)
;;; Connection reset by peer
(define	$errno/connreset	104)
;;; No buffer space available
(define	$errno/nobufs		105)
;;; Transport endpoint is already connected
(define	$errno/isconn		106)
;;; Transport endpoint is not connected
(define	$errno/notconn	        107)
;;; Cannot send after transport endpoint shutdown
(define	$errno/shutdown	        108)
;;; Too many references: cannot splice
(define	$errno/toomanyrefs	109)
;;; Connection timed out
(define	$errno/timedout	        110)
;;; Connection refused
(define	$errno/connrefused	111)
;;; Host is down
(define	$errno/hostdown	        112)
;;; No route to host
(define	$errno/hostunreach	113)
;;; Operation already in progress
(define	$errno/already	        114)
;;; Operation now in progress
(define	$errno/inprogress	115)
;;; Stale NFS file handle
(define	$errno/stale		116)
;;; Structure needs cleaning
(define	$errno/uclean		117)
;;; Not a XENIX named type file
(define	$errno/notnam		118)
;;; No XENIX semaphores available
(define	$errno/navail		119)
;;; Is a named type file
(define	$errno/isnam		120)
;;; Remote I/O error
(define	$errno/remoteio	        121)
;;; Quota exceeded
(define	$errno/dquot		122)
;;; No medium found
(define	$errno/nomedium	        123)
;;; Wrong medium type
(define	$errno/mediumtype	124)

;;;
;; == POSIX signals

;;; Hangup (POSIX).
(define	$signal/hup	1)
;;; Interrupt (ANSI).
(define	$signal/int	2)
;;; Quit (POSIX).
(define	$signal/quit	3)
;;; Illegal instruction (ANSI).
(define	$signal/ill	4)
;;; Trace trap (POSIX).
(define	$signal/trap	5)
;;; Abort (ANSI).
(define	$signal/abrt	6)
;;; IOT trap (4.2 BSD).
(define	$signal/iot	6)
;;; BUS error (4.2 BSD).
(define	$signal/bus	7)
;;; Floating-point exception (ANSI).
(define	$signal/fpe	8)
;;; Kill, unblockable (POSIX).
(define	$signal/kill	9)
;;; User-defined signal 1 (POSIX).
(define	$signal/usr1	10)
;;; Segmentation violation (ANSI).
(define	$signal/segv	11)
;;; User-defined signal 2 (POSIX).
(define	$signal/usr2	12)
;;; Broken pipe (POSIX).
(define	$signal/pipe	13)
;;; Alarm clock (POSIX).
(define	$signal/alrm	14)
;;; Termination (ANSI).
(define	$signal/term	15)
;;; Stack fault.
(define	$signal/stkflt	16)
;;; Child status has changed (POSIX).
(define	$signal/chld	17)
; Same as SIGCHLD (System V).
(define	$signal/cld	$signal/chld)
;;; Continue (POSIX).
(define	$signal/cont	18)
;;; Stop, unblockable (POSIX).
(define	$signal/stop	19)
;;; Keyboard stop (POSIX).
(define	$signal/tstp	20)
;;; Background read from tty (POSIX).
(define	$signal/ttin	21)
;;; Background write to tty (POSIX).
(define	$signal/ttou	22)
;;; Urgent condition on socket (4.2 BSD).
(define	$signal/urg	23)
;;; CPU limit exceeded (4.2 BSD).
(define	$signal/xcpu	24)
;;; File size limit exceeded (4.2 BSD).
(define	$signal/xfsz	25)
;;; Virtual alarm clock (4.2 BSD).
(define	$sigbal/vtalrm	26)
;;; Profiling alarm clock (4.2 BSD).
(define	$signal/prof	27)
;;; Window size change (4.3 BSD, Sun).
(define	$signal/winch	28)
;;; I/O now possible (4.2 BSD).
(define	$signal/io	29)
;;; Pollable event occurred (System V).
(define	$signal/poll	$signal/io)
;;; Power failure restart (System V).
(define	$signal/pwr	30)
;;; Bad system call.
(define $signal/sys	31)
;;; Unused signal constant
(define $signal/unused  31)
