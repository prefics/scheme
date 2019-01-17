#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include "mem.h"

extern int timer_expired ;
extern int host_signal ;

static
void handler_sig_child(int sig)
{
}

static
void handler_sig_vtalarm(int sig)
{
  timer_expired = 1 ;
}

static
void handler_sig_int(int sig)
{
  host_signal = 1 ;
}

void host_init(int argc, int start, char *argv[])
{
  /* Ignore the SIG_PIPE signal so no errors happens when writing
   * happen
   */
  struct sigaction sa ;

  memset(&sa, 0, sizeof(sa)) ;
  sa.sa_flags = 0 ;
  sigemptyset(&sa.sa_mask) ;
  sa.sa_handler = SIG_IGN ;
  sigaction(SIGPIPE, &sa, NULL) ;
  sa.sa_handler = handler_sig_child ;
  sigaction(SIGCHLD, &sa, NULL) ;
  sa.sa_handler = handler_sig_int ;
  sigaction(SIGINT, &sa, NULL) ;
            
  fcntl(0, F_SETFL, O_NONBLOCK) ;
  fcntl(1, F_SETFL, O_NONBLOCK) ;
  fcntl(2, F_SETFL, O_NONBLOCK) ;
}


int host_make_channel(int num, host_channel_t *handle)
{
  *handle = num ;
  return 0 ;
}

int host_open_input_channel(char *filename, host_channel_t *file)
{
  int fd = open(filename, O_RDONLY | O_NONBLOCK,
		S_IRUSR | S_IWUSR |
		S_IRGRP | S_IWGRP |
		S_IROTH) ;

  if (fd == -1)
    return errno ;

  *file = fd ;
  return 0 ;
}

int host_open_output_channel(char *filename, host_channel_t *file, int mode)
{
  mode_t unix_mode = O_WRONLY | O_CREAT | O_NONBLOCK;
  int fd ;

  if (mode & 0x01) unix_mode |= O_TRUNC ;
  if (mode & 0x02) unix_mode |= O_APPEND ;

  fd = open(filename, unix_mode,
	    S_IRWXU | S_IRWXG | S_IRWXO) ;

  if (fd == -1)
    return errno ;

  *file = fd ;
  return 0 ;
}

int host_read_channel(host_channel_t file, void *buffer, int start, int end)
{
  int rc ;
  char *b = (char *) buffer ;

  rc = read(file, b+start, end-start) ;

  return rc ;
}


int host_write_channel(host_channel_t file, void *buff, int start, int end)
{
  ssize_t written ;
  char *b = (char *) buff ;

  written = write(file, b+start, end-start) ;

  return written ;
}

int host_close_channel(host_channel_t file)
{
  int err = close(file) ;

  if (err == -1)
    return errno ;
  else
    return 0 ;
}

void error(char *msg)
{
  printf("%s\n", msg) ; fflush(NULL) ;
  exit(1) ;
}

static int errno_mapping[] =
  { 0,
    EPERM,         ENOENT,       ESRCH,            EINTR,
    EIO,           ENXIO,        E2BIG,            ENOEXEC,
    EBADF,         ECHILD,       EAGAIN,           ENOMEM,
    EACCES,        EFAULT,       ENOTBLK,          EBUSY,
    EEXIST,        EXDEV,        ENODEV,           ENOTDIR,
    EISDIR,        EINVAL,       ENFILE,           EMFILE,
    ENOTTY,        ETXTBSY,      EFBIG,            ENOSPC,
    ESPIPE,        EROFS,        EMLINK,           EPIPE,
    EDOM,          ERANGE,       EDEADLK,          ENAMETOOLONG,
    ENOLCK,        ENOSYS,       ENOTEMPTY,        ELOOP,
    ENOMSG,        EIDRM,        ECHRNG,           EL2NSYNC,
    EL3HLT,        EL3RST,       ELNRNG,           EUNATCH,
    ENOCSI,        EL2HLT,       EBADE,            EBADR,
    EXFULL,        ENOANO,       EBADRQC,          EBADSLT,
    EBFONT,        ENOSTR,       ENODATA,          ETIME,
    ENOSR,         ENONET,       ENOPKG,           EREMOTE,
    ENOLINK,       EADV,         ESRMNT,           ECOMM,
    EPROTO,        EMULTIHOP,    EDOTDOT,          EBADMSG,
    EOVERFLOW,     ENOTUNIQ,     EBADFD,           EREMCHG,
    ELIBACC,       ELIBBAD,      ELIBSCN,          ELIBMAX,
    ELIBEXEC,      EILSEQ,       ERESTART,         ESTRPIPE,
    EUSERS,        ENOTSOCK,     EDESTADDRREQ,     EMSGSIZE,
    EPROTOTYPE,    ENOPROTOOPT,  EPROTONOSUPPORT,  ESOCKTNOSUPPORT,
    EOPNOTSUPP,    EPFNOSUPPORT, EAFNOSUPPORT,     EADDRINUSE,
    EADDRNOTAVAIL, ENETDOWN,     ENETUNREACH,      ENETRESET,
    ECONNABORTED,  ECONNRESET,   ENOBUFS,          EISCONN,
    ENOTCONN,      ESHUTDOWN,    ETOOMANYREFS,     ETIMEDOUT,
    ECONNREFUSED,  EHOSTDOWN,    EHOSTUNREACH,     EALREADY,
    EINPROGRESS,   ESTALE,       EUCLEAN,          ENOTNAM,
    ENAVAIL,       EISNAM,       EREMOTEIO,        EDQUOT,
    ENOMEDIUM,     EMEDIUMTYPE
} ;

int host_same_error(int host_error, int error)
{
  return errno_mapping[error] == host_error ;
}
