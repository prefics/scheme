#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifdef SCM_TARGET_POSIX
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/utsname.h>
#include <sys/time.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <dirent.h>
#include <pwd.h>
#include <grp.h>
#include <utime.h>
#include <dlfcn.h>
#include <signal.h>
#include <pty.h>

#include "unix.h"
#endif

#ifdef SCM_TARGET_WIN32
#include "windows.h"
#include "winsock2.h"
#include "win32.h"
#endif

#include "prof.h"
#include "image.h"
#include "proc.h"
#include "mem.h"

#ifdef INDIRECT_THREADED_CODE
#define BYTECODE(code) code
#define NEXT  goto *(code[BVEC(code_vector)->val[pc++]])
#else
#define BYTECODE(code) case code
#define NEXT break
#endif

obj_t val ;
obj_t stack ;
int   top ;
obj_t env ;
obj_t template ;
obj_t code_vector ;
obj_t literals ;
int   pc ;
obj_t cont ;
int   argc ;
obj_t dynamic_env ;
obj_t dynamic_cont ;
obj_t trap ;
int   timer_expired = 0 ;
int   host_signal = 0 ;

/* argument registers */
obj_t a1, a2, a3, a4 ;

static obj_t new_stack(void) ;
static void push(obj_t) ;
static obj_t pop(void) ;
static obj_t suspend_cc(void) ;
static int unroll_list(void) ;

static obj_t random32(void) ;

static
obj_t new_stack(void)
{
  obj_t s = make_vector(DEFAULT_STACK_SIZE) ;
  int i ;

  for (i=0 ; i < DEFAULT_STACK_SIZE ; i++)
    VECTOR(s)->val[i] = obj_undefined ;

  return s ;
}

static
void push(obj_t obj)
{
  if (top == DEFAULT_STACK_SIZE)
    {
      printf("pushing stack\n") ; fflush(NULL) ;
      obj_t vec = new_stack() ;

      VECTOR(vec)->val[0] = stack ;
      top = 1 ;
      stack = vec ;
    }
  VECTOR(stack)->val[top++] = obj ;
}

static
obj_t pop(void)
{
  obj_t v ;

  if (top == 1)
    {
      obj_t vec = make_vector(DEFAULT_STACK_SIZE) ;
      int i ;

      printf("poping stack\n") ; fflush(NULL) ;
      stack = VECTOR(stack)->val[0] ;
      for (i=0 ; i < DEFAULT_STACK_SIZE ; i++)
        VECTOR(vec)->val[i] = VECTOR(stack)->val[i] ;
      stack = vec ;
      top = DEFAULT_STACK_SIZE ;
    }

  v = VECTOR(stack)->val[--top] ;
  VECTOR(stack)->val[top] = obj_undefined ;
  return v ;
}

static
obj_t suspend_cc(void)
{
  obj_t a1 = make_vector(top) ;
  int i ;

  for (i = 0 ; i < top ; i++)
    VECTOR(a1)->val[i] =
      VECTOR(stack)->val[i] ;

  obj_t cc = make_vector(13) ;

  VECTOR(cc)->val[0] = val ;
  VECTOR(cc)->val[1] = a1 ;
  VECTOR(cc)->val[2] = make_fixnum(top) ;
  VECTOR(cc)->val[3] = env ;
  VECTOR(cc)->val[4] = template ;
  VECTOR(cc)->val[5] = code_vector ;
  VECTOR(cc)->val[6] = literals ;
  VECTOR(cc)->val[7] = make_fixnum(pc) ;
  VECTOR(cc)->val[8] = cont ;
  VECTOR(cc)->val[9] = make_fixnum(argc) ;
  VECTOR(cc)->val[10] = dynamic_env ;
  VECTOR(cc)->val[11] = dynamic_cont ;
  VECTOR(cc)->val[12] = trap ;
  return cc ;
}

void resume_cc(obj_t cc)
{
  obj_t old = VECTOR(cc)->val[1] ;
  int i ;
  for (i = 0 ; i < fixnum_value(VECTOR(cc)->val[2]) ; i++)
    VECTOR(stack)->val[i] =
      VECTOR(old)->val[i] ;

  val          = VECTOR(cc)->val[0] ;
  //stack        = VECTOR(cc)->val[1] ;
  top          = fixnum_value(VECTOR(cc)->val[2]) ;
  env          = VECTOR(cc)->val[3] ;
  template     = VECTOR(cc)->val[4] ;
  code_vector  = VECTOR(cc)->val[5] ;
  literals     = VECTOR(cc)->val[6] ;
  pc           = fixnum_value(VECTOR(cc)->val[7]) ;
  cont         = VECTOR(cc)->val[8] ;
  argc         = fixnum_value(VECTOR(cc)->val[9]) ;
  dynamic_env  = VECTOR(cc)->val[10] ;
  dynamic_cont = VECTOR(cc)->val[11] ;
  trap         = VECTOR(cc)->val[12] ;
}

static
int unroll_list(void)
{
  int length = 0 ;

  a2 = obj_nil ;

  while (a1 != obj_nil && pairp(a1))
    {
      length++ ;
      a3 = a1 ;
      a1 = PAIR(a3)->cdr ;
      PAIR(a3)->cdr = a2 ;
      a2 = a3 ;
    }
  while (a2 != obj_nil)
    {
      a3=a2;
      _PUSH(PAIR(a3)->car) ;
      a2 = PAIR(a3)->cdr ;
      PAIR(a3)->cdr = a1 ;
      a1 = a3 ;
    }
  return length ;
}

obj_t run(obj_t t, obj_t args)
{
#ifdef INDIRECT_THREADED_CODE
  static void *code[] = {
    &&LIT,&&ARGS,&&GLOBAL,&&SET_GLOBAL,&&LOCAL,&&SET_LOCAL,&&CALL,
    &&TAIL,&&IFFALSE,&&JUMP,&&RET,&&PUSH,&&CLOS,&&LEAVE,&&ARGSGT,
    &&UNDEF,&&ENV,&&BADCODE,&&BADCODE,&&BADCODE,&&BADCODE,&&BADCODE,
    &&BADCODE,&&BADCODE,&&BADCODE,&&BADCODE,&&BADCODE,&&BADCODE,
    &&BADCODE,&&BADCODE,&&BADCODE,&&BADCODE,

    &&PLUS,&&MINUS,&&QUOTIENT,&&REMAINDER,&&MODULO,
    &&MULT,&&LT,&&LTE,&&EQUAL,&&GT,&&GTE,&&SYMBOLP,&&EQP,&&FIXNUMP,
    &&PAIRP,&&CONS,

    &&CAR,&&CDR,&&SETCAR,&&SETCDR,&&NULLP,&&VECTORP,&&MAKEVECTOR,
    &&VECTORLENGTH,&&VECTORREF,&&VECTORSET,&&MAKESTRING,&&STRINGLENGTH,
    &&STRINGREF,&&STRINGSET,&&MAKECHANNEL,&&WRITE,

    &&READ,&&EOFP,&&CHANNEL_NUMBER,&&CHANNELP,&&OPENINPUTCHANNEL,&&OPENOUTPUTCHANNEL,
    &&CHAR2FIXNUM,&&FIXNUM2CHAR,&&SYMBOL2STRING,&&STRING2SYMBOL,
    &&APPLY,&&MAKESTOB,&&STOBREF,&&STOBSET,&&STOBCLASS,&&STOBP,

    &&STOBLENGTH,&&CALLCC,&&RESUMECC,&&MAKEBVEC,&&BVECP,&&BVECREF,
    &&BVECSET,&&BVECLENGTH,&&SAVEIMAGE,&&BADCODE,&&BADCODE,
    &&MAKEPROCEDURE,&&PROCEDUREP,&&PROCEDURESET,&&PROCEDUREREF,
    &&PROCEDURELENGTH,

    &&CHARP,
    &&CLOSECHANNEL,&&PEEKCHANNEL,&&STRINGP,
    &&MAKEREF,&&REFP,&&REFNAME,&&REFVALUE,&&REFMODULE,&&SETREFNAME,
    &&SETREFVALUE,&&SETREFMODULE,&&SETSTOBCLASS,&&BADCODE,&&BADCODE,
    &&BADCODE,

    &&BADCODE,&&BADCODE,
    &&BITOR,&&BITAND,&&BITXOR,
    &&BITASH,&&BITNOT,&&OBJECT_HASH,&&BADCODE,&&DIV,
    &&BADCODE,&&BADCODE,&&BADCODE,&&BADCODE,&&BADCODE,&&BADCODE,

    &&UNIX_OPEN,&&UNIX_CLOSE,&&UNIX_READ,&&UNIX_WRITE,&&UNIX_CHDIR,
    &&UNIX_ACCESS,&&UNIX_MKDIR,&&UNIX_STAT,&&UNIX_TIME,
    &&UNIX_LOCALTIME,&&UNIX_GMTIME,&&UNIX_SEEK,&&UNIX_SYNC,&&UNIX_RMDIR,
    &&UNIX_UNLINK,&&UNIX_RENAME,

    &&UNIX_CHMOD,&&UNIX_CHGRP,&&UNIX_CHOWN,
    &&UNIX_GETENV,&&UNIX_SETENV,
    &&UNIX_FORK,&&UNIX_EXEC,&&UNIX_DIRFILES,
    &&UNIX_RECV,&&UNIX_SEND,&&UNIX_SOCKET,&&UNIX_BIND,&&UNIX_LISTEN,
    &&UNIX_ACCEPT,&&UNIX_CONNECT,&&UNIX_SELECT,

    &&UNIX_MKNOD,&&UNIX_TEMPNAM,
    &&UNIX_GETPWNAM,&&UNIX_GETGRNAM,&&UNIX_UTIME,
    &&UNIX_UMASK,&&UNIX_GETPID,
    &&UNIX_GETPPID,&&UNIX_GETSID,&&UNIX_SETSID,&&UNIX_GETUID,&&UNIX_GETEUID,
    &&UNIX_GETGID,&&UNIX_GETEGID,&&UNIX_SETUID,&&UNIX_SETEGID,

    &&UNIX_SETEUID,&&UNIX_SETGID,&&UNIX_EXIT,&&UNIX_WAITPID,&&UNIX_GETCWD,
    &&UNIX_PIPE,
    &&UNIX_DUP2,&&FFI_DLOPEN,&&FFI_DLERROR,&&FFI_DLSYM,&&FFI_DLCLOSE,
    &&FFI_APPLY,&&FFI_MEM_REF,&&FFI_STRING_REF,&&FFI_MEM_SET,&&FFI_MIRROR,

    &&FFI_MEM_DEREF,&&FFI_MALLOC,&&FFI_FREE,&&FFI_U8_REF,&&FFI_U16_REF,
    &&FFI_U32_REF,&&FFI_U64_REF,&&FFI_DOUBLE_REF,
    &&UNIX_KILL,&&UNIX_OPENPTY,&&BADCODE,&&BADCODE,&&BADCODE,&&BADCODE,&&BADCODE,
    &&BADCODE,

    &&BADCODE,&&BADCODE,&&BADCODE,&&BADCODE,&&UNIX_UNAME,
    &&UNIX_SETSOCKOPT,&&UNIX_GETSOCKOPT,&&UNIX_UNIX_ADDRESS,&&UNIX_INET_ADDRESS,
    &&UNIX_SIGNALBLOCK,&&ASSQ,&&RECORD_REF,&&RECORD_SET,&&TEST_AND_SET,
    &&REAL2STRING,&&STRING2REAL,

    &&FIXNUM2REAL,&&REALP,
    &&ROUND,&&TRUNCATE,
    &&CEILING,&&FLOOR,&&RATIONALIZE,&&ZEXP,&&ZLOG,&&ZSIN,&&ZCOS,&&ZTAN,
    &&ZASIN,&&ZACOS,&&ZATAN,&&ZATAN2,

    &&ZSQRT,&&ZEXPT,
    &&NATIVE_CALL,&&CHANNEL_FLUSH,&&HOSTERROR,&&HOSTSAMEERROR,
    &&SETTIMER,&&UNIX_SYMLINK,&&UNIX_READLINK,&&RANDOM,&&CLASSOF,&&SETCLASSES,&&INSTANCE,
    &&SUBTYPE,&&BADCODE,&&BADCODE
  } ;
#endif
  val = obj_undefined ;
  stack = new_stack() ;
  VECTOR(stack)->val[0] = obj_undefined ;
  top = 1 ;
  env = obj_nil ;
  template = t ;
  pc = 0 ;
  code_vector = VECTOR(template)->val[0] ;
  literals = VECTOR(template)->val[1] ;
  cont = obj_undefined ;
  dynamic_env = obj_nil ;
  dynamic_cont = obj_nil ;
  trap = obj_undefined ;
  argc = 1 ;
  _PUSH(args) ;

#ifdef INDIRECT_THREADED_CODE
  NEXT;
#else
  for(;;)
    {
      switch(BVEC(code_vector)->val[pc++])
        {
#endif
        BYTECODE(LIT):
          val = VECTOR(literals)->val[BVEC(code_vector)->val[pc++]] ;
          NEXT ;
        BYTECODE(PUSH):
          _PUSH(val) ;
          NEXT ;
        BYTECODE(ARGS):
          {
            int args = BVEC(code_vector)->val[pc++] ;
	    int i ;

            a1 = make_vector(argc+1) ;
            for (i=1 ; i <= argc ; i++) VECTOR(a1)->val[i] = obj_undefined ;
	    for (i=1 ; i <= argc ; i++)
	      {
		obj_t v = pop() ;
		VECTOR(a1)->val[i] = v ;
	      }

            VECTOR(a1)->val[0] = env ;
            if (argc != args)
              {
		push(a1) ;
		push(val) ;
		TRAPCALL(TRAP_BAD_ARGS, 2) ;
                NEXT ;
              }

            env = a1 ;

	    if (timer_expired)
	      {
		timer_expired = 0 ;
		TRAP0(TRAP_TIMER) ;
	      }
	    if (host_signal)
	      {
		host_signal = 0 ;
		TRAP0(TRAP_SIGNAL) ;
	      }

            NEXT ;
          }
        BYTECODE(ARGSGT):
          {
            int args = BVEC(code_vector)->val[pc++] ;
            int i ;

            if (argc < args)
	      {
		a1 = make_vector(argc+1) ;
		for (i=1 ; i <= argc ; i++) VECTOR(a1)->val[i] = obj_undefined ;
		for (i=1 ; i <= argc ; i++)
		  {
		    obj_t v = pop() ;
		    VECTOR(a1)->val[i] = v ;
		  }

		VECTOR(a1)->val[0] = env ;
		push(a1) ;
		push(val) ;
		TRAPCALL(TRAP_BAD_ARGS, 2) ;
		NEXT ;
	      }
            a1 = make_vector(args+2) ;
            for (i=0 ; i <= args+1 ; i++) VECTOR(a1)->val[i] = obj_undefined ;
            a2 = make_list(argc-args) ;
            for (i=1 ; i <= args ; i++)
	      {
		obj_t v = pop() ;
		VECTOR(a1)->val[i] = v ;
	      }
            VECTOR(a1)->val[args+1] = a2 ;
            for (i=args, a3=a2; i < argc ; i++, a3=PAIR(a3)->cdr)
	      {
		obj_t v = pop() ;
		PAIR(a3)->car = v ;
	      }
            VECTOR(a1)->val[0] = env ;
            env = a1 ;

	    if (timer_expired)
	      {
		timer_expired = 0 ;
		TRAP0(TRAP_TIMER) ;
	      }

	    if (host_signal)
	      {
		host_signal = 0 ;
		TRAP0(TRAP_SIGNAL) ;
	      }
            NEXT ;
          }
        BYTECODE(GLOBAL):
          val = REF(VECTOR(literals)->val[BVEC(code_vector)->val[pc]])->value ;
#ifdef SCHEMY_DEBUG_GLOBAL
	  printf("Global '%s'",
		 &SYMBOL(REF(VECTOR(literals)->val[BVEC(code_vector)->val[pc]])->name)->ch[0]) ;
	  fflush(NULL) ;
#endif
	  if (val == obj_unbound)
            {
	      obj_t global = VECTOR(literals)->val[BVEC(code_vector)->val[pc++]] ;
              TRAP1(TRAP_UNBOUND_GLOBAL, global) ;
            }
          else
            pc++ ;
          NEXT ;
        BYTECODE(SET_GLOBAL):
	  if (refp(VECTOR(literals)->val[BVEC(code_vector)->val[pc]]))
	    REF(VECTOR(literals)->val[BVEC(code_vector)->val[pc++]])->value = val ;
	  else
	    {
	      write_obj(VECTOR(literals)->val[BVEC(code_vector)->val[pc]]) ;
	      printf("SET-GLOBAL! not a ref \n") ; fflush(NULL) ;
	    }
          NEXT ;
        BYTECODE(LOCAL):
          {
            int i, j ;
            obj_t e ;

            i = BVEC(code_vector)->val[pc++] ;
            j = BVEC(code_vector)->val[pc++] ;
            for (e=env ; i > 0 ; i--)
              e = VECTOR(e)->val[0] ;
            val = VECTOR(e)->val[j] ;
            NEXT ;
          }
        BYTECODE(SET_LOCAL):
          {
            int i, j ;
            obj_t e ;

            i = BVEC(code_vector)->val[pc++] ;
            j = BVEC(code_vector)->val[pc++] ;
            for (e=env ; i > 0 ; i--)
              e = VECTOR(e)->val[0] ;
            VECTOR(e)->val[j] = val ;
            NEXT ;
          }
        BYTECODE(CALL):
	  a1 = make_vector(4) ;

	  VECTOR(a1)->val[0] = cont ;
	  VECTOR(a1)->val[1] = env ;
	  VECTOR(a1)->val[2] = template ;
	  VECTOR(a1)->val[3] = make_fixnum(pc+1) ;
	  cont = a1 ;
	BYTECODE(TAIL):
          if (do_sample_profile)
            profiler_sample() ;

	  if (closurep(val))
	    {
	      /* call closure */
	      argc = BVEC(code_vector)->val[pc] ;
	      env = CLOSURE(val)->val[0] ;
	      template = CLOSURE(val)->val[1] ;
	      code_vector = VECTOR(template)->val[0] ;
	      literals = VECTOR(template)->val[1] ;
	      pc = 0 ;

              /* code for printing the function being called
              obj_t debug = VECTOR(template)->val[2] ;
              if (vectorp(debug))
                {
                  int i ;

                  printf("Calling") ;
                  write_obj(VECTOR(VECTOR(template)->val[2])->val[0]) ;
                  for(i=1; i <= argc ; i++)
                    write_obj(VECTOR(stack)->val[top-i]) ;
                  printf("\n") ; fflush(NULL) ;
                }
              */
	    }
	  else if (vectorp(trap) && VECTOR(trap)->val[TRAP_NO_PROCEDURE])
	    {
	      push(val) ;
	      val = VECTOR(trap)->val[TRAP_NO_PROCEDURE] ;
	      argc = BVEC(code_vector)->val[pc]+1 ;
	      env = CLOSURE(val)->val[0] ;
	      template = CLOSURE(val)->val[1] ;
	      code_vector = VECTOR(template)->val[0] ;
	      literals = VECTOR(template)->val[1] ;
	      pc = 0 ;
	    }
	  else *((int *) 0) = 0 ;
	  NEXT ;
        BYTECODE(IFFALSE):
          if (val == obj_false)
            pc = 256*BVEC(code_vector)->val[pc]+BVEC(code_vector)->val[pc+1] ;
          else
            pc+=2 ;
          NEXT ;
        BYTECODE(JUMP):
          pc = 256*BVEC(code_vector)->val[pc]+BVEC(code_vector)->val[pc+1] ;
          NEXT ;
        BYTECODE(RET):
          if (cont == obj_undefined)
            return val ;

          pc = fixnum_value(VECTOR(cont)->val[3]) ;
          template = VECTOR(cont)->val[2] ;
          code_vector = VECTOR(template)->val[0] ;
          literals = VECTOR(template)->val[1] ;
          env = VECTOR(cont)->val[1] ;
          cont = VECTOR(cont)->val[0] ;

          /* code for returning the return value
          printf("returning => ") ;
          write_obj(val) ;
          printf("\n") ;
          */
          NEXT ;
        BYTECODE(CLOS):
          val = make_closure(2) ;
	  CLOSURE(val)->val[1] = VECTOR(literals)->val[BVEC(code_vector)->val[pc++]] ;
	  CLOSURE(val)->val[0] = env ;
          NEXT ;
        BYTECODE(LEAVE):
          env = VECTOR(env)->val[0] ;
          NEXT ;
        BYTECODE(UNDEF):
          {
            int size = BVEC(code_vector)->val[pc++] ;
            int i ;

            a1 = make_vector(size+1) ;
            VECTOR(a1)->val[0] = env ;
            for (i = 1 ; i <= size ; i++)
	      VECTOR(a1)->val[i] = obj_undefined ;
            env = a1 ;
            NEXT ;
          }
        BYTECODE(ENV):
          {
            int args = BVEC(code_vector)->val[pc++] ;
	    int i ;

            a1 = make_vector(args+1) ;
            VECTOR(a1)->val[0] = env ;
            for (i=1 ; i <= args ; i++) VECTOR(a1)->val[i] = obj_undefined ;
	    for (i=1 ; i <= args ; i++)
	      {
		obj_t v = pop() ;
		VECTOR(a1)->val[i] = v ;
	      }
            env = a1 ;
            NEXT ;
          }
        BYTECODE(PLUS):
	  a1 = val ;
	  a2 = pop() ;
	  if (fixnump(a1))
	    {
	      if (fixnump(a2))
		val = make_fixnum(fixnum_value(a1)+fixnum_value(a2)) ;
	      else if (realp(a2))
		val = make_real(fixnum_value(a1)+ REAL(a2)->value) ;
	      else if (bvecp(a2))
		{
		  obj_t v1 = fixnum_value(a1) ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1+v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		{
		  TRAP2(TRAP_PRIMITIVE, a1, a2) ;
		}
	    }
	  else if (realp(a1))
	    {
	      if (fixnump(a2))
		val = make_real(REAL(a1)->value + fixnum_value(a2)) ;
	      else if (realp(a2))
		val = make_real(REAL(a1)->value + REAL(a2)->value) ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a1)->val[0] ;
		  val = make_real(REAL(a1)->value+v2) ;
		}
	      else
		{
		  TRAP2(TRAP_PRIMITIVE, a1, a2) ;
		}
	    }
	  else if (bvecp(a1))
	    {
	      if (fixnump(a2))
		{
		  obj_t v2 = fixnum_value(a2) ;
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t res = v1+v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else if (realp(a2))
		{
		  obj_t v1 = *(obj_t*)&BVEC(a1)->val[0] ;
		  val = make_real(v1+REAL(a2)->value) ;
		}
	      else if (bvecp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1+v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		{
		  TRAP2(TRAP_PRIMITIVE, a1, a2) ;
		}
	    }
	  else
	    {
	      TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  NEXT ;
        BYTECODE(MINUS):
	  a1 = val ;
	  a2 = pop() ;
	  if (fixnump(a1))
	    {
	      if (fixnump(a2))
		val = make_fixnum(fixnum_value(a1)-fixnum_value(a2)) ;
	      else if (realp(a2))
		val = make_real(fixnum_value(a1)-REAL(a2)->value) ;
	      else if (bvecp(a2))
		{
		  obj_t v1 = fixnum_value(a1) ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1-v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (realp(a1))
	    {
	      if (fixnump(a2))
		val = make_real(REAL(a1)->value-fixnum_value(a2)) ;
	      else if (realp(a2))
		val = make_real(REAL(a1)->value-REAL(a2)->value) ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = REAL(a1)->value-v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (bvecp(a1))
	    {
	      if (fixnump(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0];
		  obj_t v2 =  fixnum_value(a2);
		  obj_t res = v1-v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else if (realp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  val = make_real(v1-REAL(a2)->value) ;
		}
	      else if (bvecp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1-v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(QUOTIENT):
	  a1 = val ;
	  a2 = pop() ;
	  if (fixnump(a1))
	    {
	      if (fixnump(a2))
		val = make_fixnum(fixnum_value(a1) / fixnum_value(a2)) ;
	      else if (bvecp(a2))
		{
		  obj_t v1 = fixnum_value(a1) ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1/v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (bvecp(a1))
	    {
	      if (fixnump(a2))
		{
		  obj_t v2 = fixnum_value(a2) ;
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t res = v1/v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else if (bvecp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1/v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(REMAINDER):
	  a1 = val ;
	  a2 = pop() ;
	  if (fixnump(a1))
	    {
	      if (fixnump(a2))
		val = make_fixnum(fixnum_value(a1) % fixnum_value(a2)) ;
	      else if (bvecp(a2))
		{
		  obj_t v1 = fixnum_value(a1) ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1%v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (bvecp(a1))
	    {
	      if (fixnump(a2))
		{
		  obj_t v2 = fixnum_value(a2) ;
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t res = v1%v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else if (bvecp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1%v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(MODULO):
	  a1 = val ;
	  a2 = pop() ;
	  if (fixnump(a1))
	    {
	      if (fixnump(a2))
		val = make_fixnum(fixnum_value(a1) % fixnum_value(a2)) ;
	      else if (bvecp(a2))
		{
		  obj_t v1 = fixnum_value(a1) ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1%v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (bvecp(a1))
	    {
	      if (fixnump(a2))
		{
		  obj_t v2 = fixnum_value(a2) ;
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t res = v1%v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else if (bvecp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1%v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(MULT):
	  a1 = val ;
	  a2 = pop() ;
	  if (fixnump(a1))
	    {
	      if (fixnump(a2))
		val = make_fixnum(fixnum_value(a1)*fixnum_value(a2)) ;
	      else if (realp(a2))
		val = make_real(fixnum_value(a1)*REAL(a2)->value) ;
	      else if (bvecp(a2))
		{
		  obj_t v1 = fixnum_value(a1) ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1*v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (realp(a1))
	    {
	      if (fixnump(a2))
		val = make_real(REAL(a1)->value * fixnum_value(a2)) ;
	      else if (realp(a2))
		val = make_real(REAL(a1)->value * REAL(a2)->value) ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a1)->val[0] ;
		  val = make_real(REAL(a1)->value*v2) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (bvecp(a1))
	    {
	      if (fixnump(a2))
		{
		  obj_t v2 = fixnum_value(a2) ;
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t res = v1*v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else if (realp(a2))
		{
		  obj_t v1 = *(obj_t*)&BVEC(a1)->val[0] ;
		  val = make_real(REAL(a2)->value*v1) ;
		}
	      else if (bvecp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1*v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(DIV):
	  a1 = val ;
	  a2 = pop() ;
	  if (fixnump(a1))
	    {
	      if (fixnump(a2))
		{
		  if (fixnum_value(a1) % fixnum_value(a2) == 0)
		    val = make_fixnum(fixnum_value(a1)/fixnum_value(a2)) ;
		  else
		    val = make_real(((double) fixnum_value(a1)) / fixnum_value(a2)) ;
		}
	      else if (realp(a2))
		val = make_real(((double)fixnum_value(a1))/REAL(a2)->value) ;
	      else if (bvecp(a2))
		{
		  obj_t v1 = fixnum_value(a1) ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1/v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (realp(a1))
	    {
	      if (fixnump(a2))
		val = make_real(REAL(a1)->value / fixnum_value(a2)) ;
	      else if (realp(a2))
		val = make_real(REAL(a1)->value / REAL(a2)->value) ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a1)->val[0] ;
		  val = make_real(REAL(a1)->value/v2) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (bvecp(a1))
	    {
	      if (fixnump(a2))
		{
		  obj_t v2 = fixnum_value(a2) ;
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t res = v1/v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else if (realp(a2))
		{
		  obj_t v1 = *(obj_t*)&BVEC(a1)->val[0] ;
		  val = make_real(v1/REAL(a2)->value) ;
		}
	      if (bvecp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  obj_t res = v1/v2 ;
		  val = make_bvec(sizeof(obj_t)) ;
		  memcpy(&BVEC(val)->val[0], &res, sizeof(obj_t)) ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(LT):
	  a1 = val ;
	  a2 = pop() ;
	  if (fixnump(a1))
	    {
	      if (fixnump(a2))
		val = fixnum_value(a1) < fixnum_value(a2) ? obj_true : obj_false ;
	      else if (realp(a2))
		val = fixnum_value(a1) < REAL(a2)->value ? obj_true : obj_false ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = fixnum_value(a1) < v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (realp(a1))
	    {
	      if (fixnump(a2))
		val = REAL(a1)->value < fixnum_value(a2) ? obj_true : obj_false ;
	      else if (realp(a2))
		val = REAL(a1)->value < REAL(a2)->value ? obj_true : obj_false ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = REAL(a1)->value < v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (bvecp(a1))
	    {
	      if (fixnump(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;

		  val = v1 < fixnum_value(a2) ? obj_true : obj_false ;
		}
	      else if (realp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;

		  val = v1 < REAL(a2)->value ? obj_true : obj_false ;
		}
	      else if (bvecp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = v1 < v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(LTE):
	  a1 = val ;
	  a2 = pop() ;
	  if (fixnump(a1))
	    {
	      if (fixnump(a2))
		val = fixnum_value(a1) <= fixnum_value(a2) ? obj_true : obj_false ;
	      else if (realp(a2))
		val = fixnum_value(a1) <= REAL(a2)->value ? obj_true : obj_false ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = fixnum_value(a1) <= v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (realp(a1))
	    {
	      if (fixnump(a2))
		val = REAL(a1)->value <= fixnum_value(a2) ? obj_true : obj_false ;
	      else if (realp(a2))
		val = REAL(a1)->value <= REAL(a2)->value ? obj_true : obj_false ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = REAL(a1)->value <= v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (bvecp(a1))
	    {
	      if (fixnump(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;

		  val = v1 <= fixnum_value(a2) ? obj_true : obj_false ;
		}
	      else if (realp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;

		  val = v1 <= REAL(a2)->value ? obj_true : obj_false ;
		}
	      else if (bvecp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = v1 <= v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(EQUAL):
	  a1 = val ;
	  a2 = pop() ;
	  if (fixnump(a1))
	    {
	      if (fixnump(a2))
		val = fixnum_value(a1) == fixnum_value(a2) ? obj_true : obj_false ;
	      else if (realp(a2))
		val = fixnum_value(a1) == REAL(a2)->value ? obj_true : obj_false ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = fixnum_value(a1) == v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (realp(a1))
	    {
	      if (fixnump(a2))
		val = REAL(a1)->value == fixnum_value(a2) ? obj_true : obj_false ;
	      else if (realp(a2))
		val = REAL(a1)->value == REAL(a2)->value ? obj_true : obj_false ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = REAL(a1)->value == v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (bvecp(a1))
	    {
	      if (fixnump(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;

		  val = v1 == fixnum_value(a2) ? obj_true : obj_false ;
		}
	      else if (realp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;

		  val = v1 == REAL(a2)->value ? obj_true : obj_false ;
		}
	      else if (bvecp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = v1 == v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(GT):
	  a1 = val ;
	  a2 = pop() ;
	  if (fixnump(a1))
	    {
	      if (fixnump(a2))
		val = fixnum_value(a1) > fixnum_value(a2) ? obj_true : obj_false ;
	      else if (realp(a2))
		val = fixnum_value(a1) > REAL(a2)->value ? obj_true : obj_false ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = fixnum_value(a1) > v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (realp(a1))
	    {
	      if (fixnump(a2))
		val = REAL(a1)->value > fixnum_value(a2) ? obj_true : obj_false ;
	      else if (realp(a2))
		val = REAL(a1)->value > REAL(a2)->value ? obj_true : obj_false ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = REAL(a1)->value > v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (bvecp(a1))
	    {
	      if (fixnump(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;

		  val = v1 > fixnum_value(a2) ? obj_true : obj_false ;
		}
	      else if (realp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;

		  val = v1 > REAL(a2)->value ? obj_true : obj_false ;
		}
	      else if (bvecp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = v1 > v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(GTE):
	  a1 = val ;
	  a2 = pop() ;
	  if (fixnump(a1))
	    {
	      if (fixnump(a2))
		val = fixnum_value(a1) >= fixnum_value(a2) ? obj_true : obj_false ;
	      else if (realp(a2))
		val = fixnum_value(a1) >= REAL(a2)->value ? obj_true : obj_false ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = fixnum_value(a1) >= v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (realp(a1))
	    {
	      if (fixnump(a2))
		val = REAL(a1)->value >= fixnum_value(a2) ? obj_true : obj_false ;
	      else if (realp(a2))
		val = REAL(a1)->value >= REAL(a2)->value ? obj_true : obj_false ;
	      else if (bvecp(a2))
		{
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = REAL(a1)->value < v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else if (bvecp(a1))
	    {
	      if (fixnump(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;

		  val = v1 >= fixnum_value(a2) ? obj_true : obj_false ;
		}
	      else if (realp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;

		  val = v1 >= REAL(a2)->value ? obj_true : obj_false ;
		}
	      else if (bvecp(a2))
		{
		  obj_t v1 = *(obj_t *)&BVEC(a1)->val[0] ;
		  obj_t v2 = *(obj_t *)&BVEC(a2)->val[0] ;
		  val = v1 >= v2 ? obj_true : obj_false ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(SYMBOLP):
          val = (symbolp(val) ? obj_true : obj_false) ; NEXT;
        BYTECODE(EQP):
	  a1 = val ;
	  a2 = pop() ;
	  val = a1==a2 ? obj_true : obj_false ;
	  NEXT ;
        BYTECODE(FIXNUMP):
          val = fixnump(val) ? obj_true : obj_false ; NEXT ;
        BYTECODE(PAIRP):
          val = pairp(val) ? obj_true : obj_false ; NEXT ;
        BYTECODE(CONS):
	  a1 = make_pair() ;
	  PAIR(a1)->car = obj_false ;
	  PAIR(a1)->cdr = obj_false ;
	  a2 = val ;
          a3 = pop() ;
	  PAIR(a1)->car = a2 ;
	  PAIR(a1)->cdr = a3 ;
	  val = a1 ;
	  NEXT ;
        BYTECODE(CAR):
          if (pairp(val))
            val = PAIR(val)->car ;
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(CDR):
          if (pairp(val))
            val = PAIR(val)->cdr ;
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(SETCAR):
	  a1 = val ;
	  a2 = pop() ;
	  if (pairp(a1))
	    PAIR(a1)->car = a2 ;
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(SETCDR):
	  a1 = val ;
	  a2 = pop() ;
	  if (pairp(a1))
	    PAIR(a1)->cdr = a2 ;
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(NULLP):
          val = val == obj_nil ? obj_true : obj_false ; NEXT ;
        BYTECODE(VECTORP):
          val = vectorp(val) ? obj_true : obj_false ; NEXT ;
        BYTECODE(MAKEVECTOR):
	  if (fixnump(val) && fixnum_value(val) >= 0)
	    {
	      int i ;
	      a1 = make_vector(fixnum_value(val)) ;
	      for (i = 0 ; i < fixnum_value(val) ; i++)
		VECTOR(a1)->val[i] = obj_undefined ;
	      a2 = pop() ;
	      for (i=0 ; i < fixnum_value(val) ; i++)
		VECTOR(a1)->val[i] = a2 ;
	      val = a1 ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, val, pop()) ;
	  NEXT ;
	BYTECODE(VECTORLENGTH):
          if (vectorp(val))
            val = make_fixnum(HEADER_SIZE(*pointer_value(val)) - 1) ;
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(VECTORREF):
	  a1 = val;
          a2 = pop() ;
	  if (fixnump(a2) &&
	      vectorp(a1) &&
	      fixnum_value(a2) < HEADER_SIZE(*pointer_value(a1))-1 &&
	      fixnum_value(a2) >= 0)
	    val = VECTOR(a1)->val[fixnum_value(a2)] ;
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(VECTORSET):
	  a1 = val;
	  a2 = pop() ;
	  a3 = pop() ;
	  if (fixnump(a2) &&
	      vectorp(a1) &&
	      fixnum_value(a2) < HEADER_SIZE(*pointer_value(a1))-1)
	    VECTOR(a1)->val[fixnum_value(a2)] = a3 ;
	  else
	    TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	  NEXT ;
        BYTECODE(MAKESTRING):
	  a1 = val;
	  a2 = pop() ;
	  if (fixnump(a1) && fixnum_value(a1) >= 0)
	    {
	      obj_t str = make_string(fixnum_value(a1)+1) ;
	      if (charp(a2))
		{
		  unsigned char ch = char_value(a2) ;
		  int i ;
		  for (i=0 ; i < fixnum_value(a1) ; i++)
		    STRING(str)->ch[i] = ch ;
		  STRING(str)->ch[fixnum_value(a1)] = '\0' ;
		  val = str ;
		}
	      else
		TRAP2(TRAP_PRIMITIVE, val, a1) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, val, a1) ;
	  NEXT ;
        BYTECODE(STRINGLENGTH):
          if (stringp(val))
            val = make_fixnum(STRING(val)->len-1) ;
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(STRINGREF):
	  a1 = val;
	  a2 = pop() ;
	  if (stringp(a1) &&
	      fixnum_value(a2) < (STRING(a1)->len-1) &&
	      fixnum_value(a2) >= 0)
	    val = make_char(STRING(a1)->ch[fixnum_value(a2)]) ;
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(STRINGSET):
	  a1 = val;
	  a2 = pop() ;
	  a3 = pop() ;
	  if (stringp(a1) &&
	      fixnum_value(a2) < (STRING(a1)->len-1) &&
	      charp(a3))
	    STRING(a1)->ch[fixnum_value(a2)] = char_value(a3) ;
	  else
	    TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	  NEXT ;
	BYTECODE(MAKECHANNEL):
	  if (fixnump(val))
	    {
	      host_channel_t channel ;
	      int res = host_make_channel(fixnum_value(val),
					  &channel) ;
	      if (res)
		val = make_fixnum(res) ;
	      else
		{
		  val = make_channel(channel) ;
		  a1 = make_bvec(CHANNEL_BUFFER_SIZE) ;
		  CHANNEL(val)->buffer = a1 ;
		}
	    }
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
        BYTECODE(WRITE):
	  {
	    int res ;
	    a1 = val ;
	    a2 = pop() ;
	    a3 = pop() ;
	    a4 = pop() ;

	    if (channelp(a1) && fixnump(a3) && fixnump(a4))
	      {
		if (bvecp(a2))
		  {
		    res = host_write_channel(CHANNEL(a1)->channel_number,
					     &BVEC(a2)->val[0],
					     fixnum_value(a3),
					     fixnum_value(a4)) ;
		  }
		else if (stringp(a2))
		  {
		    res = host_write_channel(CHANNEL(a1)->channel_number,
					     &STRING(a2)->ch[0],
					     fixnum_value(a3),
					     fixnum_value(a4)) ;
		  }
		else if (charp(a2))
		  {
		    unsigned char ch ;
		    ch = char_value(a2) ;
		    res = host_write_channel(CHANNEL(a1)->channel_number,
					     &ch,
					     fixnum_value(a3),
					     fixnum_value(a4)) ;
		  }
		else
		  {
		    write_obj(a2) ;
		    fflush(NULL);
		    res = 0 ;
		  }
		{
		  if (res == -1)
		    val = obj_false ;
		  else
		    val = make_fixnum(res) ;
		  NEXT ;
		}
	      }
	    else
	      TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  }
        BYTECODE(READ):
	  a1 = val ;
	  if (channelp(a1))
	    {
	      if (CHANNEL(a1)->index == CHANNEL(a1)->available)
		{
		  int rc = host_read_channel(CHANNEL(a1)->channel_number,
					     &BVEC(CHANNEL(a1)->buffer)->val[0],
					     0,
					     CHANNEL_BUFFER_SIZE) ;

		  if (rc == 0)
		    val = obj_eof ;
		  else if (rc == -1)
		    val = obj_false ;
		  else if (rc > 0)
		    {
		      CHANNEL(a1)->available = make_fixnum(rc) ;
		      CHANNEL(a1)->index = make_fixnum(1) ;
		      obj_t buffer = CHANNEL(a1)->buffer ;
		      unsigned char ch = BVEC(buffer)->val[0] ;
		      val = make_char(ch) ;
		    }
		}
	      else
		{
		  int i = fixnum_value(CHANNEL(a1)->index) ;
		  val = make_char(BVEC(CHANNEL(a1)->buffer)->val[i]) ;
		  CHANNEL(a1)->index = make_fixnum(i+1) ;
		}
	    }
	  else
	    TRAP1(TRAP_PRIMITIVE, a1) ;
	  NEXT ;
        BYTECODE(EOFP):
          val = (val == obj_eof ? obj_true : obj_false) ; NEXT ;
        BYTECODE(CHANNEL_NUMBER):
          if (channelp(val))
            val = make_fixnum((int) CHANNEL(val)->channel_number) ;
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(CHANNELP):
          val = (channelp(val) ? obj_true : obj_false) ; NEXT ;
        BYTECODE(OPENINPUTCHANNEL):
          if (stringp(val))
            {
	      host_channel_t channel ;
              int rc = host_open_input_channel((char *)&STRING(val)->ch[0],
					       &channel) ;
	      if (rc == 0)
		{
		  val = make_channel(channel) ;
		  /* write_obj(val); */
		  a1 = make_bvec(CHANNEL_BUFFER_SIZE) ;
		  CHANNEL(val)->buffer = a1 ;
		  /* write_obj(val) ; */
		}
	      else
		val = make_fixnum(rc) ;
            }
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(OPENOUTPUTCHANNEL):
	  a1 = val ;
	  a2 = pop() ;
          if (stringp(a1) && fixnump(a2))
            {
	      host_channel_t channel ;
              int rc = host_open_output_channel((char *)&STRING(a1)->ch[0],
						&channel, fixnum_value(a2)) ;

	      if (rc == 0)
		{
		  val = make_channel(channel) ;
		  a1 = make_bvec(CHANNEL_BUFFER_SIZE) ;
		  CHANNEL(val)->buffer = a1 ;
		}
	      else
		val = make_fixnum(rc) ;
            }
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(CHAR2FIXNUM)	  :
          if (charp(val))
            val = make_fixnum(char_value(val)) ;
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(FIXNUM2CHAR):
          if (fixnump(val))
            val = make_char(fixnum_value(val)) ;
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(SYMBOL2STRING):
          if (symbolp(val))
            {
              int size = strlen((char *)SYMBOL(val)->ch)+1 ;
              obj_t str = make_string(size) ;
              int i ;
              for (i=0; i < size ; i++)
                STRING(str)->ch[i] = SYMBOL(val)->ch[i] ;
              val = str ;
            }
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(STRING2SYMBOL):
          if (stringp(val))
            {
              int size = STRING(val)->len ;
              obj_t sym = make_symbol(size) ;
              int i ;
              for (i=0; i < size ; i++)
                SYMBOL(sym)->ch[i] = tolower(STRING(val)->ch[i]) ;
	      val = sym ; /* prevent SYM to be GCed when INTERN allocates */
              val = intern() ;
            }
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(APPLY):
          a1 = pop() ;
          if (pairp(a1))
	    argc = unroll_list() ;
	  else
	    argc = 0 ;

	  if (closurep(val))
	    {
	      /* call closure */
	      env = CLOSURE(val)->val[0] ;
	      template = CLOSURE(val)->val[1] ;
	      code_vector = VECTOR(template)->val[0] ;
	      literals = VECTOR(template)->val[1] ;
	      pc = 0 ;
	    }
	  else if (vectorp(trap) && VECTOR(trap)->val[TRAP_NO_PROCEDURE])
	    {
	      _PUSH(val) ;
	      val = VECTOR(trap)->val[TRAP_NO_PROCEDURE] ;
	      argc++ ;
	      env = CLOSURE(val)->val[0] ;
	      template = CLOSURE(val)->val[1] ;
	      code_vector = VECTOR(template)->val[0] ;
	      literals = VECTOR(template)->val[1] ;
	      pc = 0 ;
	    }
	  else *((int *) 0x00) = 0 ;
          NEXT ;
        BYTECODE(MAKESTOB):

	  a1 = pop() ;
	  if (fixnump(val) && fixnum_value(val) >= 0)
	    {
	      int i ;
	      a2 = make_stob(fixnum_value(val)) ;
	      STOB(a2)->class = a1 ;
	      for (i = 0 ; i < fixnum_value(val) ; i++)
		STOB(a2)->slot[i] = obj_unbound ;
	      val = a2 ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, val, a1) ;
	  NEXT ;
        BYTECODE(STOBREF):
	  a1 = val;
	  a2 = pop() ;
	  if (fixnump(a2) &&
	      stobp(a1) &&
	      fixnum_value(a2) < HEADER_SIZE(*pointer_value(a1))-2 &&
	      fixnum_value(a2) >= 0)
	    val = STOB(a1)->slot[fixnum_value(a2)] ;
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(STOBSET):
	  a1 = val;
	  a2 = pop() ;
	  a3 = pop() ;
	  if (fixnump(a2) &&
	      stobp(a1) &&
	      fixnum_value(a2) < HEADER_SIZE(*pointer_value(a1))-2)
	    STOB(a1)->slot[fixnum_value(a2)] = a3 ;
	  else
	    TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	  NEXT ;
        BYTECODE(STOBCLASS):
          if (stobp(val))
            val = STOB(val)->class ;
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
	BYTECODE(SETSTOBCLASS):
	  a1 = val ;
	  a2 = pop() ;
	  if (stobp(a1))
	    STOB(a1)->class = a2 ;
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT;
        BYTECODE(STOBP):
          val = (stobp(val) ? obj_true : obj_false) ; NEXT ;
        BYTECODE(STOBLENGTH):
          if (stobp(val))
            val = make_fixnum(HEADER_SIZE(*pointer_value(val))) ;
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(CALLCC):
          if (closurep(val))
            {
	      a1 = suspend_cc() ;
	      _PUSH(a1) ;
              argc = 1 ;
              env = CLOSURE(val)->val[0] ;
              template = CLOSURE(val)->val[1] ;
              code_vector = VECTOR(template)->val[0] ;
              literals = VECTOR(template)->val[1] ;
              pc = 0 ;
            }
          else TRAP1(TRAP_NO_PROCEDURE, val) ;
	  /* validate_heap(123) ; */
          NEXT ;
        BYTECODE(RESUMECC):
	  {
            /*            a1 = val ;
                          a2 = pop() ;
                          resume_cc(a1) ;
                          val=a2 ;
                          NEXT ;
            */
	    a1 = pop() ;
	    resume_cc(val) ;
	    val=a1 ;
	    NEXT ;
	  }
        BYTECODE(MAKEBVEC):
	  if (fixnump(val) && fixnum_value(val) >= 0)
	    {
	      obj_t bvec = make_bvec(fixnum_value(val)) ;
	      int i ;
	      for (i = 0 ; i < fixnum_value(val) ; i++)
		BVEC(bvec)->val[i] = 0 ;
	      val = bvec ;
	    }
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
        BYTECODE(BVECREF):
	  a1 = val;
	  a2 = pop() ;
	  if (fixnump(a2) &&
	      bvecp(a1) &&
	      fixnum_value(a2) < BVEC(a1)->size &&
	      fixnum_value(a2) >= 0)
	    val = make_fixnum(BVEC(a1)->val[fixnum_value(a2)]) ;
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(BVECSET):
	  a1 = val;
	  a2 = pop() ;
	  a3 = pop() ;
	  if (fixnump(a2) &&
	      bvecp(a1) &&
	      (fixnump(a3) || charp(a3)) &&
	      fixnum_value(a2) < BVEC(a1)->size)
	    if (fixnump(a3) &&
		fixnum_value(a3) < 256 &&
                  fixnum_value(a3) >= 0)
	      BVEC(a1)->val[fixnum_value(a2)] = fixnum_value(a3) ;
	    else
	      BVEC(a1)->val[fixnum_value(a2)] = char_value(a3) ;
	  else
	    TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	  NEXT ;
        BYTECODE(BVECP):
          val = (bvecp(val) ? obj_true : obj_false) ; NEXT ;
        BYTECODE(BVECLENGTH):
          if (bvecp(val))
            val = make_fixnum(BVEC(val)->size) ;
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(SAVEIMAGE):
	  a1 = val ;
	  a2 = pop() ;

          top = 1 ;
          stack = val = template = code_vector = literals = cont = obj_nil ;
	  mem_gc() ;
	  if (closurep(a2) && stringp(a1))
	    {
	      save_image(a1, a2) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  return obj_true ;
	BYTECODE(MAKEPROCEDURE):
	  if (fixnump(val) && fixnum_value(val) >= 0)
	    {
	      int i ;
	      obj_t proc = make_closure(fixnum_value(val)) ;
	      for (i = 0 ; i < fixnum_value(val) ; i++)
		CLOSURE(proc)->val[i] = obj_nil;
	      val = proc ;
	    }
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
	BYTECODE(PROCEDUREP):
	  val = closurep(val) ? obj_true : obj_false ; NEXT ;
	BYTECODE(PROCEDURESET):
	  a1 = val;
	  a2 = pop() ;
          a3 = pop() ;
	  if (fixnump(a2) &&
	      closurep(a1) &&
	      fixnum_value(a2) < CLOSURE(a1)->size)
	    CLOSURE(a1)->val[fixnum_value(a2)] = a3 ;
	  else
	    TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	  NEXT ;
 	BYTECODE(PROCEDUREREF):
	  a1 = val;
	  a2 = pop() ;
	  if (fixnump(a2) &&
	      closurep(a1) &&
	      fixnum_value(a2) < HEADER_SIZE(*pointer_value(a1))-1 &&
	      fixnum_value(a2) >= 0)
	    val = CLOSURE(a1)->val[fixnum_value(a2)] ;
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(PROCEDURELENGTH):
          if (closurep(val))
            val = make_fixnum(HEADER_SIZE(*pointer_value(val)) - 2) ;
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
	BYTECODE(CHARP):
	  val = charp(val) ? obj_true : obj_false ; NEXT ;
	BYTECODE(CLOSECHANNEL):
	  if (channelp(val))
	    {
	      /*
	      obj_t buffer = CHANNEL(val)->buffer ;
	      int i = fixnum_value(CHANNEL(val)->index) ;
	      int res = -1 ;
	      while (res == -1)
		{
		  res = host_write_channel(CHANNEL(val)->channel_number,
					   &BVEC(buffer)->val[0],
					   i) ;
		}
	      */
	      host_close_channel(CHANNEL(val)->channel_number) ;
	    }
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
	BYTECODE(PEEKCHANNEL):
	  if (channelp(val))
	    {
	      unsigned char ch ;
	      int rc = read(CHANNEL(val)->channel_number, &ch, 1) ;
	      if (rc == 1)
		{
		  lseek(CHANNEL(val)->channel_number, -1, SEEK_CUR) ;
		  val = make_char(ch) ;
		}
	      else
		val=obj_eof ;
	    }
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
        BYTECODE(STRINGP):
          val = stringp(val) ? obj_true : obj_false ;
          NEXT ;
        BYTECODE(REFP):
          val = refp(val) ? obj_true : obj_false ; NEXT ;
        BYTECODE(MAKEREF):
	  a1 = make_ref() ;
	  a2 = val ;
          a3 = pop() ;
	  a4 = pop() ;
	  REF(a1)->name = a2 ;
	  REF(a1)->module = a3 ;
	  REF(a1)->value = a4 ;
	  val = a1 ;
	  NEXT ;
        BYTECODE(REFNAME):
          if (refp(val))
            val = REF(val)->name ;
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(REFVALUE):
          if (refp(val))
            val = REF(val)->value ;
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
	BYTECODE(REFMODULE):
	  if (refp(val))
	    val = REF(val)->module ;
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
        BYTECODE(SETREFNAME):
	  a1 = val ;
	  a2 = pop() ;
	  if (refp(a1))
	    REF(a1)->name = a2 ;
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(SETREFVALUE):
	  a1 = val ;
	  a2 = pop() ;
	  if (refp(a1))
	    REF(a1)->value = a2 ;
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(SETREFMODULE):
	  a1 = val ;
	  a2 = pop() ;
	  if (refp(a1))
	    REF(a1)->module = a2 ;
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
#ifdef SCM_TARGET_WIN32
	BYTECODE(MAKESOCKET):
          {
            SOCKET socket_fd = socket(AF_INET, SOCK_STREAM, 0) ;
            int yes = 1;
	    if (socket_fd == INVALID_SOCKET)
	      {
		val = obj_false ;
	      }
	    else
	      {
		if (setsockopt(socket_fd,
			       SOL_SOCKET,
			       SO_REUSEADDR,
			       &yes,
			       sizeof(int)) == -1)
		  {
		    val = obj_false ;
		  }
		else
		  val = make_channel((host_channel_t) socket_fd) ;
	      }
            NEXT ;
          }
	BYTECODE(SOCKETBIND):
          {
	    SOCKADDR_IN addr ;
            a1 = val ;
            a2 = pop () ;
	    if (channelp(a1) && fixnump(a2))
	      {
		addr.sin_family = AF_INET ;
		addr.sin_port = htons(fixnum_value(a2)) ;
		addr.sin_addr.s_addr = htonl(INADDR_ANY) ;
		bzero(&(addr.sin_zero), 8) ;
		if (bind((SOCKET) CHANNEL(a1)->channel_number,
			 (SOCKADDR *) &addr,
			 sizeof(SOCKADDR)) == SOCKET_ERROR)
		  val = obj_false ;
		else
		  val = a1 ;
	      }
	    else
	      TRAP2(TRAP_PRIMITIVE,a1, a2) ;
            NEXT ;
          }
	BYTECODE(SOCKETLISTEN):
          a1 = val ;
          a2 = pop () ;
	  if (channelp(a1) && fixnump(a2))
	    {
	      if (listen((SOCKET) CHANNEL(a1)->channel_number,
			 fixnum_value(a2)) == SOCKET_ERROR)
		val = obj_false ;
	      else
		val = a1 ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(SOCKETACCEPT):
          {
	    SOCKADDR_IN other ;
	    int sin_size = sizeof(SOCKADDR_IN) ;

	    if (channelp(val))
	      {
		int fd = accept((SOCKET) CHANNEL(val)->channel_number,
				(SOCKADDR *) &other,
				&sin_size) ;
		if (fd == INVALID_SOCKET)
		  val = obj_false ;
		else
		  {
		    val = make_pair() ;
		    PAIR(val)->car = obj_false ;
		    PAIR(val)->cdr = obj_false ;
		    a1 = make_channel((host_channel_t) fd) ;
		    PAIR(val)->car = a1 ;
		    a1 = make_channel((host_channel_t) fd) ;
		    PAIR(val)->cdr = a1 ;
		    a1 = make_bvec(CHANNEL_BUFFER_SIZE) ;
		    CHANNEL(PAIR(val)->car)->buffer = a1 ;
		    a1 = make_bvec(CHANNEL_BUFFER_SIZE) ;
		    CHANNEL(PAIR(val)->cdr)->buffer = a1 ;
		  }
	      }
	    else
	      TRAP1(TRAP_PRIMITIVE, val) ;
            NEXT ;
          }
	BYTECODE(SOCKETCONNECT):
	  {
	    SOCKET socket_fd ;
	    HOSTENT *host_entry ;
	    SOCKADDR_IN his ;

	    a1 = val ;
	    a2 = pop() ;
	    if (stringp(a1) && fixnump(a2))
	      {
		if ((host_entry = gethostbyname(&STRING(a1)->ch[0])) == NULL)
		  val = obj_false ;
		else
		  {
		    if ((socket_fd = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET)
		      val = obj_false ;
		    else
		      {
			his.sin_family = AF_INET ;
			his.sin_port = htons(fixnum_value(a2)) ;
			memcpy(&his.sin_addr, host_entry->h_addr_list[0], sizeof(IN_ADDR));
			bzero(&(his.sin_zero), 8) ;
			if (connect(socket_fd,
				    (SOCKADDR *) &his,
				    sizeof(SOCKADDR)) == SOCKET_ERROR)
			  val = obj_false ;
			else
			  {
			    val = make_channel((host_channel_t) socket_fd) ;
			    a1 = make_bvec(CHANNEL_BUFFER_SIZE) ;
			    CHANNEL(val)->buffer = a1 ;
			  }
		      }
		  }
	      }
	    else
	      TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    NEXT ;
	  }
#endif
        BYTECODE(BITOR):
          a2 = pop() ;
          a1 = val ;
          if (fixnump(a1) && fixnump(a2))
            {
              val = make_fixnum(fixnum_value(a1) | fixnum_value(a2)) ;
            }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
          NEXT ;
        BYTECODE(BITAND):
          a2 = pop() ;
          a1 = val ;
          if (fixnump(a1) && fixnump(a2))
            {
              val = make_fixnum(fixnum_value(a1) & fixnum_value(a2)) ;
            }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
          NEXT ;
        BYTECODE(BITXOR):
          a2 = pop() ;
          a1 = val ;
          if (fixnump(a1) && fixnump(a2))
            {
              val = make_fixnum(fixnum_value(a1) ^ fixnum_value(a2)) ;
            }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
          NEXT ;
        BYTECODE(BITASH):
          a2 = pop() ;
          a1 = val ;
          if (fixnump(a1) && fixnump(a2))
            {
              if (fixnum_value(a2) < 0)
                val = make_fixnum(fixnum_value(a1) >> (-fixnum_value(a2))) ;
              else
                val = make_fixnum(fixnum_value(a1) << fixnum_value(a2)) ;
            }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
          NEXT ;
        BYTECODE(BITNOT):
          if (fixnump(val))
            {
              val = make_fixnum(!fixnum_value(val)) ;
            }
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
	BYTECODE(OBJECT_HASH):
	  a1 = val ;
	  if (stringp(a1) || symbolp(a1))
	    {
	      unsigned char *s ;
	      unsigned h = 0 , g ;

	      if (stringp(a1))
		s = (unsigned char *)&STRING(a1)->ch[0] ;
	      else if (symbolp(a1))
		s = (unsigned char *)&SYMBOL(a1)->ch[0] ;
	      else s = 0 ; /* cannot happen, really */
	      for (s=s; *s != '\0'; s++)
		{
		  h = (h << 4) + (*s) ;
		  if ((g=h&0xf0000000) != 0)
		    {
		      h = h ^ (g >> 24) ;
		      h = h ^ g ;
		    }
		}
	      val = make_fixnum(h) ;
	    }
	  else if (fixnump(a1)) val = a1 ;
	  NEXT ;
        BYTECODE(NATIVE_CALL):
          a1 = val ;
          a2 = pop() ;
          if (bvecp(a1))
            {
              obj_t (*proc)(obj_t env) ;

              proc = (obj_t (*)(obj_t)) &BVEC(a1)->val[0] ;
              val = proc(a2) ;
            }
          else
            TRAP2(TRAP_PRIMITIVE, a1, a2) ;
          NEXT ;
#ifdef SCM_TARGET_POSIX
	BYTECODE(UNIX_OPEN):
	  a1 = val ;
	  a2 = pop() ;
	  if (stringp(a1) && fixnump(a2))
	    {
	      int fd = open(&STRING(a1)->ch[0], fixnum_value(a2)) ;
	      if (fd < 0)
		val = make_fixnum(errno) ;
	      else
		{
		  val = make_channel(fd) ;
		  a1 = make_bvec(CHANNEL_BUFFER_SIZE) ;
		  CHANNEL(val)->buffer = a1 ;
		}
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(UNIX_CLOSE):
	  if (channelp(val))
	    {
	      int rc = close(CHANNEL(val)->channel_number) ;
	      CHANNEL(val)->channel_number = 1 ;

	      if (rc == -1)
		val = make_fixnum(errno) ;
	      else
		val = make_fixnum(0) ;
	    }
	  else
	    {
	      TRAP1(TRAP_PRIMITIVE, val) ;
	    }
	  NEXT ;
	BYTECODE(UNIX_READ):
	  a1 = val ;
	  a2 = pop() ;
	  a3 = pop() ;
	  if (channelp(a1) && stringp(a2) && fixnump(a3))
	    {
	      int rc = read(CHANNEL(a1)->channel_number,
			    &STRING(a2)->ch[0],
			    fixnum_value(a3)) ;
	      if (rc == -1)
		val = make_fixnum(-(errno)) ;
	      else
		val = make_fixnum(rc) ;
	    }
	  else
	    TRAP3(TRAP_PRIMITIVE,a1,a2,a3) ;
	  NEXT ;
	BYTECODE(UNIX_WRITE):
	  a1 = val ;
	  a2 = pop() ;
	  a3 = pop() ;
	  if (channelp(a1) && stringp(a2) && fixnump(a3))
	    {
	      int rc = write(CHANNEL(a1)->channel_number,
			     &STRING(a2)->ch[0],
			     fixnum_value(a3)) ;
	      if (rc == -1)
		val = make_fixnum(-(errno)) ;
	      else
		val = make_fixnum(rc) ;
	    }
	  else
	    TRAP3(TRAP_PRIMITIVE,a1,a2,a3) ;
	  NEXT ;
	BYTECODE(UNIX_CHDIR):
	  if (stringp(val))
	    {
	      if (chdir(&STRING(val)->ch[0]) == -1)
		val = make_fixnum(errno) ;
	      else
		val = obj_false ;
	    }
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
	BYTECODE(UNIX_ACCESS):
	  a1 = val ;
	  a2 = pop() ;
	  if (stringp(a1) && fixnump(a2))
	    {
	      if (access(&STRING(a1)->ch[0], fixnum_value(a2)) == 0)
		val = obj_false ;
	      else
		val = make_fixnum(errno) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(UNIX_MKDIR):
	  a1 = val ;
	  a2 = pop() ;
	  if (stringp(a1) && fixnump(a2))
	    {
	      if (mkdir(&STRING(a1)->ch[0], fixnum_value(a2)) == 0)
		val = obj_false ;
	      else
		val = make_fixnum(errno) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(UNIX_STAT):
	  a1 = val ;
	  a2 = pop() ;
          /* write_obj(a1) ; write_obj(a2) ; */
	  if ((stringp(a1) || channelp(a1)) &&
	      stobp(a2) && HEADER_SIZE(*HEADER(a2)) == 13)
	    {
	      struct stat buf ;
	      int rc ;

              memset(&buf, 0, sizeof(struct stat)) ;
	      if (stringp(a1))
		rc = lstat(&STRING(a1)->ch[0], &buf) ;
	      else
		rc = fstat(CHANNEL(a1)->channel_number, &buf) ;

	      if (rc == 0)
		{
		  STOB(a2)->slot[0] =
                    (S_ISFIFO(buf.st_mode) ? make_fixnum(1) : 0) |
                    (S_ISCHR(buf.st_mode)  ? make_fixnum(2) : 0) |
                    (S_ISDIR(buf.st_mode)  ? make_fixnum(4) : 0) |
                    (S_ISBLK(buf.st_mode)  ? make_fixnum(8) : 0) |
                    (S_ISREG(buf.st_mode)  ? make_fixnum(16) : 0) |
                    (S_ISLNK(buf.st_mode)  ? make_fixnum(32) : 0) |
                    (S_ISSOCK(buf.st_mode) ? make_fixnum(64) : 0) |
                    make_fixnum(0);
                  STOB(a2)->slot[1] = make_fixnum(buf.st_dev) ;
		  STOB(a2)->slot[2] = make_fixnum(buf.st_ino) ;
                  STOB(a2)->slot[3] = make_fixnum(buf.st_mode) ;
		  STOB(a2)->slot[4] = make_fixnum(buf.st_nlink) ;
		  STOB(a2)->slot[5] = make_fixnum(buf.st_uid) ;
		  STOB(a2)->slot[6] = make_fixnum(buf.st_gid) ;
		  STOB(a2)->slot[7] = make_fixnum(buf.st_size) ;
		  val = make_bvec(sizeof(time_t)) ;
		  memcpy(&BVEC(val)->val[0], &buf.st_atime, sizeof(time_t)) ;
		  STOB(a2)->slot[8] = val ;
		  val = make_bvec(sizeof(time_t)) ;
		  memcpy(&BVEC(val)->val[0], &buf.st_mtime, sizeof(time_t)) ;
		  STOB(a2)->slot[9] = val ;
		  val = make_bvec(sizeof(time_t)) ;
		  memcpy(&BVEC(val)->val[0], &buf.st_ctime, sizeof(time_t)) ;
		  STOB(a2)->slot[10] = val ;
		  val = a2 ;
		}
	      else
		val = make_fixnum(errno) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(UNIX_TIME):
	  {
	    struct timeval tv ;
	    int rc = gettimeofday(&tv, NULL) ;

	    if (rc == 0)
	      {
		val = make_bvec(sizeof(time_t)) ;
		memcpy(&BVEC(val)->val[0], &tv.tv_sec, sizeof(time_t)) ;

		a1 = make_pair() ;
		PAIR(a1)->car = val ;
		PAIR(a1)->cdr = make_fixnum(tv.tv_usec) ;
		val = a1 ;
	      }
	    else
	      val = make_fixnum(errno) ;

	    NEXT ;
	  }
	BYTECODE(UNIX_LOCALTIME):
	  a1 = val ;
	  a2 = pop() ;
	  if (bvecp(a1) && stobp(a2) && HEADER_SIZE(*HEADER(a2)) == 13)
	    {
	      time_t the_time ;
	      struct tm *t ;
	      memcpy(&the_time, &BVEC(a1)->val[0], sizeof(time_t)) ;

	      t=localtime(&the_time) ;

	      STOB(a2)->slot[0]  = make_fixnum(t->tm_sec) ;
	      STOB(a2)->slot[1]  = make_fixnum(t->tm_min) ;
	      STOB(a2)->slot[2]  = make_fixnum(t->tm_hour) ;
	      STOB(a2)->slot[3]  = make_fixnum(t->tm_mday) ;
	      STOB(a2)->slot[4]  = make_fixnum(t->tm_mon+1) ;
	      STOB(a2)->slot[5]  = make_fixnum(t->tm_year) ;
	      STOB(a2)->slot[6]  = make_fixnum(0) ;
	      STOB(a2)->slot[7]  = make_fixnum(0) ;
	      STOB(a2)->slot[8]  = make_fixnum(t->tm_isdst) ;
	      STOB(a2)->slot[9]  = make_fixnum(t->tm_wday) ;
	      STOB(a2)->slot[10] = make_fixnum(t->tm_yday) ;
	      val = a2 ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(UNIX_GMTIME):
	  a1 = val ;
	  a2 = pop() ;
	  if (bvecp(a1) && stobp(a2) && HEADER_SIZE(*HEADER(a2)) == 13)
	    {
	      time_t the_time ;
	      struct tm *t ;
	      memcpy(&the_time, &BVEC(a1)->val[0], sizeof(time_t)) ;

	      t=gmtime(&the_time) ;

	      STOB(a2)->slot[0] = make_fixnum(t->tm_sec) ;
	      STOB(a2)->slot[1] = make_fixnum(t->tm_min) ;
	      STOB(a2)->slot[2] = make_fixnum(t->tm_hour) ;
	      STOB(a2)->slot[3] = make_fixnum(t->tm_mday) ;
	      STOB(a2)->slot[4] = make_fixnum(t->tm_mon) ;
	      STOB(a2)->slot[5] = make_fixnum(t->tm_year) ;
	      STOB(a2)->slot[6] = make_fixnum(0) ;
	      STOB(a2)->slot[7] = make_fixnum(0) ;
	      STOB(a2)->slot[8] = make_fixnum(t->tm_wday) ;
	      STOB(a2)->slot[9] = make_fixnum(t->tm_yday) ;
	      STOB(a2)->slot[10] = make_fixnum(t->tm_isdst) ;
	      val = a2 ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(UNIX_SEEK):
	  a1 = val ;
	  a2 = pop () ;
	  a3 = pop () ;
	  if (channelp(a1) && fixnump(a2) && fixnump(a3))
	    {
	      off_t r = lseek(CHANNEL(a1)->channel_number,
			      fixnum_value(a2),
			      fixnum_value(a3)) ;
	      if (r == -1)
		val = make_fixnum(errno) ;
	      else
		val = make_fixnum(r) ;
	    }
	  else
	    TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	  NEXT ;
	BYTECODE(UNIX_SYNC):
	  if (channelp(val))
	    {
	      if (fsync(CHANNEL(val)->channel_number) == 0)
		val = obj_false ;
	      else
		val = make_fixnum(errno) ;
	    }
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
	BYTECODE(UNIX_RMDIR):
	  if (stringp(val))
	    {
	      if (rmdir(&STRING(val)->ch[0]) == 0)
		val = obj_false ;
	      else
		val = make_fixnum(errno) ;
	    }
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
	BYTECODE(UNIX_UNLINK):
	  if (stringp(val))
	    {
	      if (unlink(&STRING(val)->ch[0]) == 0)
		val = obj_false ;
	      else
		val = make_fixnum(errno) ;
	    }
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
	BYTECODE(UNIX_RENAME):
	  a1 = val ;
	  a2 = pop () ;
	  if (stringp(a1) && stringp(a2))
	    {
	      if (rename(&STRING(a1)->ch[0], &STRING(a2)->ch[0]) == 0)
		val = obj_false ;
	      else
		val = make_fixnum(errno) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(UNIX_CHMOD):
	  a1 = val ;
	  a2 = pop () ;
	  if (stringp(a1) && fixnump(a2))
	    {
	      if (chmod(&STRING(a1)->ch[0], fixnum_value(a2)) == 0)
		val = obj_false ;
	      else
		val = make_fixnum(errno) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(UNIX_CHGRP):
	  val = obj_false ;
	  NEXT ;
	BYTECODE(UNIX_CHOWN):
	  a1 = val ;
	  a2 = pop () ;
	  a3 = pop () ;
	  if (stringp(a1) && fixnump(a2) && fixnump(a3))
	    {
	      if (chown(&STRING(a1)->ch[0],
			fixnum_value(a2),
			fixnum_value(a3)) == 0)
		val = obj_false ;
	      else
		val = make_fixnum(errno) ;
	    }
	  else if (channelp(a1) && fixnump(a2) && fixnump(a3))
	    {
	      if (fchown(CHANNEL(a1)->channel_number,
			 fixnum_value(a2),
			 fixnum_value(a3)) == 0)
		val = obj_false ;
	      else
		val = make_fixnum(errno) ;
	    }
	  else
	    TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	  NEXT ;
	BYTECODE(UNIX_GETENV):
	  if (stringp(val))
	    {
	      char *v = getenv(&STRING(val)->ch[0]) ;
	      if (v)
		{
		  val = make_string(strlen(v)+1) ;
		  strcpy(&STRING(val)->ch[0], v) ;
		}
	      else
		val = obj_false ;
	    }
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
	BYTECODE(UNIX_SETENV):
	  a1 = val ;
	  a2 = pop () ;
	  if (stringp(a1) && stringp(a2))
	    {
              /*	      if (puttenv(&STRING(a1)->ch[0], &STRING(a2)->ch[0], 1) == 0)
		val = obj_true;
	      else
              val = obj_false ; */
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(UNIX_FORK):
	  {
	    pid_t pid = fork() ;
	    if (pid == -1)
	      val = make_fixnum(errno) ;
	    else
	      val = make_fixnum(pid) ;
	    NEXT ;
	  }
	BYTECODE(UNIX_EXEC):
	  a1 = val ;
	  a2 = pop() ;
	  a3 = pop() ;
	  if (stringp(a1))
	    {
	      int l = 0 ;
	      obj_t p = a2 ;
	      char **av ;

	      for (l = 0, p = a2 ; pairp(p) ; l++, p=PAIR(p)->cdr) ;
              l++ ; /* Need to be zero terminated */
              av=(char **) malloc(l*sizeof(char *)) ;
              for (l = 0, p = a2 ; pairp(p) ; p=PAIR(p)->cdr)
                {
                  av[l++] = &STRING(PAIR(p)->car)->ch[0] ;
                }
              av[l] = NULL ;

	      if (execv(&STRING(a1)->ch[0], av) == -1)
		val = make_fixnum(errno) ;
	      else
		val = obj_false ;
	    }
	  else
	    TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	  NEXT ;
	BYTECODE(UNIX_DIRFILES):
	  {
	    a1 = val ;
	    if (stringp(a1))
	      {
		DIR *dir = opendir(&STRING(a1)->ch[0]) ;
		if (dir == NULL)
		  val = make_fixnum(errno) ;
		else
		  {
		    struct dirent *entry ;

		    a2 = obj_nil ;
		    for (entry = readdir(dir) ; entry ; entry = readdir(dir))
		      {
			a3 = make_string(strlen(entry->d_name)+1) ;
			strcpy(&STRING(a3)->ch[0], entry->d_name) ;
                        a1 = make_pair() ;
                        PAIR(a1)->car = a3 ;
                        PAIR(a1)->cdr = a2 ;
                        a2 = a1 ;
		      }
		    closedir(dir) ;
		    val = a2 ;
		  }
	      }
	    else
	      TRAP1(TRAP_PRIMITIVE, a1) ;
	    NEXT ;
	  }
	BYTECODE(UNIX_RECV):
	  a1 = val ;
	  a2 = pop() ;
	  a3 = pop() ;
	  if (channelp(val) && fixnump(a2) & fixnump(a3))
	    {
	      int l ;
	      val = make_string(fixnum_value(a2)+1) ;
	      l = recv(CHANNEL(a1)->channel_number,
		       &STRING(val)->ch[0],
		       fixnum_value(a2),
		       fixnum_value(a3)) ;
	      if (l == -1)
		val = make_fixnum(l) ;
	    }
	  else
	    TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	  NEXT ;
	BYTECODE(UNIX_SEND):
	  a1 = val ;
	  a2 = pop() ;
	  a3 = pop() ;
	  if (channelp(a1) &&
	      (stringp(a2) || bvecp(a2) || symbolp(a2)) &&
	      fixnump(a3))
	    {
	      int len ;
	      if (stringp(a2))
		len = send(CHANNEL(a1)->channel_number,
			   STRING(a2),
			   STRING(a2)->len-1,
			   fixnum_value(a3)) ;
	      else if (bvecp(a2))
		len = send(CHANNEL(a1)->channel_number,
			   STRING(a2),
			   strlen((char *)&STRING(a2)->ch[0]),
			   fixnum_value(a3)) ;
	      else if (symbolp(a2))
		len = send(CHANNEL(a1)->channel_number,
			   STRING(a2),
			   strlen((char *)&STRING(a2)->ch[0]),
			   fixnum_value(a3)) ;
	      else len = 0 ;

	      if (len == -1)
		val = make_fixnum(errno) ;
	      else
		val = obj_false ;
	    }
	  else
	    TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	  NEXT ;
	BYTECODE(UNIX_SOCKET):
          {
            a1 = val ;
            a2 = pop () ;
            if (fixnump(a1) && fixnump(a2))
              {
		int family[] = { PF_UNIX, PF_INET } ;
		int type[] = { SOCK_DGRAM, SOCK_STREAM, SOCK_RAW } ;

                int socket_fd = socket(family[fixnum_value(a1)],
				       type[fixnum_value(a2)], 0) ;
                int yes = 1;
                if (socket_fd == -1)
                  val = make_fixnum(errno) ;
                else
                  {
		    fcntl(socket_fd, F_SETFL, O_NONBLOCK) ;
                    if (setsockopt(socket_fd,
                                   SOL_SOCKET,
                                   SO_REUSEADDR,
                                   &yes,
                                   sizeof(int)) == -1)
                      {
                        val = make_fixnum(errno) ;
                      }
                    else
		      {
			val = make_pair() ;
			PAIR(val)->car = obj_false ;
			PAIR(val)->cdr = obj_false ;
			a1 = make_channel(socket_fd) ;
			PAIR(val)->car = a1 ;
			a2 = make_bvec(CHANNEL_BUFFER_SIZE) ;
			CHANNEL(a1)->buffer = a2 ;
			a1 = make_channel(socket_fd) ;
			PAIR(val)->cdr = a1 ;
			a2 = make_bvec(CHANNEL_BUFFER_SIZE) ;
			CHANNEL(a1)->buffer = a2 ;
		      }
                  }
                NEXT ;
              }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
          }
	BYTECODE(UNIX_BIND):
          {
	    struct sockaddr *addr ;
            a1 = val ;
            a2 = pop () ;
	    if (channelp(a1) && bvecp(a2))
	      {
		addr = (struct sockaddr *)&BVEC(a2)->val[0] ;
		if (bind(CHANNEL(a1)->channel_number, addr,
			 BVEC(a2)->size) == -1)
		  val = obj_false ;
		else
		  val = a1 ;
	      }
	    else
	      TRAP2(TRAP_PRIMITIVE,a1, a2) ;
            NEXT ;
          }
	BYTECODE(UNIX_LISTEN):
          a1 = val ;
          a2 = pop () ;
	  if (channelp(a1) && fixnump(a2))
	    {
	      if (listen(CHANNEL(a1)->channel_number, fixnum_value(a2)) == -1)
		val = obj_false ;
	      else
		val = a1 ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(UNIX_ACCEPT):
          {
	    struct sockaddr_in other ;
	    socklen_t sin_size = sizeof(struct sockaddr_in) ;

	    if (channelp(val))
	      {
		int fd = accept(CHANNEL(val)->channel_number,
				(struct sockaddr *) &other,
				&sin_size) ;
		if (fd == -1)
                    val = make_fixnum(errno) ;
		else
		  {
		    char *peer ;
		    fcntl(fd, F_SETFL, O_NONBLOCK) ;
		    val = make_vector(3) ;
		    VECTOR(val)->val[0] = obj_undefined ;
		    VECTOR(val)->val[1] = obj_undefined ;
		    VECTOR(val)->val[2] = obj_undefined ;
		    a1 = make_channel(fd) ;
		    VECTOR(val)->val[0] = a1 ;
		    a1 = make_channel(fd) ;
		    VECTOR(val)->val[1] = a1 ;
		    a1 = make_bvec(CHANNEL_BUFFER_SIZE) ;
		    CHANNEL(VECTOR(val)->val[0])->buffer = a1 ;
		    a1 = make_bvec(CHANNEL_BUFFER_SIZE) ;
		    CHANNEL(VECTOR(val)->val[1])->buffer = a1 ;
		    peer = inet_ntoa(other.sin_addr) ;
		    a1 = make_string(strlen(peer)+1) ;
		    VECTOR(val)->val[2] = a1 ;
		    strcpy(&STRING(a1)->ch[0], peer) ;
		  }
	      }
	    else
	      TRAP1(TRAP_PRIMITIVE, val) ;
            NEXT ;
          }
	BYTECODE(UNIX_UNIX_ADDRESS):
	  {
	    a1 = val ;

	    if (stringp(a1))
	      {
		struct sockaddr_un his ;

		if (strlen((char *)&STRING(a1)->ch[0])+1 < sizeof(his.sun_path))
		  {
		    his.sun_family = AF_UNIX ;
		    strcpy(his.sun_path, &STRING(a1)->ch[0]) ;
		    his.sun_path[sizeof(his.sun_path)-1] = '\0' ;
		    val = make_bvec(sizeof(struct sockaddr_un)) ;
		    memcpy(&BVEC(val)->val[0], &his, sizeof(struct sockaddr_un)) ;
		  }
		else
		  val = obj_false ;
	      }
	    else
	      TRAP1(TRAP_PRIMITIVE, val) ;
	    NEXT ;
	  }
	BYTECODE(UNIX_INET_ADDRESS):
	  {
	    struct hostent *host_entry ;
	    struct sockaddr_in his ;

	    a1 = val ;
	    a2 = pop() ;

	    if ((a1 == obj_false || stringp(a1)) && fixnump(a2))
	      {
		his.sin_family = AF_INET ;
		his.sin_port = htons(fixnum_value(a2)) ;
		if (a1 == obj_false)
		  {
		    his.sin_addr.s_addr = htonl(INADDR_ANY) ;
		    bzero(&(his.sin_zero), 8) ;
		    val = make_bvec(sizeof(struct sockaddr_in)) ;
		    memcpy(&BVEC(val)->val[0], &his, sizeof(struct sockaddr_in)) ;
		  }
		else
		  {
		    if ((host_entry = gethostbyname(&STRING(a1)->ch[0])) == NULL)
		      val = obj_false ;
		    else
		      {
			his.sin_addr = *((struct in_addr*) host_entry->h_addr);
			bzero(&(his.sin_zero), 8) ;
			val = make_bvec(sizeof(struct sockaddr_in)) ;
			memcpy(&BVEC(val)->val[0], &his, sizeof(struct sockaddr_in)) ;
		      }
		  }
	      }
	    else
	      TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    NEXT ;
	  }
	BYTECODE(UNIX_CONNECT):
	  {
	    int socket_fd ;
	    struct sockaddr *his ;

	    a1 = val ;
	    a2 = pop() ;

	    if (channelp(a1) && bvecp(a2))
	      {
		socket_fd = CHANNEL(a1)->channel_number ;
		his = (struct sockaddr *)&BVEC(a2)->val[0] ;

		if (connect(socket_fd, his, BVEC(a2)->size) == -1)
		  val = make_fixnum(errno) ;
		else
		  {
		    val = a1 ;
		  }
	      }
	    else
	      TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    NEXT ;
	  }
	BYTECODE(UNIX_GETSOCKOPT):
	  a1 = val ;
	  a2 = pop() ;
	  a3 = pop() ;
	  if (channelp(a1) && fixnump(a2) && fixnump(a3))
	    {
	      int level[] = { SOL_SOCKET, IPPROTO_IP, IPPROTO_TCP, IPPROTO_UDP } ;
	      int options[] = { SO_DEBUG,
				SO_ACCEPTCONN,
				SO_REUSEADDR,
				SO_KEEPALIVE,
				SO_DONTROUTE,
				SO_BROADCAST,
				-1, /* USE LOOP BACK */
				SO_OOBINLINE,
				-1, /* USE PRIVILEGED */
				-1, /* CAN'T SIGNAL */
				TCP_NODELAY,
				SO_SNDBUF,
				SO_RCVBUF,
				SO_SNDLOWAT,
				SO_RCVLOWAT,
				SO_ERROR,
				SO_TYPE,
				IP_MULTICAST_TTL,
				TCP_MAXSEG,
				SO_SNDTIMEO,
				SO_RCVTIMEO,
				SO_BINDTODEVICE,
				SO_LINGER
	      } ;
	      if ((fixnum_value(a2) >= 0) &&
		  (fixnum_value(a3) >= 0) &&
		  (fixnum_value(a2) < sizeof(level)) &&
		  (fixnum_value(a3) < sizeof(options)))
		{
		  int v;
		  socklen_t vlen=sizeof(v) ;
		  int o = options[fixnum_value(a3)] ;

		  if (o == -1)
		    {
		      val = obj_false ;
		    }
		  else
		    {
		      if (getsockopt(CHANNEL(a1)->channel_number,
				     level[fixnum_value(a2)],
				     o,
				     &v, &vlen) == 0)
			{
			  val = make_fixnum(v) ;
			}
		      else
			{
			  val = make_fixnum(-errno) ;
			}
		    }
		}
	      else
		TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	    }
	  else
	    TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	  NEXT;
	BYTECODE(UNIX_SETSOCKOPT):
	  a1 = val ;
	  a2 = pop() ;
	  a3 = pop() ;
	  a4 = pop() ;
	  if (channelp(a1) && fixnump(a2) && fixnump(a3))
	    {
	      int level[] = { SOL_SOCKET, IPPROTO_IP, IPPROTO_TCP, IPPROTO_UDP } ;
	      int options[] = { SO_DEBUG,
				SO_ACCEPTCONN,
				SO_REUSEADDR,
				SO_KEEPALIVE,
				SO_DONTROUTE,
				SO_BROADCAST,
				-1, /* USE LOOP BACK */
				SO_OOBINLINE,
				-1, /* USE PRIVILEGED */
				-1, /* CAN'T SIGNAL */
				TCP_NODELAY,
				SO_SNDBUF,
				SO_RCVBUF,
				SO_SNDLOWAT,
				SO_RCVLOWAT,
				SO_ERROR,
				SO_TYPE,
				IP_MULTICAST_TTL,
				TCP_MAXSEG,
				SO_SNDTIMEO,
				SO_RCVTIMEO,
				SO_BINDTODEVICE,
				SO_LINGER,
	      } ;
	      if ((fixnum_value(a2) >= 0) &&
		  (fixnum_value(a3) >= 0) &&
		  (fixnum_value(a2) < sizeof(level)) &&
		  (fixnum_value(a3) < sizeof(options)))
		{
		  long v ;
		  if (fixnump(a4))
		    v = fixnum_value(a4) ;
		  else if (a4 == obj_true)
		    v = 1 ;
		  else if (a4 == obj_false)
		    v = 0 ;
		  else if (stringp(a4))
		    v = (long) &STRING(a4)->ch[0] ;

		  int l = level[fixnum_value(a2)] ;
		  int o = options[fixnum_value(a3)] ;

		  if (o != -1)
		    {
		      if (setsockopt(CHANNEL(a1)->channel_number,
				     l, o,
				     &v, sizeof(v)) == 0)
			{
			  val = obj_false ;
			}
		      else
			{
			  val = make_fixnum(errno);
			}
		    }
		  else
		    val = obj_false ;
		}
	      else
		TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	    }
	  else
	    TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	  NEXT;
	BYTECODE(UNIX_SELECT):
          {
            a1 = val ;
            a2 = pop() ;
            a3 = pop() ;
            if ((pairp (a1)  || a1 == obj_nil) &&
                (pairp(a2)   || a2 == obj_nil) &&
                (fixnump(a3) || a3 == obj_false))
              {
                fd_set read_set, *read_set_ptr = &read_set ;
                fd_set write_set, *write_set_ptr = &write_set ;
                struct timeval tm ;
                struct timeval *timeout = &tm ;
                int nfds = 0, rc ;
		obj_t t ;

                FD_ZERO(&read_set) ;
                FD_ZERO(&write_set) ;
                if (fixnump(a3))
                  {
                    tm.tv_sec = fixnum_value(a3) / 1000000 ;
                    tm.tv_usec = fixnum_value(a3) % 1000000 ;
                  }
                else
                  timeout = NULL ;

		if (a1 == obj_nil)
		  {
		    read_set_ptr = NULL ;
		  }
		else
		  for (t = a1 ; pairp(t) ; t = PAIR(t)->cdr)
		    {
		      int fd = CHANNEL(PAIR(t)->car)->channel_number ;
		      FD_SET(fd, &read_set) ;
		      if (nfds < fd) nfds = fd ;
		    }

		if (a2 == obj_nil)
		  {
		    write_set_ptr = NULL ;
		  }
		else
		  for (t = a2 ; pairp(t) ; t = PAIR(t)->cdr)
		    {
		      int fd = CHANNEL(PAIR(t)->car)->channel_number ;
		      FD_SET(fd, &write_set) ;
		      if (nfds < fd) nfds = fd ;
		    }

                rc = select(nfds+1, read_set_ptr, write_set_ptr, NULL, timeout) ;
                if (rc == -1)
                  {
                    val = make_fixnum(errno) ;
                  }
                else if (rc == 0)
                  {
                    val = obj_false ;
                  }
                else
                  {
                    int i ;

                    val = obj_nil ;
                    for (i = 0 ; i < nfds+1 ; i++)
                      {
                        if (FD_ISSET(i, &read_set))
                          {
			    for (t = a1 ; pairp(t) ; t = PAIR(t)->cdr)
			      if (CHANNEL(PAIR(t)->car)->channel_number == i)
				{
				  a3 = make_pair() ;
				  PAIR(a3)->cdr = val ;
				  PAIR(a3)->car = PAIR(t)->car ;
				  val = a3 ;
				  NEXT ;
				}
                          }
			else if (FD_ISSET(i, &write_set))
                          {
			    for (t = a2 ; pairp(t) ; t = PAIR(t)->cdr)
			      if (CHANNEL(PAIR(t)->car)->channel_number == i)
				{
				  a3 = make_pair() ;
				  PAIR(a3)->cdr = val ;
				  PAIR(a3)->car = PAIR(t)->car ;
				  val = a3 ;
				  NEXT ;
				}
                          }
                      }
                  }
              }
            else
              TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
            NEXT ;
          }
        BYTECODE(UNIX_MKNOD):
	  a1 = val ;
	  a2 = pop() ;
	  if (stringp(a1) && fixnump(a2))
	    {
	      if (mkdir(&STRING(a1)->ch[0], fixnum_value(a2)) == 0)
		val = obj_false ;
	      else
		val = make_fixnum(errno) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(UNIX_TEMPNAM):
	  a1 = val ;
	  a2 = pop() ;
	  if (stringp(a1) && stringp(a2) )
	    {
	      char *name = tempnam(&STRING(a1)->ch[0],
				   &STRING(a2)->ch[0]) ;
	      val = make_string(strlen(name)+1) ;
	      strcpy(&STRING(val)->ch[0], name) ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(UNIX_GETPWNAM):
	  a1 = val ;
	  a2 = pop() ;
          if (stobp(a2))
            {
              struct passwd *p ;

              if (stringp(a1))
                p = getpwnam(&STRING(a1)->ch[0]) ;
              else if (fixnump(a1))
                p = getpwuid(fixnum_value(a1)) ;
              else
                p = NULL ;

              if (p != NULL)
                {
                  obj_t v = make_string(strlen(p->pw_name)+1) ;
                  STOB(a2)->slot[0] = v ;
                  strcpy(&STRING(STOB(a2)->slot[0])->ch[0], p->pw_name) ;
                  v = make_string(strlen(p->pw_passwd)+1) ;
                  STOB(a2)->slot[1] = v ;
                  strcpy(&STRING(STOB(a2)->slot[1])->ch[0], p->pw_passwd) ;
                  STOB(a2)->slot[2] = make_fixnum(p->pw_uid) ;
                  STOB(a2)->slot[3] = make_fixnum(p->pw_gid) ;
                  v = make_string(strlen(p->pw_gecos)+1) ;
                  STOB(a2)->slot[4] = v ;
                  strcpy(&STRING(STOB(a2)->slot[4])->ch[0], p->pw_gecos) ;
                  v = make_string(strlen(p->pw_dir)+1) ;
                  STOB(a2)->slot[5] = v ;
                  strcpy(&STRING(STOB(a2)->slot[5])->ch[0], p->pw_dir) ;
                  v = make_string(strlen(p->pw_shell)+1) ;
                  STOB(a2)->slot[6] = v ;
                  strcpy(&STRING(STOB(a2)->slot[6])->ch[0], p->pw_shell) ;
                  val = a2 ;
                }
              else
                val = obj_false ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
	BYTECODE(UNIX_UNAME):
	  {
	    a1 = val ;
	    if (stobp(a1))
	      {
		struct utsname info ;

		int rc = uname(&info) ;
		if (rc == 0)
		  {
		    obj_t v = make_string(strlen(info.sysname)+1) ;
		    strcpy(&STRING(v)->ch[0], info.sysname) ;
		    STOB(a1)->slot[0] = v ;
		    v = make_string(strlen(info.nodename)+1) ;
		    strcpy(&STRING(v)->ch[0], info.nodename) ;
		    STOB(a1)->slot[1] = v ;
		    v = make_string(strlen(info.release)+1) ;
		    strcpy(&STRING(v)->ch[0], info.release) ;
		    STOB(a1)->slot[2] = v ;
		    v = make_string(strlen(info.version)+1) ;
		    strcpy(&STRING(v)->ch[0], info.version) ;
		    STOB(a1)->slot[3] = v ;
		    v = make_string(strlen(info.machine)+1) ;
		    strcpy(&STRING(v)->ch[0], info.machine) ;
		    STOB(a1)->slot[4] = v ;
		  }
		else
		  val = make_fixnum(errno) ;
	      }
	    else
	      TRAP1(TRAP_PRIMITIVE, a1) ;
	  }
	  NEXT ;
        BYTECODE(UNIX_GETGRNAM):
	  a1 = val ;
	  a2 = pop() ;
	  if (stobp(a2) && (stringp(a1) || fixnump(a1)))
	    {
	      struct group *g ;

	      if (stringp(a1))
		g = getgrnam(&STRING(a1)->ch[0]) ;
	      else if (fixnump(a1))
		g = getgrgid(fixnum_value(a1)) ;
	      else g = 0 ; /* cannot happen really, I promise */

	      obj_t v = make_string(strlen(g->gr_name)+1) ;
	      STOB(a2)->slot[0] = v ;
	      strcpy(&STRING(STOB(a2)->slot[0])->ch[0], g->gr_name) ;
	      v = make_string(strlen(g->gr_passwd)+1) ;
	      STOB(a2)->slot[1] = v ;
	      strcpy(&STRING(STOB(a2)->slot[1])->ch[0], g->gr_passwd) ;
	      STOB(a2)->slot[2] = make_fixnum(g->gr_gid) ;
	      STOB(a2)->slot[3] = obj_nil ; /* make_string(strlen(g->gr_mem)+1) ; */
              val = a2 ;
	    }
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(UNIX_UTIME):
	  a1 = val ;
	  a1 = pop() ;
	  a3 = pop() ;
	  if (stringp(a1) && fixnump(a2) && fixnump(a3))
	    {
	      struct utimbuf buf ;

	      buf.actime = fixnum_value(a2) ;
	      buf.modtime = fixnum_value(a3) ;
	      val = make_fixnum(utime(&STRING(a1)->ch[0], &buf)) ;
	    }
	  else
	    TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	  NEXT ;
        BYTECODE(UNIX_UMASK):
	  a1 = val ;
	  if (fixnump(a1))
	    val = make_fixnum(umask(fixnum_value(a1))) ;
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
        BYTECODE(UNIX_GETPID):
	  val = make_fixnum(getpid()) ;
	  NEXT ;
        BYTECODE(UNIX_GETPPID):
	  val = make_fixnum(getppid()) ;
	  NEXT ;
        BYTECODE(UNIX_GETSID):
	  a1 = val ;
	  if (fixnump(a1))
	    val = make_fixnum(getsid(fixnum_value(a1))) ;
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
        BYTECODE(UNIX_SETSID):
	  val = make_fixnum(setsid()) ;
	  NEXT ;
        BYTECODE(UNIX_GETUID):
	  val = make_fixnum(getuid()) ;
	  NEXT ;
        BYTECODE(UNIX_GETEUID):
	  val = make_fixnum(geteuid()) ;
	  NEXT ;
        BYTECODE(UNIX_GETGID):
	  val = make_fixnum(getgid()) ;
	  NEXT ;
        BYTECODE(UNIX_GETEGID):
	  val = make_fixnum(getegid()) ;
          NEXT ;
        BYTECODE(UNIX_SETUID):
          if (fixnump(val))
              val = make_fixnum(setuid(fixnum_value(val))) ;
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(UNIX_SETEGID):
          if (fixnump(val))
              val = make_fixnum(setegid(fixnum_value(val))) ;
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(UNIX_SETEUID):
	  a1 = val ;
	  if (fixnump(a1) )
	    val = make_fixnum(seteuid(fixnum_value(a1))) ;
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
        BYTECODE(UNIX_SETGID):
	  a1 = val ;
	  if (fixnump(a1))
	    val = make_fixnum(setgid(fixnum_value(a1))) ;
	  else
	    TRAP1(TRAP_PRIMITIVE, val) ;
	  NEXT ;
        BYTECODE(UNIX_EXIT):
          if (fixnump(val))
            exit(fixnum_value(val)) ;
          else
            TRAP1(TRAP_PRIMITIVE, val);
          NEXT ;
	BYTECODE(UNIX_WAITPID):
	  {
	    int status = 0 ;
	    pid_t pid ;

            if (fixnump(val))
              {
                pid = waitpid(fixnum_value(val), &status, WNOHANG) ;
                if (pid == 0)
                  val = obj_false ;
		else if (pid == -1)
		  val = make_fixnum(errno) ;
                else
                  {
                    val=make_vector(5) ;
                    VECTOR(val)->val[0] = make_fixnum(pid) ;
                    VECTOR(val)->val[1] = WIFEXITED(status) == 0 ? obj_false : obj_true ;
                    VECTOR(val)->val[2] = WIFEXITED(status) == 0 ? obj_false : make_fixnum(WEXITSTATUS(status)) ;
                    VECTOR(val)->val[3] = WIFSIGNALED(status) == 0 ? obj_false : obj_true ;
                    VECTOR(val)->val[4] = WIFSIGNALED(status) == 0 ? obj_false : make_fixnum(WTERMSIG(status)) ;
                  }
              }
            else
              TRAP1(TRAP_PRIMITIVE, val) ;
	    NEXT ;
	  }
	BYTECODE(UNIX_GETCWD):
	  {
	    char *wd = NULL;
	    int size = 4096 ;
            int err = 0 ;

	    for (size = 4096 ; wd == NULL ; size=size+256 )
              {
                wd = malloc(size*sizeof(char)) ;
                if (getcwd(wd, size) == NULL)
                  {
                    if (errno == ERANGE)
                      {
                        free(wd) ;
                        wd = NULL ;
                      }
                    else
                      {
                        err = errno ;
                      }
                  }
              }

            if (err != 0)
              val = make_fixnum(errno) ;
            else
              {
                val=make_string(strlen(wd)+1) ;
                strcpy(&STRING(val)->ch[0], wd) ;
              }
            free(wd) ;
	    NEXT ;
	  }
        BYTECODE(UNIX_PIPE):
          {
            int desc[2] ;
            int rc = pipe(desc) ;

            if (rc == 0)
              {
                val = make_pair() ;
		PAIR(val)->car = obj_false ;
		PAIR(val)->cdr = obj_false ;
		a1 = make_channel(desc[0]) ;
                PAIR(val)->car = a1 ;
		a1 = make_bvec(CHANNEL_BUFFER_SIZE) ;
		CHANNEL(PAIR(val)->car)->buffer = a1 ;
		a1 = make_channel(desc[1]) ;
                PAIR(val)->cdr = a1 ;
		a1 = make_bvec(CHANNEL_BUFFER_SIZE) ;
		CHANNEL(PAIR(val)->cdr)->buffer = a1 ;
              }
            else
              val = make_fixnum(errno) ;
            NEXT ;
          }
        BYTECODE(UNIX_DUP2):
          {
            a1 = val ;
            a2 = pop() ;
            if (channelp(a1) && channelp(a2))
              {
                int rc = dup2(CHANNEL(a1)->channel_number,
                              CHANNEL(a2)->channel_number) ;
                if (rc == 0)
                  val = obj_true ;
                else if (rc == EINTR)
                  val = obj_false ;
                else
                  val = make_fixnum(errno) ;
              }
            else
              TRAP2(TRAP_PRIMITIVE, a1, a2) ;
            NEXT ;
          }
	BYTECODE(UNIX_SIGNALBLOCK):
	  {
	    sigset_t signal_set ;

	    sigfillset(&signal_set) ;
	    if (val == obj_false)
	      {
		sigprocmask(SIG_UNBLOCK, &signal_set, NULL) ;
	      }
	    else
	      {
		sigprocmask(SIG_BLOCK, &signal_set, NULL) ;
	      }
	    NEXT;
	  }
	BYTECODE(UNIX_KILL):
	  {
	    a1 = pop() ;

	    if (fixnump(val) && fixnump(a1))
	      {
		pid_t pid = (pid_t) fixnum_value(val) ;
		int   sig = fixnum_value(a1) ;

		int rc = kill(pid, sig) ;

		if (rc < 0)
		  val = make_fixnum(errno) ;
		else
		  val = make_fixnum(0) ;
	      }
	    else
	      TRAP2(TRAP_PRIMITIVE, val, a1) ;
	    NEXT ;
	  }
        BYTECODE(UNIX_OPENPTY):
          {
            a1 = pop() ;
            a2 = pop() ;

            if (pairp(val) && fixnump(a1) && fixnump(a2))
              {
                struct winsize ws ;
                int master, slave ;

                ws.ws_row = fixnum_value(a1) ;
                ws.ws_col = fixnum_value(a2) ;

                int rc = openpty(&master, &slave, NULL, NULL, &ws) ;
                if (rc == 0)
                  {
                    PAIR(val)->car = make_channel(master) ;
                    PAIR(val)->cdr = make_channel(slave) ;

                    a1 = make_bvec(CHANNEL_BUFFER_SIZE) ;
                    a2 = make_bvec(CHANNEL_BUFFER_SIZE) ;

                    CHANNEL(PAIR(val)->car)->buffer = a1 ;
                    CHANNEL(PAIR(val)->cdr)->buffer = a2 ;
                  }
                else
                  {
                    PAIR(val)->car = obj_false ;
                    PAIR(val)->cdr = obj_false ;

                    val = obj_false ;
                  }
              }
            else
              TRAP3(TRAP_PRIMITIVE, val, a1, a2) ;
            NEXT ;
          }
#endif
        BYTECODE(REAL2STRING):
          if (realp(val))
          {
            char buffer[48] ;
            /* sprintf(buffer, "%.17g", REAL(val)->value) ; */
            /* This seems better to print REAL number:
               (sqrt 2) => 1.414213562373095
               (expt 10 -1) => 0.1
            */
            sprintf(buffer, "%0.16g", REAL(val)->value) ;
	    buffer[47] = '\0' ;

            val = make_string(strlen(buffer)+1) ;
            strcpy(&STRING(val)->ch[0], buffer) ;
            NEXT ;
          }
          else
	    TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(STRING2REAL):
          if (stringp(val))
            {
              double d = atof(&STRING(val)->ch[0]) ;

              val = make_real(d) ;
            }
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(FIXNUM2REAL):
          if (fixnump(val))
            val = make_real(fixnum_value(val)) ;
	  else if (realp(val))
	    val = val ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(REALP):
          val = realp(val) ? obj_true : obj_false ; NEXT ;
        BYTECODE(ROUND):
          if (realp(val))
            {
              double d = round(REAL(val)->value) ;
              val = make_fixnum((obj_t)d) ;
            }
          else if (fixnump(val))
            val = val ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(TRUNCATE):
          if (realp(val))
            {
              double d = trunc(REAL(val)->value) ;
              val = make_fixnum((obj_t)d) ;
            }
          else if (fixnump(val))
            val = val ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(CEILING):
          if (realp(val))
            {
              double d = ceil(REAL(val)->value) ;
              val = make_fixnum((obj_t)d) ;
            }
          else if (fixnump(val))
            val = val ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(FLOOR):
          if (realp(val))
            {
              double d = floor(REAL(val)->value) ;
              val = make_fixnum((obj_t)d) ;
            }
          else if (fixnump(val))
            val = val ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(RATIONALIZE):
          if (realp(val))
            val = make_fixnum(REAL(val)->value) ;
          else if (fixnump(val))
            val = val ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(ZEXP):
          if (realp(val))
            val = make_real(exp(REAL(val)->value)) ;
          else if (fixnump(val))
            val = make_real(exp(fixnum_value(val))) ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(ZLOG):
          if (realp(val))
            val = make_real(log(REAL(val)->value)) ;
          else if (fixnump(val))
            val = make_real(log(fixnum_value(val))) ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(ZSIN):
          if (realp(val))
            val = make_real(sin(REAL(val)->value)) ;
          else if (fixnump(val))
            val = make_real(sin(fixnum_value(val))) ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(ZCOS):
          if (realp(val))
            val = make_real(cos(REAL(val)->value)) ;
          else if (fixnump(val))
            val = make_real(cos(fixnum_value(val))) ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(ZTAN):
          if (realp(val))
            val = make_real(tan(REAL(val)->value)) ;
          else if (fixnump(val))
            val = make_real(tan(fixnum_value(val))) ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(ZASIN):
          if (realp(val))
            val = make_real(asin(REAL(val)->value)) ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(ZACOS):
          if (realp(val))
            val = make_real(acos(REAL(val)->value)) ;
          else if (fixnump(val))
            val = make_real(asin(fixnum_value(val))) ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(ZATAN):
          if (realp(val))
            val = make_real(atan(REAL(val)->value)) ;
          else if (fixnump(val))
            val = make_real(atan(fixnum_value(val))) ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(ZATAN2):
          a1 = val ;
          a2 = pop() ;
          if (realp(a1) && realp(a2))
            val = make_real(atan2(REAL(a1)->value, REAL(a2)->value)) ;
          else
            TRAP2(TRAP_PRIMITIVE, a1, a2) ;
          NEXT ;
        BYTECODE(ZSQRT):
          if (realp(val))
            val = make_real(sqrt(REAL(val)->value)) ;
          else if (fixnump(val))
            val = make_real(sqrt(fixnum_value(val))) ;
          else
            TRAP1(TRAP_PRIMITIVE, val) ;
          NEXT ;
        BYTECODE(ZEXPT):
          a1 = val ;
          a2 = pop() ;
          if (realp(a1) && realp(a2))
            val = make_real(pow(REAL(a1)->value, REAL(a2)->value)) ;
          else
            TRAP2(TRAP_PRIMITIVE, a1, a2) ;
          NEXT ;
	BYTECODE(FFI_DLOPEN):
	  {
	    if (stringp(val))
	      {
		void *handle = dlopen(&STRING(val)->ch[0], RTLD_LAZY|RTLD_GLOBAL) ;
		if (handle)
		  {
		    val = make_bvec(sizeof(void *)) ;
		    memcpy(&BVEC(val)->val[0], &handle, sizeof(void *)) ;
		  }
		else
		  val = obj_false ;
	      }
	    else
	      TRAP1(TRAP_PRIMITIVE, val) ;
	    NEXT ;
	  }
	BYTECODE(FFI_DLERROR):
	  {
	    char *msg = dlerror() ;
	    obj_t string = make_string(strlen(msg)+1) ;
	    strcpy(&STRING(string)->ch[0], msg) ;
	    val = string ;
	    NEXT ;
	  }
	BYTECODE(FFI_DLSYM):
	  {
	    obj_t lib, name ;
	    lib = val ;
	    name = pop() ;
	    if (stringp(name) && bvecp(lib))
	      {
		void *lib_handle ;
		void *handle ;

		memcpy(&lib_handle, &BVEC(lib)->val[0], sizeof(void *)) ;
		handle = dlsym(lib_handle, &STRING(name)->ch[0]) ;
		if (handle)
		  {
		    val = make_bvec(sizeof(void *)) ;
		    memcpy(&BVEC(val)->val[0], &handle, sizeof(void *)) ;
		  }
		else
		  val = obj_false ;
	      }
	    else
	      TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    NEXT ;
	  }
	BYTECODE(FFI_DLCLOSE):
	  {
	    if (bvecp(val))
	      {
		dlclose(*(void **)&BVEC(val)->val[0]) ;
		val = obj_true ;
	      }
	    else
	      TRAP1(TRAP_PRIMITIVE, val) ;
	    NEXT ;
	  }
	BYTECODE(FFI_APPLY):
	  {
	    obj_t a1 = val ;
	    obj_t a2 = pop() ;
	    if (bvecp(a1))
	      {
		int len ;
		long a[11] ;
		long (*p)(long a, ...);
		long (*p0)(void) ;
		long r ;
		obj_t args ;

		for (len = 0 , args = a2 ;
		     pairp(args) && args != obj_nil && len < 10 ;
		     args = PAIR(args)->cdr, len++)
		  {
		    obj_t arg = PAIR(args)->car ;
		    if (fixnump(arg))
		      a[len] = fixnum_value(arg) ;
		    else if (charp(arg))
		      a[len] = char_value(arg) ;
		    else if (arg == obj_true || arg == obj_false)
		      a[len] = arg == obj_true ? 1 : 0 ;
		    else if (stringp(arg))
		      a[len] = (long) &STRING(arg)->ch[0] ;
		    else if (symbolp(arg))
		      a[len] = (long) &SYMBOL(arg)->ch[0] ;
		    else if (bvecp(arg))
		      memcpy(&a[len], &BVEC(arg)->val[0], sizeof(long)) ;
		    else
		      a[len] = arg ;
		  }

		memcpy(&p, &BVEC(val)->val[0], sizeof(p)) ;

		switch (len)
		  {
		  case 0: p0 =(long (*)(void)) p ; r = p0() ; break ;
		  case 1: r = p(a[0]) ; break ;
		  case 2: r = p(a[0], a[1]) ; break ;
		  case 3: r = p(a[0], a[1], a[2]) ; break ;
		  case 4: r = p(a[0], a[1], a[2], a[3]) ; break ;
		  case 5: r = p(a[0], a[1], a[2], a[3], a[4]) ; break ;
		  case 6: r = p(a[0], a[1], a[2], a[3], a[4], a[5]) ; break ;
		  case 7: r = p(a[0], a[1], a[2], a[3], a[4], a[5], a[6]) ; break ;
		  case 8: r = p(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]) ; break ;
		  case 9: r = p(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]) ; break ;
		  case 10: r = p(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]) ; break ;
		  default:
		    r = 0 ;
		  }
		val = make_bvec(sizeof(long)) ;
		memcpy(&BVEC(val)->val[0], &r, sizeof(r)) ;
		NEXT ;
	      }
	  }
	BYTECODE(FFI_MEM_REF):
	  {
	    obj_t ptr = val ;
	    obj_t offset = pop() ;
	    obj_t size = pop() ;

	    if (bvecp(ptr) && fixnump(offset) && fixnump(size))
	      {
		val = make_bvec(fixnum_value(size)) ;

		memcpy(&BVEC(val)->val[0],
		       &BVEC(ptr)->val[fixnum_value(offset)],
		       fixnum_value(size)) ;
	      }
	    else
	      val = obj_false ;
	    NEXT ;
	  }
	BYTECODE(FFI_STRING_REF):
	  {
	    obj_t ptr = val ;

	    if (bvecp(ptr))
	      {
		int size ;
		void *mem ;
		memcpy(&mem, &BVEC(ptr)->val[0], sizeof(void *)) ;
		size = strlen(mem)+1 ;
		val = make_string(size) ;

		memcpy(&STRING(val)->ch[0], mem, size) ;
	      }
	    else
	      val = obj_false ;
	    NEXT ;
	  }
	BYTECODE(FFI_MEM_SET):
	  {
	    obj_t dst = val ;
	    obj_t src = pop() ;
	    obj_t off = pop() ;
	    if (bvecp(dst) && bvecp(src) && fixnump(off))
	      {
		memcpy(&BVEC(dst)->val[fixnum_value(off)],
		       &BVEC(src)->val[0],
		       BVEC(src)->size) ;
	      }
	    val = obj_undefined ;
	    NEXT ;
	  }
	BYTECODE(FFI_MIRROR):
	  {
	    if (fixnump(val))
	      {
		obj_t ffi = make_bvec(sizeof(obj_t)) ;
		long n = fixnum_value(val) ;

		memcpy(&BVEC(ffi)->val[0], &n, sizeof(obj_t)) ;
		val = ffi ;
	      }
	    else if (charp(val))
	      {
		obj_t ffi = make_bvec(sizeof(obj_t)) ;
		obj_t c = char_value(val) ;

		memcpy(&BVEC(ffi)->val[0], &c, sizeof(obj_t));
		val = ffi ;
	      }
	    else if (val == obj_true || val == obj_false)
	      {
		obj_t ffi = make_bvec(1) ;
		BVEC(ffi)->val[0] = val == obj_true ? 1: 0 ;
		val = ffi ;
	      }
	    else if (stringp(val))
	      {
		obj_t ffi = make_bvec(sizeof(obj_t)) ;
		obj_p str = (obj_p) &STRING(val)->ch[0] ;
		memcpy(&BVEC(ffi)->val[0], &str, sizeof(obj_t)) ;
		val = ffi ;
	      }
	    else if (bvecp(val))
	      {
		obj_t ffi = make_bvec(sizeof(obj_t)) ;
		obj_p bvec = (obj_p) &BVEC(val)->val[0] ;
		memcpy(&BVEC(ffi)->val[0], &bvec, sizeof(obj_t));
		val = ffi ;
	      }
	    else if (symbolp(val))
	      {
		obj_t ffi = make_bvec(sizeof(obj_t)) ;
		obj_p sym = (obj_p) &SYMBOL(val)->ch[0] ;
		memcpy(&BVEC(ffi)->val[0], &sym, sizeof(obj_t));
		val = ffi ;
	      }
	    else if (realp(val))
	      {
		obj_t ffi = make_bvec(sizeof(double)) ;
		double *d = &REAL(val)->value ;
		memcpy(&BVEC(ffi)->val[0], &d, sizeof(double));
		val = ffi ;
	      }
	    else
	      val = obj_undefined ;

	    NEXT ;
	  }
	BYTECODE(FFI_MEM_DEREF):
	  {
	    obj_t ptr = val ;
	    obj_t off = pop() ;
	    obj_t len = pop() ;

	    if (bvecp(ptr) && fixnump(off) && fixnump(len))
	      {
		int off_ = fixnum_value(off) ;
		int len_ = fixnum_value(len) ;
		if (off_ >= 0 && len_ > 0)
		  {
		    obj_t ffi = make_bvec(len_) ;
		    void *mem ;
		    memcpy(&mem, &BVEC(ptr)->val[off_], sizeof(void *)) ;
		    memcpy(&BVEC(ffi)->val[0], mem, len_) ;
		    val = ffi ;
		  }
		else
		  val = obj_false ;
	      }
	    else
	      val = obj_false ;
	    NEXT ;
	  }
	BYTECODE(FFI_MALLOC):
	  {
	    if (fixnump(val))
	      {
		int size = fixnum_value(val) ;
		void *mem = malloc(size) ;
		if (mem)
		  {
		    val = make_bvec(sizeof(void *)) ;
		    memcpy(&BVEC(val)->val[0], &mem, sizeof(void *)) ;
		  }
		else
		  val = obj_false ;
	      }
	    else
	      val = obj_false ;
	    NEXT ;
	  }
	BYTECODE(FFI_FREE):
	  if (bvecp(val))
	    {
	      void *mem ;
	      memcpy(&mem, &BVEC(val)->val[0], sizeof(void *)) ;
	      free(mem) ;
	    }
	  else
	    val = obj_false ;
	  NEXT ;
	BYTECODE(FFI_DOUBLE_REF):
	  if (bvecp(val))
	    {
	      double n ;
	      memcpy(&n, &BVEC(val)->val[0], sizeof(double)) ;
	      val = make_real(n) ;
	    }
	  else
	    val = obj_false ;
	  NEXT ;
	BYTECODE(FFI_U8_REF):
	  if (bvecp(val))
	    {
	      unsigned char n ;
	      memcpy(&n, &BVEC(val)->val[0], sizeof(unsigned char)) ;
	      val = make_fixnum(n) ;
	    }
	  else
	    val = obj_false ;
	  NEXT ;
	BYTECODE(FFI_U16_REF):
	  if (bvecp(val))
	    {
	      unsigned short n ;
	      memcpy(&n, &BVEC(val)->val[0], sizeof(unsigned short)) ;
	      val = make_fixnum(n) ;
	    }
	  else
	    val = obj_false ;
	  NEXT ;
	BYTECODE(FFI_U32_REF):
	  if (bvecp(val))
	    {
	      unsigned int n ;
	      memcpy(&n, &BVEC(val)->val[0], sizeof(unsigned int)) ;
	      val = make_fixnum(n) ;
	    }
	  else
	    val = obj_false ;
	  NEXT ;
	BYTECODE(FFI_U64_REF):
	  if (bvecp(val))
	    {
	      long long n ;
	      memcpy(&n, &BVEC(val)->val[0], sizeof(long long)) ;
	      val = make_fixnum(n) ;
	    }
	  else
	    val = obj_false ;
	  NEXT ;
	BYTECODE(TEST_AND_SET):
	  {
	    obj_t a1 = val ;
	    obj_t a2 = pop () ;
	    obj_t a3 = pop () ;
	    if (pairp(val))
	      {
		if (PAIR(val)->cdr == a2)
		  {
		    PAIR(val)->cdr = a3 ;
		    val = a2 ;
		  }
		else
		  val = PAIR(val)->cdr ;
	      }
	    else
	      TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	    NEXT ;
	  }
	  /* Some speed primitives */
	BYTECODE(ASSQ):
	  {
	    obj_t a1 = val ;
	    obj_t a2 = pop() ;
	    obj_t o ;

	    for (o = a2 ;
		 pairp(o) && pairp(PAIR(o)->car) &&
		   PAIR(PAIR(o)->car)->car != a1;
		 o = PAIR(o)->cdr) ;

	    if (pairp(o) && pairp(PAIR(o)->car) &&
		PAIR(PAIR(o)->car)->car == a1)
	      val = PAIR(o)->car ;
	    else
	      val = obj_false ;
	    NEXT ;
	  }
	BYTECODE(RECORD_REF):
	  {
	    obj_t a1 = val ;
	    obj_t a2 = pop() ;

	    if (stobp(a1) && symbolp(a2))
	      {
		obj_t type = STOB(a1)->class ;
		obj_t fields = STOB(type)->slot[3] ;
		int offset ;

		for (offset = 0 ;
		     pairp(fields) && PAIR(fields)->car != a2 ;
		     fields = PAIR(fields)->cdr, offset++) ;

		if (pairp(fields))
		  val = STOB(a1)->slot[offset] ;
		else
		  TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	      }
	    else
	      TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    NEXT ;
	  }
	BYTECODE(RECORD_SET):
	  {
	    obj_t a1 = val ;
	    obj_t a2 = pop() ;
	    obj_t a3 = pop() ;

	    if (stobp(a1) && symbolp(a2))
	      {
		obj_t type = STOB(a1)->class ;
		obj_t fields = STOB(type)->slot[3] ;
		int offset ;

		for (offset = 0 ;
		     pairp(fields) && PAIR(fields)->car != a2 ;
		     fields = PAIR(fields)->cdr, offset++) ;

		if (pairp(fields))
		  STOB(a1)->slot[offset] = a3 ;
		else
		  TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	      }
	    else
	      TRAP3(TRAP_PRIMITIVE, a1, a2, a3) ;
	    NEXT ;
	  }
	BYTECODE(HOSTERROR):
	  val = make_fixnum(errno) ;
	  NEXT ;
	BYTECODE(HOSTSAMEERROR):
	  a1 = val ;
	  a2 = pop() ;
	  if (fixnump(a1) && fixnump(a2))
	    val = host_same_error(fixnum_value(a1), fixnum_value(a2)) ?
	      obj_true : obj_false ;
	  else
	    TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	  NEXT ;
        BYTECODE(CHANNEL_FLUSH):
          NEXT ;
	BYTECODE(SETTIMER):
	  {
	    a1 = val ;
	    a2 = pop() ;
	    if (fixnump(a1) && fixnump(a2))
	      {
		struct itimerval timer ;

		timer.it_interval.tv_sec = 0 ;
		timer.it_interval.tv_usec = 0 ;
		timer.it_value.tv_sec = fixnum_value(a1) ;
		timer.it_value.tv_usec = fixnum_value(a2) ;
		setitimer(ITIMER_VIRTUAL, &timer, NULL) ;
		timer_expired = 0 ;
	      }
	    else
	      TRAP2(TRAP_PRIMITIVE, a1, a2) ;
	    NEXT ;
	  }
        BYTECODE(UNIX_SYMLINK):
          {
            a1 = val ;
            a2 = pop() ;
            if (stringp(a1) && stringp(a2))
              {
                int rc = symlink(&STRING(a1)->ch[0],
                                 &STRING(a2)->ch[0]) ;
                if (rc == 0)
                  val = obj_false ;
                else
                  val = make_fixnum(errno) ;
              }
            else
              TRAP2(TRAP_PRIMITIVE, a1, a2) ;
            NEXT ;
          }
        BYTECODE(UNIX_READLINK):
          {
            a1 = val ;
            if (stringp(a1))
              {
                char buf[PATH_MAX+1] ;
                int rc = readlink(&STRING(a1)->ch[0], buf, PATH_MAX) ;
                if (rc == -1)
                  val = make_fixnum(errno) ;
                else
                  {
                    obj_t str = make_string(strlen(buf)+1) ;
                    strcpy(&STRING(str)->ch[0], buf) ;
                    val = str ;
                  }
              }
            else
              TRAP1(TRAP_PRIMITIVE, a1) ;
            NEXT ;
          }
        BYTECODE(RANDOM):
          {
            val = random32() ;
            NEXT ;
          }
	BYTECODE(CLASSOF):
	  {
	    val = mem_class_of(val) ;
	    NEXT;
	  }
	BYTECODE(SETCLASSES):
	  {
	    mem_set_classes(val) ;
	    NEXT;
	  }
	BYTECODE(INSTANCE):
	  {
	    a1 = val ;
	    a2 = pop() ;
	    val = mem_instancep(a1, a2) ;
	    NEXT ;
	  }
	BYTECODE(SUBTYPE):
	  {
	    a1 = val ;
	    a2 = pop() ;
	    val = mem_subtypep(a1, a2) ;
	    NEXT ;
	  }
#ifdef INDIRECT_THREADED_CODE
        BYTECODE(BADCODE):
          printf("Unknown byte code\n") ;
        error("Unknown bytecode") ;
#else
        default:
	  printf("Unknown byte code: %d\n",
		 BVEC(code_vector)->val[pc-1]) ;
          error("unknown bytecode") ;
        }
      /*validate_heap(lastbc) ;*/
    }
#endif
  return val ;
}

/* Taken from http://www.pcg-random.org/download.html */

typedef struct { uint64_t state;  uint64_t inc; } pcg32_random_t;

static pcg32_random_t rng = { 0x853c49e6748fea9bULL, 0xda3e39cb94b95bdbULL } ;

static
uint32_t pcg32_random_r(pcg32_random_t* rng)
{
    uint64_t oldstate = rng->state;
    // Advance internal state
    rng->state = oldstate * 6364136223846793005ULL + (rng->inc|1);
    // Calculate output function (XSH RR), uses old state for max ILP
    uint32_t xorshifted = ((oldstate >> 18u) ^ oldstate) >> 27u;
    uint32_t rot = oldstate >> 59u;
    return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}

static
obj_t random32()
{
  return make_fixnum(pcg32_random_r(&rng)) ;
}
