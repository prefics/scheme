#ifndef PROC_H
#define PROC_H

#include "mem.h"

#ifndef INDIRECT_THREADED_CODE
#define LIT                0
#define ARGS               1
#define GLOBAL             2
#define SET_GLOBAL         3
#define LOCAL              4
#define SET_LOCAL          5
#define CALL               6
#define TAIL               7
#define IFFALSE            8
#define JUMP               9
#define RET                10
#define PUSH               11
#define CLOS               12
#define LEAVE              13
#define ARGSGT             14
#define UNDEF              15
#define ENV                16

#define PLUS               32
#define MINUS              33
#define QUOTIENT           34
#define REMAINDER          35
#define MODULO             36
#define MULT               37

#define LT                 38
#define LTE                39
#define EQUAL              40
#define GT                 41
#define GTE                42

#define SYMBOLP            43
#define EQP                44
#define FIXNUMP            45
#define NULLP              52

#define ALLOCUVEC          46
#define UVECREF            47
#define UVECSET            48
#define UVECTYPE           49
#define UVECLENGTH         50

#define PAIRP              46
#define CONS               47
#define CAR                48
#define CDR                49
#define SETCAR             50
#define SETCDR             51

#define VECTORP            53
#define MAKEVECTOR         54
#define VECTORLENGTH       55
#define VECTORREF          56
#define VECTORSET          57

#define MAKESTRING         58
#define STRINGLENGTH       59
#define STRINGREF          60
#define STRINGSET          61

#define MAKECHANNEL        62
#define WRITE              63
#define READ               64
#define EOFP               65
#define CHANNEL_NUMBER     66
#define CHANNELP           67
#define OPENINPUTCHANNEL   68
#define OPENOUTPUTCHANNEL  69

#define CHAR2FIXNUM        70
#define FIXNUM2CHAR        71
#define SYMBOL2STRING      72
#define STRING2SYMBOL      73

#define APPLY              74

#define MAKESTOB           75
#define STOBREF            76
#define STOBSET            77
#define STOBCLASS          78
#define STOBP              79
#define STOBLENGTH         80

#define CALLCC             81
#define RESUMECC           82

#define MAKEBVEC           83
#define BVECP              84
#define BVECREF            85
#define BVECSET            86
#define BVECLENGTH         87

#define SAVEIMAGE          88

#define STRING2KEYWORD     89
#define KEYWORD2STRING     90

#define MAKEPROCEDURE      91
#define PROCEDUREP         92
#define PROCEDURESET       93
#define PROCEDUREREF       94
#define PROCEDURELENGTH    95

#define CHARP              96

#define CLOSECHANNEL       97
#define PEEKCHANNEL        98

#define STRINGP            99

#define MAKEREF            100
#define REFP               101
#define REFNAME            102
#define REFVALUE           103
#define REFMODULE          104
#define SETREFNAME         105
#define SETREFVALUE        106
#define SETREFMODULE       107
#define SETSTOBCLASS       108

#define BITOR              114
#define BITAND             115
#define BITXOR             116
#define BITASH             117
#define BITNOT             118

#define OBJECT_HASH        119
#define KEYWORDP           120
#define DIV                121

#define MAKE_ALIEN         122
#define ALIENTYPE          123
#define ALIENWREF          124
#define ALIENWSET          125
#define ALIENBREF          126
#define ALIENBSET          127

#define UNIX_OPEN          128
#define UNIX_CLOSE         129
#define UNIX_READ          130
#define UNIX_WRITE         131
#define UNIX_CHDIR         132
#define UNIX_ACCESS        133
#define UNIX_MKDIR         134
#define UNIX_STAT          135
#define UNIX_TIME          136
#define UNIX_LOCALTIME     137
#define UNIX_GMTIME        138
#define UNIX_SEEK          139
#define UNIX_SYNC          140
#define UNIX_RMDIR         141
#define UNIX_UNLINK        142
#define UNIX_RENAME        143
#define UNIX_CHMOD         144
#define UNIX_CHGRP         145
#define UNIX_CHOWN         146
#define UNIX_GETENV        147
#define UNIX_SETENV        148
#define UNIX_FORK          149
#define UNIX_EXEC          150
#define UNIX_DIRFILES      151
#define UNIX_RECV          152
#define UNIX_SEND          153
#define UNIX_SOCKET        154
#define UNIX_BIND          155
#define UNIX_LISTEN        156
#define UNIX_ACCEPT        157
#define UNIX_CONNECT       158
#define UNIX_SELECT        159

#define UNIX_MKNOD         160
#define UNIX_TEMPNAM       161
#define UNIX_GETPWNAM      162
#define UNIX_GETGRNAM      163
#define UNIX_UTIME         164
#define UNIX_UMASK         165
#define UNIX_GETPID        166
#define UNIX_GETPPID       167
#define UNIX_GETSID        168
#define UNIX_SETSID        169
#define UNIX_GETUID        170
#define UNIX_GETEUID       171
#define UNIX_GETGID        172
#define UNIX_GETEGID       173
#define UNIX_SETUID        174
#define UNIX_SETEGID       175
#define UNIX_SETEUID       176
#define UNIX_SETGID        177

#define UNIX_EXIT          178
#define UNIX_WAITPID       179
#define UNIX_GETCWD        180
#define UNIX_PIPE          181
#define UNIX_DUP2          182

#define FFI_DLOPEN         183
#define FFI_DLERROR        184
#define FFI_DLSYM          185
#define FFI_DLCLOSE        186
#define FFI_APPLY          187
#define FFI_MEM_REF        188
#define FFI_STRING_REF     189
#define FFI_MEM_SET        190
#define FFI_MIRROR         191
#define FFI_MEM_DEREF      192
#define FFI_MALLOC         193
#define FFI_FREE           194
#define FFI_U8_REF         195
#define FFI_U16_REF        196
#define FFI_U32_REF        197
#define FFI_U64_REF        198
#define FFI_DOUBLE_REF     199

#define UNIX_KILL          200
#define UNIX_UNAME         212
#define UNIX_SETSOCKOPT    213
#define UNIX_GETSOCKOPT    214
#define UNIX_UNIX_ADDRESS  215
#define UNIX_INET_ADDRESS  216

#define UNIX_SIGNALBLOCK   217
#define ASSQ               218
#define RECORD_REF         219
#define RECORD_SET         220

#define TEST_AND_SET       221

#define REAL2STRING        222
#define STRING2REAL        223
#define FIXNUM2REAL        224
#define REALP              225

#define ROUND              226
#define TRUNCATE           227
#define CEILING            228
#define FLOOR              229

#define RATIONALIZE        230
#define ZEXP               231
#define ZLOG               232
#define ZSIN               233
#define ZCOS               234
#define ZTAN               235
#define ZASIN              236
#define ZACOS              237
#define ZATAN              238
#define ZATAN2             239
#define ZSQRT              240
#define ZEXPT              241
#define NATIVE_CALL        242

#define CHANNEL_FLUSH      243
#define HOSTERROR          244
#define HOSTSAMEERROR      245

#define SETTIMER           246
#define UNIX_SYMLINK       247
#define UNIX_READLLINK     248
#endif

#define DEFAULT_STACK_SIZE 4096
#define DEFAULT_CONT_SIZE  65536

#define TRAP_BAD_ARGS       0
#define TRAP_UNBOUND_GLOBAL 1
#define TRAP_NO_PROCEDURE   2
#define TRAP_PRIMITIVE      3
#define TRAP_TIMER          4
#define TRAP_SIGNAL         5
#define TRAP_OUT_OF_MEMORY  6

/* printf("TRAPPING %d at %d bytecode %d!!!\n",
   n,
   pc-1,
   BVEC(code_vector)->val[pc-1]) ; */
/* printf("TRAPPING @ %d in %s\n", __LINE__, __FILE__) ; */
#define TRAPCALL(n, ac)							\
  argc = (ac) ;								\
  env = CLOSURE(VECTOR(trap)->val[n])->val[0] ;				\
  template = CLOSURE(VECTOR(trap)->val[n])->val[1] ;			\
  code_vector = VECTOR(template)->val[0] ;				\
  literals = VECTOR(template)->val[1] ;	                                \
  pc = 0 ;

#define TRAP(n,ac)							\
  if (trap != obj_undefined &&						\
      closurep(VECTOR(trap)->val[n]))					\
    {									\
      a1 = make_vector(4) ;						\
      VECTOR(a1)->val[0] = cont ;					\
      VECTOR(a1)->val[1] = env ;					\
      VECTOR(a1)->val[2] = template ;					\
      VECTOR(a1)->val[3] = make_fixnum(pc) ;				\
      cont = a1 ;							\
      TRAPCALL(n, ac)							\
    }									\
  else									\
    {									\
      printf("TRAP %d not set", n) ;					\
      *(int *) 0 = 0 ;							\
    }

#define TRAP0(n)             { TRAP(n,0) ; }
#define TRAP1(n,a)           { push(a) ; TRAP(n, 1) }
#define TRAP2(n,a1,a2)       { push(a2) ; push(a1) ; TRAP(n,2) ; }
#define TRAP3(n,a1,a2,a3)    { push(a3) ; push(a2) ; push(a1) ; TRAP(n, 3) ; }
#define TRAP4(n,a1,a2,a3,a4) { push(a1) ; push(a2) ; push(a3) ; push(a4) ; TRAP(n, 4) }

#define _PUSH(obj)							\
  {									\
    if (top == DEFAULT_STACK_SIZE)					\
      {									\
	obj_t vec = new_stack() ;					\
	printf("pushing stack\n") ; fflush(NULL) ;			\
	VECTOR(vec)->val[0] = stack ;					\
	top = 1 ;							\
	stack = vec ;							\
      }									\
    VECTOR(stack)->val[top++] = obj ;					\
  }

extern obj_t val ;
extern obj_t stack ;
extern int   top ;
extern obj_t env ;
extern obj_t template ;
extern obj_t code_vector ;
extern obj_t literals ;
extern int   pc ;
extern obj_t cont ;
extern int   argc ;
extern obj_t dynamic_env ;
extern obj_t dynamic_cont ;
extern obj_t a1, a2, a3, a4 ;
extern obj_t trap ;
extern int   timer_expired ;
extern int   host_signal ;

extern void resume_cc(obj_t) ;
extern obj_t run(obj_t closure, obj_t args) ;
#endif
