#ifdef SCM_TARGET_POSIX
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "mem.h"
#include "proc.h"

int mem_heap_size ;
int mem_word_alloced ;
obj_t *mem_start ;
obj_t *mem_end ;
obj_t *mem_free ;

void validate_heap(int bc) ;
void print_backtrace(void) ;

static void  mem_mark(obj_t *obj) ;
static void  mem_compact(void) ;
static obj_t mem_reloc(obj_t val) ;
static void  mem_update(void) ;
static int   validate_pointer(obj_t ptr) ;
static obj_t *mem_alloc(int size) ;
static int   symbolcmp (obj_t sym1, obj_t sym2) ;
static void  mem_close_channels(void) ;
static void  mem_grow_channels(void) ;
static void  grow_symbol_table(void) ;
static unsigned long symbol_hash(obj_t) ;
static void mem_out_of_memory_check() ;

static int num_channels = 10 ;
static obj_t *channels ;

obj_t classes ;

void print_backtrace(void)
{
  obj_t frame ;
  obj_t templ ;
  obj_t debug ;

  for (frame = cont ; frame != obj_false ; frame = VECTOR(frame)->val[0])
    {
      templ = VECTOR(frame)->val[2] ;
      debug = VECTOR(templ)->val[2] ;

      write_obj(VECTOR(debug)->val[0]) ; printf("\n") ;
    }
  *(char *) 0 = 0 ;
}

void mem_gc(void)
{
  int i ;
#ifdef SCM_TARGET_POSIX
  long diff ;
  clock_t start, stop ;

  start = clock() ;
#endif
  /*  printf("\n\nGarbage collecting: marking...") ; fflush(NULL) ; */

  /* write_obj(literals) ;
     write_obj(code_vector) ;
  */

  mem_mark(&classes) ;
  mem_mark(&symbol_table) ;

  mem_mark(&val) ;

  if (vectorp(stack))
    {
      SET_HEADER_MARK(*pointer_value(stack), 1) ;
      for (i=0 ; i < top ; i++)
        mem_mark(&VECTOR(stack)->val[i]) ;
    }
  else mem_mark(&stack) ;

  mem_mark(&env) ;

  mem_mark(&template) ;

  mem_mark(&code_vector) ;
  mem_mark(&literals) ;

  mem_mark(&cont) ;

  mem_mark(&dynamic_env) ;
  mem_mark(&dynamic_cont) ;
  mem_mark(&a1) ;
  mem_mark(&a2) ;
  mem_mark(&a3) ;
  mem_mark(&a4) ;
  mem_mark(&trap) ;

  /*  write_obj(literals) ;
      write_obj(code_vector) ; */

  mem_close_channels() ;

  /*  printf("compacting...") ; */
  mem_compact() ;
#ifdef SCM_TARGET_POSIX
  stop = clock() ;
  diff = stop-start ;
  /*    printf("done in %01d [sec] %3d [msec] %d heap in use\n\n",
	 diff/CLOCKS_PER_SEC,
	 (diff % CLOCKS_PER_SEC) / (CLOCKS_PER_SEC / 1000),
         (mem_free-mem_start)) ;
  */
#endif
  validate_heap(-1) ;
  /*
    write_obj(literals) ;
    write_obj(code_vector) ;
  */
  mem_out_of_memory_check() ;
}

static void mem_out_of_memory_check()
{
  if (mem_end - mem_free < 4096)
    {
      obj_t cc = VECTOR(trap)->val[TRAP_OUT_OF_MEMORY] ;

      while (cc != obj_nil && mem_end - mem_free < 4096)
        {
          obj_t point = PAIR(cc)->car ;
          cc = PAIR(cc)->cdr ;
          resume_cc(point) ;
          mem_gc() ;
        }

      if (mem_end - mem_free < 4096)
        {
          printf("OUT OF MEMORY") ; fflush(NULL) ;
          *((int *) 0x00) = 0  ;
        }

      val = obj_true ;
    }
}

int validate_pointer(obj_t ptr)
{
  switch (tag(ptr))
    {
    case tag_pointer:
      if (tag(*pointer_value(ptr)) == tag_header)
        return 1 ;
      else
        return 0 ;
    case tag_fixnum:
      return 1;
    case tag_immediate:
      return 1;
    case tag_header:
      return 0 ;
    }
  return 0 ;
}

void validate_heap(int bc)
{
  obj_t *o ;

  for (o = mem_start ; o < mem_free ; o+=HEADER_SIZE(*o))
    {
      switch(HEADER_TYPE(*o))
        {
        case header_ref:
          if (!validate_pointer(REF_(o)->name))
            printf("Error in heap REF has invalid NAME attr\n") ;
          if (!validate_pointer(REF_(o)->module))
            printf("Error in heap REF has invalid MODULE attr\n") ;
          if (!validate_pointer(REF_(o)->value))
            printf("Error in heap REF has invalid VALUE attr\n") ;
          break;
        case header_pair:
          if (!validate_pointer(PAIR_(o)->car))
            printf("Error in heap PAIR has invalid CAR attr\n") ;
          if (!validate_pointer(PAIR_(o)->cdr))
            printf("Error in heap PAIR has invalid CDR attr\n") ;
          break ;
        case header_string:
          break ;
        case header_vector:
          {
            int size = HEADER_SIZE(*o) - 1 ;
            int j ;

            for (j=0; j < size; j++)
              if (!validate_pointer(VECTOR_(o)->val[j]))
                {
                  printf("Error in heap VECTOR of size %d has invalid %d obj\n",
                         size, j) ;
                  write_obj(VECTOR_(o)->val[j]) ;
                  printf("after bytecode %d", bc) ;
		  print_backtrace() ;
                }
            break ;
          }
        case header_closure:
          {
            int size = CLOSURE_(o)->size ;
            int j ;

            for (j = 0 ; j < size ; j++)
              if (!validate_pointer(CLOSURE_(o)->val[j]))
                printf("Error in heap CLOSURE of size %dhas invalid %d obj\n",
                       size, j) ;
            break ;
          }
          break ;
        case header_stob:
          {
            int size = HEADER_SIZE(*o) - 2 ;
            int j ;

            if (!validate_pointer(STOB_(o)->class))
              printf("STOB has invalid CLASS attr\n") ;
            for (j = 0 ; j < size ; j++)
              if (!validate_pointer(STOB_(o)->slot[j]))
                printf("STOB of size %d has invalid SLOT %d\n", size, j) ;
            break ;
          }
        case header_channel:
	  if (!validate_pointer(CHANNEL_(o)->buffer))
	    printf("Error in heap CHANNEL has invalid BUFFER slot\n") ;
	  if (!validate_pointer(CHANNEL_(o)->index))
	    printf("Error in heap CHANNEL has invalid BUFFER slot\n") ;
	  if (!validate_pointer(CHANNEL_(o)->available))
	    printf("Error in heap CHANNEL has invalid AVAILABLE slot\n") ;
	  break ;
        case header_bvec:
        case header_symbol:
        case header_free:
        case header_real:
	      break ;
        default:
          printf("Unknown header %ld", HEADER_TYPE(*o)) ; fflush(NULL) ;
          *((int *) 0x00) = 0  ;
          error("unknow header type") ;
        }
    }
  if (!validate_pointer(val))
    printf("VAL %08lx is invalid\n", val) ;
  if (!validate_pointer(stack))
    printf("STACK %08lx is invalid\n", stack) ;
  if (!validate_pointer(env))
    printf("ENV %08lx is invalid\n", env) ;
  if (!validate_pointer(dynamic_env))
    printf("DYNAMIC_ENV %08lx is invalid\n", dynamic_env) ;
  if (!validate_pointer(dynamic_cont))
    printf("DYNAMIC_CONT %08lx is invalid\n", dynamic_cont) ;
  if (!validate_pointer(trap))
    printf("TRAP %08lx is invalid\n", trap) ;
  if (!validate_pointer(template))
    printf("TEMPLATE %08lx is invalid\n", template) ;
  if (!validate_pointer(code_vector))
    printf("CODE_VECTOR %08lx is invalid\n", code_vector) ;
  if (!validate_pointer(literals))
    printf("LITERALS %08lx is invalid\n", literals) ;
  if (!validate_pointer(cont))
    printf("CONT %08lx is invalid\n", cont) ;
  if (!validate_pointer(a1))
    printf("A1 %08lx is invalid\n", a1) ;
  if (!validate_pointer(a2))
    printf("A2 %08lx is invalid\n", a2) ;
  if (!validate_pointer(a3))
    printf("A3 %08lx is invalid\n", a3) ;
  if (!validate_pointer(a4))
    printf("A4 %08lx is invalid\n", a4) ;

}

void mem_mark(obj_t *obj)
{
  if (tag(*obj) == tag_pointer)
    {
      obj = pointer_value(*obj) ;
      if (HEADER_MARK(*obj) == 0)
	{
	  SET_HEADER_MARK(*obj,1) ;
	  switch(HEADER_TYPE(*obj))
	    {
	    case header_ref:
	      mem_mark(&REF_(obj)->name) ;
	      mem_mark(&REF_(obj)->module) ;
	      mem_mark(&REF_(obj)->value) ;
	      break;
	    case header_pair:
	      mem_mark(&PAIR_(obj)->car) ;
	      mem_mark(&PAIR_(obj)->cdr) ;
	      break ;
	    case header_vector:
	      {
		int size = HEADER_SIZE(*obj) - 1 ;
                int j ;

		for (j=0; j < size; j++) mem_mark(&VECTOR_(obj)->val[j]) ;
		break ;
	      }
	    case header_closure:
	      {
		int size = CLOSURE_(obj)->size ;
                int j ;

		for (j = 0 ; j < size ; j++)
                  mem_mark(&CLOSURE_(obj)->val[j]) ;
		break ;
	      }
	      break ;
	    case header_stob:
	      {
		int size = HEADER_SIZE(*obj) - 2 ;
                int j ;

		mem_mark(&STOB_(obj)->class) ;
		for (j = 0 ; j < size ; j++) mem_mark(&STOB_(obj)->slot[j]) ;
		break ;
	      }
	    case header_channel:
	      mem_mark(&CHANNEL_(obj)->buffer) ;
	      mem_mark(&CHANNEL_(obj)->available) ;
	      mem_mark(&CHANNEL_(obj)->index) ;
	      break ;
            case header_real:
	    case header_string:
	    case header_bvec:
	    case header_symbol:
	    case header_free:
	      break ;
	    default:
              printf("Unknown header %ld", HEADER_TYPE(*obj)) ; fflush(NULL) ;
              *((int *) 0x00) = 0  ;
	      error("unknow header type") ;
	    }
	}
    }
}

obj_t *mem_alloc(int size)
{
  int length=SIZE(size) ;

  if (mem_free + length >= mem_end)
    mem_gc() ;

  if (mem_free + length < mem_end)
    {
      obj_t *alloc = mem_free ;

      mem_free += length ;
      *mem_free = make_header(mem_end-mem_free, header_free) ;
      SET_HEADER_SIZE(*alloc, length) ;
      return alloc ;
    }
  else
    error("HEAP too full !") ;

  return NULL ;
}

/* Object part */

obj_t make_real(double val)
{
  obj_t *real = mem_alloc(sizeof(real_t)) ;
  SET_HEADER_TYPE(*real, header_real) ;
  REAL_(real)->value = val ;
  return make_pointer(real) ;
}

obj_t realp(obj_t obj)
{
  return (tag(obj) == tag_pointer &&
          HEADER_TYPE(*pointer_value(obj)) == header_real) ;
}

obj_t make_ref()
{
  obj_t *ref = mem_alloc(sizeof(ref_t)) ;
  SET_HEADER_TYPE(*ref, header_ref) ;

  return make_pointer(ref) ;
}

obj_t refp(obj_t obj)
{
  return (tag(obj) == tag_pointer &&
          HEADER_TYPE(*pointer_value(obj)) == header_ref) ;
}

obj_t make_pair(void)
{
  obj_t *pair = mem_alloc(sizeof(pair_t)) ;
  SET_HEADER_TYPE(*pair, header_pair) ;
  return make_pointer(pair) ;
}

obj_t make_list(int size)
{
  obj_t *list ;
  obj_t *pair ;
  int i ;

  if (size > 0)
    {
      pair = list = mem_alloc(sizeof(pair_t)*size) ;
      for (i=0; i < size ; i++)
        {
          *pair = make_header(3, header_pair) ;
          PAIR_(pair)->car = obj_nil ;
          PAIR_(pair)->cdr = make_pointer(pair+3) ;
          pair+=3 ;
        }
      pair -=3 ;
      PAIR_(pair)->cdr = obj_nil ;

      return make_pointer(list) ;
    }
  else
    return obj_nil ;
}

obj_t pairp(obj_t obj)
{
  return (tag(obj) == tag_pointer &&
          HEADER_TYPE(*pointer_value(obj)) == header_pair) ;
}

obj_t make_string(int size)
{
  obj_t *string = mem_alloc(sizeof(string_t)+size) ;
  STRING_(string)->len = size ;
  STRING_(string)->ch[size] = '\0' ;
  SET_HEADER_TYPE(*string, header_string) ;
  return make_pointer(string) ;
}

obj_t stringp(obj_t obj)
{
  return (tag(obj) == tag_pointer &&
          HEADER_TYPE(*pointer_value(obj)) == header_string) ;
}

obj_t make_vector(int size)
{
  obj_t *vec = mem_alloc(sizeof(vector_t)+size*sizeof(obj_t)) ;

  if (size < 0)
    print_backtrace() ;
  SET_HEADER_TYPE(*vec, header_vector) ;
  return make_pointer(vec) ;
}

obj_t vectorp(obj_t obj)
{
  return (tag(obj) == tag_pointer &&
          HEADER_TYPE(*pointer_value(obj)) == header_vector) ;
}

obj_t make_bvec(int size)
{
  obj_t *bvec = mem_alloc(sizeof(bvec_t)+size) ;
  if (size < 0)
    print_backtrace() ;
  SET_HEADER_TYPE(*bvec, header_bvec) ;
  BVEC_(bvec)->size = size ;
  return make_pointer(bvec) ;
}

obj_t bvecp(obj_t obj)
{
  return (tag(obj) == tag_pointer &&
          HEADER_TYPE(*pointer_value(obj)) == header_bvec) ;
}

obj_t make_closure(int size)
{
  obj_t *clos = mem_alloc(sizeof(closure_t)+size*sizeof(obj_t)) ;
  if (size < 0)
    print_backtrace() ;
  SET_HEADER_TYPE(*clos, header_closure) ;
  CLOSURE_(clos)->size = size ;
  return make_pointer(clos) ;
}

obj_t closurep(obj_t obj)
{
  return (tag(obj) == tag_pointer &&
          HEADER_TYPE(*pointer_value(obj)) == header_closure) ;
}

obj_t make_stob(int size)
{
  obj_t *stob = mem_alloc(sizeof(stob_t)+size*sizeof(obj_t)) ;
#ifdef SCHEMY_DEBUG_STOB_ALLOC
  printf("Allocating STOB @ %0xld size=%d header_size=%d\n",
	 stob, size, HEADER_SIZE(*stob)) ; fflush(NULL) ;
#endif
  if (size < 0)
    print_backtrace() ;
  SET_HEADER_TYPE(*stob, header_stob) ;
  return make_pointer(stob) ;
}

obj_t stobp(obj_t obj)
{
  return (tag(obj) == tag_pointer &&
          HEADER_TYPE(*pointer_value(obj)) == header_stob) ;
}

obj_t make_channel(host_channel_t ch)
{
  obj_t *channel = mem_alloc(sizeof(channel_t)) ;
  SET_HEADER_TYPE(*channel, header_channel) ;
  CHANNEL_(channel)->channel_number = ch ;
  CHANNEL_(channel)->buffer = obj_false ;
  CHANNEL_(channel)->available = make_fixnum(0) ;
  CHANNEL_(channel)->index = make_fixnum(0) ;

  mem_remember_channel(make_pointer(channel)) ;

  return make_pointer(channel) ;
}

obj_t channelp(obj_t obj)
{
  return (tag(obj) == tag_pointer &&
          HEADER_TYPE(*pointer_value(obj)) == header_channel) ;
}

obj_t symbol_table ; /* ROOT */

void grow_symbol_table(void)
{
  unsigned long len = HEADER_SIZE(*pointer_value(symbol_table))-1 ;
  unsigned long new_len = 2*len ;
  obj_t new_symbol_table = make_vector(new_len) ;

  for (uintptr_t i = 0 ; i < new_len ; i++)
    VECTOR(new_symbol_table)->val[i] = obj_false ;

  for (uintptr_t i = 0 ; i < len ; i++)
    {
      obj_t symbol = VECTOR(symbol_table)->val[i] ;
      unsigned long bucket = symbol_hash(symbol) % new_len ;
      uintptr_t j ;

      for (j = bucket ; VECTOR(new_symbol_table)->val[j] != obj_false ; j=(j+1) % new_len) ;
      VECTOR(new_symbol_table)->val[j] = symbol ;
    }
  symbol_table = new_symbol_table ;
}

int symbolcmp (obj_t sym1, obj_t sym2)
{
  return strcmp((char *)&SYMBOL(sym1)->ch[0],
		(char *)&SYMBOL(sym2)->ch[0]) ;
}

unsigned long symbol_hash(obj_t sym)
{
    unsigned long hash = 5381;
    unsigned char *s = (unsigned char *)&SYMBOL(sym)->ch[0] ;
    int c;

    while (c = *s++)
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

obj_t intern(void)
{
  unsigned long hash = symbol_hash(val) ;
  unsigned long len = HEADER_SIZE(*pointer_value(symbol_table))-1 ;
  uintptr_t start_index = hash % len ;

  if (VECTOR(symbol_table)->val[start_index] == obj_false)
    {
      VECTOR(symbol_table)->val[start_index] = val ;
      return val ;
    }
  else if (!symbolcmp(val, VECTOR(symbol_table)->val[start_index]))
    {
      return VECTOR(symbol_table)->val[start_index] ;
    }
  else
    {
      uintptr_t i ;

      for (i = (start_index + 1) % len ;
	   i != start_index &&
	   VECTOR(symbol_table)->val[i] != obj_false &&
	   symbolcmp(val, VECTOR(symbol_table)->val[i]);
	   i=(i+1) % len) ;

      if (i == start_index)
	{
	  /* hash table full */
	  grow_symbol_table() ;
	  return intern() ;
	}
      else if (VECTOR(symbol_table)->val[i] == obj_false)
	{
	  /* We have found an empty bucket */
	  VECTOR(symbol_table)->val[i] = val ;
	  return val ;
	}
      else
	{
	  /* We have found the entry */
	  return VECTOR(symbol_table)->val[i] ;
	}
    }
}

obj_t symbolp(obj_t obj)
{
  return (tag(obj) == tag_pointer &&
          HEADER_TYPE(*pointer_value(obj)) == header_symbol) ;
}

obj_t make_symbol(int size)
{
  obj_t *sym = mem_alloc(sizeof(symbol_t)+size) ;
  if (size < 0)
    print_backtrace() ;
  SET_HEADER_TYPE(*sym, header_symbol) ;
  return make_pointer(sym) ;
}

void mem_init(int heap_size)
{
  int i ;

  mem_heap_size = heap_size ;
  mem_start     = (obj_t *) malloc(heap_size*sizeof(obj_t)) ;
  mem_end       = mem_start+heap_size ;
  *mem_start    = make_header(heap_size, header_free) ;
  mem_free      = mem_start ;

  /*  printf("MEM @ %08x size %d end @ %08x\n", mem_start, heap_size, mem_end) ; */
  channels = (obj_t *) malloc (num_channels * (sizeof(obj_t))) ;
  for (i = 0 ; i < num_channels ; i++)
    channels[i] = obj_undefined ;

  /* initialize symbol table */
  symbol_table = make_vector(1027) ;

  for (uint32_t i = 0 ; i < 1027 ; i++)
    VECTOR(symbol_table)->val[i] = obj_false ;

  classes = obj_false ;
}

obj_t write_obj(obj_t obj)
{
  if (obj == obj_nil)
    printf("#{nil}") ;
  else if (obj == obj_undefined)
    printf("#{undefined}") ;
  else if (obj == obj_unbound)
    printf("#{unbound}") ;
  else if (obj == obj_eof)
    printf("#{EOF}") ;
  else if (obj == obj_true)
    printf("#t") ;
  else if (obj == obj_false)
    printf("#f") ;
  else if (fixnump(obj))
    printf("%ld", (long) fixnum_value(obj)) ;
  else if (charp(obj))
    printf("#\\%04d ", (int) char_value(obj)) ;
  else if (tag(obj) == tag_pointer)
    {
      switch (HEADER_TYPE(*pointer_value(obj)))
        {
        case header_real:
          printf("%f", REAL(obj)->value) ;
          break ;
	case header_ref:
	  printf("#{ref ") ;
	  write_obj(REF(obj)->name) ;
	  printf(",") ;
	  write_obj(REF(obj)->module) ;
	  printf("}") ;
	  break ;
        case header_pair:
          {
            obj_t p ;
            printf("(") ;
            for (p=obj ; pairp(p) ; p = PAIR(p)->cdr)
              {
                write_obj(PAIR(p)->car) ;
                if (pairp(PAIR(p)->cdr))
                  printf(" ") ;
              }
            if (p != obj_nil)
              {
                printf(" . ") ;
                write_obj(p) ;
              }
            printf(")") ;
            break ;
          }
        case header_string:
          printf("\"%s\"", &STRING(obj)->ch[0]) ;
          break ;
        case header_vector:
	  {
	    int i ;
	    printf("'#(") ;
	    for (i=0 ; i < HEADER_SIZE(*pointer_value(obj))-1 ; i++)
	      {
		write_obj(VECTOR(obj)->val[i]) ;
		if (i != HEADER_SIZE(*pointer_value(obj))-2)
		  printf(" ") ;
	      }
	    printf(")") ;
	    break ;
	  }
        case header_closure:
          printf("#{procedure size=%d address=%p}",
                 CLOSURE(obj)->size, pointer_value(obj)) ;
          break ;
        case header_stob:
          {
            int i ;
            printf("#{stob %ld", HEADER_SIZE(*pointer_value(obj))-2) ;
            for (i = 0 ; i < HEADER_SIZE(*pointer_value(obj))-2 ; i++)
              {
                write_obj(STOB(obj)->slot[i]) ;
                if (i != HEADER_SIZE(*pointer_value(obj))-2)
                  printf(" ") ;
              }
            printf("}") ;
            break ;
          }
        case header_bvec:
          {
            int i ;
            printf("#[") ;
            for (i=0 ; i < BVEC(obj)->size ; i++)
              printf("%02x ", BVEC(obj)->val[i]) ;
            printf("]") ;
            break ;
          }
        case header_channel:
          printf("#{channel %d,%lx,%lx, %lx}",
		 CHANNEL(obj)->channel_number,
		 CHANNEL(obj)->buffer,
		 CHANNEL(obj)->available,
		 CHANNEL(obj)->index
		 ) ;
          break ;
        case header_symbol:
          printf("'%s", &SYMBOL(obj)->ch[0]) ;
          break ;
        case header_free:
          printf("#{free block %p}", pointer_value(obj)) ;
          break ;
        default:
          printf("#{Unknown object %p}", pointer_value(obj)) ;
	  break ;
        }
    }
  else
    printf("unknow object (%lx)", obj) ;
  fflush(NULL) ;
  return obj ;
}

#define RELOC(a) if (tag(a) == tag_pointer) (a) += delta
void mem_adjust(long delta)
{
  obj_t *scan ;

  for (scan = mem_start ; HEADER_TYPE(*scan) != header_free ; scan+=HEADER_SIZE(*scan))
    /*
      printf("start=%08x, end=%08x, scan=%0x8x, type=%02x, size=%08x\n",
	   mem_start, mem_end, scan,
	   HEADER_TYPE(*scan), HEADER_SIZE(*scan)) ;
    */
    switch(HEADER_TYPE(*scan))
      {
      case header_ref:
	RELOC(REF_(scan)->name) ;
	RELOC(REF_(scan)->module) ;
	RELOC(REF_(scan)->value) ;
	break;
      case header_pair:
        RELOC(PAIR_(scan)->car) ;
        RELOC(PAIR_(scan)->cdr) ;
        break ;
      case header_vector:
        {
          int size = HEADER_SIZE(*scan) - 1 ;
          int j ;

          for (j=0; j < size; j++)
            RELOC(VECTOR_(scan)->val[j]) ;
          break ;
        }
      case header_closure:
        {
          int size = CLOSURE_(scan)->size ;
          int j ;

          for (j = 0 ; j < size ; j++)
            RELOC(CLOSURE_(scan)->val[j]) ;
          break ;
        }
        break ;
      case header_stob:
        {
          int size = HEADER_SIZE(*scan) - 2 ;
          int j ;

          RELOC(STOB_(scan)->class) ;
          for (j = 0 ; j < size ; j++)
            RELOC(STOB_(scan)->slot[j]) ;
          break ;
        }
      case header_channel:
	RELOC(CHANNEL_(scan)->buffer) ;
	RELOC(CHANNEL_(scan)->available) ;
	RELOC(CHANNEL_(scan)->index) ;
	break ;
      case header_real:
      case header_bvec:
      case header_string:
      case header_symbol:
      case header_free:
        break ;
      default:
        error("Unknown header type in MEM_RELOC") ;
      }
}
/* Compacting stuff */

static obj_t *reloc[16384] ;
static int delta[16384] ;
static int reloc_index ;
static obj_t *mem_min ;
static obj_t *mem_max ;
static obj_t *compact ;

obj_t mem_reloc(obj_t val)
{
  if (tag(val) == tag_pointer &&
      mem_min <= pointer_value(val) &&
      pointer_value(val) < mem_max)
    {
      int i, min, max ;
      obj_t *ptr = pointer_value(val) ;

      min = 0 ; max = reloc_index ;
      do {
	i = (min+max) / 2 ;
	if (ptr < reloc[i]) max = i ; else min = i ;
      } while (max-min > 1) ;

      return make_pointer(ptr+delta[min]) ;
    }
  else
    return val ;
}

void mem_update(void)
{
  obj_t *scan ;

  for (scan=mem_start ; scan < mem_free ; scan += HEADER_SIZE(*scan))
    {
    switch(HEADER_TYPE(*scan))
      {
      case header_ref:
	REF_(scan)->name  = mem_reloc(REF_(scan)->name) ;
	REF_(scan)->module = mem_reloc(REF_(scan)->module) ;
	REF_(scan)->value = mem_reloc(REF_(scan)->value) ;
	break;
      case header_pair:
        PAIR_(scan)->car = mem_reloc(PAIR_(scan)->car) ;
        PAIR_(scan)->cdr = mem_reloc(PAIR_(scan)->cdr) ;
        break ;
      case header_vector:
        {
          int size = HEADER_SIZE(*scan) - 1 ;
          int j ;

          for (j=0; j < size; j++)
            VECTOR_(scan)->val[j] = mem_reloc(VECTOR_(scan)->val[j]) ;
          break ;
        }
      case header_closure:
        {
          int size = CLOSURE_(scan)->size ;
          int j ;

          for (j = 0 ; j < size ; j++)
            CLOSURE_(scan)->val[j] = mem_reloc(CLOSURE_(scan)->val[j]) ;
          break ;
        }
      case header_stob:
        {
          int size = HEADER_SIZE(*scan) - 2 ;
          int j ;

          STOB_(scan)->class = mem_reloc(STOB_(scan)->class) ;
          for (j = 0 ; j < size ; j++)
            STOB_(scan)->slot[j] = mem_reloc(STOB_(scan)->slot[j]) ;
          break ;
        }
      case header_channel:
	CHANNEL_(scan)->buffer = mem_reloc(CHANNEL_(scan)->buffer) ;
	CHANNEL_(scan)->available = mem_reloc(CHANNEL_(scan)->available) ;
	CHANNEL_(scan)->index  = mem_reloc(CHANNEL_(scan)->index) ;
	break ;
      case header_real:
      case header_bvec:
      case header_string:
      case header_symbol:
      case header_free:
        break ;
      default:
        error("Unknown header type in MEM_UPDATE") ;
      }
    }

  symbol_table  = mem_reloc(symbol_table) ;
  val           = mem_reloc(val) ;
  env           = mem_reloc(env) ;
  stack         = mem_reloc(stack) ;
  cont          = mem_reloc(cont) ;
  dynamic_env   = mem_reloc(dynamic_env) ;
  dynamic_cont  = mem_reloc(dynamic_cont) ;
  template      = mem_reloc(template) ;
  code_vector   = mem_reloc(code_vector) ;
  literals      = mem_reloc(literals) ;
  trap          = mem_reloc(trap) ;
  a1            = mem_reloc(a1) ;
  a2            = mem_reloc(a2) ;
  a3            = mem_reloc(a3) ;
  a4            = mem_reloc(a4) ;

  classes       = mem_reloc(classes) ;

  int i ;
  for (i = 0 ; i < num_channels ; i++)
    if (channels[i] != obj_undefined)
      channels[i] = mem_reloc(channels[i]) ;
}

void mem_compact(void)
{
  obj_t *scan ;

  reloc_index=0 ;
  for (scan = compact = mem_min = mem_start ;
       scan < mem_free ;)
    {
      for (scan=scan ;
           scan < mem_free && HEADER_MARK(*scan) == 0 ;
           scan+=HEADER_SIZE(*scan))
	{
	  /*	  printf("Skipping obj of type: %d, of length: %d at %x\n",  */
	  /*		 HEADER_TYPE(*scan), HEADER_SIZE(*scan), scan) ;     */
	  /*	  fflush(NULL) ;                                             */
	}

      if (scan < mem_free)
        {
          if (reloc_index >= 16384)
            {
	      printf("Reloc table full!\n") ; fflush(NULL) ;
	      mem_max = scan ;
              mem_update() ;
	      mem_min = scan ;
              reloc_index = 0 ;
            }
          reloc[reloc_index] = scan ;
          delta[reloc_index] = compact-scan ;
	  reloc_index++ ;

	  if (scan-compact > 0)
	    {
	      while (scan < mem_free && HEADER_MARK(*scan) == 1)
		{
		  int size = HEADER_SIZE(*scan) ;
		  int i ;

		  /*	      printf("Compacting object of type %d, of length: %d from 0x%x to 0x%x\n", */
		  /*		     HEADER_TYPE(*scan), size, scan, compact) ;                         */
		  /*	      fflush(NULL) ;                                                            */
		  SET_HEADER_MARK(*scan, 0) ;
		  for (i=0 ; i < size ; i++)
		    *compact++ = *scan++ ;
		}
	      *compact=make_header(scan-compact, header_free) ;
	    }
	  else
	    while (scan < mem_free && HEADER_MARK(*scan) == 1)
	      {
                SET_HEADER_MARK(*scan,0) ;
		compact += HEADER_SIZE(*scan) ;
		scan += HEADER_SIZE(*scan) ;
	      }
        }
    }
  /*  printf("Compacting done, updating object\n") ; fflush(NULL) ; */
  mem_max = scan ;
  mem_update() ;
  mem_free = compact ;
}

void mem_remember_channel(obj_t channel)
{
  int i ;

  for (i = 0 ; i < num_channels && channels[i] != obj_undefined ; i++) ;

  if (i < num_channels)
    {
      channels[i] = channel ;
    }
  else
    {
      mem_grow_channels() ;
      mem_remember_channel(channel) ;
    }
}

void mem_grow_channels()
{
  int n = num_channels + num_channels/2 ;
  int j ;

  obj_t *new = (obj_t *) malloc(n * sizeof(obj_t)) ;

  for (j = 0 ; j < n ; j++)
    {
      if (j < num_channels)
	new[j] = channels[j] ;
      else
	new[j] = obj_undefined ;
    }
  free(channels) ;
  channels = new ;
  num_channels = n ;
}

void mem_close_channels()
{
  int i ;

  for (i = 0 ; i < num_channels ; i++)
    {
      obj_t c = channels[i] ;

      /* printf("analysing entry") ; write_obj(c) ; */
      if (c != obj_undefined)
	{
	  int j ;
	  int close_ok = 1 ;

	  for (j = 0 ; j < num_channels ; j++)
	    {
	      obj_t c2 = channels[j] ;

	      if (channelp(c2) &&
		  CHANNEL(c)->channel_number == CHANNEL(c2)->channel_number &&
		  HEADER_MARK(*(pointer_value(c2))) != 0)
		close_ok = 0 ;
	    }

	  if (close_ok)
	    {
	      /* printf("closing channel=%d\n", CHANNEL(c)->channel_number) ; */
	      host_close_channel(CHANNEL(c)->channel_number) ;
	    }

	  if (HEADER_MARK(*(pointer_value(c))) == 0)
	    channels[i] = obj_undefined ;
	}
      /* printf("\n") ; */
    }
}

enum {
  FIXNUM_OFFSET, REAL_OFFSET, STRING_OFFSET, CHAR_OFFSET, CHANNEL_OFFSET,
  VECTOR_OFFSET, BVEC_OFFSET, PAIR_OFFSET, DUMMY_OFFSET, DUMMY1_OFFSET, GENERIC_OFFSET,
  CLOSURE_OFFSET, SYMBOL_OFFSET, REF_OFFSET, NULL_OFFSET, BOOL_OFFSET,
  EOF_OFFSET, UNBOUND_OFFSET, UNDEFINED_OFFSET,
  TOP_OFFSET, CLASS_OFFSET, UNION_OFFSET, SINGLETON_OFFSET, SUBCLASS_OFFSET } ;

static int genericp(obj_t object)
{
  return closurep(object) &&
    (CLOSURE(object)->val[1] == VECTOR(classes)->val[DUMMY_OFFSET] ||
     CLOSURE(object)->val[1] == VECTOR(classes)->val[DUMMY1_OFFSET]) ;
}

obj_t mem_class_of(obj_t object)
{
  if (stobp(object)) return STOB(object)->class ;
  else if (fixnump(object)) return VECTOR(classes)->val[FIXNUM_OFFSET] ;
  else if (realp(object)) return VECTOR(classes)->val[REAL_OFFSET] ;
  else if (stringp(object)) return VECTOR(classes)->val[STRING_OFFSET] ;
  else if (charp(object)) return VECTOR(classes)->val[CHAR_OFFSET] ;
  else if (channelp(object)) return VECTOR(classes)->val[CHANNEL_OFFSET] ;
  else if (vectorp(object)) return VECTOR(classes)->val[VECTOR_OFFSET] ;
  else if (bvecp(object)) return VECTOR(classes)->val[BVEC_OFFSET] ;
  else if (pairp(object)) return VECTOR(classes)->val[PAIR_OFFSET] ;
  else if (genericp(object)) return VECTOR(classes)->val[GENERIC_OFFSET] ;
  else if (closurep(object)) return VECTOR(classes)->val[CLOSURE_OFFSET] ;
  else if (symbolp(object)) return VECTOR(classes)->val[SYMBOL_OFFSET] ;
  else if (refp(object)) return VECTOR(classes)->val[REF_OFFSET] ;
  else if (object == obj_nil) return VECTOR(classes)->val[NULL_OFFSET] ;
  else if (object == obj_true) return VECTOR(classes)->val[BOOL_OFFSET] ;
  else if (object == obj_false) return VECTOR(classes)->val[BOOL_OFFSET] ;
  else if (object == obj_eof) return VECTOR(classes)->val[EOF_OFFSET] ;
  else if (object == obj_unbound) return VECTOR(classes)->val[UNBOUND_OFFSET] ;
  else if (object == obj_undefined) return VECTOR(classes)->val[UNDEFINED_OFFSET] ;
  else return obj_false ;
}

void mem_set_classes(obj_t vector)
{
  classes = vector ;
}

obj_t mem_type_class_p(obj_t) ;
obj_t mem_type_union_p(obj_t) ;
obj_t mem_type_subclass_p(obj_t) ;
obj_t mem_type_singleton_p(obj_t) ;

obj_t mem_type_class_p(obj_t t)
{
  return stobp(t) && STOB(t)->class == VECTOR(classes)->val[CLASS_OFFSET] ;
}

obj_t mem_type_union_p(obj_t t)
{
  return stobp(t) && STOB(t)->class == VECTOR(classes)->val[UNION_OFFSET] ;
}

obj_t mem_type_subclass_p(obj_t t)
{
  return stobp(t) && STOB(t)->class == VECTOR(classes)->val[SUBCLASS_OFFSET] ;
}

obj_t mem_type_singleton_p(obj_t t)
{
  return stobp(t) && STOB(t)->class == VECTOR(classes)->val[SINGLETON_OFFSET] ;
}

obj_t mem_subtypep(obj_t, obj_t) ;
obj_t mem_instancep(obj_t, obj_t) ;
obj_t mem_subclass_p(obj_t, obj_t) ;

obj_t mem_any_subtype(obj_t, obj_t) ;

obj_t mem_any_subtype(obj_t t, obj_t ts)
{
  for (obj_t e = ts ; e != obj_nil ; e = PAIR(e)->cdr)
    if (mem_subtypep(t, ts)) return obj_true ;
  return obj_false ;
}

obj_t mem_subclass_p(obj_t t1, obj_t t2)
{
  if (t1 == t2) return obj_true ;
  if (t2 == VECTOR(classes)->val[TOP_OFFSET]) return obj_true ;
  for (obj_t e = STOB(t1)->slot[5]; e != obj_nil ; e = PAIR(e)->cdr)
    if (t2 == PAIR(e)->car) return obj_true ;
  return obj_false ;
}

obj_t mem_subtype_class_p(obj_t t1, obj_t t2)
{
  if (mem_type_class_p(t2)) return mem_subclass_p(t1, t2) ;
  else if (mem_type_singleton_p(t2)) return obj_false ;
  else if (mem_type_union_p(t2)) return mem_any_subtype(t1, STOB(t2)->slot[0]) ;
  else if (mem_type_subclass_p(t2))
    {
      if (t1 == VECTOR(classes)->val[CLASS_OFFSET] && STOB(t2)->slot[0] == VECTOR(classes)->val[CLASS_OFFSET])
	return obj_true ;
      else
	return obj_false ;
    }

  return obj_false ;
}

obj_t mem_subtype_singleton_p(obj_t t1, obj_t t2)
{
  if (mem_type_singleton_p(t2) && STOB(t1)->slot[0] == STOB(t2)->slot[0])
    return obj_true ;
  else if (mem_type_class_p(t2)) return mem_instancep(STOB(t1)->slot[0], t2) ;
  else if (mem_type_subclass_p(t2) &&
	   mem_instancep(STOB(t1)->slot[0], VECTOR(classes)->val[CLASS_OFFSET]) == obj_true)
    return mem_subclass_p(STOB(t1)->slot[0], STOB(t2)->slot[0]);
  else if (mem_type_union_p(t2)) return mem_any_subtype(t1, STOB(t2)->slot[0]) ;
  else return obj_false ;
}

obj_t mem_subtype_union_p(obj_t t1, obj_t t2)
{
  for (obj_t e = STOB(t1)->slot[0] ; e != obj_nil ; e = PAIR(e)->cdr)
    if (mem_subtypep(PAIR(e)->car, t2) == obj_false)
      return obj_false ;
  return obj_true ;
}

obj_t mem_subtype_subclass_p(obj_t t1, obj_t t2)
{
  if (mem_type_class_p(t2)) return mem_subclass_p(VECTOR(classes)->val[CLASS_OFFSET], t2) ;
  if (mem_type_singleton_p(t2)) return obj_false ;
  if (mem_type_subclass_p(t2)) return mem_subclass_p(STOB(t1)->slot[0], STOB(t2)->slot[0]) ;
  if (mem_type_union_p(t2)) return mem_any_subtype(t1, STOB(t2)->slot[0]) ;
  return obj_false ;
}

obj_t mem_instancep(obj_t obj, obj_t t)
{
  if (mem_type_class_p(t)) return mem_subtypep(mem_class_of(obj), t) ;
  if (mem_type_singleton_p(t) && obj == STOB(t)->slot[0]) return obj_true ;
  if (mem_type_union_p(t))
    {
      for (obj_t e = STOB(t)->slot[0] ; e != obj_nil ; e = PAIR(e)->cdr)
	if (mem_instancep(obj, PAIR(e)->car) == obj_false)
	  return obj_false ;
      return obj_true ;
    }
  if (mem_type_subclass_p(t))
    {
      if (mem_type_class_p(obj))
	return mem_subtypep(obj, STOB(t)->slot[0]) ;
      else
	return obj_false ;
    }
  return obj_false ;
}

obj_t mem_subtypep(obj_t t1, obj_t t2)
{
  if (mem_type_class_p(t1)) return mem_subtype_class_p(t1, t2) ;
  if (mem_type_singleton_p(t1)) return mem_subtype_singleton_p(t1, t2) ;
  if (mem_type_union_p(t1)) return mem_subtype_union_p(t1, t2) ;
  if (mem_type_subclass_p(t1)) return mem_subtype_subclass_p(t1, t2) ;
  return obj_false ;
}
