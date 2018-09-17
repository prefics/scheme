#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#include "image.h"

#define FASL_INTEGER         0
#define FASL_TRUE            1
#define FASL_FALSE           2
#define FASL_CHAR            3
#define FASL_PAIR            4
#define FASL_STRING          5
#define FASL_VECTOR          6
#define FASL_BVEC            7
#define FASL_NIL             8
#define FASL_UNSPECIFIC      9
#define FASL_UNBOUND         10
#define FASL_SYMBOL          11
#define FASL_REF             12
#define FASL_EOF             14
#define FASL_REAL            15
#define FASL_NEGATIVE_FIXNUM 16

#define TYPE_DEFINITION     0
#define TYPE_SYNTAX         1
#define TYPE_EXPR           2

obj_t load_image(char *filename)
{
  host_channel_t f ;
  int rc ;

  rc = host_open_input_channel(filename, &f) ;

  if (rc == 0)
    {
      obj_t restart ;
      obj_t *start ;
      image_t image ;

      rc = host_read_channel(f, &image, 0, sizeof(image_t)) ;
      
      if (rc != sizeof(image_t))
	error("Couldn't read IMAGE header") ;

      if (image.size > mem_heap_size)
	error("Image file too large, specify a bigger heap with -h options") ;

      start = image.start ;
      mem_free = image.free ;
      restart = image.restart ;
      symbol_table = image.symbol_table ;

      rc = host_read_channel(f, mem_start, 0, image.size * sizeof(obj_t)) ;
      
      if (rc != image.size * sizeof(obj_t))
	error("Couldn't read IMGAE data") ;

      *(mem_start+image.size) = make_header(mem_end-mem_start+image.size, header_free) ;
      if (mem_start-start != 0)
        mem_adjust(sizeof(obj_t)*(mem_start-start)) ;

      if (tag(symbol_table) == tag_pointer)
        symbol_table += sizeof(obj_t)*(mem_start-start) ;
      if(tag(restart) == tag_pointer) 
        restart += sizeof(obj_t)*(mem_start-start) ;

      mem_free += (mem_start-start) ;

      host_close_channel(f) ;

      /*      printf("delta = %d", mem_start-start) 
      printf("image.size=%d", image.size) ;
      printf("template=%lx", CLOSURE(restart)->val[1]) ;
      printf("offset=%d", CLOSURE(restart)->val[1]-(obj_t) mem_start) ;
      printf("mem_free = %d", mem_free-mem_start) ;
      fflush(NULL);
      write_obj(CLOSURE(restart)->val[1]) ;
      */
      return restart ;
    }
  else
    {
      error("file error in LOAD-IMAGE") ;
      return obj_nil ;
    }
}

void save_image(obj_t filename, obj_t restart)
{
  host_channel_t f ;
  int rc ;

  rc = host_open_output_channel((char *)&STRING(filename)->ch[0], &f, 1) ;
  if (rc == 0)
    {
      image_t image ;

      strncpy(image.exec, "#!", 256) ;
      strncat(image.exec, SCM_BINARY, 256) ;
      strncat(image.exec, "  --image\n", 256) ;
      image.size    = (mem_free-mem_start) ;
      image.start   = mem_start ;
      image.free    = mem_free ;
      image.restart = restart ;
      image.symbol_table = symbol_table ;
          
      rc = host_write_channel(f, &image, 0, sizeof(image_t)) ;
      if (rc != sizeof(image_t))
	error("Error writing IMAGE header") ;
      rc = host_write_channel(f, mem_start, 0, (mem_free-mem_start) * sizeof(obj_t)) ;
      if (rc != (mem_free-mem_start) * sizeof(obj_t))
	error("Error writing IMAGE") ;

      /*
      printf("offset=%d", CLOSURE(restart)->val[1]-(obj_t) mem_start) ;
      printf("mem_free = %d", mem_free-mem_start) ;
      write_obj(CLOSURE(restart)->val[1]) ;
      */
      host_close_channel(f) ;
#ifdef SCM_TARGET_POSIX
      chmod((char *)STRING(filename)->ch, 
	    (S_IRUSR | S_IWUSR | S_IXUSR) |
	    (S_IRGRP | S_IWGRP | S_IXGRP) | (S_IROTH | S_IXOTH)) ;
#endif
    }
  else 
    error("Cannot open file for saving image") ;
}

static
unsigned char read_byte(host_channel_t file)
{
  unsigned char byte ;
  int rc ;

  rc = host_read_channel(file, &byte, 0, 1) ;
 
  if (rc != 1)
    error ("ERROR/END OF FILE when reading a FASL !?!") ;

  return byte ;
}

static 
long read_long(host_channel_t fasl)
{
  unsigned int c1 = read_byte(fasl) ;
  unsigned int c2 = read_byte(fasl) ;
  unsigned int c3 = read_byte(fasl) ;
  unsigned int c4 = read_byte(fasl) ;

  long v = (c1 << 24) | (c2 << 16) | (c3 << 8) | c4 ;
  return v ;
}

obj_t refs = obj_nil ;

void read_definition(host_channel_t fasl, obj_t *val) 
{
  unsigned char ch = read_byte(fasl) ;
  obj_t v ;

  switch(ch)
    {
    case TYPE_DEFINITION:
    case TYPE_EXPR:
      /*      v = write_obj(read_fasl(fasl)) ; */
      v = read_fasl(fasl) ;
      break ;
    case TYPE_SYNTAX:
      error("SYNTAX is not supported in internal FASL reader") ;
    default:
      error("Unknown definition type in READ_DEFINITION") ;
    }
  *val = v ;
}

extern obj_t val ;

obj_t read_fasl(host_channel_t fasl)
{
  unsigned char ch=read_byte(fasl) ;

  switch(ch)
    {
    case FASL_INTEGER:
      return make_fixnum(read_long(fasl)) ;
    case FASL_NEGATIVE_FIXNUM:
      return make_fixnum(-read_long(fasl)) ;
    case FASL_TRUE:
      return obj_true ;
    case FASL_FALSE:
      return obj_false ;
    case FASL_CHAR:
      return make_char(read_byte(fasl)) ;
    case FASL_PAIR:
      {
	obj_t pair = make_pair() ;
	PAIR(pair)->car = read_fasl(fasl) ;
	PAIR(pair)->cdr = read_fasl(fasl) ;

	return pair ;
      }
    case FASL_STRING:
      { 
        int l = read_long(fasl) ;
        obj_t str = make_string(l+1) ;
        int i ;
        for ( i=0 ; i < l ; i++)
          STRING(str)->ch[i] = read_byte(fasl) ;
	STRING(str)->ch[l] = '\0' ;
        return str ;
      }
    case FASL_VECTOR:
      { 
        int l = read_long(fasl) ;
        obj_t vec = make_vector(l) ;
        int i ;
        for ( i=0 ; i < l ; i++)
          VECTOR(vec)->val[i] = read_fasl(fasl) ;
        return vec ;
      }
    case FASL_SYMBOL:
      { 
        int l = read_long(fasl) ;
        obj_t sym = make_symbol(l+1) ;
        int i ;
        for ( i=0 ; i < l ; i++)
          SYMBOL(sym)->ch[i] = read_byte(fasl) ;
	SYMBOL(sym)->ch[l] = '\0' ;
	val = sym ;
        return intern() ;
      }
    case FASL_BVEC:
      { 
        int l = read_long(fasl) ;
        obj_t bvec = make_bvec(l) ;
        int i ;
        for ( i=0 ; i < l ; i++)
          BVEC(bvec)->val[i] = read_byte(fasl) ;
        return bvec ;
      }
    case FASL_NIL:
      return obj_nil ;
    case FASL_UNSPECIFIC:
      return obj_undefined ;
    case FASL_UNBOUND:
      return obj_unbound ;
    case FASL_EOF:
      return obj_eof ;
    case FASL_REF:
      {
        int l = read_long(fasl) ;
        obj_t sym = make_symbol(l+1) ;
	obj_t ref ;
	int i ;
        for ( i=0 ; i < l ; i++)
          SYMBOL(sym)->ch[i] = read_byte(fasl) ;
	SYMBOL(sym)->ch[l] = '\0' ;
	l = read_long(fasl) ;
	for (i=0 ; i < l ; i++) read_byte(fasl) ;
	val = sym ;
	sym = intern() ;
	for (ref = refs ; ref != obj_nil && REF(PAIR(ref)->car)->name != sym ; ref = PAIR(ref)->cdr) ;
	if (ref == obj_nil)
	  {
	    obj_t r = make_pair() ;

	    ref = make_ref() ;
	    REF(ref)->name = sym ;
	    REF(ref)->value = obj_undefined ;
	    REF(ref)->module = obj_undefined ;
	    PAIR(r)->car = ref ;
	    PAIR(r)->cdr = refs ;
	    refs = r ;
	  }
	else
	  ref = PAIR(ref)->car ;
	return ref ;
      }
    case FASL_REAL:
      {
        obj_t real = make_real(1.0) ;
        double d ;
        char *dp = (char *) &d ;
        int i ;

        for (i = 0 ; i < sizeof(double) ; i++)
          *dp++ = read_byte(fasl) ;

        REAL(real)->value = d ;
        return real ;
      }
    default:
      printf("FASL header %d\n", ch) ;
      error("unknown FASL kind") ;
      return obj_nil ;
    }
}
