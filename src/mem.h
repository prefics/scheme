/* mem.h
 * 
 * support for the following objects:
 * - fixnum
 * - boolean
 * - char
 * - pairs
 * - strings
 * - vectors
 * - bvectors
 * - closure
 * - nil, undefined, unbound, eof
 * - stobs
 * - channels
 * - refs
 */

#ifndef MEM_H
#define MEM_H

#include <stdio.h>
#include <stdint.h>
#include "unix.h"

typedef intptr_t obj_t, *obj_p ;

/* Object Format 
   xxxxxxx00 - fixnum
   xxxxxxx01 - immediate
   xxxxxxx02 - pointer
   xxxxxxx03 - header
*/

#define SIZE(o)         (((o)+sizeof(obj_t)-1) / sizeof(obj_t))
#define tag(o)          ((o) & 0x03)
#define tag_fixnum      0x00
#define tag_immediate   0x01
#define tag_pointer     0x02
#define tag_header      0x03

#define make_fixnum(i) ((i) * 4)
#define fixnum_value(f) ((f) / 4)
#define fixnump(f) (tag(f) == tag_fixnum)

#define make_char(c) ((((uintptr_t) c & 0xff) << 6) | 0x01)
#define char_value(c) ((c) >> 6)
#define charp(c) (((c) & 0x3f) == tag_immediate)

#define obj_nil         0x05          /* 0b000101 */
#define obj_undefined   0x09          /* 0b001001 */
#define obj_unbound     0x0d          /* 0b001101 */
#define obj_eof         0x11          /* 0b010001 */
#define obj_true        0x15          /* 0b010101 */
#define obj_false       0x19          /* 0b011001 */
#define obj_key         0x1d          /* 0b011101 */
#define obj_rest        0x21          /* 0b100001 */
#define obj_opt         0x25          /* 0b100101 */

#define make_pointer(o) ((obj_t) (((obj_t) (o))+tag_pointer))
#define pointer_value(o) ((obj_t *) ((o)-tag_pointer))

#define header_pair      0
#define header_string    1
#define header_vector    2
#define header_closure   3
#define header_stob      4
#define header_bvec      5
#define header_channel   6
#define header_symbol    7
#define header_keyword   8
#define header_ref       9
#define header_real      10
#define header_free      15

typedef obj_t header_t;

#define make_header(s, t) ((((s) << 7) | (t) << 3) | tag_header)
#define HEADER_MARK(h) (((h) >> 2) & 0x01)
#define HEADER_TYPE(h) (((h) >> 3) & 0x0f)
#define HEADER_SIZE(h) (((h) >> 7))
#define SET_HEADER_MARK(h,m) ((h) = (((h) & 0xfffffffb) | ((m) << 2) | tag_header))
#define SET_HEADER_TYPE(h,t) ((h) = (((h) & 0xffffff87) | ((t) << 3) | tag_header))
#define SET_HEADER_SIZE(h,s) ((h) = (((h) & 0x0000007f) | ((s) << 7) | tag_header))

#define HEADER(o) ((header_t *) (pointer_value(o)))
#define HEADER_(o) ((header_t *) (o))

typedef struct {
  header_t header ;
  obj_t car ;
  obj_t cdr ;
} pair_t ;
#define PAIR(p) ((pair_t *) (pointer_value(p)))
#define PAIR_(p) ((pair_t *) (p))

extern obj_t make_pair(void) ;
extern obj_t make_list(int size) ;
extern obj_t pairp(obj_t obj) ;

typedef struct {
  header_t header ;
  int len ;
  char ch[0] ;
} string_t ;
#define STRING(o) ((string_t *) (pointer_value(o)))
#define STRING_(o) ((string_t *) (o))

extern obj_t make_string(int size) ;
extern obj_t stringp(obj_t obj) ;

typedef struct {
  header_t header ;
  obj_t val[0] ;
} vector_t ;
#define VECTOR(o) ((vector_t *) (pointer_value(o)))
#define VECTOR_(o) ((vector_t *) (o))

extern obj_t make_vector(int size) ;
extern obj_t vectorp(obj_t obj) ;

typedef struct {
  header_t header ;
  int size ;
  unsigned char val[0] ;
} bvec_t ;
#define BVEC(o) ((bvec_t *) (pointer_value(o)))
#define BVEC_(o) ((bvec_t *) (o))
#define ASLONG(v) (*(obj_t *)(v))

extern obj_t make_bvec(int size) ;
extern obj_t bvecp(obj_t obj) ;

typedef struct {
  header_t header ;
  int size ;
  obj_t val[0] ;
} closure_t ;
#define CLOSURE(o) ((closure_t *) (pointer_value(o)))
#define CLOSURE_(o) ((closure_t *) (o))

extern obj_t make_closure(int size) ;
extern obj_t closurep(obj_t obj) ;

typedef struct {
  header_t header ;
  obj_t class ;
  obj_t slot[0] ;
} stob_t ;
#define STOB(o) ((stob_t *) (pointer_value(o)))
#define STOB_(o) ((stob_t *) (o))

extern obj_t make_stob(int size) ;
extern obj_t stobp(obj_t obj) ;

typedef struct {
  header_t header ;
  host_channel_t channel_number ;
  obj_t buffer ;
  obj_t available ;
  obj_t index ;
} channel_t ;
#define CHANNEL(o) ((channel_t *) (pointer_value(o)))
#define CHANNEL_(o) ((channel_t *) (o))

#define CHANNEL_BUFFER_SIZE 4096

extern obj_t make_channel(host_channel_t channel) ;
extern obj_t channelp(obj_t obj) ;

typedef struct {
  header_t header ;
  unsigned char ch[0] ;
} symbol_t ;
#define SYMBOL(o) ((symbol_t *) (pointer_value(o)))
#define SYMBOL_(o) ((symbol_t *) (o))

typedef struct {
  header_t header ;
  unsigned char ch[0] ;
} keyword_t ;
#define KEYWORD(o) ((keyword_t *) (pointer_value(o)))
#define KEYWORD_(o) ((keyword_t *) (o))

typedef struct {
  header_t header ;
  obj_t    name ;
  obj_t    module ;
  obj_t    value ;
} ref_t ;
#define REF(o) ((ref_t *) (pointer_value(o)))
#define REF_(o) ((ref_t *) (o))

extern obj_t refp(obj_t obj) ;
extern obj_t make_ref(void) ;

typedef struct {
  header_t header ;
  double   value ;
} real_t ;
#define REAL(o) ((real_t *) (pointer_value(o)))
#define REAL_(o) ((real_t *) (o))

extern obj_t realp(obj_t obj) ;
extern obj_t make_real(double value) ;

#define DEFAULT_HEAP_SIZE   5000000

extern obj_t intern(void) ;
extern obj_t symbolp(obj_t obj) ;
extern obj_t make_symbol(int size) ;

extern obj_t symbol_table ; /* ROOT */
extern obj_t classes ; /* ROOT */

extern obj_t write_obj(obj_t obj) ;

extern obj_t mem_class_of(obj_t) ;
extern void mem_set_classes(obj_t) ;
extern obj_t mem_instancep(obj_t, obj_t) ;
extern obj_t mem_subtypep(obj_t, obj_t) ;

extern obj_t *mem_start ;
extern obj_t *mem_end ;
extern obj_t *mem_free ;
extern int    mem_heap_size ;

extern void mem_init(int) ;
extern void mem_gc(void) ;
extern void mem_adjust(long) ;
extern void mem_remember_channel(obj_t) ;

#endif

