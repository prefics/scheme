#ifndef IMAGE_H
#define IMAGE_H

#include "mem.h"
#ifdef SCM_TARGET_WIN32
#include "win32.h"
#endif
#ifdef SCM_TARGET_POSIX
#include "unix.h"
#endif

typedef struct {
  char   exec[256] ;
  int    size ;
  obj_t *start ;
  obj_t *free ;
  obj_t  restart ;
  obj_t  symbol_table ;
  obj_t  classes ; /* No longer used */
} image_t ;

extern void read_definition(host_channel_t fasl, obj_t *result) ;
extern obj_t load_image(char *filename) ;
extern obj_t read_fasl(host_channel_t f) ;
extern void save_image(obj_t filename, obj_t restart) ;
#endif
