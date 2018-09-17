#include <string.h>
#include <stdlib.h>

#include "mem.h"
#include "proc.h"
#ifdef SCM_TARGET_POSIX
#include "unix.h"
#endif
#ifdef SCM_TARGET_WIN32
#include <windows.h>
#include "win32.h"
#endif
#include "image.h"

static
void usage(void)
{
  printf("schemy \n"
	 "[--image IMAGE_FILENAME] specify the filename of image to use\n"
	 "[--fasl FASL_FILENAME] specify a fasl file to run\n"
	 "[--heap-size SIZE] specify the heap size\n"
	 
	 "[--help]\n") ;
  exit(0) ;
}

static 
int strprefix(char *s, char *p)
{
  int i ;

  for (i = 0 ; *(s+i) != '\0' && *(p+i) != '\0' && *(s+i) == *(p+i); i++) ;
  if (*(p+i) == '\0')
    return 0 ;
  else
    return 1 ;
}

int main(int argc, char *argv[])
{
  obj_t template ;
  obj_t args, ret ;
  int i ;
  int heap_size = DEFAULT_HEAP_SIZE ;
  char *image_file = NULL ;
  char *fasl_file = NULL ;
  
  for (i=1 ; i < argc ; i++)
    {
      if (strcmp(argv[i], "--image") == 0)
	{
	  image_file = argv[i+1] ;
	  i += 1 ;
	}
      else if (strcmp(argv[i], "--fasl") == 0)
	{
	  fasl_file = argv[i+1] ;
	  i += 1 ;
	}
      else if (strcmp(argv[i], "--heap-size") == 0)
	{
	  heap_size = atoi(argv[i+1]) ;
          printf("Setting heap size to %d", heap_size) ;
	  i += 1 ;
	}
      else if (strcmp(argv[i], "--help") == 0)
	usage() ;
      else if (strprefix(argv[i], "--image=") == 0)
	{
	  image_file = (argv[i])+8 ;
	}
      else if (strprefix(argv[i], "--fasl=") == 0)
	{
	  fasl_file = (argv[i])+7 ;
	}
      else if (strprefix(argv[i], "--heap-size=") == 0)
	{
	  heap_size = atoi((argv[i])+12) ;
	}
      else
        break ; 
    }

  mem_init(heap_size) ;
  
  if (image_file)
    {
      obj_t clos = load_image(image_file) ;

      template = CLOSURE(clos)->val[1] ;
    }
  else if (fasl_file)
    {
      host_channel_t fasl ;
      int rc ;

      rc = host_open_input_channel(fasl_file, &fasl) ;

      if (rc != 0)
	{
	  printf("Fasl file '%s' does not exists or is not readable\n",
		 fasl_file) ;
	  exit(1) ;
	}
      read_definition(fasl, &template) ;
      host_close_channel(fasl) ;
    }
  else
    {
      printf("You need to specify at least a FASL file to load or an image file to resume\n") ;
      exit(1) ;
    }

  host_init(argc, i, argv) ;
  args = obj_nil ;
  for (i = i ; i < argc ; i++)
    {
      obj_t arg = make_string(strlen(argv[i]) +1) ;
      obj_t pair ;
      
      strcpy(&STRING(arg)->ch[0], argv[i]) ;
      pair = make_pair() ;
      PAIR(pair)->car = arg ;
      PAIR(pair)->cdr = args ;
      args = pair ;
    }
  ret = run(template, args) ;
  /*  write_obj(ret) ; 
   *  printf("\n") ;
   */
  exit(0) ;
}
