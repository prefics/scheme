#include <windows.h>
#include <winsock2.h>
#include "gc.h"

void host_init(int argc, int start, char *argv[])
{
  int i ;
  obj_t args ;

  WORD wVersionRequested;
  WSADATA wsaData;
  int err;
 
  wVersionRequested = MAKEWORD( 2, 2 );
  
  err = WSAStartup( wVersionRequested, &wsaData );
  if ( err != 0 ) {
    /* Tell the user that we could not find a usable */
    /* WinSock DLL.                                  */
    exit(-1);
  }
}

void error(char *msg) ;
void mem_adjust(int offset) ;

obj_t load_image(char *filename)
{
  HANDLE f ;

  f = CreateFile(filename, GENERIC_READ, 0, NULL,
		 OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL) ;
  if (f != INVALID_HANDLE_VALUE)
    {
      obj_t restart ;
      obj_t *start ;
      image_t image ;
      DWORD r ;
  
      ReadFile(f, &image, sizeof(image_t), &r, NULL) ;
      if (image.size > mem_heap_size)
	{
	  printf("Size of image is %d when heap is %d",
		 image.size, mem_heap_size) ;
	  error("Image file too large, specify a bigger heap with -h options") ;
	}

      start = image.start ;
      mem_free = image.free ;
      restart = image.restart ;
      symbol_table = image.symbol_table ;
      keyword_table = image.keyword_table ;

      ReadFile(f, mem_start, sizeof(obj_t)*image.size, &r, NULL) ;
      if (mem_start-start != 0)
        mem_adjust(4*(mem_start-start)) ;

      if (tag(symbol_table) == tag_pointer)
        symbol_table += 4*(mem_start-start) ;
      if (tag(keyword_table) == tag_pointer)
        keyword_table += 4*(mem_start-start) ;
      if(tag(restart) == tag_pointer) 
        restart += 4*(mem_start-start) ;
      mem_free += 4*(mem_start-start) ;

      *mem_free = make_header(mem_end-mem_free, header_free) ;
      CloseHandle(f) ;
      return restart ;
    }
  else
    error("file error in LOAD-IMAGE") ;
  return obj_undefined ;
}

/* CHANNELS ======================================================*/

int host_make_channel(int num, HANDLE *hChannel)
{
  switch(num)
    {
    case 0:
      *hChannel = GetStdHandle(STD_INPUT_HANDLE) ;
      break ;
    case 1:
      *hChannel = GetStdHandle(STD_OUTPUT_HANDLE) ;
      break ;
    case 2:
      *hChannel = GetStdHandle(STD_ERROR_HANDLE) ;
      break ;
    default:
      *hChannel = GetStdHandle(STD_OUTPUT_HANDLE) ;
      break ;
    }

  if (*hChannel != INVALID_HANDLE_VALUE)
    return 0 ;
  else
    return GetLastError() ;
}

int host_open_input_channel(char *filename, HANDLE *hFile)
{
  *hFile = CreateFile(filename, GENERIC_READ, 0, NULL, OPEN_ALWAYS,
		      FILE_ATTRIBUTE_NORMAL, NULL) ;
  if (*hFile != INVALID_HANDLE_VALUE)
    return 0 ;
  else
    return GetLastError() ;
}

int host_open_output_channel(char *filename, HANDLE *hFile) 
{
  *hFile = CreateFile(filename, GENERIC_WRITE, 0, NULL, OPEN_ALWAYS,
		     FILE_ATTRIBUTE_NORMAL, NULL) ;
  if (*hFile != INVALID_HANDLE_VALUE)
    return 0 ;
  else
    return GetLastError() ;
}

int host_read_channel(HANDLE hFile, char *ch, int *eofp)
{
  DWORD bytesRead ;
  BOOL res ;

  res = ReadFile(hFile, ch, 1, &bytesRead, NULL) ;
  if (res)
    {
      if (bytesRead == 1)
	{
	  *eofp = 0 ;
	  return 0 ;
	}
      else if (bytesRead == 0)
	{
	  *eofp = 1 ;
	  return 0 ;
	}
      else return 0 ;
    }
  else 
    return GetLastError() ;
}

int host_write_channel(HANDLE hFile, char *buff, int length)
{
  DWORD bytesWritten = 0 ;
  BOOL res = TRUE ;

  for (bytesWritten = 0 ; bytesWritten < length ;)
    {
      res = WriteFile(hFile, buff+bytesWritten, 
		      length-bytesWritten, &bytesWritten, NULL) ;
      if (!res)
	break ;
    }
  if (res)
    return 0 ;
  else
    return GetLastError() ;
}

int host_close_channel(HANDLE hFile)
{
  BOOL res ;

  res = CloseHandle(hFile) ;
  if (res)
    return 0 ;
  else
    return GetLastError() ;
}
