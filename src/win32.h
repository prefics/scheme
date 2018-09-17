#ifndef HOST_WIN32_H
#define HOST_WIN32_H

typedef HANDLE host_channel_t ;

obj_t host_init(int argc, int i, char *argv[]) ;
int host_make_channel(int num, host_channel_t *handle) ;
int host_open_input_channel(char *filename, host_channel_t *hFile) ;
int host_open_output_channel(char *filename, host_channel_t *hFile) ;
int host_read_channel(host_channel_t hFile, char *ch, int *eofp) ;
int host_write_channel(host_channel_t hFile, char *buff, int length) ;
int host_close_channel(host_channel_t hFile) ;

#endif
