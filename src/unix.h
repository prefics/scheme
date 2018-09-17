#ifndef UNIX_H
#define UNIX_H

typedef int host_channel_t ;

extern void host_init(int argc, int i, char *argv[]) ;
extern int host_make_channel(int num, host_channel_t *channel) ;

extern int host_open_input_channel(char *filename, host_channel_t *channel) ;
extern int host_open_output_channel(char *filename, host_channel_t *channel, int mode) ;

extern int host_read_channel(host_channel_t channel, void *buff, int start, int end) ;
extern int host_write_channel(host_channel_t channel, void *buff, int start, int end) ;

extern int host_close_channel(host_channel_t channel) ;

extern int host_same_error(int error, int test_error) ;

extern void error(char *msg) ;
#endif
