#include <sys/time.h>
#include <signal.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "prof.h"
#include "proc.h"

#define DEFAULT_SAMPLING_RATE 20000
#define DEFAULT_PROFILE_ENTRY_CAPACITY 256

struct profile_entry_t {
  char *backtrace ;
  int count ;
} ;

int opt_enable_profiler = 0 ;
char *opt_profile_filename = "profile-data.log" ;
int opt_sampling_rate = DEFAULT_SAMPLING_RATE ;
volatile int do_sample_profile ;

static int profile_entry_capacity = DEFAULT_PROFILE_ENTRY_CAPACITY ;
static int profile_entry_size = 0 ;
static struct profile_entry_t *profile_entries ;

void profiler_sample(void) ;
void stop_profiling(void) ;
void handle_profiling_sig(int) ;
static void record_profile_data(char *) ;

void init_profiling()
{
  if (opt_enable_profiler)
    {
      struct sigaction sa ;
      struct itimerval timer ;

      memset(&sa, 0, sizeof(sa)) ;
      sigemptyset(&sa.sa_mask) ; 
      sa.sa_flags = 0;
      sa.sa_handler = handle_profiling_sig ;
      if (sigaction(SIGVTALRM, &sa, NULL) == -1)
        {
          printf("Error setting up profiling signal handler\n") ;
        };

      timer.it_value.tv_sec = 0 ;
      timer.it_value.tv_usec = opt_sampling_rate ; 
      timer.it_interval.tv_sec = 0 ;
      timer.it_interval.tv_usec = opt_sampling_rate ;
  
      if (setitimer(ITIMER_VIRTUAL, &timer, NULL) == -1)
        {
          printf("Error setting interval timer for profiling: errno=%d\n", errno) ;
        }

      profile_entries = malloc(sizeof(struct profile_entry_t) *
                               profile_entry_capacity) ;
      profile_entry_size = 0 ;
    }
}

void stop_profiling()
{
  if (opt_enable_profiler)
    {
      FILE *output = fopen(opt_profile_filename, "w") ;
      if (output)
        {
          for (int i = 0 ; i < profile_entry_size ; i++)
            {
              fprintf(output, "%s %d\n",
                     profile_entries[i].backtrace,
                     profile_entries[i].count) ;
            }
        }
      fflush(output) ;
      fclose(output) ;
    }
}

void handle_profiling_sig(int signum)
{
  do_sample_profile = 1 ;
}

void profiler_sample()
{
  obj_t frame ;
  obj_t templ ;
  obj_t debug ;
  obj_t name ;

  char sample[1024];
  sample[1023] = '\0' ;
  
  int start = 1023 ;
  
  for (frame = cont ; vectorp(frame) ; frame = VECTOR(frame)->val[0])
    {
      templ = VECTOR(frame)->val[2] ;
      debug = VECTOR(templ)->val[2] ;
      name = VECTOR(debug)->val[0] ;
      
      if (stringp(name))
        {
          sample[start-1] = ';' ;
          start-- ;

          int len = strlen(&STRING(name)->ch[0]) ;
          start -= len ;

          if (start >= 0)
            {
              char *s = &sample[start] ;
              char *ch = &STRING(name)->ch[0] ;
              while(*ch != '\0')
                {
                  *s = *ch++ ;
                  if (*s == ' ')
                    *s++ = '.' ;
                  else
                    s++;
                }
            }
        }
    }
  if (start >= 0)
    record_profile_data(&sample[start]) ;

  do_sample_profile = 0 ;
}

static
void record_profile_data(char *line)
{
  int i = 0 ;

  for (i = 0 ; i < profile_entry_size; i++)
    if (strcmp(profile_entries[i].backtrace, line) == 0)
      break ;

  if (i == profile_entry_size)
    {
      /* if our table is full, realloc it */
      if (i == profile_entry_capacity)
        {
          struct profile_entry_t *new_entries =
            malloc(sizeof(struct profile_entry_t) * 2 *
                   profile_entry_capacity) ;
          int i = 0 ;
          for (i = 0 ; i < profile_entry_capacity ; i++)
            {
              new_entries[i].backtrace = profile_entries[i].backtrace ;
              new_entries[i].count = profile_entries[i].count ;
            }
          free(profile_entries) ;
          profile_entry_capacity *= 2 ;
          profile_entries = new_entries ;
        }

      struct profile_entry_t *entry = &profile_entries[i] ;
      
      entry->count = 1 ;
      entry->backtrace = malloc(strlen(line)+1) ;
      strcpy(entry->backtrace, line) ;
      profile_entry_size++ ;
    }
  else
    {
      profile_entries[i].count++ ;
    }
}
