#ifndef PROFILER_H
#define PROFILER_H

extern int opt_enable_profiler ;
extern char *opt_profile_filename ;
extern int opt_sampling_rate ;
extern volatile int do_sample_profile ;

extern void init_profiling(void) ;
extern void stop_profiling(void) ;
extern void profiler_sample(void) ;
#endif
