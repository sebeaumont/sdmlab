#include <mach/mach_time.h>

double conversion_factor;

static inline void timer_init() {
  mach_timebase_info_data_t timebase;
  mach_timebase_info(&timebase);
  conversion_factor = (double)timebase.numer / (double)timebase.denom;
}

static inline uint64_t absolute_time() {
  return mach_absolute_time();
}


/*

  uint64_t t1, t2;

  Init();

  t1 = mach_absolute_time();
  // code here
  t2 = mach_absolute_time();

  double duration_ns = (double)(t2 - t1) * conversion_factor;
*/
