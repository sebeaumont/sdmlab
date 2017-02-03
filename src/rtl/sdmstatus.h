#pragma once

typedef enum {
  AOK = 0,
  ANEW = (1),
  AOLD = (1<<1),
  // errors
  ESPACE = (-1<<1),
  ESYMBOL = (-1<<2),
  EMEMORY = (-1<<4),
  ERUNTIME = (-1<<5)
} status_t;  

inline const bool sdm_error(status_t s) { return (s<0); }
