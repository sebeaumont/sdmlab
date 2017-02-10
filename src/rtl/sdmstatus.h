#pragma once

typedef enum {
  
  // sucess sometimes with info 

  AOK = 0,
  ANEW = 1,
  AOLD = 2,

  // errors
  
  ESPACE = -2,
  ESYMBOL = -4,
  EMEMORY = -8,
  ERUNTIME = -16, 
  EUNIMPLEMENTED = -32

} status_t;  

// check what c-language will accept here is bool ansi c?
inline int sdm_error(status_t s) { return (s<0); }
