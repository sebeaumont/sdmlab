#include <stdio.h>
#include <string.h>
#include "api.h"


api_t myfunction(int a, int b) {
  api_t ret;
  ret.type = Int;
  ret.value.int_v = a + b;
  return ret;
}


api_t badfunction(int a, int b) {
  api_t ret;
  ret.type = Fail;
  ret.value.errno_v = errno;
  return ret;
}


int main(int argc, char** argv) {

  api_t f = myfunction(10, 20);

  if (f.type == Fail)
    printf("OOPS: %s\n", strerror(f.value.errno_v));
  else
    printf("OK: %d\n", f.value.int_v);

}
