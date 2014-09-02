/* cbits
$ gcc -fPIC -shared cbits.c -o cbits.so
$ clang -fPIC -shared cbits.c -o cbits.so
*/

#include "stdio.h"

int arrowPrint(int x) {
  putchar((char)x);
  fflush(stdout);
  return 0;
}

int test() {
  return 5;
}
