#include <stdint.h>
#include <stdio.h>

int main() {
  uint64_t a = 0;
  uint64_t b = 0;
  a = 9663;
  while (a != 1) {
    b = b + 1;
    if (a % 2 == 0)
      a = a / 2;
    else
      a = 3 * a + 1;
  }
  printf("%llu %llu\n", a, b);
}
