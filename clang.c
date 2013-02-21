#include <stdio.h>

int main (int argc, char const *argv[])
{
  int y = 4;
  int *z = &y;
  int a = y**z;
  printf("%d", a);
  return 0;
}