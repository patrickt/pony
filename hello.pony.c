#include <stdio.h>

int main(int argc, char const** argv)
{
  char *buffer = malloc(sizeof(char) * 4096);
  printf("Successfully allocated memory\n");
  return 0;
}