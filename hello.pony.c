int main(int argc, char const** argv)
{
  char *buffer = malloc(sizeof(char) * 4096);
  printf("Successfully allocated memory\n");
  free(buffer);
  if (buffer = malloc(sizeof(char) * 4096)) {
    printf("I am sure about this memory\n");
    free(buffer);
  }
  int a = 7 >>> 3;
  return 0;
}