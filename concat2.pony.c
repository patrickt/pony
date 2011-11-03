int main(int argc, const char * argv[]) {
  char * first = "Hello";
  char * second = "from Pony!";
  char * third;
  unsigned int needed_size = strlen(first) + strlen(second);
  third = alloca(needed_size);
  strlcpy(third,first,needed_size);
  strlcat(third,second,needed_size);
  char * fourth;
  fourth = first <+> second;
  printf("%s\n",third);
  printf("%s\n",fourth);
  return 0;
}
