int main(int argc, char const** argv)
{
  switch(argc) {
    case 1:
    printf("Got no arguments\n");
    break;
    
    case 2:
    printf("Got one arguments\n");
    break;
    
    case 3:
    printf("Got two arguments\n");
    break;
    
    case 4:
    printf("Got three arguments\n");
    break;
    
    default:
    printf("Got a lot of arguments: %d\n", argc);
    break
  }
  return 0;
}