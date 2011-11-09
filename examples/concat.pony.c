#include <string.h>

int main (int argc, char const *argv[])
{
    char *first = "Hello";
    char *second = "from Pony!";
    char *third;
    third = first <+> second;
    char * fourth;
    fourth = first <+> second;
    printf("%s\n", third);
    printf("%s\n", fourth);
    return 0;
}
