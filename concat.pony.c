int main (int argc, char const *argv[])
{
    char *first = "Hello";
    char *second = "from Pony!";
    char *third;
    third = first <+> second;
    printf(third);
    return 0;
}