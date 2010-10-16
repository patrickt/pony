int puts(const char *);
void* malloc(void *, unsigned int);

int main(int argc, char const** argv)
{
	puts("Hello, world!");
	void *buffer;
	buffer = malloc(sizeof(int) * 10);
	return 0;
}