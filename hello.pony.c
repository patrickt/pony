#include <stdio.h>

int main(int argc, char const** argv)
{
	puts("Hello, world!");
	
	void *buffer;
	buffer = malloc(255);
	free(buffer);
	
	int logicalshift = 500 >>> 23;
	
	char *something = "foo" <***> "bar";
	
	return 0;
}