// If I could do this in pure Haskell, I would. But even after searching the GHC API, I couldn't 
// find any way to do it. Pointers to a non-hacky solution are welcome.

#include <stdio.h>
#include <stdlib.h>

int main (int argc, char const *argv[])
{
	FILE *hask = fopen(argv[1], "w");
	if (!hask) {
		perror("Error encountered while determining machine-specific sizes");
		return EXIT_FAILURE;
	}
	
	fprintf(hask, "-- The following file is automatically generated. Do not edit it.\n\n");
	
	fprintf(hask, "module Language.Pony.MachineSizes where\n");
	
	fprintf(hask, "sizeOfChar :: Int\n");
	fprintf(hask, "sizeOfChar = %lu\n\n", sizeof(signed char) * 8);
	
	fprintf(hask, "sizeOfShort :: Int\n");
	fprintf(hask, "sizeOfShort = %lu\n\n", sizeof(short int) * 8);
	
	fprintf(hask, "sizeOfInt :: Int\n");
	fprintf(hask, "sizeOfInt = %lu\n\n", sizeof(int) * 8);
	
	fprintf(hask, "sizeOfLong :: Int\n");
	fprintf(hask, "sizeOfLong = %lu\n\n", sizeof(long int) * 8);
	
	fprintf(hask, "sizeOfLongLong :: Int\n");
	fprintf(hask, "sizeOfLongLong = %lu\n\n", sizeof(long long int) * 8);
	
	fprintf(hask, "sizeOfFloat :: Int\n");
	fprintf(hask, "sizeOfFloat = %lu\n\n", sizeof(float) * 8);
	
	fprintf(hask, "sizeOfDouble :: Int\n");
	fprintf(hask, "sizeOfDouble = %lu\n\n", sizeof(double) * 8);
	
	fprintf(hask, "sizeOfLongDouble :: Int\n");
	fprintf(hask, "sizeOfLongDouble = %lu\n\n", sizeof(long double) * 8);
	
	return 0;
}