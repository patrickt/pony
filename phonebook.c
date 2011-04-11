#include <stdio.h>
#include "dlist.h"

list_t *create() {
	list_t *alex = cons((void *)"Alex:555-1000", nil);
	list_t *jeremiah = cons((void *)"Jeremiah:555-1000", alex);
	list_t *joalton = cons((void *)"Joalton:555-1200", jeremiah);
	return joalton;
}

