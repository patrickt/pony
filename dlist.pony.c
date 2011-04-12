#include <stdio.h>

void *malloc(size_t size);
void *calloc(size_t size, size_t count);
void *memset(void *b, int c, size_t len);
void free(void* ptr);

typedef enum {
	LIST, VALUE
} tag_t;

union ldata {
	void *value;
	struct list_s *list;
};

typedef struct list_s {
	struct list_s *next;
	tag_t tag;
	union ldata c;
} list_t;

list_t *nil = NULL;

list_t *cons(void *val, list_t *existing) {
	list_t *new = calloc(sizeof(list_t), 1);
	new->next = existing;
	new->tag = VALUE;
	new->c.value = val;
	return new;
}

tag_t first(list_t *list, union ldata *data) {
	data->value = list->c.value;
	return list->tag;
}

list_t *rest(list_t *list) {
	return list->next;
}

void del(list_t *list) {
	if (list != nil) {
		list_t *next = rest(list);
		free(list);
		del(next);
	}
}

void print(list_t *list) {
	printf("%s ", (char*)list->c.value);
	if (rest(list) != nil) {
		print(rest(list));
	} else {
		printf("\n");
	}
}

void set_rest(list_t *list) {
	list_t *new = cons((void*)"Natalie: 555-5000", cons("Krista: 555-6000", nil));
	list->next = new;
}

#include "gc.h"

int main (int argc, char const *argv[])
{
	list_t *l = cons("Joalton: 555-1000", nil);
	list_t *l2 = cons("Jeremiah: 555-2000", cons("Alex: 555-3000", cons("Nate: 555-4000", l)));
	print(l2);
	set_rest(l2);
	print(l2);
	return 0;
}