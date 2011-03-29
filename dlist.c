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
	list_t *new = malloc(sizeof(list_t));
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
	if (list_t != nil) {
		list_t *next = rest(list);
		free(list);
		del(next);
	}
}

int main (int argc, char const *argv[])
{
	list_t *l = cons((void*)1, nil);
	list_t *l2 = cons((void*)2, l2);
	return 0;
}