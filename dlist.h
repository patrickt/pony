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

list_t *cons(void *val, list_t *existing);
tag_t first(list_t *list, union ldata *data);
list_t *rest(list_t *list);
void del(list_t *list);
void print(list_t *list);
