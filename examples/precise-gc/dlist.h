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

// what about something that dif

/*

Declaration { Typedef { name = "tag_t"}, typeOf = (Enumeration Void [ (Ident "list"), (Ident "Value")]))
Composite { kind = Union, name = "ldata" }) 
  [ Variable { ofType = PointerTo Void, name = "value"}
  , Variable { ofType = PointerTo (Composite Struct "list_s" Empty), name = "list" }
  ]

Typedef { name = "list_t"}, typeOf = (Composite { kind = Struct, name = "list_s" }) 
  , Variable { ofType = PointerTo (Composite Struct "list_s" Empty, name = "next"}
  , Variable { ofType = Typedef { name = "tag_t", ...}, name = "tag" }
  , Variable { ofType = Composite Union {name = "ldata", members = Empty} name = "c"}, 

Declaration { typeOf = PointerTo (Typedef "list_t")}

*/