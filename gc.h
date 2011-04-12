void __mark_list(list_t *l) {
  if ((l == nil) || (l->marked)) return;
  
  l->marked = 1;
  if (l->tag != VALUE) {
    __mark_list(l->c.list);
  }
  __mark_list(l->next);
}

void mark(struct ref_list_s *ref) {
  for (int ii=0; ii<ref->nptrs; ii++) {
    __mark_list(ref->ref_lists[ii]);
  }
  if (ref->parent) mark(ref->parent);
}

void sweep() {
  list_t *iter = NULL;
  list_t *next = NULL;
  for (iter=all_lists; iter!=nil; iter = next) {
    next = iter->next;
    if (!iter->marked) {
      free(iter);
    } else {
      iter->marked = 0;
    }
  }
}