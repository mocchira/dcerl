#ifndef __DCERL__
#define __DCERL__

#include "runtime.h"
#include "lru.h"
#include "slabs.h"

typedef struct _dcerl_t {
  Hmap* hm;
  slabs_t slab;
  lru_t *lru;
  unsigned long long items_length;
} dcerl_t;

#define dcerl_items_length(dcerl) ((dcerl)->items_length)

void dcerl_init(dcerl_t *dcerl);
void* dcerl_get(dcerl_t *dcerl, void * key, int length);
bool dcerl_put(dcerl_t *dcerl, void * key, int length);
void dcerl_remove(dcerl_t *dcerl, void * key, int length);
void* dcerl_eldest(dcerl_t *dcerl, int* eldest_len);
void dcerl_destroy(dcerl_t *dcerl);

#endif
