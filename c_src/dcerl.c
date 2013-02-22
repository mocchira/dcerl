#include <stdio.h>
#include <string.h>
#include "dcerl.h"
#include "common.h"

static void dcerl_eject_callback(dcerl_t *dcerl, char *key, int length);

/**
 * Initialize LRU-Storage
 */
void dcerl_init(dcerl_t *dcerl) {
  dcerl->hm = runtime_makemap_c(&StrMapType, 4096);
  memset(&dcerl->slab, 0, sizeof(slabs_t));
  slabs_init(&dcerl->slab, 0, 2, false);
  dcerl->lru  = lru_create();
  dcerl->items_length = 0;
}


/**
 * Insert an object into LRU-Storage
 */
// node -> item -> value
bool dcerl_put(dcerl_t *dcerl, void *key, int length) {
  lru_item_t * item;
  String skey, sval;
  bool exists;

  // Prepare put-operation
  size_t bufsiz = sizeof(size_t) + length;
  void* buf = slabs_alloc(&dcerl->slab, bufsiz);
  if (buf == NULL) {
    return false;
  }
  *((size_t*)buf) = bufsiz;
  char* bufkey = (char*)((char*)buf + sizeof(size_t));

  skey.str = (byte*)bufkey;
  skey.len = length;

  memcpy(bufkey, key, length);
  runtime_mapaccess(&StrMapType, dcerl->hm, (byte*)&skey, (byte*)&sval, &exists);

  if (exists) {
    item = (lru_item_t*)sval.str;
    dcerl_remove(dcerl, lru_item_key(item), lru_item_keylen(item));
  }

  // Insert an object into lru-storage
  item = lru_insert(dcerl->lru, bufkey, length, NULL, 0, NULL);

  // After put-operation
  sval.str = (byte*)item;
  runtime_mapassign(&StrMapType, dcerl->hm, (byte*)&skey, (byte*)&sval);

  dcerl->items_length++;
  return true;

}


/**
 * Retrieve an object from LRU-Storage
 */
void* dcerl_get(dcerl_t *dcerl, void *key, int length) {
  lru_item_t * item;
  String skey, sval;
  bool exists;

  // Prepare get-operation
  skey.str = (byte*)key;
  skey.len = length;

  // Retrieve an object
  runtime_mapaccess(&StrMapType, dcerl->hm, (byte*)&skey, (byte*)&sval, &exists);

  if (!exists) {
    return nil;
  } else {
    item = (lru_item_t *)sval.str;
    lru_touch(dcerl->lru, item);
    return lru_item_key(item);
  }
}

/**
 * Free a stored memory
 */
static inline void dcerl_slab_free(slabs_t* slab, char* key) {
  size_t* psize = (size_t*)key;
  psize--;
  slabs_free(slab, (void*)psize, *psize);
}


/**
 * Callback
 */
static void dcerl_eject_callback(dcerl_t *dcerl, char *key, int length) {
  lru_item_t *item;
  String skey, sval;
  bool exists;
  int32 ret;

  skey.str = (byte*)key;
  skey.len = length;
  runtime_mapaccess(&StrMapType, dcerl->hm, (byte*)&skey, (byte*)&sval, &exists);

  if (!exists) {
    return;
  }

  item = (lru_item_t*)sval.str;
  dcerl_slab_free(&dcerl->slab, lru_item_key(item));
  ret = runtime_mapassign(&StrMapType, dcerl->hm, (byte*)&skey, nil);

  if (ret) {
    dcerl->items_length--;
  }
}

/**
 * Remove an object from LRU-Storage
 */
void dcerl_remove(dcerl_t *dcerl, void *key, int length) {
  lru_item_t *item;
  String skey, sval;
  bool exists;

  skey.str = (byte*)key;
  skey.len = length;
  runtime_mapaccess(&StrMapType, dcerl->hm, (byte*)&skey, (byte*)&sval, &exists);

  if (!exists) {
    return;
  }

  item = (lru_item_t *)sval.str;
  dcerl_slab_free(&dcerl->slab, lru_item_key(item));

  lru_remove_and_destroy(dcerl->lru, item);
  dcerl->items_length--;

  runtime_mapassign(&StrMapType, dcerl->hm, (byte*)&skey, nil);
}

void* dcerl_eldest(dcerl_t *dcerl, int* eldest_len) {
  lru_item_t *item;
  item = lru_eldest(dcerl->lru);
  if (item == NULL) {
    *eldest_len = 0;
    return NULL;
  }
  *eldest_len = lru_item_keylen(item);
  return lru_item_key(item);
}

void* dcerl_iterator(dcerl_t *dcerl, int* item_len) {
  lru_item_t* item;
  d_node_t* node;
  node = lru_iterator(dcerl->lru);
  if (node == NULL) {
    *item_len = 0;
    return NULL;
  }
  item = (lru_item_t*)node->data;
  *item_len = lru_item_keylen(item);
  return lru_item_key(item);
}

void* dcerl_iterator_next(dcerl_t *dcerl, int* item_len) {
  lru_item_t* item;
  d_node_t* node;
  node = lru_iterator_next(dcerl->lru);
  if (node == NULL) {
    *item_len = 0;
    return NULL;
  }
  item = (lru_item_t*)node->data;
  *item_len = lru_item_keylen(item);
  return lru_item_key(item);
}

bool dcerl_iterator_has_next(dcerl_t *dcerl) {
    return lru_iterator_has_next(dcerl->lru);
}

/**
 * Destroy LRU-Storage
 */
void dcerl_destroy(dcerl_t *dcerl) {
  runtime_mapdestroy(dcerl->hm);
  lru_destroy(dcerl->lru);
}

