/* -*- Mode: C; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/*
 * Slabs memory allocation, based on powers-of-N. Slabs are up to 1MB in size
 * and are divided into chunks. The chunk sizes start off at the size of the
 * "item" structure plus space for a small key and value. They increase by
 * a multiplier factor from there, up to half the maximum slab size. The last
 * slab size is always 1MB, since that's the maximum item size allowed by the
 * memcached protocol.
 */


#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <sys/resource.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <pthread.h>
#include <math.h>

#include "slabs.h"

/*
 * Forward Declarations
 */
static int do_slabs_newslab(slabs_t* pst, const unsigned int id);
static void *memory_allocate(slabs_t* pst, size_t size);

/*
 * slab pool management
 */
void* pool_new(slabs_t* pst) {
    void *ptr;
    slabheader_t *shp;
    if (pst->pool_freelist == NULL) {
        ptr = memory_allocate(pst, SETTING_ITEM_SIZE_MAX);
        if (!ptr) return NULL;
        shp = (slabheader_t*)ptr;
        shp->next = NULL;
        pst->pool_freelist = ptr;
    }
    shp = pst->pool_freelist;
    pst->pool_freelist = shp->next;
    return (void*)shp;
}

void pool_free(slabs_t* pst, void* ptr) {
    slabheader_t *shp;
    shp = (slabheader_t*)ptr;
    shp->next = pst->pool_freelist;
    pst->pool_freelist = shp;
}

/*
 * slab list management per slabclass
 */
bool slab_add(slabs_t* pst, slabclass_t* psct, void* ptr) {
    size_t need_byte;
    slablist_t* pslt = (slablist_t*)memory_allocate(pst, sizeof(slablist_t));
    if (!pslt) return false;
    need_byte = (size_t)ceil(psct->perslab / 8.0);
    pslt->used_bitmap = (unsigned char*)memory_allocate(pst, need_byte);
    if (!pslt->used_bitmap) return false;
    memset(pslt->used_bitmap, 0, need_byte);
    pslt->ptr = ptr;
    pslt->next = psct->slab_list;
    psct->slab_list = pslt;
    return true;
}

void* slab_remove(slabs_t* pst, slabclass_t* psct, slablist_t* pslt_target) {
    void* pret;
    slablist_t* pslt = psct->slab_list;
    slablist_t* pprev = NULL;
    while (pslt != NULL) {
        if (pslt == pslt_target) {
            if (pprev) {
                pprev->next = pslt->next;
            } else {
                psct->slab_list = pslt->next;
            }
            pret = pslt->ptr;
            free(pslt->used_bitmap);
            free(pslt);
            return pret;
        }
        pprev = pslt;
        pslt = pslt->next;
    }
    return NULL;
}

slablist_t* slab_search(slabs_t* pst, slabclass_t* psct, char* ptr_in_slab) {
    slablist_t* pslt = psct->slab_list;
    char* pstart;
    char* pend;
    while (pslt != NULL) {
        pstart = (char*)pslt->ptr;
        pend = pstart + SETTING_ITEM_SIZE_MAX;
        if (ptr_in_slab >= pstart && ptr_in_slab <= pend) return pslt;
        pslt = pslt->next;
    }
    return NULL;
}

/*
 * slab free space management per slab
 */
#define SLABLIST_USED_IDX(pi, pbi, psct, pslt, ptr_in_slab) \
    size_t byte_offset = (size_t)(ptr_in_slab - ((char*)pslt->ptr)); \
    *pi = (size_t)(byte_offset / psct->size); \
    *pbi = (size_t)round(index / 8)

inline void slablist_used(slabclass_t* psct, slablist_t* pslt, char* ptr_in_slab) {
    size_t index;
    size_t bmp_index;
    SLABLIST_USED_IDX(&index, &bmp_index, psct, pslt, ptr_in_slab);
    unsigned char bitmask = (unsigned char)(1 << (index % 8));
    pslt->used_bitmap[bmp_index] |= bitmask;
}

inline void slablist_unused(slabclass_t* psct, slablist_t* pslt, char* ptr_in_slab) {
    size_t index;
    size_t bmp_index;
    SLABLIST_USED_IDX(&index, &bmp_index, psct, pslt, ptr_in_slab);
    unsigned char bitmask = ~(unsigned char)(1 << (index % 8));
    pslt->used_bitmap[bmp_index] &= bitmask;
}

inline bool slablist_is_empty(slabclass_t* psct, slablist_t* pslt) {
    unsigned char* pcurrent = (unsigned char*)pslt->used_bitmap;
    size_t need_byte = (size_t)ceil(psct->perslab / 8.0);
    while (need_byte > 0) {
        if (need_byte >= sizeof(unsigned int)) {
            if (*((unsigned int*)pcurrent)) return false;
            need_byte -= sizeof(unsigned int);
            pcurrent += sizeof(unsigned int);
        } else if (need_byte >= sizeof(unsigned short)) {
            if (*((unsigned short*)pcurrent)) return false;
            need_byte -= sizeof(unsigned short);
            pcurrent += sizeof(unsigned short);
        } else {
            if (*pcurrent) return false;
            need_byte -= sizeof(unsigned char);
            pcurrent += sizeof(unsigned char);
        }
    }
    return true;
}

/*
 * Figures out which slab class (chunk size) is required to store an item of
 * a given size.
 *
 * Given object size, return id to use when allocating/freeing memory for object
 * 0 means error: can't store such a large object
 */

static unsigned int slabs_clsid(slabs_t* pst, const size_t size) {
    int res = POWER_SMALLEST;

    if (size == 0)
        return 0;
    while (size > pst->slabclass[res].size)
        if (res++ == pst->power_largest)     /* won't fit in the biggest slab */
            return 0;
    return res;
}

/**
 * Determines the chunk sizes and initializes the slab class descriptors
 * accordingly.
 */
void slabs_init(slabs_t* pst, const size_t limit, const double factor, const bool prealloc) {
    int i = POWER_SMALLEST - 1;
    unsigned int size = sizeof(slabheader_t) + SETTING_CHUNK_SIZE;

    pst->mem_limit = limit;
    pst->pool_freelist = NULL;

    if (prealloc) {
        /* Allocate everything in a big chunk with malloc */
        pst->mem_base = malloc(pst->mem_limit);
        if (pst->mem_base != NULL) {
            pst->mem_current = pst->mem_base;
            pst->mem_avail = pst->mem_limit;
        } else {
            fprintf(stderr, "Warning: Failed to allocate requested memory in"
                    " one large chunk.\nWill allocate in smaller chunks\n");
        }
    }

    memset(pst->slabclass, 0, sizeof(pst->slabclass));

    while (++i < POWER_LARGEST && size <= SETTING_ITEM_SIZE_MAX / factor) {
        /* Make sure items are always n-byte aligned */
        if (size % CHUNK_ALIGN_BYTES)
            size += CHUNK_ALIGN_BYTES - (size % CHUNK_ALIGN_BYTES);

        pst->slabclass[i].size = size;
        pst->slabclass[i].perslab = SETTING_ITEM_SIZE_MAX / pst->slabclass[i].size;
        size *= factor;
        if (SETTING_VERBOSE > 1) {
            fprintf(stderr, "slab class %3d: chunk size %9u perslab %7u\n",
                    i, pst->slabclass[i].size, pst->slabclass[i].perslab);
        }
    }

    pst->power_largest = i;
    pst->slabclass[pst->power_largest].size = SETTING_ITEM_SIZE_MAX;
    pst->slabclass[pst->power_largest].perslab = 1;
    if (SETTING_VERBOSE > 1) {
        fprintf(stderr, "slab class %3d: chunk size %9u perslab %7u\n",
                i, pst->slabclass[i].size, pst->slabclass[i].perslab);
        fprintf(stderr, "pst:%p\n", pst);
    }

}

//static int grow_slab_list (slabs_t* pst, const unsigned int id) {
//    slabclass_t *p = &pst->slabclass[id];
//    if (p->slabs == p->list_size) {
//        size_t new_size =  (p->list_size != 0) ? p->list_size * 2 : 16;
//        void *new_list = realloc(p->slab_list, new_size * sizeof(void *));
//        if (new_list == 0) return 0;
//        p->list_size = new_size;
//        p->slab_list = new_list;
//    }
//    return 1;
//}

static int do_slabs_newslab(slabs_t* pst, const unsigned int id) {
    slabclass_t *p = &pst->slabclass[id];
    //int len = settings.slab_reassign ? settings.item_size_max
    //    : p->size * p->perslab;
    //@TODO int len = p->size * p->perslab;
/*
    int len = SETTING_ITEM_SIZE_MAX;// fixed for reusing by any slabclass
    char *ptr;

    if ((pst->mem_limit && pst->mem_malloced + len > pst->mem_limit && p->slabs > 0) ||
        (grow_slab_list(pst, id) == 0) ||
        ((ptr = memory_allocate(pst, (size_t)len)) == 0)) {

        return 0;
    }
*/
    void* ptr = pool_new(pst);
    if (ptr == NULL) return 0;
    //memset(ptr, 0, (size_t)len);
    p->end_page_ptr = ptr;
    p->end_page_free = p->perslab;

    //@TODO p->slab_list[p->slabs++] = ptr;
    bool ret = slab_add(pst, p, ptr);
    if (!ret) return 0;
    pst->mem_malloced += SETTING_ITEM_SIZE_MAX;
    //MEMCACHED_SLABS_SLABCLASS_ALLOCATE(id);

    return 1;
}

/*@null@*/
static void *do_slabs_alloc(slabs_t* pst, const size_t size, unsigned int id) {
    slabclass_t *p;
    void *ret = NULL;
    slabheader_t *it = NULL;
    slablist_t* pslt = NULL;

    if (id < POWER_SMALLEST || id > pst->power_largest) {
        //MEMCACHED_SLABS_ALLOCATE_FAILED(size, 0);
        return NULL;
    }

    p = &pst->slabclass[id];
    //assert(p->sl_curr == 0 || ((slabheader_t*)p->slots)->slabs_clsid == 0);

    /* fail unless we have space at the end of a recently allocated page,
       we have something on our freelist, or we could allocate a new page */
    if (! (p->end_page_ptr != 0 || p->sl_curr != 0 ||
           do_slabs_newslab(pst, id) != 0)) {
        /* We don't have more memory available */
        ret = NULL;
    } else if (p->sl_curr != 0) {
        /* return off our freelist */
        it = (slabheader_t*)p->slots;
        p->slots = it->next;
        if (it->next) it->next->prev = 0;
        p->sl_curr--;
        ret = (void *)it;
        pslt = slab_search(pst, p, (char*)ret);
        slablist_used(p, pslt, (char*)ret);
    } else {
        /* if we recently allocated a whole page, return from that */
        assert(p->end_page_ptr != NULL);
        ret = p->end_page_ptr;
        if (--p->end_page_free != 0) {
            p->end_page_ptr = ((caddr_t)p->end_page_ptr) + p->size;
        } else {
            p->end_page_ptr = 0;
        }
        pslt = slab_search(pst, p, (char*)ret);
        slablist_used(p, pslt, (char*)ret);
    }

    if (ret) {
        p->requested += size;
        //MEMCACHED_SLABS_ALLOCATE(size, id, p->size, ret);
    } else {
        //MEMCACHED_SLABS_ALLOCATE_FAILED(size, id);
    }
    //printf("alloc ps:%p sid:%u p:%p np:%p cnt:%u used:%lu rest:%u \n", pst, id, ret, p->end_page_ptr, p->sl_curr, p->requested, p->end_page_free);

    return ret;
}

static void do_slabs_free(slabs_t* pst, void *ptr, const size_t size, unsigned int id) {
    slabclass_t *p;
    slabheader_t* it;
    slablist_t* pslt;
    bool ret;
    void* ppool;
    //assert(((item *)ptr)->slabs_clsid == 0);
    assert(id >= POWER_SMALLEST && id <= pst->power_largest);
    if (id < POWER_SMALLEST || id > pst->power_largest)
        return;

    //MEMCACHED_SLABS_FREE(size, id, ptr);
    p = &pst->slabclass[id];

    it = (slabheader_t*)ptr;
    //it->it_flags |= ITEM_SLABBED;
    it->prev = 0;
    it->next = p->slots;
    if (it->next) it->next->prev = it;
    p->slots = it;

    p->sl_curr++;
    p->requested -= size;

    pslt = slab_search(pst, p, (char*)ptr);
    slablist_unused(p, pslt, (char*)ptr);
    ret = slablist_is_empty(p, pslt);
    if (ret) {
        // release slab from freelist(slots)
        slabheader_t* pit = it;
        slabheader_t* pprev = NULL;
        while (pit != NULL) {
            slablist_t* pslt_curr = slab_search(pst, p, (char*)pit);
            if (pslt == pslt_curr) {
                if (pprev) {
                    pprev->next = pit->next;
                } else {
                    p->slots = pit->next;
                }
                p->sl_curr--;
            } else {
                pprev = pit;
            }
            pit = pit->next;
        }
        // release slab from end_page(end_page_ptr, end_page_free)
        slablist_t* pslt_endp = slab_search(pst, p, (char*)p->end_page_ptr);
        if (pslt_endp == pslt) {
            p->end_page_ptr = NULL;
            p->end_page_free = 0;
        }
        // release slab from slab_list
        ppool = slab_remove(pst, p, pslt);
        pool_free(pst, ppool);
    }

    return;
}

/*
  static int nz_strcmp(int nzlength, const char *nz, const char *z) {
  int zlength=strlen(z);
  return (zlength == nzlength) && (strncmp(nz, z, zlength) == 0) ? 0 : -1;
  }

  bool get_stats(const char *stat_type, int nkey, ADD_STAT add_stats, void *c) {
  bool ret = true;

  if (add_stats != NULL) {
  if (!stat_type) {
  STATS_LOCK();
  APPEND_STAT("bytes", "%llu", (unsigned long long)stats.curr_bytes);
  APPEND_STAT("curr_items", "%u", stats.curr_items);
  APPEND_STAT("total_items", "%u", stats.total_items);
  APPEND_STAT("evictions", "%llu",
  (unsigned long long)stats.evictions);
  APPEND_STAT("reclaimed", "%llu",
  (unsigned long long)stats.reclaimed);
  STATS_UNLOCK();
  } else if (nz_strcmp(nkey, stat_type, "items") == 0) {
  item_stats(add_stats, c);
  } else if (nz_strcmp(nkey, stat_type, "slabs") == 0) {
  slabs_stats(add_stats, c);
  } else if (nz_strcmp(nkey, stat_type, "sizes") == 0) {
  item_stats_sizes(add_stats, c);
  } else {
  ret = false;
  }
  } else {
  ret = false;
  }

  return ret;
  }

  static void do_slabs_stats(ADD_STAT add_stats, void *c) {
  int i, total;

  struct thread_stats thread_stats;
  threadlocal_stats_aggregate(&thread_stats);

  total = 0;
  for(i = POWER_SMALLEST; i <= power_largest; i++) {
  slabclass_t *p = &slabclass[i];
  if (p->slabs != 0) {
  uint32_t perslab, slabs;
  slabs = p->slabs;
  perslab = p->perslab;

  char key_str[STAT_KEY_LEN];
  char val_str[STAT_VAL_LEN];
  int klen = 0, vlen = 0;

  APPEND_NUM_STAT(i, "chunk_size", "%u", p->size);
  APPEND_NUM_STAT(i, "chunks_per_page", "%u", perslab);
  APPEND_NUM_STAT(i, "total_pages", "%u", slabs);
  APPEND_NUM_STAT(i, "total_chunks", "%u", slabs * perslab);
  APPEND_NUM_STAT(i, "used_chunks", "%u",
  slabs*perslab - p->sl_curr - p->end_page_free);
  APPEND_NUM_STAT(i, "free_chunks", "%u", p->sl_curr);
  APPEND_NUM_STAT(i, "free_chunks_end", "%u", p->end_page_free);
  APPEND_NUM_STAT(i, "mem_requested", "%llu",
  (unsigned long long)p->requested);
  APPEND_NUM_STAT(i, "get_hits", "%llu",
  (unsigned long long)thread_stats.slab_stats[i].get_hits);
  APPEND_NUM_STAT(i, "cmd_set", "%llu",
  (unsigned long long)thread_stats.slab_stats[i].set_cmds);
  APPEND_NUM_STAT(i, "delete_hits", "%llu",
  (unsigned long long)thread_stats.slab_stats[i].delete_hits);
  APPEND_NUM_STAT(i, "incr_hits", "%llu",
  (unsigned long long)thread_stats.slab_stats[i].incr_hits);
  APPEND_NUM_STAT(i, "decr_hits", "%llu",
  (unsigned long long)thread_stats.slab_stats[i].decr_hits);
  APPEND_NUM_STAT(i, "cas_hits", "%llu",
  (unsigned long long)thread_stats.slab_stats[i].cas_hits);
  APPEND_NUM_STAT(i, "cas_badval", "%llu",
  (unsigned long long)thread_stats.slab_stats[i].cas_badval);
  APPEND_NUM_STAT(i, "touch_hits", "%llu",
  (unsigned long long)thread_stats.slab_stats[i].touch_hits);
  total++;
  }
  }



  APPEND_STAT("active_slabs", "%d", total);
  APPEND_STAT("total_malloced", "%llu", (unsigned long long)mem_malloced);
  add_stats(NULL, 0, NULL, 0, c);
  }
*/

static void *memory_allocate(slabs_t* pst, size_t size) {
    void *ret;

    if (pst->mem_base == NULL) {
        /* We are not using a preallocated large memory chunk */
        ret = malloc(size);
    } else {
        ret = pst->mem_current;

        if (size > pst->mem_avail) {
            return NULL;
        }

        /* mem_current pointer _must_ be aligned!!! */
        if (size % CHUNK_ALIGN_BYTES) {
            size += CHUNK_ALIGN_BYTES - (size % CHUNK_ALIGN_BYTES);
        }

        pst->mem_current = ((char*)pst->mem_current) + size;
        if (size < pst->mem_avail) {
            pst->mem_avail -= size;
        } else {
            pst->mem_avail = 0;
        }
    }

    return ret;
}

void *slabs_alloc(slabs_t* pst, size_t size) {
    void *ret;
    size += sizeof(slabheader_t);
    unsigned int id = slabs_clsid(pst, size);
    ret = do_slabs_alloc(pst, size, id);
    if (ret == NULL) {
        return NULL;
    }
    return (void*)((char*)ret + sizeof(slabheader_t));
}

void slabs_free(slabs_t* pst, void *ptr, size_t size) {
    void *header;
    size += sizeof(slabheader_t);
    unsigned int id = slabs_clsid(pst, size);
    header = (void*)((char*)ptr - sizeof(slabheader_t));
    do_slabs_free(pst, header, size, id);
}
/*
  void slabs_stats(ADD_STAT add_stats, void *c) {
  pthread_mutex_lock(&slabs_lock);
  do_slabs_stats(add_stats, c);
  pthread_mutex_unlock(&slabs_lock);
  }
*/
