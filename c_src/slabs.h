/* slabs memory allocation */
#ifndef SLABS_H
#define SLABS_H

#include "runtime.h"

#define		SETTING_CHUNK_SIZE	128
#define		SETTING_ITEM_SIZE_MAX	1024 * 1024 * 8
#define		POWER_LARGEST		200
#define		MAX_NUMBER_OF_SLAB_CLASSES	(POWER_LARGEST + 1)
#define		POWER_SMALLEST		1
#define		CHUNK_ALIGN_BYTES	8
#define		SETTING_VERBOSE		2
#define		MAX_NUMBER_OF_SLAB_CLASSES	(POWER_LARGEST + 1)

typedef struct slabheader {
	struct slabheader *next;
	struct slabheader *prev;
} slabheader_t;

typedef struct slablist {
    void *ptr;
    unsigned char *used_bitmap; // using perslab/8
    struct slablist *next;
} slablist_t;

typedef struct {
    unsigned int size;      /* sizes of items */
    unsigned int perslab;   /* how many items per slab */

    void *slots;           /* list of item ptrs */
    unsigned int sl_curr;   /* total free items in list */

    void *end_page_ptr;         /* pointer to next free item at end of page, or 0 */
    unsigned int end_page_free; /* number of items remaining at end of last alloced page */

    unsigned int slabs;     /* how many slabs were allocated for this class */

    //@TODO void **slab_list;       /* array of slab pointers */
    slablist_t *slab_list;       /* array of slab pointers */
    unsigned int list_size; /* size of prev array */

    unsigned int killing;  /* index+1 of dying slab, or zero if none */
    size_t requested; /* The number of requested bytes */
} slabclass_t;
// per thread
typedef struct {
	slabclass_t slabclass[MAX_NUMBER_OF_SLAB_CLASSES];
	size_t mem_limit;
	size_t mem_malloced;
	int power_largest;
	void *mem_base;
	void *mem_current;
	size_t mem_avail;
	void *pool_freelist;
} slabs_t;

void* pool_new(slabs_t* pst);
void pool_free(slabs_t* pst, void* ptr);
bool slab_add(slabs_t* pst, slabclass_t* psct, void* ptr);
void* slab_remove(slabs_t* pst, slabclass_t* psct, slablist_t* pslt_target);
slablist_t* slab_search(slabs_t* pst, slabclass_t* psct, char* ptr_in_slab);
void slablist_used(slabclass_t* psct, slablist_t* pslt, char* ptr_in_slab);
void slablist_unused(slabclass_t* psct, slablist_t* pslt, char* ptr_in_slab);
bool slablist_is_empty(slabclass_t* psct, slablist_t* pslt);

/** Init the subsystem. 1st argument is the limit on no. of bytes to allocate,
    0 if no limit. 2nd argument is the growth factor; each slab will use a chunk
    size equal to the previous slab's chunk size times this factor.
    3rd argument specifies if the slab allocator should allocate all memory
    up front (if true), or allocate memory in chunks as it is needed (if false)
*/
void slabs_init(slabs_t* pst, const size_t limit, const double factor, const bool prealloc);


/**
 * Given object size, return id to use when allocating/freeing memory for object
 * 0 means error: can't store such a large object
 */

//unsigned int slabs_clsid(slabs_t* pst, const size_t size);

/** Allocate object of given length. 0 on error */ /*@null@*/
void *slabs_alloc(slabs_t* pst, const size_t size);

/** Free previously allocated object */
void slabs_free(slabs_t* pst, void *ptr, size_t size);

/** Fill buffer with stats */ /*@null@*/
//void slabs_stats(ADD_STAT add_stats, void *c);

#endif
