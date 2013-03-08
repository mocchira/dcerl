#include <check.h>
#include <stdio.h>
#include <math.h>
#include "common.h"
#include "slabs.h"

static slabs_t slab;

START_TEST(pool)
{  
    void* ps1 = pool_new(&slab); 
    fail_unless(NULL != ps1);
    fail_unless(NULL == slab.pool_freelist);
    void* ps2 = pool_new(&slab); 
    fail_unless(NULL != ps2);
    fail_unless(NULL == slab.pool_freelist);
    pool_free(&slab, ps1);
    fail_unless(ps1 == slab.pool_freelist);
    pool_free(&slab, ps2);
    fail_unless(ps2 == slab.pool_freelist);
    slabheader_t *shp = (slabheader_t*)slab.pool_freelist;
    fail_unless(ps1 == shp->next);
    void* ps3 = pool_new(&slab);
    fail_unless(ps2 == ps3);
    fail_unless(ps1 == slab.pool_freelist);
    void* ps4 = pool_new(&slab);
    fail_unless(ps1 == ps4);
    fail_unless(NULL == slab.pool_freelist);
    void* ps5 = pool_new(&slab);
    fail_unless(NULL != ps5);
    fail_unless(NULL == slab.pool_freelist);
}
END_TEST

START_TEST(slablist)
{  
    slabclass_t* psct = &slab.slabclass[1]; // size:144 perslab:58254
    void* ps = pool_new(&slab);
    bool ret = slab_add(&slab, psct, ps);
    fail_unless(ret);
    fail_unless(NULL != psct->slab_list);
    fail_unless(ps == psct->slab_list->ptr);
    fail_unless(NULL == psct->slab_list->next);
    size_t need_byte = (size_t)ceil(psct->perslab / 8);
    void* pv = malloc(need_byte);
    memset(pv, 0, need_byte);
    fail_unless(0 == memcmp(pv, psct->slab_list->used_bitmap, need_byte));
    void* ps2 = pool_new(&slab);
    ret = slab_add(&slab, psct, ps2);
    fail_unless(ret);
    fail_unless(NULL != psct->slab_list);
    fail_unless(ps2 == psct->slab_list->ptr);
    fail_unless(NULL != psct->slab_list->next);
    fail_unless(NULL == psct->slab_list->next->next);
    void* ps3 = pool_new(&slab);
    ret = slab_add(&slab, psct, ps3);
    fail_unless(ret);
    slablist_t* pslt = slab_search(&slab, psct, ((char*)ps + 4));
    fail_unless(pslt ==  psct->slab_list->next->next);
    pslt = slab_search(&slab, psct, ((char*)ps2));
    fail_unless(pslt ==  psct->slab_list->next);
    pslt = slab_search(&slab, psct, ((char*)ps3 + SETTING_ITEM_SIZE_MAX));
    fail_unless(pslt ==  psct->slab_list);
    pslt = slab_search(&slab, psct, 0);
    fail_unless(pslt ==  NULL);
    void* ps4 = slab_remove(&slab, psct, psct->slab_list->next);
    fail_unless(ps4 ==  ps2);
    fail_unless(psct->slab_list->ptr == ps3);
    fail_unless(psct->slab_list->next->ptr == ps);
    fail_unless(psct->slab_list->next->next == NULL);
    void* ps5 = slab_remove(&slab, psct, psct->slab_list);
    fail_unless(ps5 ==  ps3);
    fail_unless(psct->slab_list->ptr == ps);
    fail_unless(psct->slab_list->next == NULL);
}
END_TEST

START_TEST(slab_used_bitmap)
{  
    slabclass_t* psct = &slab.slabclass[1]; // size:144 perslab:58254
    void* ps = pool_new(&slab);
    slab_add(&slab, psct, ps);
    slablist_t* pslt = psct->slab_list;
    fail_unless(slablist_is_empty(psct, pslt));
    char* pc1 = (char*)(pslt->ptr) + psct->size * 8; // should point to the head of 2byte
    slablist_used(psct, pslt, pc1);
    fail_unless(pslt->used_bitmap[1] == 1);
    fail_unless(!slablist_is_empty(psct, pslt));
    char* pc2 = (char*)(pslt->ptr) + psct->size * 9;
    slablist_used(psct, pslt, pc2);
    fail_unless(pslt->used_bitmap[1] == 3);
    fail_unless(!slablist_is_empty(psct, pslt));
    slablist_unused(psct, pslt, pc1);
    fail_unless(pslt->used_bitmap[1] == 2);
    fail_unless(!slablist_is_empty(psct, pslt));
    slablist_unused(psct, pslt, pc2);
    fail_unless(pslt->used_bitmap[1] == 0);
    fail_unless(slablist_is_empty(psct, pslt));
}
END_TEST

START_TEST(slab_it)
{  
    // The freed slab exist at both(slots, end_page)
    slab.pool_freelist = NULL;
    void* p1 = slabs_alloc(&slab, 2000000);
    void* p2 = slabs_alloc(&slab, 2000000);
    slabs_free(&slab, p1, 2000000);
    fail_unless(slab.pool_freelist == NULL);
    slabclass_t* psct = &slab.slabclass[15];
    fail_unless(psct->slab_list != NULL);
    fail_unless(psct->slab_list->next == NULL);
    fail_unless(psct->sl_curr == 1);
    fail_unless(psct->end_page_ptr != NULL);
    fail_unless(psct->end_page_free == 1);
    slabs_free(&slab, p2, 2000000);
    fail_unless(slab.pool_freelist != NULL);
    fail_unless(psct->slab_list == NULL);
    fail_unless(psct->sl_curr == 0);
    fail_unless(psct->end_page_ptr == NULL);
    fail_unless(psct->end_page_free == 0);
    // The freed slab exist at end_page
    void* p3 = slabs_alloc(&slab, 2000000);
    fail_unless(p1 == p3);
    fail_unless(slab.pool_freelist == NULL);
    fail_unless(psct->slots == NULL);
    slabs_free(&slab, p3, 2000000);
    fail_unless(slab.pool_freelist != NULL);
    slabheader_t *shp = (slabheader_t*)slab.pool_freelist;
    fail_unless(shp->next == NULL);
    fail_unless(psct->slab_list == NULL);
    fail_unless(psct->sl_curr == 0);
    fail_unless(psct->end_page_ptr == NULL);
    fail_unless(psct->end_page_free == 0);
    // The freed slab exist at slots
    void* p11 = slabs_alloc(&slab, 2000000);
    void* p12 = slabs_alloc(&slab, 2000000);
    void* p13 = slabs_alloc(&slab, 2000000);
    void* p14 = slabs_alloc(&slab, 2000000); // second slab start
    void* p15 = slabs_alloc(&slab, 2000000);
    void* p16 = slabs_alloc(&slab, 2000000);
    fail_unless(psct->end_page_ptr == NULL);
    fail_unless(psct->end_page_free == 0);
    fail_unless(psct->slab_list != NULL);
    fail_unless(psct->slab_list->next != NULL);
    fail_unless(psct->slab_list->next->next == NULL);
    slabs_free(&slab, p11, 2000000);
    slabs_free(&slab, p13, 2000000);
    slabs_free(&slab, p14, 2000000);
    slabs_free(&slab, p15, 2000000);
    fail_unless(psct->slots != NULL);
    slabs_free(&slab, p16, 2000000);
    shp = (slabheader_t*)psct->slots;
    fail_unless((shp + 1) == p13);
    fail_unless((shp->next + 1) == p11);
    fail_unless(shp->next->next == NULL);
    fail_unless(psct->end_page_ptr == NULL);
    fail_unless(psct->end_page_free == 0);
    shp = (slabheader_t*)slab.pool_freelist;
    fail_unless(shp->next == NULL);
}
END_TEST

Suite* slabs_suite(void) {
  Suite *s = suite_create("slabs.c");

  /* Core test case */
  memset(&slab, 0, sizeof(slab));
  slabs_init(&slab, 0, 2, false);

  TCase *tc_core = tcase_create("Core");
  tcase_add_test(tc_core, pool);
  tcase_add_test(tc_core, slablist);
  tcase_add_test(tc_core, slab_used_bitmap);
  tcase_add_test(tc_core, slab_it);
  suite_add_tcase(s, tc_core);

  return s;
}
