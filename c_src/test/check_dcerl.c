#include <check.h>
#include "common.h"
#include <stdio.h>
#include "cherly.h"

static slabs_t slab;

static void print_visitor(void *arg, int32 level, void *data) {
	int i;
	String* key = (String*)data;
	String* val = (String*)((byte*)data+runtime_rnd(sizeof(String), sizeof(void*)));
	if (key->str == nil) return;
	printf("%s\n", (const char*)arg);
	for (i = 0; i < level; i++) 
		printf("\t");
	printf("mapassign: level=%d key=%s val=%s pk=%p pv=%p \n", level, key->str, val->str, key, val);
}

START_TEST(basic_get_and_put)
{  cherly_t cherly;
  char* key   = "exist";
  char* stuff = "stuff";
  int len;
  
  void* buf = slabs_alloc(&slab, 20);
  printf("basic: buf=%p \n", buf);
  memset(buf, 0, 20);
  memcpy(buf, key, 5);
  void* vbuf = ((char*)buf)+6;
  memcpy(vbuf, stuff, 5);
  cherly_init(&cherly, 0, 120);
  fail_unless(NULL == cherly_get(&cherly, "noexist", 7, &len));
  cherly_put(&cherly, buf, 5, vbuf, 5, NULL);
  hash_visit(cherly.hm, print_visitor, "basic");
  fail_unless(NULL == cherly_get(&cherly, "noexist", 7, &len));
  void* ret = cherly_get(&cherly, key, 5, &len);
  fail_unless(memcmp(ret, vbuf, len) == 0);
  cherly_destroy(&cherly);
  //slabs_free(&slab, buf, 20);
}
END_TEST

START_TEST(put_already_there)
{
  cherly_t cherly;
  char* stuff = "stuff";
  char* otherstuff = "blah";
  
  void* buf = slabs_alloc(&slab, 20);
  printf("basic: buf=%p \n", buf);
  memset(buf, 0, 20);
  memcpy(buf, stuff, 5);
  void* nbuf = ((char*)buf)+6;
  memcpy(nbuf, otherstuff, 4);
  cherly_init(&cherly, 0, 120);
  cherly_put(&cherly, "exist", 5, buf, 5, NULL);
  fail_unless(10 == cherly_size(&cherly));
  cherly_put(&cherly, "exist", 5, nbuf, 4, NULL);
  hash_visit(cherly.hm, print_visitor, "put already");
  cherly_destroy(&cherly);
}
END_TEST

START_TEST(put_beyond_limit)
{
  cherly_t cherly;
  
  cherly_init(&cherly, 0, 12);
  void* buf = slabs_alloc(&slab, 10);
  printf("basic: buf=%p \n", buf);
  memset(buf, 0, 10);
  memcpy(buf, "one", 3);
  cherly_put(&cherly, "one", 3, buf, 3, NULL);
  hash_visit(cherly.hm, print_visitor, "put1");
  slabs_free(&slab, buf, 10);

  void* buf2 = slabs_alloc(&slab, 10);
  printf("basic: buf2=%p \n", buf2);
  memset(buf2, 0, 10);
  memcpy(buf2, "two", 3);
  cherly_put(&cherly, "two", 3, buf2, 3, NULL);
  hash_visit(cherly.hm, print_visitor, "put2");
  slabs_free(&slab, buf2, 10);

  void* buf3 = slabs_alloc(&slab, 10);
  printf("basic: buf3=%p \n", buf3);
  memset(buf3, 0, 10);
  memcpy(buf3, "three", 5);
  cherly_put(&cherly, "three", 5, buf3, 5, NULL);
  hash_visit(cherly.hm, print_visitor, "put3");
  fail_unless(1 == cherly_items_length(&cherly), "cherly length was %d", cherly_items_length(&cherly));
  fail_unless(10 == cherly_size(&cherly), "cherly size was %d", cherly_size(&cherly));
}
END_TEST

Suite * cherly_suite(void) {
  Suite *s = suite_create("cherly.c");

  /* Core test case */
  memset(&slab, 0, sizeof(slab));
  slabs_init(&slab, 0, 1.5, false);

  TCase *tc_core = tcase_create("Core");
  tcase_add_test(tc_core, basic_get_and_put);
  tcase_add_test(tc_core, put_already_there);
  tcase_add_test(tc_core, put_beyond_limit);
  suite_add_tcase(s, tc_core);

  return s;
}
