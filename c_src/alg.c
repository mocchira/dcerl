// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"
#include "hashmap.h"
#include "type.h"

void
runtime_memhash(uintptr *h, uintptr s, void *a)
{
	byte *b;
	uintptr hash;

	b = a;
	if(sizeof(hash) == 4)
		hash = 2860486313U;
	else
		hash = 33054211828000289ULL;
	while(s > 0) {
		if(sizeof(hash) == 4)
			hash = (hash ^ *b) * 3267000013UL;
		else
			hash = (hash ^ *b) * 23344194077549503ULL;
		b++;
		s--;
	}
	*h ^= hash;
}

void
runtime_memequal(bool *eq, uintptr s, void *a, void *b)
{
	byte *ba, *bb, *aend;

	if(a == b) {
		*eq = 1;
		return;
	}
	ba = a;
	bb = b;
	aend = ba+s;
	while(ba != aend) {
		if(*ba != *bb) {
			*eq = 0;
			return;
		}
		ba++;
		bb++;
	}
	*eq = 1;
	return;
}

void
runtime_strhash(uintptr *h, uintptr s, void *a)
{
	runtime_memhash(h, ((String*)a)->len, ((String*)a)->str);
}

void
runtime_strequal(bool *eq, uintptr s, void *a, void *b)
{
	int32 alen;

	alen = ((String*)a)->len;
	if(alen != ((String*)b)->len) {
		*eq = false;
		return;
	}
	runtime_memequal(eq, alen, ((String*)a)->str, ((String*)b)->str);
}

void
runtime_strcopy(uintptr s, void *a, void *b)
{
	if(b == nil) {
		((String*)a)->str = 0;
		((String*)a)->len = 0;
		return;
	}
	((String*)a)->str = ((String*)b)->str;
	((String*)a)->len = ((String*)b)->len;
}

Alg StrAlg = { runtime_strhash, runtime_strequal, runtime_strcopy };
Type StrType = { sizeof(String), &StrAlg };
MapType StrMapType = { &StrType, &StrType };
