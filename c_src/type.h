// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef __TYPE_H__
#define __TYPE_H__

struct Type
{
	uintptr size;
	Alg *alg;
};

struct MapType
{
	Type *key;
	Type *elem;
};
#endif
