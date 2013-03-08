// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef __RUNTIME__
#define __RUNTIME__

/*
 * basic types
 */
typedef	signed char		int8;
typedef	unsigned char		uint8;
typedef	signed short		int16;
typedef	unsigned short		uint16;
typedef	signed int		int32;
typedef	unsigned int		uint32;
typedef	signed long long int	int64;
typedef	unsigned long long int	uint64;
typedef	float			float32;
typedef	double			float64;

#if defined(__ia64) || defined(__x86_64) || defined(__amd64)
typedef	uint64		uintptr;
typedef	int64		intptr;
#else
typedef	uint32		uintptr;
typedef int32		intptr;
#endif

/*
 * get rid of C types
 * the / / / forces a syntax error immediately,
 * which will show "last name: XXunsigned".
 */
//#define	unsigned		XXunsigned / / /
//#define	signed			XXsigned / / /
//#define	char			XXchar / / /
//#define	short			XXshort / / /
//#define	int			XXint / / /
//#define	long			XXlong / / /
//#define	float			XXfloat / / /
//#define	double			XXdouble / / /
#define nelem(x)        (sizeof(x)/sizeof((x)[0]))
#define nil             ((void*)0)
#define      offsetof(s,m)   (uint32)(&(((s*)0)->m))

/*
 * defined types
 */
typedef	uint8			bool;
typedef	uint8			byte;
typedef	struct	String		String;
typedef	struct	Type		Type;
typedef	struct	MapType		MapType;
typedef	struct	Hmap		Hmap;
typedef	struct	hash_iter	hash_iter;

enum
{
	true	= 1,
	false	= 0,
};

struct String
{
	byte*	str;
	int32	len;
};
typedef	struct	Alg		Alg;
struct	Alg
{
	void	(*hash)(uintptr*, uintptr, void*);
	void	(*equal)(bool*, uintptr, void*, void*);
	//void	(*print)(uintptr, void*);
	void	(*copy)(uintptr, void*, void*);
};

extern	Alg	StrAlg;
extern	Type	StrType;
extern	MapType	StrMapType;

void	runtime_memhash(uintptr*, uintptr, void*);
void	runtime_strhash(uintptr*, uintptr, void*);

void	runtime_memequal(bool*, uintptr, void*, void*);
void	runtime_strequal(bool*, uintptr, void*, void*);

void	runtime_strcopy(uintptr, void*, void*);

uint32	runtime_rnd(uint32, uint32);
byte*	runtime_mchr(byte*, byte, byte*);
int32	runtime_mcmp(byte*, byte*, uint32);
int32	runtime_atoi(byte*);
uint32	runtime_fastrand1(void);

int32	runtime_mapassign(MapType*, Hmap*, byte*, byte*);
void	runtime_mapaccess(MapType*, Hmap*, byte*, byte*, bool*);
void	runtime_mapiternext(hash_iter*);
bool	runtime_mapiterkey(hash_iter*, void*);
void	runtime_mapiterkeyvalue(hash_iter*, void*, void*);
void	runtime_mapiterinit(MapType*, Hmap*, hash_iter*);
Hmap*	runtime_makemap_c(MapType*, int64);
void	runtime_mapdestroy(Hmap*);
// for debug
void	hash_visit (Hmap*, void (*data_visit) (void*, int32 , void*), void*);
#endif
