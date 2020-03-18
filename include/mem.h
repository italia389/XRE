// mem.h - XRE memory allocator.
//
// (c) Copyright 2020 Richard W. Marinelli
//
// This work is based on TRE ver. 0.7.5 (c) Copyright 2001-2006 Ville Laurikari <vl@iki.fi> and is licensed
// under the GNU Lesser General Public License (LGPLv3).  To view a copy of this license, see the "License.txt"
// file included with this distribution or visit http://www.gnu.org/licenses/lgpl-3.0.en.html.

#ifndef xre_mem_h
#define xre_mem_h

#include <stdlib.h>

#define MemBlockSize	1024

#ifdef __cplusplus
extern "C" {
#endif

typedef struct memlist {
	void *data;
	struct memlist *next;
	} memlist_t;

typedef struct {
	memlist_t *blocks;
	memlist_t *current;
	char *ptr;
	size_t n;
	bool failed;
	} memhdr_t;

// Return a new memory allocator, or NULL if out of memory.
extern memhdr_t *mem_new(void);

// Routines which allocate a block of 'size' bytes from 'mem', possibly zeroed.  They return a pointer to the allocated block,
// or NULL if out of memory.
extern void *memAlloc(memhdr_t *mem, size_t size, bool zero);
#define mem_alloc(mem, size) memAlloc(mem, size, false)
#define mem_calloc(mem, count, size) memAlloc(mem, (count) * (size), true)

// Free the memory allocator and all memory allocated with it.
extern void mem_free(memhdr_t *mem);

// Stack definitions.

typedef struct xstack_rec xstack_t;

// Macros which save some typing.
#define StackPush(s, typetag, value) {\
	if((status = xstack_push_ ## typetag(s, value)) != REG_OK)\
		return status;\
		}

#define StackPushR(s, typetag, value) {\
	if((status = xstack_push_ ## typetag(s, value)) != REG_OK)\
		goto Retn;\
		}

extern xstack_t *xstack_new(int size, int max_size, int increment);
extern void xstack_free(xstack_t *s);
extern int xstack_num_objects(xstack_t *s);
extern int xstack_push_voidptr(xstack_t *s, void *ptr);
extern int xstack_push_int(xstack_t *s, int i);
extern void *xstack_pop_voidptr(xstack_t *s);
extern int xstack_pop_int(xstack_t *s);

#ifdef __cplusplus
	}
#endif
#endif
