// mem.c - XRE memory allocator and stack management.
//
// (c) Copyright 2022 Richard W. Marinelli
//
// This work is based on TRE ver. 0.7.5 (c) Copyright 2001-2006 Ville Laurikari <vl@iki.fi> and is licensed
// under the GNU Lesser General Public License (LGPLv3).  To view a copy of this license, see the "License.txt"
// file included with this distribution or visit http://www.gnu.org/licenses/lgpl-3.0.en.html.

// This memory allocator is for allocating small memory blocks efficiently in terms of memory overhead and execution speed.
// The allocated blocks cannot be freed individually, only all at once.  There can be multiple allocators, though.

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "xre.h"
#include "internal.h"
#include "mem.h"

#ifdef __cplusplus
extern "C" {
#endif

// Return a new memory allocator, or NULL if out of memory.
memhdr_t *mem_new(void) {

	return (memhdr_t *) calloc(1, sizeof(memhdr_t));
	}

// Free the memory allocator and all memory allocated with it.
void mem_free(memhdr_t *mem) {
	memlist_t *blkn, *blk = mem->blocks;

	while(blk != NULL) {
		free(blk->data);
		blkn = blk->next;
		free(blk);
		blk = blkn;
		}
	free(mem);
	}

// Allocate a block of 'size' bytes from 'mem' and return a pointer to the allocated block, or NULL if malloc() failed.
void *memAlloc(memhdr_t *mem, size_t size, bool zero) {
	void *ptr;

	if(mem->failed) {
		DPrintf((stderr, "memAlloc: oops, called after failure!\n"));
		return NULL;
		}

	if(mem->n < size) {

		// We need more memory than is available in the current block.  Allocate a new one.
		memlist_t *blk;
		size_t blkSize = (size * 8 > MemBlockSize) ? size * 8 : MemBlockSize;
		DPrintf((stderr, "memAlloc: allocating new %lu byte block\n", blkSize));
		if((blk = malloc(sizeof(*blk))) == NULL)
			goto Fail;
		if((blk->data = malloc(blkSize)) == NULL) {
			free(blk);
			goto Fail;
			}
		blk->next = NULL;
		if(mem->current != NULL)
			mem->current->next = blk;
		if(mem->blocks == NULL)
			mem->blocks = blk;
		mem->current = blk;
		mem->ptr = blk->data;
		mem->n = blkSize;
		}

	// Make sure the next pointer will be aligned.
	size += AlignBytes(mem->ptr + size, unsigned long);

	// Allocate from current block.
	ptr = mem->ptr;
	mem->ptr += size;
	mem->n -= size;

	// Set to zero if needed.
	if(zero)
		memset(ptr, 0, size);

	return ptr;
Fail:
	mem->failed = true;
	return NULL;
	}

/*** Stack management routines ***/

// Create a new stack object.  'size' is initial size in bytes, 'max_size' is maximum size, and 'increment' specifies how much
// more space will be allocated with realloc() if all space gets used up.  Return the stack object, or NULL if out of memory.
xstack_t *xstack_new(int size, int max_size, int increment) {
	xstack_t *s;

	if((s = malloc(sizeof(*s))) != NULL) {
		if((s->stack = malloc(sizeof(*s->stack) * size)) == NULL) {
			free(s);
			return NULL;
			}
		s->size = size;
		s->max_size = max_size;
		s->increment = increment;
		s->idx = 0;
		}
	return s;
	}

// Free the given stack object.
void xstack_free(xstack_t *s) {

	free(s->stack);
	free(s);
	}

// Return the current number of objects in the given stack.
int xstack_num_objects(xstack_t *s) {

	return s->idx;
	}

// Push 'value' onto given stack and return status.  Stack is extended if necessary.
static int xstack_push(xstack_t *s, union xstack_item value) {

	if(s->idx < s->size)
		s->stack[s->idx++] = value;
	else if(s->size >= s->max_size) {
		DPrintf((stderr, "xstack_push: stack full!\n"));
		return REG_ESPACE;
		}
	else {
		union xstack_item *newBuffer;
		int newSize;

		DPrintf((stderr, "xstack_push: trying to realloc more space\n"));
		newSize = s->size + s->increment;
		if(newSize > s->max_size)
			newSize = s->max_size;
		if((newBuffer = realloc(s->stack, sizeof(*newBuffer) * newSize)) == NULL) {
			DPrintf((stderr, "xstack_push: realloc failed!\n"));
			return REG_ESPACE;
			}
		DPrintf((stderr, "xstack_push: realloc succeeded.\n"));
		assert(newSize > s->size);
		s->size = newSize;
		s->stack = newBuffer;
		xstack_push(s, value);
		}
	return 0;
	}

// Push a void pointer onto given stack and return status.
int xstack_push_voidptr(xstack_t *s, void *ptr) {
	union xstack_item item;

	item.ptr = ptr;
	return xstack_push(s, item);
	}

// Push an integer onto given stack and return status.
int xstack_push_int(xstack_t *s, int i) {
	union xstack_item item;

	item.i = i;
	return xstack_push(s, item);
	}

// Pop a void pointer from given stack and return it.  The stack must not be empty.
void *xstack_pop_voidptr(xstack_t *s) {

	return s->stack[--s->idx].ptr;
	}

// Pop an integer from given stack and return it.  The stack must not be empty.
int xstack_pop_int(xstack_t *s) {

	return s->stack[--s->idx].i;
	}

#ifdef __cplusplus
	}
#endif
