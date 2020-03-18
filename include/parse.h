// parse.h - Regular expression parsing definitions, including those for abstract syntax trees (AST).
//
// (c) Copyright 2020 Richard W. Marinelli
//
// This work is based on TRE ver. 0.7.5 (c) Copyright 2001-2006 Ville Laurikari <vl@iki.fi> and is licensed
// under the GNU Lesser General Public License (LGPLv3).  To view a copy of this license, see the "License.txt"
// file included with this distribution or visit http://www.gnu.org/licenses/lgpl-3.0.en.html.

#ifndef xre_parse_h
#define xre_parse_h

#include "mem.h"
#include "internal.h"

#ifdef __cplusplus
extern "C" {
#endif

// Tagging object.
typedef struct {
	int position;
	int code_min;
	int code_max;
	int *tags;
	int assertions;
	xctype_t class;
	xctype_t *neg_classes;
	int backref;
	params_t *params;
	} pos_and_tags_t;

// The different AST node types.
typedef enum {
	AST_Lit,
	AST_Cat,
	AST_Iter,
	AST_Union
	} ast_type_t;

// Special subtypes of AST_Lit.
#define LitEmpty		-1	// Empty leaf (denotes empty string).
#define LitAssert		-2	// Assertion leaf.
#define LitTag			-3	// Tag leaf.
#define LitBackref		-4	// Back reference leaf.
#define LitParam		-5	// Approximate parameters leaf.

#define IsSpecial(x) ((x)->code_min < 0)
#define IsEmpty(x) ((x)->code_min == LitEmpty)
#define IsAssert(x) ((x)->code_min == LitAssert)
#define IsTag(x) ((x)->code_min == LitTag)
#define IsBackref(x) ((x)->code_min == LitBackref)
#define IsParam(x) ((x)->code_min == LitParam)

// A generic AST node.  All AST nodes consist of this node at the top level with 'obj' pointing to the actual content.
typedef struct {
	ast_type_t type;		// Type of node.
	void *obj;			// Pointer to actual node.
	int nullable;			// Node matches the empty string.
	int submatch_id;
	int num_submatches;
	int num_tags;
	pos_and_tags_t *firstpos;
	pos_and_tags_t *lastpos;
	} ast_node_t;

// A "literal" node.  These are created for assertions, back references, tags, matching parameter settings, and all expressions
// that match one character.
typedef struct {
	int code_min;
	int code_max;
	int position;
	union {
		xctype_t class;
		params_t *params;
		} u;
	xctype_t *neg_classes;
	} ast_lit_t;

// A "catenation" node.  These are created when two REs are concatenated.  If there is more than one subexpressions in sequence,
// the 'left' part holds all but the last, and 'right' part holds the last subexpression.  (Catenation is left associative.)
typedef struct {
	ast_node_t *left;
	ast_node_t *right;
	} ast_cat_t;

// An "iteration" node.  These are created for the "*", "+", "?", and "{m, n}" operators.
typedef struct {
	ast_node_t *arg;			// Subexpression to match.
	int min;				// Minimum number of consecutive matches.
	int max;				// Maximum number of consecutive matches.
	char minimal;				// If 0, match as many characters as possible; if 1 match as few as possible.
						// Note that this does not always mean the same thing as matching as many/few
						// repetitions as possible.
	params_t *params;			// Approximate matching parameters (or NULL).
	} ast_iter_t;

// An "union" node.  These are created for the "|" operator.
typedef struct {
	ast_node_t *left;
	ast_node_t *right;
	} ast_union_t;

extern ast_node_t *ast_newNode(memhdr_t *mem, ast_type_t type, size_t size);
extern ast_node_t *ast_newLit(memhdr_t *mem, int code_min, int code_max, int position);
extern ast_node_t *ast_newIter(memhdr_t *mem, ast_node_t *arg, int min, int max, int minimal);
extern ast_node_t *ast_newUnion(memhdr_t *mem, ast_node_t *left, ast_node_t *right);
extern ast_node_t *ast_newCat(memhdr_t *mem, ast_node_t *left, ast_node_t *right);

#ifdef XRE_Debug
extern void printAST(ast_node_t *tree, const char *label);
#endif

// Parse context object.
typedef struct {
	memhdr_t *mem;				// Memory allocator.  The AST is allocated using this.
	xstack_t *stack;			// Stack used for keeping track of regexp syntax.
	ast_node_t *result;			// The parse result.
	const xchar_t *re;			// The regexp to parse and its length.
	int len;
	const xchar_t *re_start;		// The first character of the entire regexp.
	const xchar_t *re_end;			// The first character after the end of the regexp.
	int submatch_id;			// Current submatch ID.
	int position;				// Current position.
	int max_backref;			// The highest back reference, or zero if none seen so far.
	int cflags;				// Compilation flags.
	int pflags;				// Property flags.  PropHaveApprox is set during parsing if applicable.
	bool keepfirst;				// Capture submatch at top level?
	int cur_max;				// The CUR_MAX in use.
	} parse_ctx_t;

// Parses a wide character regexp pattern into a syntax tree.  Handles ERE syntax, including the XRE extensions.
extern int parsePat(parse_ctx_t *ctx);

#ifdef __cplusplus
	}
#endif
#endif
