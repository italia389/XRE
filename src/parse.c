// parse.c - Regular expression parsing and abstract syntax tree (AST) routines.
//
// (c) Copyright 2022 Richard W. Marinelli
//
// This work is based on TRE ver. 0.7.5 (c) Copyright 2001-2006 Ville Laurikari <vl@iki.fi> and is licensed
// under the GNU Lesser General Public License (LGPLv3).  To view a copy of this license, see the "License.txt"
// file included with this distribution or visit http://www.gnu.org/licenses/lgpl-3.0.en.html.

#ifdef XRE_Debug
#include <stdio.h>
#endif
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <limits.h>

#include "xre.h"
#include "parse.h"

#ifdef __cplusplus
extern "C" {
#endif

// AST routines.

ast_node_t *ast_newNode(memhdr_t *mem, ast_type_t type, size_t size) {
	ast_node_t *node;

	if((node = mem_calloc(mem, 1, sizeof(*node))) == NULL || (node->obj = mem_calloc(mem, 1, size)) == NULL)
		return NULL;
	node->type = type;
	node->nullable = -1;
	node->submatch_id = -1;

	return node;
	}

ast_node_t *ast_newLit(memhdr_t *mem, int code_min, int code_max, int position) {
	ast_node_t *node;
	ast_lit_t *lit;

	if((node = ast_newNode(mem, AST_Lit, sizeof(ast_lit_t))) == NULL)
		return NULL;
	lit = node->obj;
	lit->code_min = code_min;
	lit->code_max = code_max;
	lit->position = position;

	return node;
	}

ast_node_t *ast_newIter(memhdr_t *mem, ast_node_t *arg, int min, int max, int minimal) {
	ast_node_t *node;
	ast_iter_t *iter;

	if((node = ast_newNode(mem, AST_Iter, sizeof(ast_iter_t))) == NULL)
		return NULL;
	iter = node->obj;
	iter->arg = arg;
	iter->min = min;
	iter->max = max;
	iter->minimal = minimal;
	node->num_submatches = arg->num_submatches;

	return node;
	}

ast_node_t *ast_newUnion(memhdr_t *mem, ast_node_t *left, ast_node_t *right) {
	ast_node_t *node;

	if((node = ast_newNode(mem, AST_Union, sizeof(ast_union_t))) == NULL)
		return NULL;
	((ast_union_t *) node->obj)->left = left;
	((ast_union_t *) node->obj)->right = right;
	node->num_submatches = left->num_submatches + right->num_submatches;

	return node;
	}

ast_node_t *ast_newCat(memhdr_t *mem, ast_node_t *left, ast_node_t *right) {
	ast_node_t *node;

	if((node = ast_newNode(mem, AST_Cat, sizeof(ast_cat_t))) == NULL)
		return NULL;
	((ast_cat_t *) node->obj)->left = left;
	((ast_cat_t *) node->obj)->right = right;
	node->num_submatches = left->num_submatches + right->num_submatches;

	return node;
	}

#ifdef XRE_Debug

void printParams(bool printLabel, regaparams_t *params) {
	static char *labels[] = {"costs", "edits", NULL};

	if(params != NULL) {
		int n;
		int *p = (int *) params;
		char **label = labels;
		char *s;

		// params: costs [unset, unset, unset, 2147483647], edits [0, 0, 1, 2147483647]
		if(printLabel)
			fputs("params: ", stderr);
		for(;;) {
			fputs(*label++, stderr);
			s = " [";
			n = 4;
			do {
				fputs(s, stderr);
				switch(*p) {
					case ParamUnset:
						fputs("unset", stderr);
						break;
					case ParamDefault:
						fputs("default", stderr);
						break;
					default:
						fprintf(stderr, "%d", *p);
					}
				++p;
				s = ", ";
				} while(--n > 0);
			fputc(']', stderr);
			if(*label == NULL)
				break;
			fputs(s, stderr);
			}
		}
	}

static int vizchar(int c) {

	return (c >= ' ' && c <= '~') ? c : '?';
	}

static void findent(int i) {

	while(i-- > 0)
		fputc(' ', stderr);
	}

static void printNode(ast_node_t *ast, int indent) {
	int num_tags = ast->num_tags;
	ast_lit_t *lit;
	ast_iter_t *iter;

	findent(indent);
	switch(ast->type) {
		case AST_Lit:
			lit = ast->obj;
			switch(lit->code_min) {
				case LitEmpty:
					fprintf(stderr, "empty literal\n");
					break;
				case LitAssert:
					{int i;
					char *assertions[] = {"bol", "eol", "ctype", "!ctype", "bow", "eow", "wb", "!wb"};
					if(lit->code_max >= AssertLast << 1)
						assert(0);
					fprintf(stderr, "assertions: ");
					for(i = 0; (1 << i) <= AssertLast; ++i)
						if(lit->code_max & (1 << i))
							fprintf(stderr, "%s ", assertions[i]);
					fputc('\n', stderr);
					}
					break;
				case LitTag:
					fprintf(stderr, "tag %d\n", lit->code_max);
					break;
				case LitBackref:
					fprintf(stderr, "backref %d, pos %d\n", lit->code_max, lit->position);
					break;
				case LitParam:
					printParams(true, &lit->u.params->pa);
					fputc('\n', stderr);
					break;
				default:
					fprintf(stderr,
					 "literal (%d '%c', %d '%c'), pos %d, subid %d, %d tags\n",
					 lit->code_min, vizchar(lit->code_min), lit->code_max, vizchar(lit->code_max),
					 lit->position, ast->submatch_id, num_tags);
				}
			break;
		case AST_Iter:
			iter = ast->obj;
			fprintf(stderr, "iteration {%d, %d}, subid %d, %d tags, %s\n",
			 iter->min, iter->max, ast->submatch_id, num_tags, iter->minimal ? "minimal" : "greedy");
			printNode(iter->arg, indent + 2);
			break;
		case AST_Union:
			fprintf(stderr, "union, subid %d, %d tags\n", ast->submatch_id, num_tags);
			printNode(((ast_union_t *) ast->obj)->left, indent + 2);
			printNode(((ast_union_t *) ast->obj)->right, indent + 2);
			break;
		case AST_Cat:
			fprintf(stderr, "catenation, subid %d, %d tags\n", ast->submatch_id, num_tags);
			printNode(((ast_cat_t *) ast->obj)->left, indent + 2);
			printNode(((ast_cat_t *) ast->obj)->right, indent + 2);
			break;
		default:
			assert(0);
		}
	}

void printAST(ast_node_t *tree, const char *label) {

	fprintf(stderr, "-----\nAST - %s:\n", label);
	printNode(tree, 0);
	}

#endif // XRE_Debug.

// Parsing routines.

// Characters with special meaning in regexp syntax.
#define CharPipe		L'|'
#define CharLParen		L'('
#define CharRParen		L')'
#define CharLBrace		L'{'
#define CharRBrace		L'}'
#define CharLBracket		L'['
#define CharRBracket		L']'
#define CharMinus		L'-'
#define CharAsterisk		L'*'
#define CharHook		L'?'
#define CharPlus		L'+'
#define CharPeriod		L'.'
#define CharColon		L':'
#define CharEqual		L'='
#define CharComma		L','
#define CharCircumflex		L'^'
#define CharDollar		L'$'
#define CharBackslash		L'\\'
#define CharHash		L'#'
#define CharTilde		L'~'

#define MaxNegClasses		64		// Maximum number of character classes that can occur in a negated bracket
						// expression.

#define Rest(re) (int)(ctx->re_end - (re)), (re)

// Bracketed expression parsing objects.
typedef struct {
	int n;
	int size;
	ast_node_t **nodes;
	} item_list_t;
typedef struct {
	int n;
	xctype_t neg_classes[MaxNegClasses];
	} nclass_list_t;

#if !EnableWChar

// isalnum() and the rest may be macros, so wrap them to functions.
int xisalnum_func(xcint_t c) { return xisalnum(c); }
int xisalpha_func(xcint_t c) { return xisalpha(c); }

#ifdef xisascii
int xisascii_func(xcint_t c) { return xisascii(c); }
#elif 0		// Wide characters enabled (EnableWChar); therefore, this is not needed.
int xisascii_func(xcint_t c) { return !(c & ~0177); }
#endif

int xisblank_func(xcint_t c) { return xisblank(c); }
int xiscntrl_func(xcint_t c) { return xiscntrl(c); }
int xisdigit_func(xcint_t c) { return xisdigit(c); }
int xisgraph_func(xcint_t c) { return xisgraph(c); }
int xislower_func(xcint_t c) { return xislower(c); }
int xisprint_func(xcint_t c) { return xisprint(c); }
int xispunct_func(xcint_t c) { return xispunct(c); }
int xisspace_func(xcint_t c) { return xisspace(c); }
int xisupper_func(xcint_t c) { return xisupper(c); }
int xisxdigit_func(xcint_t c) { return xisxdigit(c); }

struct mapitem {
	char *name;
	int (*func)(xcint_t);
	} xctype_map[] = {
		{"alnum", &xisalnum_func},
		{"alpha", &xisalpha_func},
#ifdef xisascii
		{"ascii", &xisascii_func},
#endif
		{"blank", &xisblank_func},
		{"cntrl", &xiscntrl_func},
		{"digit", &xisdigit_func},
		{"graph", &xisgraph_func},
		{"lower", &xislower_func},
		{"print", &xisprint_func},
		{"punct", &xispunct_func},
		{"space", &xisspace_func},
		{"upper", &xisupper_func},
		{"xdigit", &xisxdigit_func},
		{NULL, NULL}};

xctype_t xctype(const char *name) {
	struct mapitem *item = xctype_map;
	do {
		if(strcmp(name, item->name) == 0)
			return item->func;
		} while((++item)->name != NULL);
	return NULL;
	}
#endif // !EnableWChar

// Table for expanding \w, \s, etc.
typedef struct {
	const char c;
	const char *expansion;
	} Shortcut;
static const Shortcut xtab[] = {
	{'t', "\t"},
	{'n', "\n"},
	{'r', "\r"},
	{'f', "\f"},
	{'e', "\033"},

	{'a', "[[:alpha:]]"},
	{'A', "[^[:alpha:]]"},
	{'d', "[[:digit:]]"},
	{'D', "[^[:digit:]]"},
	{'h', "[[:blank:]]"},
	{'H', "[^[:blank:]]"},
	{'l', "[[:lower:]]"},
	{'L', "[^[:lower:]]"},
	{'s', "[[:space:]]"},
	{'S', "[^[:space:]]"},
	{'u', "[[:upper:]]"},
	{'U', "[^[:upper:]]"},
	{'w', "[[:alnum:]_]"},
	{'W', "[^[:alnum:]_]"},
	{'\0', NULL}};

// Expand possible shortcut character at 're' to 'buf', which is assumed to have sufficient space.  buf[0] is set to zero if
// shortcut not found.  If 'strip' is true, leading bracket is skipped during expansion and negated expansions are treated as
// "not found".
static void expandShortcut(xchar_t *buf, const xchar_t *re, bool strip) {
	const Shortcut *x = xtab;

	*buf = 0;
	do {
		if((!strip || islower(x->c)) && (xchar_t) x->c == *re) {
			const char *str = (strip && *x->expansion == '[') ? x->expansion + 1 : x->expansion;
#ifdef XRE_Debug
			fprintf(stderr, "Expanding shortcut '%c' => ", x->c);
			if(*str < ' ')
				fprintf(stderr, "char %d\n", (int) *str);
			else
				fprintf(stderr, "'%s'\n", str);
#endif
			do {
				*buf++ = (xchar_t) ((unsigned char) *str++);
				} while(*str != '\0');
			*buf = 0;
			break;
			}
		} while((++x)->c != '\0');
	}

static int newItem(memhdr_t *mem, int min, int max, item_list_t *ilist) {
	ast_node_t **array = ilist->nodes;

	// Allocate more space if necessary.
	if(ilist->n == ilist->size) {
		ast_node_t **new_items;
		DPrintf((stderr, "out of array space, n = %d\n", ilist->n));

		// If the array is already 1024 items large, give up -- there's probably an error in the regexp; e.g., not a
		// '\0' terminated string and/or missing ']'.
		if(ilist->size > 1024)
			return REG_ESPACE;
		ilist->size *= 2;
		if((new_items = realloc(array, sizeof(*ilist->nodes) * ilist->size)) == NULL)
			return REG_ESPACE;
		ilist->nodes = array = new_items;
		}

	// Add literal node to list.
	return (array[ilist->n++] = ast_newLit(mem, min, max, -1)) == NULL ? REG_ESPACE : REG_OK;
	}

// Expand a named character class to character ranges to improve performance for 8-bit character sets.
static int expandCC(memhdr_t *mem, xctype_t class, item_list_t *ilist, int cflags) {
	int status;
	xcint_t c;
	int j;
	int min = -1, max = 0;
	assert(XRE_MB_CUR_MAX == 1);

	DPrintf((stderr, "  expanding class to character ranges\n"));
	for(j = 0; j < 256; ++j) {
		c = (xcint_t) j;
		if(xisctype(c, class) || ((cflags & REG_ICASE) && (xisctype(xtolower(c), class) ||
		 xisctype(xtoupper(c), class)))) {
			if(min < 0)
				min = c;
			max = c;
			}
		else if(min >= 0) {
			DPrintf((stderr, "  range conversion %c (%d) to %c (%d)\n", min, min, max, max));
			if((status = newItem(mem, min, max, ilist)) != REG_OK)
				return status;
			min = -1;
			}
		}
	return (min >= 0) ? newItem(mem, min, max, ilist) : REG_OK;
	}

// Forward.
static int parseBracketItems(parse_ctx_t *ctx, item_list_t *ilist, nclass_list_t *nclass);

// Parse a class name of form "[:xxx:]".  *pre is assumed to point at the opening left bracket.
static int parseCC(parse_ctx_t *ctx, const xchar_t **pre, xctype_t *pclass) {
	const xchar_t *re = *pre + 2;
	const xchar_t *re1 = re;
	int len;
	char tmp_str[64];

	DPrintf((stderr, "  named class: '%.*" StrF "'\n", Rest(re)));

	// Find end of name.
	for(;;) {
		if(re1 == ctx->re_end)
			return REG_ECTYPE;
		if(*re1 == CharColon)
			break;
		++re1;
		}
	if(re1 + 1 == ctx->re_end || re1[1] != CharRBracket)
		return REG_ECTYPE;

	// Check if name is valid.
	len = Min(re1 - re, 63);
#if EnableWChar
	xchar_t tmp_wcs[64];
	wcsncpy(tmp_wcs, re, (size_t) len);
	tmp_wcs[len] = 0;
	len = wcstombs(tmp_str, tmp_wcs, 63);
#else
	strncpy(tmp_str, (const char *) re, len);
	tmp_str[len] = '\0';
#endif
	DPrintf((stderr, "  class name: %s\n", tmp_str));
	if(!(*pclass = xctype(tmp_str)))
		return REG_ECTYPE;

	// Success.
	*pre = re1 + 2;
	return REG_OK;
	}

// Parse next item inside bracketed expression, which includes a range.
static int parseItem(parse_ctx_t *ctx, item_list_t *ilist, nclass_list_t *nclass, const xchar_t **pre) {
	bool cshortcut = false;
	int min = -1, max = -1;
	const xchar_t *re = *pre;
	int status, c;
	xctype_t class = (xctype_t) 0;

	// Find next item by scanning for range endpoints, checking for illegal constructs.  A single literal, escaped
	// character, or character class name is simply the first endpoint of a "range", not followed by '-'.  After each item
	// is parsed, exactly one of the min, max, class, or cshortcut state variables is updated.
	for(;;) {
		c = *re;

		// Check for "\x".
		if(c == CharBackslash) {
			if(re + 1 == ctx->re_end)
				return REG_EBRACK;
			if(!(ctx->cflags & REG_ENHANCED))
				goto Literal;

			// Expand shortcut if possible.
			xchar_t buf[64];
			c = *++re;
			expandShortcut(buf, re, true);
			if(buf[0] == L'\0')
				goto Literal;
			if(buf[1] == L'\0') {
				c = buf[0];
				goto Literal;
				}

			// Found class shortcut.  Verify its not a range endpoint and parse expanded text.
			if(min != -1)
				return REG_ERANGE;
			parse_ctx_t subctx;
			memcpy(&subctx, ctx, sizeof(subctx));
			subctx.re = subctx.re_start = buf;
			subctx.len = xstrlen(buf);
			subctx.re_end = buf + subctx.len;
			subctx.keepfirst = false;
			if((status = parseBracketItems(&subctx, ilist, nclass)) != REG_OK)
				return status;
			++re;
			cshortcut = true;
			}

		// Check for "[:xxx:]".
		else if(re + 1 < ctx->re_end && c == CharLBracket) {
			switch(re[1]) {
				case CharPeriod:
				case CharEqual:
					return REG_ECOLLATE;
				case CharColon:
					{xctype_t newclass;
					if((status = parseCC(ctx, &re, &newclass)) != REG_OK)
						return status;
					if(min != -1)
						return REG_ERANGE;
					class = newclass;
					}
					break;
				default:
					goto Literal;
				}
			}
		else {
Literal:
			// Have literal character... save it.
			DPrintf((stderr, "  char: '%.*" StrF "'\n", Rest(re)));
			if(min == -1)
				min = c;
			else {
				// XXX - Should use collation order instead of encoding values in character ranges.
				if(min > c)
					return REG_ERANGE;
				max = c;
				}
			++re;
			}

		// One atom parsed successfully.  Check for range.
		if(re + 1 >= ctx->re_end || re[0] != CharMinus || re[1] == CharRBracket)
			break;
		if(class || cshortcut || max != -1)
			return REG_ERANGE;
		++re;
		}

	// Item parsed successfully.  Set 'max' if needed.
	if(min != -1) {
		if(max == -1)
			max = min;
#ifdef XRE_Debug
		else
			fprintf(stderr, "  range %c (%d) to %c (%d)\n", min, min, max, max);
#endif
		}
	else if(class) {
		min = 0;
		max = XRE_CHAR_MAX;

		// Optimize named character classes for 8-bit character sets.
		if(ctx->cur_max == 1) {
			if((status = expandCC(ctx->mem, class, ilist, ctx->cflags)) != REG_OK)
				return status;
			goto Retn;
			}

		// Add class to negated class list if needed.
		if(nclass != NULL) {
			if(nclass->n == MaxNegClasses)
				return REG_ESPACE;
			nclass->neg_classes[nclass->n++] = class;
			}
		}

	if(!cshortcut) {
		// Add item to list.
		if((status = newItem(ctx->mem, min, max, ilist)) != REG_OK)
			return status;
		((ast_lit_t *) ilist->nodes[ilist->n - 1]->obj)->u.class = class;

		// Add opposite-case counterpoints if REG_ICASE flag set.  This is broken if there are more than two "same"
		// characters.
		if(ctx->cflags & REG_ICASE && !class) {
			xcint_t cmin, cmax;
			DPrintf((stderr, "adding opposite-case counterpoints\n"));
			do {
				if(xislower(min)) {
					cmin = cmax = xtoupper(min++);
					while(min <= max && xislower(min) && xtoupper(min) == cmax + 1)
						cmax = xtoupper(min++);
					goto NewItem;
					}
				else if(xisupper(min)) {
					cmin = cmax = xtolower(min++);
					while(min <= max && xisupper(min) && xtolower(min) == cmax + 1)
						cmax = xtolower(min++);
NewItem:
					if((status = newItem(ctx->mem, cmin, cmax, ilist)) != REG_OK)
						return status;
					}
				else
					++min;
				} while(min <= max);
			}
		}
Retn:
	*pre = re;
	return REG_OK;
	}

// Parse the contents of a bracketed expression, beginning just past the '[' and '^', if any.
static int parseBracketItems(parse_ctx_t *ctx, item_list_t *ilist, nclass_list_t *nclass) {
	int status;
	const xchar_t *re = ctx->re;

	DPrintf((stderr, "parseBracketItems: parsing '%.*" StrF "', len %d\n", Rest(re), (int)(ctx->re_end - re)));

	// Build an array of the items in the bracketed expression.
	for(;;) {
		// End of pattern?
		if(re == ctx->re_end)
			return REG_EBRACK;

		// End of bracketed expression?
		if(*re == CharRBracket && re > ctx->re) {
			DPrintf((stderr, "parseBracketItems: done: '%.*" StrF "'\n", Rest(re)));
			++re;
			break;
			}

		// Parse next token.
		if((status = parseItem(ctx, ilist, nclass, &re)) != REG_OK)
			return status;
		}

	ctx->re = re;
	return REG_OK;
	}

// Copy negated classes to a node and return status.
static int copyNeg(parse_ctx_t *ctx, ast_lit_t *lnode, nclass_list_t *nlist) {
	xctype_t *src, *srcz, *dest;

	if((lnode->neg_classes = mem_alloc(ctx->mem, (sizeof(*lnode->neg_classes) * (nlist->n + 1)))) == NULL)
		return REG_ESPACE;
	dest = lnode->neg_classes;
	srcz = (src = nlist->neg_classes) + nlist->n;
	while(src < srcz)
		*dest++ = *src++;
	*dest = (xctype_t) 0;
	return REG_OK;
	}

static int compareItems(const void *a, const void *b) {
	const ast_node_t *node_a = *((ast_node_t * const *) a);
	const ast_node_t *node_b = *((ast_node_t * const *) b);
	ast_lit_t *l_a = node_a->obj, *l_b = node_b->obj;
	int a_min = l_a->code_min, b_min = l_b->code_min;

	return (a_min < b_min) ? -1 : (a_min > b_min) ? 1 : 0;
	}

static int parseBracket(parse_ctx_t *ctx, ast_node_t **result) {
	ast_node_t *node = NULL;
	int status = REG_OK;
	ast_node_t **pitem, **pitemz, *u, *n;
	nclass_list_t *nclass = NULL;
	int curr_max, curr_min;
	item_list_t items = {0, 32, NULL};
	nclass_list_t nclasses = {0, {0}};

	// Start off with an array of 'size' elements.
	if((items.nodes = malloc(sizeof(*items.nodes) * items.size)) == NULL)
		return REG_ESPACE;

	// Parse the bracketed expression.
	if(*ctx->re == CharCircumflex) {
		DPrintf((stderr, "parseBracket: negate: '%.*" StrF "'\n", Rest(ctx->re)));
		nclass = &nclasses;
		++ctx->re;
		}
	if((status = parseBracketItems(ctx, &items, nclass)) != REG_OK)
		goto Retn;

	// Sort the array if we need to negate it.
	if(nclass != NULL)
		qsort(items.nodes, (unsigned) items.n, sizeof(*items.nodes), compareItems);

	// Build a union of the items in the array, negated if necessary.
	curr_max = curr_min = 0;
	for(pitemz = (pitem = items.nodes) + items.n; pitem < pitemz; ++pitem) {
		ast_lit_t *lnode = (*pitem)->obj;
		int min = lnode->code_min;
		int max = lnode->code_max;

		DPrintf((stderr, "item: %d - %d, class %ld, curr_max = %d\n", min, max, (long) lnode->u.class, curr_max));

		if(nclass != NULL) {
			if(min < curr_max) {

				// Overlap.
				curr_max = Max(max + 1, curr_max);
				DPrintf((stderr, "overlap, curr_max = %d\n", curr_max));
				lnode = NULL;
				}
			else {
				// No overlap.
				curr_max = min - 1;
				if(curr_max >= curr_min) {
					DPrintf((stderr, "no overlap\n"));
					lnode->code_min = curr_min;
					lnode->code_max = curr_max;
					}
				else {
					DPrintf((stderr, "no overlap, zero room\n"));
					lnode = NULL;
					}
				curr_min = curr_max = max + 1;
				}
			}

		// Skip node completely if negated expression, range is a single newline, REG_NEWLINE flag set, and REG_ANY
		// flag not set so that newline is excluded.
		if(lnode != NULL && (nclass == NULL || lnode->code_min != L'\n' || lnode->code_max != L'\n' ||
		 (ctx->cflags & (REG_NEWLINE | REG_ANY)) != REG_NEWLINE)) {

			// Not newline special case... keep node.
			DPrintf((stderr, "creating item %d - %d\n", (int) lnode->code_min, (int) lnode->code_max));
			lnode->position = ctx->position;
			if(nclasses.n == 0)
				lnode->neg_classes = NULL;
			else if((status = copyNeg(ctx, lnode, nclass)) != REG_OK)
				goto Retn;

			if(node == NULL)
				node = *pitem;
			else {
				u = ast_newUnion(ctx->mem, node, *pitem);
				if(u == NULL) {
					status = REG_ESPACE;
					goto Retn;
					}
				node = u;
				}

			// Now check for newline in range: if negated expression, range includes newline, REG_NEWLINE flag set,
			// and REG_ANY flag not set, adjust node so that newline is excluded.
			if(nclass != NULL && lnode->code_min <= L'\n' && lnode->code_max >= L'\n' &&
			 (ctx->cflags & (REG_NEWLINE | REG_ANY)) == REG_NEWLINE) {
				DPrintf((stderr, "removing newline from item %d - %d\n",
				 (int) lnode->code_min, (int) lnode->code_max));
				if(lnode->code_min == L'\n')
					++lnode->code_min;
				else if(lnode->code_max == L'\n')
					--lnode->code_max;
				else {
					// Newline inside range... split it.  Use existing node for chars preceding newline.
					ast_lit_t *l2;
					int oldmax = lnode->code_max;
					lnode->code_max = L'\n' - 1;

					// Make new node for chars following newline.
					if((n = ast_newLit(ctx->mem, L'\n' + 1, oldmax, ctx->position)) == NULL) {
						status = REG_ESPACE;
						goto Retn;
						}
					l2 = n->obj;
					if(nclasses.n == 0)
						l2->neg_classes = NULL;
					else if((status = copyNeg(ctx, l2, nclass)) != REG_OK)
						goto Retn;

					if(node == NULL)
						node = n;
					else {
						u = ast_newUnion(ctx->mem, node, n);
						if(u == NULL) {
							status = REG_ESPACE;
							goto Retn;
							}
						node = u;
						}
					}
				}
			}
		}

	if(nclass != NULL) {
		DPrintf((stderr, "final: creating %d - %d\n", curr_min, (int) XRE_CHAR_MAX));
		if((n = ast_newLit(ctx->mem, curr_min, XRE_CHAR_MAX, ctx->position)) == NULL) {
			status = REG_ESPACE;
			goto Retn;
			}
		else {
			ast_lit_t *lnode = n->obj;
			if(nclasses.n == 0)
				lnode->neg_classes = NULL;
			else if((status = copyNeg(ctx, lnode, nclass)) != REG_OK)
				goto Retn;

			if(node == NULL)
				node = n;
			else {
				u = ast_newUnion(ctx->mem, node, n);
				if(u == NULL) {
					status = REG_ESPACE;
					goto Retn;
					}
				node = u;
				}
			}
		}
#ifdef XRE_Debug
	printAST(node, "post-parse-bracket");
#endif

Retn:
	free(items.nodes);
	++ctx->position;
	*result = node;
	return status;
	}

// Parse an unsigned decimal integer and return the converted value; otherwise, return -1 if no digits present.
static int parseInt(const xchar_t **rep, const xchar_t *re_end) {
	int num = 0;
	const xchar_t *re = *rep;
	if(*re < L'0' || *re > L'9')
		return -1;
	do {
		num = num * 10 + (*re - L'0');
		} while(++re < re_end && *re >= L'0' && *re <= L'9');
	*rep = re;
	return num;
	}

// Parse items between braces "{...}", which may be repeat count(s) and/or approximate matching parameters.  Syntax is:
//	"{" [ m [ ", " n ] ]
//		m	minimum repeat count.
//		n	maximum repeat count.
//	[ ";" { "+" | "-" | "#" | "~" } [ n ] ... ]
//		+n	maximum insertion count.
//		-n	maximum deletion count.
//		#n	maximum substitution count.
//		~n	maximum number of edits.
//		Any of +, -, #, or ~ not followed by a number means that the maximum count/edits is infinite.
//	[ ";" n { "i" | "d" | "s" } [ "+" ] ... [ "<=" n ] ] "}"
//		This is an equation of the form:
//			ni+nd+ns<=c
//		where i, d, and s are literal characters, and n and c are integers.  The equation sets individual costs and/or
//		the cost limit to non-default values:
//			ni	cost of an insertion.
//			nd	cost of a deletion.
//			ns	cost of a substitution.
//			c	maximum cost.
// Individual counts and costs default to 1.
static int parseBrace(parse_ctx_t *ctx, ast_node_t **result) {
	int min, max, n;
	const xchar_t *re = ctx->re;
	const xchar_t *start;
	int minimal = (ctx->cflags & REG_UNGREEDY) == REG_UNGREEDY;
#if EnableApprox
	int *pitem;
	int maxIns, maxDel, maxSubst, maxEdit;
	int costIns, costDel, costSubst, maxCost;
	unsigned short pass;
	unsigned short costsSet = 0;
	unsigned short costTotalSet = 0;
	unsigned short editsSet = 0;
	unsigned short editTotalSet = 0;
#endif
#ifdef XRE_Debug
	char *str;
#endif
	min = max = -1;
#if EnableApprox
	maxIns = maxDel = maxSubst = maxEdit = ParamUnset;
	costIns = costDel = costSubst = maxCost = ParamUnset;
#endif
	// Parse first token, which could be a number or punctuation character (if approximate matching enabled).
	start = re;
	if(re < ctx->re_end) {
#if EnableApprox
		if((n = parseInt(&re, ctx->re_end)) == -1) {
			if(ctx->cflags & REG_APPROX) {
				pass = 1;
				goto CheckEdits;
				}
			goto ERetn;
			}

		// Found an integer.  Beginning of cost equation?
		if(re == ctx->re_end)
			goto ERetn;
		if(ctx->cflags & REG_APPROX)
			switch(*re) {
				case L'i':
				case L'd':
				case L's':
					pass = 1;
					goto CostEQ;
				}
#else
		if((n = parseInt(&re, ctx->re_end)) == -1 || re == ctx->re_end)
			goto ERetn;
#endif
		// Have minimum repetition count.
		DPrintf((stderr, "parse_brace: min count: '%.*" StrF "'\n", Rest(start)));
		min = n;

		// Parse comma and second number (maximum repetition count).
		if(*re == CharComma) {
			++re;
			DPrintf((stderr, "parse_brace: max count: '%.*" StrF "'\n", Rest(re)));
			if(re == ctx->re_end)
				goto ERetn;
			max = parseInt(&re, ctx->re_end);	// Will be -1 if number not present.
			}
		else
			// One number, no comma.
			max = min;

		// Check that the repeat counts are sane.
		if((max >= 0 && min > max) || max > RE_DUP_MAX)
			goto ERetn;
		}
#if EnableApprox
	// Parse approximate match settings.  Need to keep track of each 'pass' through ';' delimiters for error checking.
	if(ctx->cflags & REG_APPROX) {
		pass = 0;
		while(re + 1 < ctx->re_end && *re == L';') {
			++pass;
			start = ++re;
			if((n = parseInt(&re, ctx->re_end)) >= 0) {

				// Have integer after ';'.  Parse cost equation term(s).
				if(costsSet || costTotalSet)
					goto ERetn;
CostEQ:
				costsSet = pass;
				for(;;) {
					if(re == ctx->re_end)
						goto ERetn;
					switch(*re) {
						case L'i':	// Insert cost.
#ifdef XRE_Debug
							str = "ins";
#endif
							pitem = &costIns;
							goto SetCost;
						case L'd':	// Delete cost.
#ifdef XRE_Debug
							str = "del";
#endif
							pitem = &costDel;
							goto SetCost;
						case L's':	// Substitute cost.
#ifdef XRE_Debug
							str = "subst";
#endif
							pitem = &costSubst;
SetCost:
							DPrintf((stderr, "parse_brace: %s cost: '%.*" StrF "'\n", str,
							 Rest(start)));
							if(*pitem != ParamUnset)
								goto ERetn;
							*pitem = n;
							break;
						default:
							goto ERetn;
						}
					if(++re == ctx->re_end)
						goto ERetn;
					if(*re == L'<')
						goto ParseCostTotal;
					if(*re != L'+')
						break;
					start = ++re;
					if(re == ctx->re_end || (n = parseInt(&re, ctx->re_end)) == -1)
						goto ERetn;
					}
				}
			else {
				// No integer after ';'.  Parse edit limits, cost total, or best match symbol.
CheckEdits:
				for(;;) {
					switch(*re) {
						case CharPlus:  // Maximum number of insertions.
#ifdef XRE_Debug
							str = "ins";
#endif
							pitem = &maxIns;
							goto SetMax;
						case CharMinus: // Maximum number of deletions.
#ifdef XRE_Debug
							str = "del";
#endif
							pitem = &maxDel;
							goto SetMax;
						case CharHash:  // Maximum number of substitutions.
#ifdef XRE_Debug
							str = "subst";
#endif
							pitem = &maxSubst;
							goto SetMax;
						case CharTilde: // Maximum number of edits.
#ifdef XRE_Debug
							str = "edit";
#endif
							pitem = &maxEdit;
SetMax:
							DPrintf((stderr, "parse_brace: %s limit: '%.*" StrF "'\n", str,
							 Rest(re)));
							if(*pitem != ParamUnset || (editsSet && editsSet != pass) ||
							 (editTotalSet && editTotalSet != pass))
								goto ERetn;
							if(*re == CharTilde)
								editTotalSet = pass;
							else
								editsSet = pass;
							if(++re == ctx->re_end)
								goto NoBrace;
							n = parseInt(&re, ctx->re_end);
							*pitem = (n < 0) ? INT_MAX : n;
							break;
						case L'<':
ParseCostTotal:
							if(++re == ctx->re_end)
								goto NoBrace;
							if(*re != L'=')
								goto ERetn;
							DPrintf((stderr, "parse_brace: max cost: '%.*" StrF "'\n", Rest(re)));
							if(maxCost != ParamUnset || (costsSet && costsSet != pass) ||
							 (editsSet && editsSet == pass) ||
							 (editTotalSet && editTotalSet == pass))
								goto ERetn;
							if(++re == ctx->re_end)
								goto NoBrace;
							n = parseInt(&re, ctx->re_end);
							maxCost = (n < 0) ? INT_MAX : n;
							costTotalSet = pass;
							goto NextSym;
						default:
							if(re == start)
								goto ERetn;
							goto NextSym;	// Closing brace or error.
						}
					}
				}
NextSym:;
			}
		}
#endif // EnableApprox

	// Missing '}'?
	if(re == ctx->re_end || *re != CharRBrace)
#if EnableApprox
NoBrace:
#endif
		return REG_EBRACE;

	// Empty contents of '{ }'?
	if(re == ctx->re)
		goto ERetn;

	// Parse trailing '?' marking minimal repetition.
	if(++re < ctx->re_end) {
		switch(*re) {
			case CharHook:
				if(ctx->cflags & REG_ENHANCED) {
					minimal = !(ctx->cflags & REG_UNGREEDY);
					++re;
					break;
					}
				// Fall through.
			case CharAsterisk:
			case CharPlus:
				// These are reserved for future extensions.
				return REG_BADRPT;
			}
		}

	// Create the AST node(s).
#if EnableApprox
	if(min < 0)
		// Only approximate parameters set, no repetitions.
		min = max = 1;
#endif
	if((*result = ast_newIter(ctx->mem, *result, min, max, minimal)) == NULL)
		return REG_ESPACE;

#if EnableApprox
	// If approximate matching parameters are set, add them to the iteration node.
	if(costTotalSet || costsSet || editTotalSet || editsSet) {
		params_t *params;
		ast_iter_t *iter = (*result)->obj;

		int *iparams[] = {
			&maxIns, &maxDel, &maxSubst, &costIns, &costDel, &costSubst, NULL};
		int **p = iparams;
		do {
			if(**p == ParamUnset)
				**p = 1;
			} while(*++p != NULL);
		if(maxEdit == ParamUnset)
			maxEdit = Max(Max(maxIns, maxDel), maxSubst);
		if(maxCost == ParamUnset)
			maxCost = INT_MAX;

		ctx->pflags |= PropHaveApprox;
		if((params = mem_alloc(ctx->mem, sizeof(*params))) == NULL)
			return REG_ESPACE;
		params->pa.cost_ins = costIns;
		params->pa.cost_del = costDel;
		params->pa.cost_subst = costSubst;
		params->pa.max_cost = maxCost;
		params->pa.max_ins = maxIns;
		params->pa.max_del = maxDel;
		params->pa.max_subst = maxSubst;
		params->pa.max_edit = maxEdit;
		params->depth = ParamUnset;
		iter->params = params;
		}
#ifdef XRE_Debug
	fprintf(stderr, "parse_brace: min %d, max %d, ", min, max);
	printParams(false, &((ast_iter_t *) (*result)->obj)->params->pa);
	fputc('\n', stderr);
#endif
#else
	DPrintf((stderr, "parse_brace: min %d, max %d\n", min, max));
#endif
	ctx->re = re;
	return REG_OK;
ERetn:
	return REG_BADBR;
	}

// Decode a non-empty hexadecimal value, possibly enclosed in braces.  Set *pval to result if successful and return REG_OK;
// otherwise, return an error.
static int hexnum(parse_ctx_t *ctx, int maxDigits, long *pval) {
	char wkbuf[maxDigits + 1], *str = wkbuf;

	for(;;) {
		if(ctx->re == ctx->re_end) {
			if(maxDigits > 2)
				return REG_EBRACE;
			break;
			}
		if(*ctx->re == CharRBrace && maxDigits > 2) {
			++ctx->re;
			break;
			}
		if(str - wkbuf == maxDigits)
			goto Done;
		if(xisxdigit(*ctx->re))
			*str++ = (char) *ctx->re++;
		else
			break;
		}
	if(str == wkbuf)
		return REG_EHEX;
Done:
	*str = '\0';
	*pval = strtol(wkbuf, NULL, 16);
	return REG_OK;
	}

typedef enum {
	ParseRE = 0,
	ParseAtom,
	ParseMarkForSubmatch,
	ParseBranch,
	ParsePiece,
	ParseCatenation,
	ParsePostCatenation,
	ParseUnion,
	ParsePostUnion,
	ParsePostfix,
	ParseRestoreCFlags
	} parse_re_stack_symbol_t;

int parsePat(parse_ctx_t *ctx) {
	ast_node_t *result = NULL;
	int status;
	xstack_t *stack = ctx->stack;
	int bottom = xstack_num_objects(stack);
	int depth = 0;
	int temp_cflags = 0;
	char *str;

	DPrintf((stderr, "parsePat: parsing '%.*" StrF "', len %d\n", ctx->len, ctx->re, ctx->len));
	if(ctx->len == 0)
		return REG_EMPTY;
	if(ctx->keepfirst) {
		StackPush(stack, int, ctx->submatch_id);
		StackPush(stack, int, ParseMarkForSubmatch);
		++ctx->submatch_id;
		}
	StackPush(stack, int, ParseRE);
	ctx->re_start = ctx->re;
	ctx->re_end = ctx->re + ctx->len;

	// The following is basically just a recursive descent parser.  An explicit stack is used instead of recursive functions
	// mostly because of two reasons: compatibility with systems which have an overflowable call stack, and efficiency (both
	// in lines of code and speed).
	while(xstack_num_objects(stack) > bottom) {
		switch(xstack_pop_int(stack)) {
			case ParseRE:
				// Parse a full regexp.  A regexp is one or more branches, separated by the union operator '|'.
				if(!(ctx->cflags & REG_LITERAL))
					StackPush(stack, int, ParseUnion);
				StackPush(stack, int, ParseBranch);
				break;
			case ParseBranch:
				// Parse a branch.  A branch is one or more pieces, concatenated. A piece is an atom possibly
				// followed by a postfix operator.
				if(!(ctx->cflags & REG_LITERAL)) {
					if(ctx->re == ctx->re_end)
						return REG_EMPTY;
					else {
						switch(*ctx->re) {
							case CharAsterisk:
							case CharPlus:
							case CharHook:
							case CharLBrace:
								return REG_BADRPT;
							case CharPipe:
								return REG_EMPTY;
							case CharRParen:
								if(depth > 0 && ctx->re[-1] != CharLParen)
									return REG_EMPTY;
							}
						}
					}
				StackPush(stack, int, ParseCatenation);
				StackPush(stack, int, ParsePiece);
				break;
			case ParsePiece:
				// Parse a piece.  A piece is an atom possibly followed by one or more postfix operators.
				if(!(ctx->cflags & REG_LITERAL))
					StackPush(stack, int, ParsePostfix);
				StackPush(stack, int, ParseAtom);
				break;
			case ParseCatenation:
				// If the expression has not ended, parse another piece.
				{xchar_t c;
				if(ctx->re == ctx->re_end)
					break;
				c = *ctx->re;
				if(!(ctx->cflags & REG_LITERAL)) {
					if(c == CharPipe)
						break;
					if(c == CharRParen && depth > 0) {
						DPrintf((stderr, "parsePat: group end: '%.*" StrF "'\n", Rest(ctx->re)));
						--depth;
						break;
						}
					}
				if(ctx->cflags & REG_RIGHTASSOC) {

					// Right associative concatenation.
					StackPush(stack, voidptr, result);
					StackPush(stack, int, ParsePostCatenation);
					StackPush(stack, int, ParseCatenation);
					StackPush(stack, int, ParsePiece);
					}
				else {
					// Default case, left associative concatenation.
					StackPush(stack, int, ParseCatenation);
					StackPush(stack, voidptr, result);
					StackPush(stack, int, ParsePostCatenation);
					StackPush(stack, int, ParsePiece);
					}
				break;
				}
			case ParsePostCatenation:
				{ast_node_t *tree = xstack_pop_voidptr(stack);
				ast_node_t *tmp_node;
				if((tmp_node = ast_newCat(ctx->mem, tree, result)) == NULL)
					return REG_ESPACE;
				result = tmp_node;
				}
				break;
			case ParseUnion:
				if(ctx->re == ctx->re_end || ctx->cflags & REG_LITERAL)
					break;
				switch(*ctx->re) {
					case CharPipe:
						DPrintf((stderr, "parsePat: union: '%.*" StrF "'\n", Rest(ctx->re)));
						StackPush(stack, int, ParseUnion);
						StackPush(stack, voidptr, result);
						StackPush(stack, int, ParsePostUnion);
						StackPush(stack, int, ParseBranch);
						++ctx->re;
						break;
					case CharRParen:
						++ctx->re;
						break;
					default:
						break;
					}
				break;
			case ParsePostUnion:
				{ast_node_t *tmp_node;
				ast_node_t *tree = xstack_pop_voidptr(stack);
				if((tmp_node = ast_newUnion(ctx->mem, tree, result)) == NULL)
					return REG_ESPACE;
				result = tmp_node;
				}
				break;
			case ParsePostfix:
				// Parse postfix operators.
				if(ctx->re == ctx->re_end || ctx->cflags & REG_LITERAL)
					break;
				switch(*ctx->re) {
					case CharAsterisk:
					case CharPlus:
					case CharHook:
						{ast_node_t *tmp_node;
						int minimal = (ctx->cflags & REG_UNGREEDY) == REG_UNGREEDY;
						int rep_min = 0;
						int rep_max = -1;
#ifdef XRE_Debug
						const xchar_t *tmp_re;
#endif

						if(*ctx->re == CharPlus)
							rep_min = 1;
						else if(*ctx->re == CharHook)
							rep_max = 1;
#ifdef XRE_Debug
						tmp_re = ctx->re;
#endif
						if(ctx->re + 1 < ctx->re_end) {
							switch(ctx->re[1]) {
								case CharHook:
									if(ctx->cflags & REG_ENHANCED) {
										minimal = !(ctx->cflags & REG_UNGREEDY);
										++ctx->re;
										break;
										}
									// Fall through.
								case CharAsterisk:
								case CharPlus:
									// These are reserved for future extensions.
									return REG_BADRPT;
								}
							}

						DPrintf((stderr, "parsePat: %s iter: '%.*" StrF "'\n",
						 minimal ? "minimal" : "greedy", Rest(tmp_re)));
						++ctx->re;
						if((tmp_node = ast_newIter(ctx->mem, result, rep_min, rep_max,
						 minimal)) == NULL)
							return REG_ESPACE;
						result = tmp_node;
						StackPush(stack, int, ParsePostfix);
						}
						break;
					case CharLBrace:
						DPrintf((stderr, "parsePat: bound: '%.*" StrF "'\n", Rest(ctx->re)));
						++ctx->re;

						if((status = parseBrace(ctx, &result)) != REG_OK)
							return status;
						StackPush(stack, int, ParsePostfix);
						break;
					}
				break;
			case ParseAtom:
				// Parse an atom.  An atom is a regular expression enclosed in '()', an empty set of '()', a
				// bracket expression, '.', '^', '$', a '\' followed by a character, or a single character.

				// End of regexp (empty string)?
				if(ctx->re == ctx->re_end || ctx->cflags & REG_LITERAL)
					goto ParseLit;
				switch(*ctx->re) {
					case CharLParen:	// Parenthesized subexpression.

						// Handle "(?...)" extensions in enhanced mode.  They work in a way similar to
						// Perl's corresponding extensions.
						if(ctx->cflags & REG_ENHANCED && (ctx->re + 1) < ctx->re_end &&
						 ctx->re[1] == CharHook) {
							char *msg;
							int flag, new_cflags = ctx->cflags;
							int bit = 1;
							DPrintf((stderr, "parsePat: extension: '%.*" StrF "\n", Rest(ctx->re)));
							ctx->re += 2;
							for(;;) {
								msg = NULL;
								switch(*ctx->re) {
									case L'A':
										msg = "approx";
										flag = REG_APPROX;
										break;
									case L'a':
										msg = "any newline";
										flag = REG_ANY;
										break;
									case L'i':
										msg = "icase";
										flag = REG_ICASE;
										break;
									case L'n':
										msg = "newline";
										flag = REG_NEWLINE;
										break;
									case L'r':
										msg = "right assoc";
										flag = REG_RIGHTASSOC;
										break;
									case L'U':
										msg = "ungreedy";
										flag = REG_UNGREEDY;
										break;
									case CharMinus:
										DPrintf((stderr, "parsePat: turn off: '%.*" StrF
										 "\n", Rest(ctx->re)));
										++ctx->re;
										bit = 0;
										break;
									case CharColon:
										DPrintf((stderr, "parsePat: non-capturing: '%.*"
										 StrF "\n", Rest(ctx->re)));
										++ctx->re;
										++depth;
										goto EndOpts;
									case CharHash:
										DPrintf((stderr, "parsePat: comment: '%.*" StrF
										 "\n", Rest(ctx->re)));

										// A comment can contain any character except a
										// right parenthesis.
										while(*ctx->re != CharRParen &&
										 ctx->re < ctx->re_end)
											++ctx->re;
										if(*ctx->re == CharRParen &&
										 ctx->re < ctx->re_end) {
											++ctx->re;
											goto EndOpts;
											}
										else
											return REG_BADPAT;
									case CharRParen:
										if(ctx->cflags & REG_REVERSED)
											return REG_EREGREV;
										++ctx->re;
										goto EndOpts;
									default:
										return REG_BADPAT;
									}
								if(msg != NULL) {
									DPrintf((stderr, "parsePat: %s: '%.*" StrF "\n", msg,
									 Rest(ctx->re)));
									if(bit)
										new_cflags |= flag;
									else
										new_cflags &= ~flag;
									++ctx->re;
									}
								}
EndOpts:
							// Turn on the cflags changes for the rest of the enclosing group.
							StackPush(stack, int, ctx->cflags);
							StackPush(stack, int, ParseRestoreCFlags);
							StackPush(stack, int, ParseRE);
							ctx->cflags = new_cflags;
							break;
							}
						++depth;
						if(ctx->cflags & REG_ENHANCED && ctx->re + 2 < ctx->re_end &&
						 ctx->re[1] == CharHook && ctx->re[2] == CharColon) {
							DPrintf((stderr, "parsePat: group begin: '%.*" StrF
							 "', no submatch\n", Rest(ctx->re)));

							// Don't mark for submatching.
							ctx->re += 3;
							StackPush(stack, int, ParseRE);
							}
						else {
							DPrintf((stderr, "parsePat: group begin: '%.*" StrF "', submatch %d\n",
							 Rest(ctx->re), ctx->submatch_id));
							++ctx->re;

							// First parse a whole RE, then mark the resulting tree for submatching.
							StackPush(stack, int, ctx->submatch_id);
							StackPush(stack, int, ParseMarkForSubmatch);
							StackPush(stack, int, ParseRE);
							++ctx->submatch_id;
							}
						break;
					case CharRParen:	// End of current subexpression.
						if(depth > 0) {
							DPrintf((stderr, "parsePat: empty: '%.*" StrF "'\n", Rest(ctx->re)));

							// We were expecting an atom, but instead the current subexpression was
							// closed.  POSIX leaves the meaning of this to be implementation-
							// defined.  We interpret this as an empty expression (which matches an
							// empty string).
							if((result = ast_newLit(ctx->mem, LitEmpty, -1, -1)) == NULL)
								return REG_ESPACE;
							}
						else
							goto ParseLit;
						break;
					case CharLBracket:	// Bracket expression.
						DPrintf((stderr, "parsePat: bracket: '%.*" StrF "'\n", Rest(ctx->re)));
						++ctx->re;
						if((status = parseBracket(ctx, &result)) != REG_OK)
							return status;
						break;
					case CharBackslash:	// Shortcut or escaped character.
						if(ctx->re + 1 == ctx->re_end)	// Trailing backslash.
							return REG_EESCAPE;

						// If enhanced mode, check if \x shortcut.  If found, parse the expanded text
						// recursively.
						if(ctx->cflags & REG_ENHANCED) {
							xchar_t buf[64];
							expandShortcut(buf, ctx->re + 1, false);
							if(buf[0] != 0) {
								parse_ctx_t subctx;
								memcpy(&subctx, ctx, sizeof(subctx));
								subctx.re = subctx.re_start = buf;
								subctx.len = xstrlen(buf);
								subctx.re_end = buf + subctx.len;
								subctx.keepfirst = false;
								if((status = parsePat(&subctx)) != REG_OK)
									return status;
								ctx->re += 2;
								ctx->position = subctx.position;
								result = subctx.result;
								break;
								}
							}
						else {
							// Escaped character.
							++ctx->re;
							goto EscChar;
							}

						// Enhanced mode.  Check for other metacharacters.
						if(ctx->re[1] == L'Q') {
							DPrintf((stderr, "parsePat: tmp literal: '%.*" StrF "'\n",
							 Rest(ctx->re)));
							ctx->cflags |= REG_LITERAL;
							temp_cflags |= REG_LITERAL;
							ctx->re += 2;
							StackPush(stack, int, ParseAtom);
							break;
							}

						DPrintf((stderr, "parsePat: bleep: '%.*" StrF "'\n", Rest(ctx->re)));
						++ctx->re;
						switch(*ctx->re) {
							case L'b':
								status = AssertAtWB;
								str = "WB";
								goto NewAssert;
							case L'B':
								status = AssertAtNegWB;
								str = "WB_NEG";
								goto NewAssert;
							case L'<':
								status = AssertAtBOW;
								str = "BOW";
								goto NewAssert;
							case L'>':
								status = AssertAtEOW;
								str = "EOW";
								goto NewAssert;
							case L'N':
								goto NotNL;
							case L'x':
								++ctx->re;	// May be past end.
								{long val;
								int maxDigits = 2;
								DPrintf((stderr, "parsePat: hex value: '%.*" StrF "'\n",
								 Rest(ctx->re - 2)));
								if(ctx->re < ctx->re_end && *ctx->re == CharLBrace) {
									maxDigits = 16;
									++ctx->re;
									}
								if((status = hexnum(ctx, maxDigits, &val)) != REG_OK)
									return status;
								result = ast_newLit(ctx->mem, (int) val, (int) val,
								 ctx->position);
								++ctx->position;
								}
								break;
							default:
								if(xisdigit(*ctx->re)) {

									// Back reference.
									int val = *ctx->re - L'0';
									DPrintf((stderr, "parsePat: backref: '%.*" StrF "'\n",
									 Rest(ctx->re - 1)));
									if(ctx->cflags & REG_REVERSED)
										return REG_EREGREV;
									if(val == 0)
										return REG_ESUBREG;
									result = ast_newLit(ctx->mem, LitBackref, val,
									 ctx->position);
									ctx->max_backref = Max(val, ctx->max_backref);
									}
								else {
EscChar:
									// Escaped character.
									DPrintf((stderr, "parsePat: escaped: '%.*" StrF "'\n",
									 Rest(ctx->re - 1)));
									result = ast_newLit(ctx->mem, *ctx->re,
									 *ctx->re, ctx->position);
									}
								++ctx->position;
								++ctx->re;
								break;
							}
						if(result == NULL)
							return REG_ESPACE;
						break;
					case CharPeriod:	// The "any" symbol.
						DPrintf((stderr, "parsePat: any: '%.*" StrF "'\n", Rest(ctx->re)));
						if((ctx->cflags & (REG_NEWLINE | REG_ANY)) == REG_NEWLINE) {
							ast_node_t *tmp1;
							ast_node_t *tmp2;
NotNL:
							if((tmp1 = ast_newLit(ctx->mem, 0, L'\n' - 1,
							 ctx->position)) == NULL ||
							 (tmp2 = ast_newLit(ctx->mem, L'\n' + 1, XRE_CHAR_MAX,
							 ctx->position)) == NULL ||
							 (result = ast_newUnion(ctx->mem, tmp1, tmp2)) == NULL)
								return REG_ESPACE;
							}
						else if((result = ast_newLit(ctx->mem, 0, XRE_CHAR_MAX,
						 ctx->position)) == NULL)
							return REG_ESPACE;
						++ctx->position;
						++ctx->re;
						break;
					case CharCircumflex:	// Beginning-of-line assertion.
						status = AssertAtBOL;
						str = "BOL";
						goto NewAssert;
					case CharDollar:	// End-of-line assertion.
						status = AssertAtEOL;
						str = "EOL";
NewAssert:
						DPrintf((stderr, "parsePat: %s: '%.*" StrF "'\n", str, Rest(ctx->re)));

						// Repetition not allowed on an assertion.
						if(ctx->re + 1 < ctx->re_end)
							switch(ctx->re[1]) {
								case CharAsterisk:
								case CharPlus:
								case CharHook:
								case CharLBrace:
									return REG_BADRPT;
								}
						if((result = ast_newLit(ctx->mem, LitAssert, status, -1)) == NULL)
							return REG_ESPACE;
						++ctx->re;
						break;
					default:;
ParseLit:
						// Check for \E following prior \Q (enhanced mode only - temp_cflags set).
						if(temp_cflags && ctx->re + 1 < ctx->re_end &&
						 ctx->re[0] == CharBackslash && ctx->re[1] == L'E') {
							DPrintf((stderr, "parsePat: end tmps: '%.*" StrF "'\n", Rest(ctx->re)));
							ctx->cflags &= ~temp_cflags;
							temp_cflags = 0;
							ctx->re += 2;
							StackPush(stack, int, ParsePiece);
							break;
							}

						// We must have an atom at this point.  If not, we have a logic error.
						assert(ctx->re < ctx->re_end);
						DPrintf((stderr, "parsePat: literal: '%.*" StrF "'\n", Rest(ctx->re)));

						// Note that we can't use an xisalpha() test here because there may be
						// characters which are alphabetic but neither upper or lower case.
						if(ctx->cflags & REG_ICASE && (xisupper(*ctx->re) || xislower(*ctx->re))) {
							ast_node_t *tmp1;
							ast_node_t *tmp2;

							// XXX - Can there be more than one opposite-case counterpoints for some
							// character in some locale?  Or more than two characters which all
							// should be regarded the same character if case is ignored?  If yes,
							// there does not seem to be a portable way to detect it.  I guess that
							// at least for multi-character collating elements there could be
							// several opposite-case counterpoints, but they cannot be supported
							// portably anyway.
							if((tmp1 = ast_newLit(ctx->mem, xtoupper(*ctx->re),
							 xtoupper(*ctx->re), ctx->position)) == NULL)
								return REG_ESPACE;
							if((tmp2 = ast_newLit(ctx->mem, xtolower(*ctx->re),
							 xtolower(*ctx->re), ctx->position)) == NULL)
								return REG_ESPACE;
							if((result = ast_newUnion(ctx->mem, tmp1, tmp2)) == NULL)
								return REG_ESPACE;
							}
						else if((result = ast_newLit(ctx->mem, *ctx->re, *ctx->re,
						 ctx->position)) == NULL)
							return REG_ESPACE;

						++ctx->position;
						++ctx->re;
						break;
					}
				break;
			case ParseMarkForSubmatch:
				{int submatch_id = xstack_pop_int(stack);
				if(result->submatch_id >= 0) {
					ast_node_t *n, *tmp_node;
					if((n = ast_newLit(ctx->mem, LitEmpty, -1, -1)) == NULL ||
					 (tmp_node = ast_newCat(ctx->mem, n, result)) == NULL)
						return REG_ESPACE;
					tmp_node->num_submatches = result->num_submatches;
					result = tmp_node;
					}
				result->submatch_id = submatch_id;
				++result->num_submatches;
				}
				break;
			case ParseRestoreCFlags:
				ctx->cflags = xstack_pop_int(stack);
				break;
			default:
				assert(0);
				break;
			}
		}

	// Check for missing closing parentheses.
	if(depth > 0)
		return REG_EPAREN;

	ctx->result = result;
	return REG_OK;
	}

#if EnableReverse

// Regular expression reversal routines.

static bool quoteMode = false;

// Copy pattern segment to dest.  Return updated src.
static const xchar_t *copylast(xchar_t **pdest, const xchar_t *src0, const xchar_t *src) {

	if(src > src0) {
		int len = src - src0;
		*pdest -= len;
		xmemcpy(*pdest, src0, len);
		}
	return src;
	}

// Span a character class.  Update src to terminating ']' if possible.  Return status.
static int ccspan(const xchar_t **psrc, int cflags) {
	const xchar_t *src = *psrc;
	xchar_t c;

	// Skip over any initial '^' or ']' character.
	if(*++src == CharCircumflex)
		++src;
	if(*src == CharRBracket)
		++src;

	// Scan characters, skipping over special sequences, until ']' is found.
	for(;;) {
		switch(*src) {
			case L'\0':
				return REG_EBRACK;			// Error.
			case CharBackslash:
				if(cflags & REG_ENHANCED) {
					if(src[1] == L'\0')
						return REG_EBRACK;
					goto Skip;
					}
				break;
			case CharLBracket:
				switch(c = src[1]) {
					case CharColon: 	// [:
					case CharPeriod:	// [.
					case CharEqual: 	// [=
						if((src = xstrchr(src + 2, c)) == NULL || src[1] != CharRBracket)
							return (c == CharColon) ? REG_ECTYPE : REG_ECOLLATE;
Skip:
						src += 2;
						continue;
					}
				break;
			case CharRBracket:
				if(src[-1] != CharMinus)
					goto Retn;
			}
		++src;
		}
Retn:
	*psrc = src;
	return REG_OK;
	}

// Copy consecutive RE atoms from 'src' to 'dest' in reverse order and copy single (literal) characters between \Q and \E.
// Scanning stops at a '|', ')', or end of pattern.  Update 'src' and 'dest' pointers and return status, including REG_EMPTY if
// an empty atom list was found.
static int atomcpy(xchar_t **pdest, const xchar_t **psrc, int cflags) {
	int status;
	xchar_t c;
	bool quoteOn;
	const xchar_t *src0;
	const xchar_t *src = *psrc;
	xchar_t *dest = *pdest;

	// Loop though source pattern, finding the position of complete atoms.  Once a character is found that is not part of
	// the current atom, write the previous one.  However, write single characters between \Q and \E.
	src0 = src;
	for(;;) {
		c = *src;

		// Check for \Q and \E.
		if(cflags & REG_ENHANCED && c == CharBackslash) {
			switch(src[1]) {
				case L'Q':
					c = L'E';
					quoteOn = true;
					goto DoQuote;
				case L'E':
					c = L'Q';
					quoteOn = false;
DoQuote:
					if(quoteOn == quoteMode)
						return REG_BADPAT;
					src0 = copylast(&dest, src0, src);
					quoteMode = quoteOn;
					dest -= 2;
					dest[0] = CharBackslash;
					dest[1] = c;
					src0 = src += 2;
					continue;
				}
			}

		if(!quoteMode) {
			switch(c) {
				case CharBackslash:
					if(src[1] == L'\0')
						return REG_EESCAPE;	// Error.
					src0 = copylast(&dest, src0, src);
					if(cflags & REG_ENHANCED) {
						switch(src[1]) {
							case L'x':
								src += 2;
								if(*src == CharLBrace)
									++src;
								while(isxdigit(*src))
									++src;
								if(*src == CharRBrace)
									++src;
								continue;
							}
						}
					src += 2;
					continue;
				case CharLBracket:
					src0 = copylast(&dest, src0, src);
					if((status = ccspan(&src, cflags)) != REG_OK)
						return status;
					goto Onward;
				case CharAsterisk:
				case CharPlus:
					goto Onward;
				case CharHook:
					if(!(cflags & REG_ENHANCED) && src > src0) {

						// Check for minimal repetition op '?', which is a literal character here.
						switch(src[-1]) {
							case CharAsterisk:
							case CharPlus:
							case CharHook:
							case CharRBrace:
								goto Literal;
							}
						}
					goto Onward;
				case CharLBrace:
					if((src = xstrchr(src, CharRBrace)) == NULL)
						return REG_EBRACE;	// Error.
					goto Onward;
				case CharLParen:

					// Beginning of a group.
					(void) copylast(&dest, src0, src);
					if((status = grpcpy(&dest, &src, cflags & ~CompTopLevel)) != REG_OK)
						return status;		// Error.
					src0 = src;
					continue;
				case CharRBrace:
					return REG_EBRACE;
				case CharRBracket:
					return REG_EBRACK;
				case CharPipe:
				case CharRParen:
				case L'\0':
					goto Done;
				default:
					goto Literal;
				}
			}
		else {
Literal:
			// Singleton atom or literal character.
			src0 = copylast(&dest, src0, src);
			}
Onward:
		++src;
		}
Done:
	(void) copylast(&dest, src0, src);
	if(src == *psrc)
		return REG_EMPTY;
	*psrc = src;
	*pdest = dest;
	return REG_OK;
	}

// Copy one RE group (parenthesized subexpression or whole RE) to 'dest' in reverse order preceded by any closure modifiers
// which follow group.  Group ends at a right paren or at end of pattern (when called at top level).  Update 'src' and 'dest'
// pointers and return status.
int grpcpy(xchar_t **pdest, const xchar_t **psrc, int cflags) {
	int status;
	xchar_t c;
	unsigned len;
	const xchar_t *src0 = NULL;
	const xchar_t *hook0 = NULL;
	const xchar_t *hook1;
	const xchar_t *src = *psrc;
	xchar_t *dest = *pdest;
	bool needrparen = false;
	xchar_t wkbuf[xstrlen(src) + 1];
	xchar_t *dest1 = wkbuf + elementsof(wkbuf) - 1;
	*dest1 = L'\0';

	// REG_ENHANCED group syntax:
	// (?# This is a comment)
	// (?:.+)
	// (?in-U)
	// (?in-U:.*)
	if(!(cflags & CompTopLevel) && *src == CharLParen) {
		needrparen = true;
		*--dest1 = CharRParen;
		++src;
		switch(*src) {
			case CharHook:
				if(cflags & REG_ENHANCED) {
					hook0 = src++;
					if(*src == CharHash) {
						if((hook1 = src = xstrchr(src, CharRParen)) == NULL)
							return REG_EPAREN;
						goto EndHook;
						}
					if(*src == CharColon)
						hook1 = ++src;
					else {					// Must be a letter.
						while(isalpha(*src) || *src == CharMinus)
							++src;
						if(*src == CharColon)
							hook1 = ++src;
						else if(*src != CharRParen)
							return REG_EPAREN;
						else {
							hook1 = src;
							goto EndHook;
							}
						}
					}
				break;
			case CharRParen:
				// Have "()".
				goto EndGroup;
			case L'\0':
				return REG_EPAREN;
			}
		}

	// Scan pattern and process each RE delimited by '|' or ')' via atomcpy().
	for(;;) {
		if((status = atomcpy(&dest1, &src, cflags)) != REG_OK)
			return status;
		switch(c = *src) {
			case L'\0':
				if(needrparen)
					return REG_EPAREN;
				goto GroupEnd;
			case CharRParen:
				if(!needrparen)
					return REG_EPAREN;
				if(hook0 != NULL) {
EndHook:
					dest1 -= (len = hook1 - hook0);
					xmemcpy(dest1, hook0, len);
					}
EndGroup:
				*--dest1 = CharLParen;
				++src;

				// Scan for closure characters and/or modifiers following ')'.
				src0 = src;
				for(;;) {
					switch(*src) {
						case CharHook:
							if(!(cflags & REG_ENHANCED)) {

								// Check for minimal repetition op '?', which is a literal
								// character here.
								switch(src[-1]) {
									case CharAsterisk:
									case CharPlus:
									case CharHook:
									case CharRBrace:
										goto GroupEnd;
									}
								}
							break;
						case CharAsterisk:
						case CharPlus:
							break;
						case CharLBrace:
							if((src = xstrchr(src, CharRBrace)) == NULL)
								return REG_EBRACE;
							break;
						default:
							goto GroupEnd;
						}
					++src;
					}
				goto GroupEnd;
			default:				// |
				*--dest1 = CharPipe;
				++src;
			}
		}
GroupEnd:
	// End of group.  Copy closure modifiers (if any) and reversed group in wkbuf to dest, and return at character past ')'
	// or at terminating null.
	if(src0 != NULL && (len = src - src0) > 0) {
		dest -= len;
		xmemcpy(dest, src0, len);
		}
	len = xstrlen(dest1);
	dest -= len;
	xmemcpy(dest, dest1, len);
	*psrc = src;
	*pdest = dest;
	return REG_OK;
	}

#endif // EnableReverse

#ifdef __cplusplus
	}
#endif
