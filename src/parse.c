// parse.c - Regular expression parsing and abstract syntax tree (AST) routines.
//
// (c) Copyright 2022 Richard W. Marinelli
//
// This work is based on TRE ver. 0.7.5 (c) Copyright 2001-2006 Ville Laurikari <vl@iki.fi> and is licensed
// under the GNU Lesser General Public License (LGPLv3).  To view a copy of this license, see the "License.txt"
// file included with this distribution or visit http://www.gnu.org/licenses/lgpl-3.0.en.html.

#if XRE_Debug
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

ast_node_t *ast_newIter(memhdr_t *mem, ast_node_t *sub, int min, int max, int minimal) {
	ast_node_t *node;
	ast_iter_t *iter;

	if((node = ast_newNode(mem, AST_Iter, sizeof(ast_iter_t))) == NULL)
		return NULL;
	iter = node->obj;
	iter->sub = sub;
	iter->min = min;
	iter->max = max;
	iter->minimal = minimal;
	node->num_submatches = sub->num_submatches;

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

#if EnableReverse

// Create reversed version of given tree in place and return it.
static ast_node_t *revtree(ast_node_t *node) {

	switch(node->type) {
		case AST_Lit:
			// Nothing needs to followed from a literal node, so we can just return it.
			break;
		case AST_Cat:
			{ast_node_t *left, *right;
			ast_cat_t *cat = node->obj;
			left = revtree(cat->left);
			right = revtree(cat->right);

			// Reverse the node order.
			cat->left = right;
			cat->right = left;
			}
			break;
		case AST_Union:
			{ast_node_t *left, *right;
			ast_union_t *u = node->obj;
			left = revtree(u->left);
			right = revtree(u->right);

			// Reverse the node order.
			u->left = right;
			u->right = left;
			}
			break;
		case AST_Iter:
			(void) revtree(((ast_iter_t *) node->obj)->sub);
			break;
		default:
			assert(0);
		}
	return node;
	}

// Renumber submatch IDs in given AST.
static void renumAST(ast_node_t *ast, int *psubid) {

	if(ast->submatch_id >= 0)
		ast->submatch_id = ++*psubid;
	switch(ast->type) {
		case AST_Lit:
			break;
		case AST_Cat:
			renumAST(((ast_cat_t *) ast->obj)->left, psubid);
			renumAST(((ast_cat_t *) ast->obj)->right, psubid);
			break;
		case AST_Union:
			renumAST(((ast_union_t *) ast->obj)->left, psubid);
			renumAST(((ast_union_t *) ast->obj)->right, psubid);
			break;
		case AST_Iter:
			renumAST(((ast_iter_t *) ast->obj)->sub, psubid);
			break;
		}
	}

// Create reversed version of given abstract syntax tree (AST).
void revAST(ast_node_t *ast) {
	int submatch_id = -1;

	// Reverse tree in place.
	(void) revtree(ast);

	// Scan reversed AST and renumber submatch IDs.
	renumAST(ast, &submatch_id);
	}
#endif
#if XRE_Debug

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
					fputs("empty literal\n", stderr);
					break;
				case LitAssert:
					{unsigned i;
					char *assertions[] = {"BOL", "EOL", "BOW", "EOW", "WB", "!WB", "CType", "!CType"};
					if(lit->code_max >= (AssertLast << 1))
						assert(0);
					fprintf(stderr, "assertions(%.4x):", lit->code_max);
					for(i = 0; (1 << i) <= AssertLast; ++i)
						if(lit->code_max & (1 << i))
							fprintf(stderr, " %s", assertions[i]);
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
			printNode(iter->sub, indent + 2);
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
int xisalnum_func(xint_t c) { return xisalnum(c); }
int xisalpha_func(xint_t c) { return xisalpha(c); }

#ifdef xisascii
int xisascii_func(xint_t c) { return xisascii(c); }
#elif 0		// Wide characters enabled (EnableWChar); therefore, this is not needed.
int xisascii_func(xint_t c) { return !(c & ~0177); }
#endif

int xisblank_func(xint_t c) { return xisblank(c); }
int xiscntrl_func(xint_t c) { return xiscntrl(c); }
int xisdigit_func(xint_t c) { return xisdigit(c); }
int xisgraph_func(xint_t c) { return xisgraph(c); }
int xislower_func(xint_t c) { return xislower(c); }
int xisprint_func(xint_t c) { return xisprint(c); }
int xispunct_func(xint_t c) { return xispunct(c); }
int xisspace_func(xint_t c) { return xisspace(c); }
int xisupper_func(xint_t c) { return xisupper(c); }
int xisxdigit_func(xint_t c) { return xisxdigit(c); }

struct mapitem {
	char *name;
	int (*func)(xint_t);
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

#define LitCharCount		5	// Number of literal characters at beginning of xtab table.

// Expand possible shortcut character at 're' to 'buf', which is assumed to have sufficient space.  buf[0] is set to zero if
// shortcut not found.  If 'strip' is true, leading bracket is skipped during expansion and negated expansions are treated as
// "not found".  Return true if escaped literal character found, otherwise false.
static bool expandShortcut(xchar_t *buf, const xchar_t *re, bool strip) {
	const Shortcut *x = xtab;

	*buf = L'\0';
	do {
		if((!strip || islower(x->c)) && (xchar_t) x->c == *re) {
			const char *str = (strip && *x->expansion == '[') ? x->expansion + 1 : x->expansion;
#if XRE_Debug
			fprintf(stderr, "Expanding shortcut '%c' => ", x->c);
			if(*str < ' ')
				fprintf(stderr, "char %d\n", (int) *str);
			else
				fprintf(stderr, "'%s'\n", str);
#endif
			do {
				*buf++ = (xchar_t) ((unsigned char) *str++);
				} while(*str != '\0');
			*buf = L'\0';
			if(x < xtab + LitCharCount)
				return true;
			break;
			}
		} while((++x)->c != '\0');
	return false;
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
	return (array[ilist->n++] = ast_newLit(mem, min, max, -1)) == NULL ? REG_ESPACE : 0;
	}

// Expand a named character class to character ranges to improve performance for 8-bit character sets.
static int expandCC(memhdr_t *mem, xctype_t class, item_list_t *ilist, int cflags) {
	int status;
	xint_t c;
	int j;
	int min = -1, max = 0;
	assert(XRE_MB_CUR_MAX == 1);

	DPrintf((stderr, "  expanding class to character ranges\n"));
	for(j = 0; j < 256; ++j) {
		c = (xint_t) j;
		if(xisctype(c, class) || ((cflags & REG_ICASE) && (xisctype(xtolower(c), class) ||
		 xisctype(xtoupper(c), class)))) {
			if(min < 0)
				min = c;
			max = c;
			}
		else if(min >= 0) {
			DPrintf((stderr, "  range conversion %c (%d) to %c (%d)\n", min, min, max, max));
			if((status = newItem(mem, min, max, ilist)) != 0)
				return status;
			min = -1;
			}
		}
	return (min >= 0) ? newItem(mem, min, max, ilist) : 0;
	}

// Forward.
static int parseBracketItems(parse_ctx_t *ctx, item_list_t *ilist, nclass_list_t *nclass);

// Parse a class name of form "[:xxx:]".  *ctx->re is assumed to point at the opening left bracket.
static int parseCC(parse_ctx_t *ctx, xctype_t *pclass) {
	const xchar_t *re1;
	int len;
	char nameBuf[64];

	re1 = ctx->re += 2;
	DPrintf((stderr, "  named class: '%.*" StrF "'\n", Rest(re1)));

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
	len = Min(re1 - ctx->re, 63);
#if EnableWChar
	xchar_t tmp_wcs[64];
	wcsncpy(tmp_wcs, ctx->re, (size_t) len);
	tmp_wcs[len] = 0;
	len = wcstombs(nameBuf, tmp_wcs, 63);
#else
	strncpy(nameBuf, (const char *) ctx->re, len);
	nameBuf[len] = '\0';
#endif
	DPrintf((stderr, "  class name: %s\n", nameBuf));
	if(!(*pclass = xctype(nameBuf)))
		return REG_ECTYPE;

	// Success.
	ctx->re = re1 + 2;
	return 0;
	}

// Parse and decode a hexadecimal value beginning at 'x' of \x, possibly enclosed in braces.  Set *pval to result if successful
// and return 0; otherwise, return an error.
static int hexchar(parse_ctx_t *ctx, long *pval) {
	int maxDigits = 2;
	bool foundLeftBrace = false, foundRightBrace = false;
	char wkbuf[8 + 1], *str = wkbuf;

	++ctx->re;		// May be past end of pattern.
	DPrintf((stderr, "hex char: '%.*" StrF "'\n", Rest(ctx->re - 2)));
	if(ctx->re < ctx->re_end && *ctx->re == CharLBrace) {
		maxDigits = 8;
		foundLeftBrace = true;
		++ctx->re;
		}
	while(ctx->re < ctx->re_end) {
		if(*ctx->re == CharRBrace) {
			if(foundLeftBrace) {
				foundRightBrace = true;
				++ctx->re;
				}
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
	if(foundLeftBrace != foundRightBrace)
		return REG_EBRACE;
	*str = '\0';
	*pval = strtol(wkbuf, NULL, 16);
	return 0;
	}

// Parse next item inside bracketed expression, which includes a range.
static int parseItem(parse_ctx_t *ctx, item_list_t *ilist, nclass_list_t *nclass) {
	bool cshortcut = false;
	bool haveHex = false;
	int min = -1, max = -1;
	int status, c;
	xctype_t class = (xctype_t) 0;

	// Find next item by scanning for range endpoints, checking for illegal constructs.  A single literal, escaped
	// character, or character class name is simply the first endpoint of a "range", not followed by '-'.  After each item
	// is parsed, exactly one of the min, max, class, or cshortcut state variables is updated.
	for(;;) {
		c = *ctx->re;

		// Check for "\x".
		if(c == CharBackslash) {
			if(ctx->re + 1 == ctx->re_end)
				return REG_EBRACK;
			if(!(ctx->cflags & REG_ENHANCED))
				goto Literal;

			// Check for hex character.
			c = *++ctx->re;
			if(c == L'x') {
				long val;

				if((status = hexchar(ctx, &val)) != 0)
					return status;
				c = val;
				haveHex = true;
				goto Literal;
				}

			// Expand shortcut if possible.
			xchar_t buf[32];
			(void) expandShortcut(buf, ctx->re, true);
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
			if((status = parseBracketItems(&subctx, ilist, nclass)) != 0)
				return status;
			++ctx->re;
			cshortcut = true;
			}

		// Check for "[:xxx:]".
		else if(ctx->re + 1 < ctx->re_end && c == CharLBracket) {
			switch(ctx->re[1]) {
				case CharPeriod:
				case CharEqual:
					return REG_ECOLLATE;
				case CharColon:
					{xctype_t newclass;
					if((status = parseCC(ctx, &newclass)) != 0)
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
			DPrintf((stderr, "  char: '%.*" StrF "'\n", Rest(ctx->re)));
			if(min == -1)
				min = c;
			else {
				// XXX - Should use collation order instead of encoding values in character ranges.
				if(min > c)
					return REG_ERANGE;
				max = c;
				}
			if(haveHex)
				haveHex = false;
			else
				++ctx->re;
			}

		// One atom parsed successfully.  Check for range.
		if(ctx->re + 1 >= ctx->re_end || ctx->re[0] != CharMinus || ctx->re[1] == CharRBracket)
			break;
		if(class || cshortcut || max != -1)
			return REG_ERANGE;
		++ctx->re;
		}

	// Item parsed successfully.  Set 'max' if needed.
	if(min != -1) {
		if(max == -1)
			max = min;
#if XRE_Debug
		else
			fprintf(stderr, "  range %c (%d) to %c (%d)\n", min, min, max, max);
#endif
		}
	else if(class) {
		min = 0;
		max = XRE_CHAR_MAX;

		// Optimize named character classes for 8-bit character sets.
		if(ctx->cur_max == 1)
			return expandCC(ctx->mem, class, ilist, ctx->cflags);

		// Add class to negated class list if applicable.
		if(nclass != NULL) {
			if(nclass->n == MaxNegClasses)
				return REG_ESPACE;
			nclass->neg_classes[nclass->n++] = class;
			}
		}

	if(!cshortcut) {
		// Add item to list.
		if((status = newItem(ctx->mem, min, max, ilist)) != 0)
			return status;
		((ast_lit_t *) ilist->nodes[ilist->n - 1]->obj)->u.class = class;

		// Add opposite-case counterpoints if REG_ICASE flag set.  This is broken if there are more than two "same"
		// characters.
		if(!class && ctx->cflags & REG_ICASE) {
			xint_t cmin, cmax;
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
					if((status = newItem(ctx->mem, cmin, cmax, ilist)) != 0)
						return status;
					}
				else
					++min;
				} while(min <= max);
			}
		}

	return 0;
	}

// Parse the contents of a bracketed expression, beginning just past the '[' and '^', if any.
static int parseBracketItems(parse_ctx_t *ctx, item_list_t *ilist, nclass_list_t *nclass) {
	int status;
	const xchar_t *re0 = ctx->re;

	DPrintf((stderr, "parseBracketItems: parsing '%.*" StrF "', len %d\n", Rest(re0), (int)(ctx->re_end - re0)));

	// Build an array of the items in the bracketed expression.
	for(;;) {
		// End of pattern?
		if(ctx->re == ctx->re_end)
			return REG_EBRACK;

		// End of bracketed expression?
		if(*ctx->re == CharRBracket && ctx->re > re0) {
			DPrintf((stderr, "parseBracketItems: done: '%.*" StrF "'\n", Rest(ctx->re)));
			++ctx->re;
			break;
			}

		// Parse next token.
		if((status = parseItem(ctx, ilist, nclass)) != 0)
			return status;
		}

	return 0;
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
	return 0;
	}

static int compareItems(const void *a, const void *b) {
	const ast_node_t *node_a = *((ast_node_t * const *) a);
	const ast_node_t *node_b = *((ast_node_t * const *) b);
	ast_lit_t *l_a = node_a->obj, *l_b = node_b->obj;
	int a_min = l_a->code_min, b_min = l_b->code_min;

	return (a_min < b_min) ? -1 : (a_min > b_min) ? 1 : 0;
	}

static int parseBracket(parse_ctx_t *ctx, ast_node_t **pnode) {
	ast_node_t *node = NULL;
	int status = 0;
	ast_node_t **pitem, **pitemz, *u, *n;
	ast_lit_t *lit;
	nclass_list_t *nclass = NULL;
	int min, max, neg_min, neg_max;
	item_list_t items = {0, 32, NULL};
	nclass_list_t nclasses = {0, {0}};

	// Start off with an array of 'size' elements.
	if((items.nodes = malloc(sizeof(*items.nodes) * items.size)) == NULL)
		return REG_ESPACE;

	// Parse the bracketed expression into an array of literal nodes.
	if(*ctx->re == CharCircumflex) {
		DPrintf((stderr, "parseBracket: negate: '%.*" StrF "'\n", Rest(ctx->re)));
		nclass = &nclasses;
		++ctx->re;
		}
	if((status = parseBracketItems(ctx, &items, nclass)) != 0)
		goto Retn;

	// Sort the array if we need to negate it.
	if(nclass != NULL && items.n > 1)
		qsort(items.nodes, (unsigned) items.n, sizeof(*items.nodes), compareItems);

	// Convert items in the array to an AST (with "node" as root), merging overlapping ranges, and negated if applicable.
	neg_min = neg_max = 0;
	for(pitemz = (pitem = items.nodes) + items.n; pitem < pitemz; ++pitem) {
		lit = (*pitem)->obj;
		min = lit->code_min;
		max = lit->code_max;

		DPrintf((stderr, "item: %d - %d, class %ld, neg_max = %d\n", min, max, (long) lit->u.class, neg_max));

		if(nclass != NULL) {

			// Skip item if has "dummy" range (a non-optimized, negated character class).
			if(min == 0 && max == XRE_CHAR_MAX) {
				DPrintf((stderr, "skipped item\n"));
				continue;
				}
			if(min < neg_max) {

				// Overlap.
				neg_max = Max(neg_max, max + 1);
				DPrintf((stderr, "overlap, neg_max => %d\n", neg_max));
				lit = NULL;
				}
			else {
				// No overlap... have a gap.
				neg_max = min - 1;
				if(neg_max >= neg_min) {
					DPrintf((stderr, "no overlap\n"));
					lit->code_min = neg_min;
					lit->code_max = neg_max;
					}
				else {
					DPrintf((stderr, "no overlap, zero room\n"));
					lit = NULL;
					}
				neg_min = neg_max = max + 1;
				}
			}

		// Skip node completely if negated expression, range is a single newline, REG_NEWLINE flag set, and REG_ANY
		// flag not set so that newline is excluded.
		if(lit != NULL && (nclass == NULL || lit->code_min != L'\n' || lit->code_max != L'\n' ||
		 (ctx->cflags & (REG_NEWLINE | REG_ANY)) != REG_NEWLINE)) {

			// Not newline special case... keep node.
			DPrintf((stderr, "creating item %d - %d\n", (int) lit->code_min, (int) lit->code_max));
			lit->position = ctx->position;
			if(nclasses.n == 0)
				lit->neg_classes = NULL;
			else if((status = copyNeg(ctx, lit, nclass)) != 0)
				goto Retn;

			if(node == NULL)
				node = *pitem;
			else {
				if((u = ast_newUnion(ctx->mem, node, *pitem)) == NULL)
					goto NoSpace;
				node = u;
				}

			// Now check for newline in range: if negated expression, range includes newline, REG_NEWLINE flag set,
			// and REG_ANY flag not set, adjust node so that newline is excluded.
			if(nclass != NULL && lit->code_min <= L'\n' && lit->code_max >= L'\n' &&
			 (ctx->cflags & (REG_NEWLINE | REG_ANY)) == REG_NEWLINE) {
				DPrintf((stderr, "removing newline from item %d - %d\n",
				 (int) lit->code_min, (int) lit->code_max));
				if(lit->code_min == L'\n')
					++lit->code_min;
				else if(lit->code_max == L'\n')
					--lit->code_max;
				else {
					// Newline inside range... split it.  Use existing node for chars preceding newline.
					ast_lit_t *l2;
					int oldmax = lit->code_max;
					lit->code_max = L'\n' - 1;

					// Make new node for chars following newline.
					if((n = ast_newLit(ctx->mem, L'\n' + 1, oldmax, ctx->position)) == NULL)
						goto NoSpace;
					l2 = n->obj;
					if(nclasses.n == 0)
						l2->neg_classes = NULL;
					else if((status = copyNeg(ctx, l2, nclass)) != 0)
						goto Retn;

					if(node == NULL)
						node = n;
					else {
						if((u = ast_newUnion(ctx->mem, node, n)) == NULL)
							goto NoSpace;
						node = u;
						}
					}
				}
			}
		}

	if(nclass != NULL) {
		DPrintf((stderr, "final: creating %d - %d\n", neg_min, XRE_CHAR_MAX));
		if((n = ast_newLit(ctx->mem, neg_min, XRE_CHAR_MAX, ctx->position)) == NULL)
			goto NoSpace;
		else {
			lit = n->obj;
			if(nclasses.n == 0)
				lit->neg_classes = NULL;
			else if((status = copyNeg(ctx, lit, nclass)) != 0)
				goto Retn;

			if(node == NULL)
				node = n;
			else {
				if((u = ast_newUnion(ctx->mem, node, n)) == NULL)
					goto NoSpace;
				node = u;
				}
			}
		}
#if XRE_Debug
	printAST(node, "post-parseBracket()");
#endif
	goto Retn;
NoSpace:
	status = REG_ESPACE;
Retn:
	free(items.nodes);
	++ctx->position;
	*pnode = node;
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
static int parseBrace(parse_ctx_t *ctx, ast_node_t **pnode) {
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
#if XRE_Debug && EnableApprox
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
#if XRE_Debug
							str = "ins";
#endif
							pitem = &costIns;
							goto SetCost;
						case L'd':	// Delete cost.
#if XRE_Debug
							str = "del";
#endif
							pitem = &costDel;
							goto SetCost;
						case L's':	// Substitute cost.
#if XRE_Debug
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
#if XRE_Debug
							str = "ins";
#endif
							pitem = &maxIns;
							goto SetMax;
						case CharMinus: // Maximum number of deletions.
#if XRE_Debug
							str = "del";
#endif
							pitem = &maxDel;
							goto SetMax;
						case CharHash:  // Maximum number of substitutions.
#if XRE_Debug
							str = "subst";
#endif
							pitem = &maxSubst;
							goto SetMax;
						case CharTilde: // Maximum number of edits.
#if XRE_Debug
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
	if((*pnode = ast_newIter(ctx->mem, *pnode, min, max, minimal)) == NULL)
		return REG_ESPACE;

#if EnableApprox
	// If approximate matching parameters are set, add them to the iteration node.
	if(costTotalSet || costsSet || editTotalSet || editsSet) {
		params_t *params;
		ast_iter_t *iter = (*pnode)->obj;

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
#if XRE_Debug
	fprintf(stderr, "parse_brace: min %d, max %d, ", min, max);
	printParams(false, &((ast_iter_t *) (*pnode)->obj)->params->pa);
	fputc('\n', stderr);
#endif
#else
	DPrintf((stderr, "parse_brace: min %d, max %d\n", min, max));
#endif
	ctx->re = re;
	return 0;
ERetn:
	return REG_BADBR;
	}

typedef enum {
	ParseRE = 0,
	ParseAtom,
	ParseMarkForSubmatch,
	ParseBranch,
	ParsePiece,
	ParseConcatenation,
	ParsePostConcatenation,
	ParseUnion,
	ParsePostUnion,
	ParsePostfix,
	ParseRestoreCFlags
	} parse_re_stack_symbol_t;

#if XRE_Debug > 1
static char *parseSymName[] = {
	"ParseRE", "ParseAtom", "ParseMarkForSubmatch", "ParseBranch", "ParsePiece", "ParseConcatenation",
	"ParsePostConcatenation", "ParseUnion", "ParsePostUnion", "ParsePostfix", "ParseRestoreCFlags"};
#endif

// Parse an atom and return status.  An atom is a regular expression enclosed in '()', an empty set of '()', a bracket
// expression, '.', '^', '$', a '\' followed by a character, or a single character.
static int parseAtom(parse_ctx_t *ctx, ast_node_t **pnode) {
	int status;
	xstack_t *stack = ctx->stack;
	char *str;

	// At end of pattern or in literal mode?
	if(ctx->re == ctx->re_end || ctx->cflags & REG_LITERAL)
		goto ParseLit;

	// Process current character (atom).
	switch(*ctx->re) {
		case CharLParen:	// Parenthesized subexpression.

			// Handle "(?...)" extensions in enhanced mode.  They work in a way similar to Perl's corresponding
			// extensions.
			if(ctx->cflags & REG_ENHANCED && ctx->re + 1 < ctx->re_end && ctx->re[1] == CharHook) {
				char *msg;
				int flag, new_cflags = ctx->cflags;
				int bit = 1;
				DPrintf((stderr, "parsePat: extension: '%.*" StrF "'\n", Rest(ctx->re)));
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
							DPrintf((stderr, "parsePat: turn off: '%.*" StrF "'\n", Rest(ctx->re)));
							++ctx->re;
							bit = 0;
							break;
						case CharColon:
							DPrintf((stderr, "parsePat: non-capturing: '%.*" StrF "'\n",
							 Rest(ctx->re)));
							++ctx->re;
							++ctx->nest_level;
							goto EndOpts;
						case CharHash:
							DPrintf((stderr, "parsePat: comment: '%.*" StrF "'\n", Rest(ctx->re)));

							// A comment can contain any character except a right parenthesis.
							for(;;) {
								if(++ctx->re == ctx->re_end)
									return REG_BADPAT;
								if(*ctx->re == CharRParen) {
									++ctx->re;
									goto EndOpts;
									}
								}
						case CharRParen:
							++ctx->re;
							goto EndOpts;
						default:
							return REG_BADPAT;
						}
					if(msg != NULL) {
						DPrintf((stderr, "parsePat: %s: '%.*" StrF "'\n", msg, Rest(ctx->re)));
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
			++ctx->nest_level;
			if(ctx->cflags & REG_ENHANCED && ctx->re + 2 < ctx->re_end && ctx->re[1] == CharHook &&
			 ctx->re[2] == CharColon) {
				DPrintf((stderr, "parsePat: group begin: '%.*" StrF "', no submatch\n", Rest(ctx->re)));

				// Don't mark for submatching.
				ctx->re += 3;
				StackPush(stack, int, ParseRE);
				}
			else {
				DPrintf((stderr, "parsePat: group begin: '%.*" StrF "', submatch %d\n", Rest(ctx->re),
				 ctx->submatch_id));
				++ctx->re;

				// First parse a whole RE, then mark the resulting tree for submatching.
				StackPush(stack, int, ctx->submatch_id);
				StackPush(stack, int, ParseMarkForSubmatch);
				StackPush(stack, int, ParseRE);
				++ctx->submatch_id;
				}
			break;
		case CharRParen:	// End of current subexpression.
			if(ctx->nest_level > 0) {
				DPrintf((stderr, "parsePat: empty: '%.*" StrF "'\n", Rest(ctx->re)));

				// We were expecting an atom, but instead the current subexpression was closed.  POSIX leaves
				// the meaning of this to be implementation-defined.  We interpret this as an empty expression
				// (which matches an empty string).
				if((*pnode = ast_newLit(ctx->mem, LitEmpty, -1, -1)) == NULL)
					return REG_ESPACE;
				}
			else
				goto ParseLit;
			break;
		case CharLBracket:	// Bracket expression.
			DPrintf((stderr, "parsePat: bracket: '%.*" StrF "'\n", Rest(ctx->re)));
			++ctx->re;
			if((status = parseBracket(ctx, pnode)) != 0)
				return status;
			break;
		case CharBackslash:	// Shortcut or escaped character.
			if(ctx->re + 1 == ctx->re_end)	// Trailing backslash.
				return REG_EESCAPE;

			// If not enhanced mode, process escaped character literally.
			if(!(ctx->cflags & REG_ENHANCED)) {
				++ctx->re;
				goto EscChar;
				}

			// Enhanced mode.  Check if \x shortcut.  If found, parse the expanded text recursively.
			{xchar_t buf[32];

			if(expandShortcut(buf, ctx->re + 1, false))
				ctx->pflags |= PropHaveEscLit;
			if(buf[0] != L'\0') {
				parse_ctx_t subctx;
				memcpy(&subctx, ctx, sizeof(subctx));
				subctx.re = subctx.re_start = buf;
				subctx.len = xstrlen(buf);
				subctx.re_end = buf + subctx.len;
				subctx.keepfirst = false;
				subctx.nest_level = 0;
				if((status = parsePat(&subctx)) != 0)
					return status;
				ctx->re += 2;
				ctx->position = subctx.position;
				*pnode = subctx.rootnode;
				break;
				}
			}

			// Check for other special characters allowed in enhanced mode.
			if(ctx->re[1] == L'Q') {
				DPrintf((stderr, "parsePat: tmp literal: '%.*" StrF "'\n", Rest(ctx->re)));
				ctx->cflags |= REG_LITERAL;
				ctx->temp_cflags |= REG_LITERAL;
				ctx->pflags |= PropHaveRegical;
				ctx->re += 2;
				if(ctx->re == ctx->re_end)
					return REG_BADPAT;
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
					{long val;
					if((status = hexchar(ctx, &val)) != 0)
						return status;
					*pnode = ast_newLit(ctx->mem, val, val, ctx->position);
					++ctx->position;
					}
					break;
				default:
					if(xisdigit(*ctx->re)) {

						// Back reference.
						int val = *ctx->re - L'0';
						DPrintf((stderr, "parsePat: backref: '%.*" StrF "'\n", Rest(ctx->re - 1)));
						if(ctx->cflags & REG_REVERSE)
							return REG_EREGREV;
						if(val == 0)
							return REG_ESUBREG;
						*pnode = ast_newLit(ctx->mem, LitBackref, val, ctx->position);
						ctx->max_backref = Max(val, ctx->max_backref);
						}
					else {
EscChar:
						// Escaped character.
						DPrintf((stderr, "parsePat: escaped: '%.*" StrF "'\n", Rest(ctx->re - 1)));
						*pnode = ast_newLit(ctx->mem, *ctx->re, *ctx->re, ctx->position);
						}
					++ctx->position;
					++ctx->re;
					break;
				}
			if(*pnode == NULL)
				return REG_ESPACE;
			break;
		case CharPeriod:	// The "any" symbol.
			DPrintf((stderr, "parsePat: any: '%.*" StrF "'\n", Rest(ctx->re)));
			if((ctx->cflags & (REG_NEWLINE | REG_ANY)) == REG_NEWLINE) {
				ast_node_t *tmp1;
				ast_node_t *tmp2;
NotNL:
				if((tmp1 = ast_newLit(ctx->mem, 0, L'\n' - 1, ctx->position)) == NULL ||
				 (tmp2 = ast_newLit(ctx->mem, L'\n' + 1, XRE_CHAR_MAX, ctx->position)) == NULL ||
				 (*pnode = ast_newUnion(ctx->mem, tmp1, tmp2)) == NULL)
					return REG_ESPACE;
				}
			else if((*pnode = ast_newLit(ctx->mem, 0, XRE_CHAR_MAX, ctx->position)) == NULL)
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
			if((*pnode = ast_newLit(ctx->mem, LitAssert, status, -1)) == NULL)
				return REG_ESPACE;
			++ctx->re;
			break;
		default:;
ParseLit:
			// Check for \E following prior \Q (enhanced mode only -- temp_cflags set).
			if(ctx->temp_cflags && ctx->re + 1 < ctx->re_end && ctx->re[0] == CharBackslash && ctx->re[1] == L'E') {
				ctx->cflags &= ~ctx->temp_cflags;
				ctx->temp_cflags = 0;
				DPrintf((stderr, "parsePat: end tmp, cflags %.4x: '%.*" StrF "'\n",
				 ctx->cflags, Rest(ctx->re)));
				ctx->re += 2;
				if(ctx->re < ctx->re_end)
					StackPush(stack, int, ParsePiece);
				break;
				}

			// We must have an atom at this point.  If not, we have a logic error.
			assert(ctx->re < ctx->re_end);
			DPrintf((stderr, "parsePat: literal: '%.*" StrF "'\n", Rest(ctx->re)));

			// Note that we can't use an xisalpha() test here because there may be characters which are alphabetic
			// but neither upper or lower case.
			if(ctx->cflags & REG_ICASE && (xisupper(*ctx->re) || xislower(*ctx->re))) {
				ast_node_t *tmp1;
				ast_node_t *tmp2;

				// XXX - Can there be more than one opposite-case counterpoints for some character in some
				// locale?  Or more than two characters which all should be regarded the same character if case
				// is ignored?  If yes, there does not seem to be a portable way to detect it.  I guess that at
				// least for multi-character collating elements there could be several opposite-case
				// counterpoints, but they cannot be supported portably anyway.
				if((tmp1 = ast_newLit(ctx->mem, xtoupper(*ctx->re), xtoupper(*ctx->re), ctx->position)) == NULL)
					return REG_ESPACE;
				if((tmp2 = ast_newLit(ctx->mem, xtolower(*ctx->re), xtolower(*ctx->re), ctx->position)) == NULL)
					return REG_ESPACE;
				if((*pnode = ast_newUnion(ctx->mem, tmp1, tmp2)) == NULL)
					return REG_ESPACE;
				}
			else if((*pnode = ast_newLit(ctx->mem, *ctx->re, *ctx->re, ctx->position)) == NULL)
				return REG_ESPACE;

			++ctx->position;
			++ctx->re;
			break;
		}

	return 0;
	}

// Parse a regular expression pattern into an abstract syntax tree (AST), save in ctx->rootnode, and return status.
int parsePat(parse_ctx_t *ctx) {
	ast_node_t *node = NULL;
	int status;
	xstack_t *stack = ctx->stack;
	int bottom = xstack_num_objects(stack);
#if XRE_Debug > 1
	parse_re_stack_symbol_t sym;
#endif

	DPrintf((stderr, "parsePat: parsing '%.*" StrF "', len %lu\n", (int) ctx->len, ctx->re, ctx->len));
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
	// for two reasons: (1), compatibility with systems which have an overflowable call stack; and (2), efficiency (both in
	// lines of code and speed).
	while(xstack_num_objects(stack) > bottom) {
#if XRE_Debug > 1
		sym = xstack_pop_int(stack);
		fprintf(stderr, "parsePat[%d]: %s begin: at '%.*" StrF "', cflags %.4x, temp_cflags %.4x\n",
		 stack->idx, parseSymName[sym], Rest(ctx->re), ctx->cflags, ctx->temp_cflags);
		switch(sym) {
#else
		switch(xstack_pop_int(stack)) {
#endif
			case ParseRE:
				// Parse a full regexp.  A regexp is one or more branches, separated by the union operator '|'.
				if(!(ctx->cflags & REG_LITERAL))
					StackPush(stack, int, ParseUnion);
				StackPush(stack, int, ParseBranch);
				break;
			case ParseBranch:
				// Parse a branch.  A branch is one or more pieces, concatenated.  A piece is an atom possibly
				// followed by a postfix operator.
				if(!(ctx->cflags & REG_LITERAL)) {
					if(ctx->re == ctx->re_end)
						return REG_EMPTY;
					switch(*ctx->re) {
						case CharAsterisk:
						case CharPlus:
						case CharHook:
						case CharLBrace:
							return REG_BADRPT;
						case CharPipe:
							return REG_EMPTY;
						case CharRParen:
							if(ctx->nest_level > 0 && ctx->re[-1] != CharLParen)
								return REG_EMPTY;
						}
					}
				StackPush(stack, int, ParseConcatenation);
				StackPush(stack, int, ParsePiece);
				break;
			case ParsePiece:
				// Parse a piece.  A piece is an atom possibly followed by one or more postfix operators.
				if(!(ctx->cflags & REG_LITERAL))
					StackPush(stack, int, ParsePostfix);
				StackPush(stack, int, ParseAtom);
				break;
			case ParseConcatenation:
				// If the expression has not ended and more remains than just "\E", parse another piece.
				{size_t n = ctx->re_end - ctx->re;
				xchar_t c;
				if(n == 0 || (n == 2 && ctx->re[0] == CharBackslash && ctx->re[1] == L'E'))
					break;
				c = *ctx->re;
				if(!(ctx->cflags & REG_LITERAL)) {
					if(c == CharPipe)
						break;
					if(c == CharRParen && ctx->nest_level > 0) {
						DPrintf((stderr, "parsePat: group end: '%.*" StrF "'\n", Rest(ctx->re)));
						--ctx->nest_level;
						break;
						}
					}
				if(ctx->cflags & REG_RIGHTASSOC) {

					// Right associative concatenation.
					StackPush(stack, voidptr, node);
					StackPush(stack, int, ParsePostConcatenation);
					StackPush(stack, int, ParseConcatenation);
					StackPush(stack, int, ParsePiece);
					}
				else {
					// Default case, left associative concatenation.
					StackPush(stack, int, ParseConcatenation);
					StackPush(stack, voidptr, node);
					StackPush(stack, int, ParsePostConcatenation);
					StackPush(stack, int, ParsePiece);
					}
				}
				break;
			case ParsePostConcatenation:
				if((node = ast_newCat(ctx->mem, xstack_pop_voidptr(stack), node)) == NULL)
					return REG_ESPACE;
				break;
			case ParseUnion:
				if(ctx->re == ctx->re_end || ctx->cflags & REG_LITERAL)
					break;
				switch(*ctx->re) {
					case CharPipe:
						DPrintf((stderr, "parsePat: union: '%.*" StrF "'\n", Rest(ctx->re)));
						StackPush(stack, int, ParseUnion);
						StackPush(stack, voidptr, node);
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
				if((node = ast_newUnion(ctx->mem, (ast_node_t *) xstack_pop_voidptr(stack), node)) == NULL)
					return REG_ESPACE;
				break;
			case ParsePostfix:
				// Parse postfix operators.
				if(ctx->re == ctx->re_end || ctx->cflags & REG_LITERAL)
					break;
				switch(*ctx->re) {
					case CharAsterisk:
					case CharPlus:
					case CharHook:
						{int minimal = (ctx->cflags & REG_UNGREEDY) == REG_UNGREEDY;
						int rep_min = 0;
						int rep_max = -1;
#if XRE_Debug
						const xchar_t *tmp_re;
#endif

						if(*ctx->re == CharPlus)
							rep_min = 1;
						else if(*ctx->re == CharHook)
							rep_max = 1;
#if XRE_Debug
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
						if((node = ast_newIter(ctx->mem, node, rep_min, rep_max, minimal)) == NULL)
							return REG_ESPACE;
						StackPush(stack, int, ParsePostfix);
						}
						break;
					case CharLBrace:
						DPrintf((stderr, "parsePat: bound: '%.*" StrF "'\n", Rest(ctx->re)));
						++ctx->re;

						if((status = parseBrace(ctx, &node)) != 0)
							return status;
						StackPush(stack, int, ParsePostfix);
						break;
					}
				break;
			case ParseAtom:
				if((status = parseAtom(ctx, &node)) != 0)
					return status;
				break;
			case ParseMarkForSubmatch:
				{int submatch_id = xstack_pop_int(stack);
				if(node->submatch_id >= 0) {
					ast_node_t *tmp_node1, *tmp_node2;
					if((tmp_node2 = ast_newLit(ctx->mem, LitEmpty, -1, -1)) == NULL ||
					 (tmp_node1 = ast_newCat(ctx->mem, tmp_node2, node)) == NULL)
						return REG_ESPACE;
					tmp_node1->num_submatches = node->num_submatches;
					node = tmp_node1;
					}
				node->submatch_id = submatch_id;
				++node->num_submatches;
				}
				break;
			case ParseRestoreCFlags:
				ctx->cflags = xstack_pop_int(stack);
				break;
			default:
				assert(0);
			}
		}
	DPrintf((stderr, "parsePat: parse '%.*" StrF "' END\n", (int) ctx->len, ctx->re_start));

	// Check for missing closing parentheses.
	if(ctx->nest_level > 0)
		return REG_EPAREN;

	ctx->rootnode = node;
	return 0;
	}

#ifdef __cplusplus
	}
#endif
