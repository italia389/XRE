// internal.h - XRE internal definitions.
//
// (c) Copyright 2022 Richard W. Marinelli
//
// This work is based on TRE ver. 0.7.5 (c) Copyright 2001-2006 Ville Laurikari <vl@iki.fi> and is licensed
// under the GNU Lesser General Public License (LGPLv3).  To view a copy of this license, see the "License.txt"
// file included with this distribution or visit http://www.gnu.org/licenses/lgpl-3.0.en.html.

#ifndef xre_internal_h
#define xre_internal_h

#if HaveHdr_wchar
#include <wchar.h>
#endif
#if HaveHdr_wctype
#include <wctype.h>
#endif

#ifdef XRE_Debug
#include <stdio.h>
#define DPrintf(msg) fprintf msg, fflush(stderr)
#else
#define DPrintf(msg)
#endif

#define elementsof(x) (sizeof(x) / sizeof(x[0]))

#if EnableWChar && EnableMultibyte
#define UseMBState
#endif

#ifdef __cplusplus
extern "C" {
#endif

// Define the character types and functions.
#if EnableWChar

// Wide characters.
#define XRE_CHAR_MAX		WCHAR_MAX

#if EnableMultibyte
#define XRE_MB_CUR_MAX		MB_CUR_MAX
#else
#define XRE_MB_CUR_MAX		1
#endif

#define xisalnum		iswalnum
#define xisalpha		iswalpha
#define xisblank		iswblank
#define xiscntrl		iswcntrl
#define xisdigit		iswdigit
#define xisgraph		iswgraph
#define xislower		iswlower
#define xisprint		iswprint
#define xispunct		iswpunct
#define xisspace		iswspace
#define xisupper		iswupper
#define xisxdigit		iswxdigit

#define xtolower		towlower
#define xtoupper		towupper
#define xstrlen			wcslen
#define xstrchr			wcschr
#define xmemcpy			wmemcpy

#include <ctype.h>
#else // !EnableWChar

// 8-bit characters.
#define XRE_CHAR_MAX		255
#define XRE_MB_CUR_MAX		1

#define xisalnum		isalnum
#define xisalpha		isalpha
#define xisascii		isascii
#define xisblank		isblank
#define xiscntrl		iscntrl
#define xisdigit		isdigit
#define xisgraph		isgraph
#define xislower		islower
#define xisprint		isprint
#define xispunct		ispunct
#define xisspace		isspace
#define xisupper		isupper
#define xisxdigit		isxdigit

#define xtolower(c)		(xcint_t)(tolower(c))
#define xtoupper(c)		(xcint_t)(toupper(c))
#define xstrlen			strlen
#define xstrchr			strchr
#define xmemcpy			memcpy

#if __gnu_linux__
#define __USE_SVID
#endif
#include <ctype.h>
#if __gnu_linux__
#undef __USE_SVID
#endif
#endif // !EnableWChar.

#if EnableWChar

// Use system provided iswctype() and wctype().
typedef wctype_t xctype_t;
#define xisctype		iswctype
#define xctype			wctype
#else

// Define our own versions of iswctype() and wctype().
typedef int (*xctype_t)(xcint_t);
#define xisctype(c, type)	((type)(c))
extern xctype_t xctype(const char *name);
#endif

typedef enum {StrByte, StrWide, StrMBS, StrUser} xstr_t;

// Returns number of bytes to add to (char *) ptr to make it properly aligned for the type.
#define AlignBytes(ptr, type) ((((unsigned long) ptr) % sizeof(type)) ?\
 (sizeof(type) - (((unsigned long) ptr) % sizeof(type))) : 0)

#undef Max
#undef Min
#define Max(a, b) (((a) >= (b)) ? (a) : (b))
#define Min(a, b) (((a) <= (b)) ? (a) : (b))

// Define StrF to the correct printf formatter for strings.
#if EnableWChar
#define StrF			"ls"
#else
#define StrF			"s"
#endif

// Approximate matching parameters that can be changed dynamically while matching.
typedef struct {
	regaparams_t pa;		// Costs and limits.
	int depth;			// Parameter depth.
	} params_t;

// TNFA transition type.  A TNFA state is an array of transitions; the terminator is a transition with NULL 'state'.
typedef struct tnfa_transition {
	xcint_t code_min;		// Range of accepted characters.
	xcint_t code_max;
	struct tnfa_transition *state;	// Pointer to the destination state.
	int state_id;			// ID number of the destination state.
	int *tags;			// -1 terminated array of tag IDs (or NULL).
	params_t *params;		// Approximate matching parameters (or NULL).
	int assertions;			// Assertion bitmap.
	union {				// Assertion parameters.
		xctype_t class;		// Character class assertion.
		int backref;		// Back reference assertion.
		} u;
	xctype_t *neg_classes;		// Negative character class assertions.
	} tnfa_transition_t;

// Assertions.
#define AssertAtBOL		0x0001 	// Beginning of line.
#define AssertAtEOL		0x0002	// End of line.
#define AssertAtBOW		0x0004	// Beginning of word.
#define AssertAtEOW		0x0008	// End of word.
#define AssertAtWB		0x0010	// Word boundary.
#define AssertAtNegWB		0x0020	// Not a word boundary.
#define AssertCC		0x0040	// Character class in 'class'.
#define AssertNegCC		0x0080	// Character classes in 'neg_classes'.
#define AssertBackref		0x0100	// A back reference in 'backref'.
#define AssertLast		0x0100

// Tag directions.
typedef enum {
	TagMinimize = 0,
	TagMaximize = 1
	} tag_direction_t;

// RE property flags.
#define PropHaveApprox		0x0001	// RE contains approximate matching features.
#define PropHaveBackrefs	0x0002	// RE contains back reference(s).

// Special approximate matching parameter values.
#define ParamDefault		-2
#define ParamUnset		-1

// Instructions to compute submatch register values from tag values after a successful match.
typedef struct submatch_data {
	int so_tag;			// Tag that gives the value for rm_so (submatch start offset).
	int eo_tag;			// Tag that gives the value for rm_eo (submatch end offset).
	int *parents;			// -1 terminated list of submatches (indices) this submatch is contained in.
	} submatch_data_t;

// TNFA definition.
typedef struct {
	tnfa_transition_t *transitions;		// Array of transitions.
	unsigned num_transitions;		// Total number of transitions.
	tnfa_transition_t *initial;		// NULL-terminated (per initial->state) array of initial transitions.
	tnfa_transition_t *final;		// Final transition.
	submatch_data_t *submatch_data;		// Array of objects holding information about each subexpression in RE.
	unsigned num_submatches;		// Number of subexpressions in RE, including whole RE (>= 1).
	char *firstpos_chars;
	int first_char;
	tag_direction_t *tag_directions;	// Array of tag_direction_t enumerators, using tag_id as index.
	int num_tags;				// Total number of tags (minimal and maximal).
	int *minimal_tags;			// -1 terminated array of minimal tag ids.
	int num_minimals;			// Number of minimal tags.
	int end_tag;				// End tag ID of whole RE match.
	int num_states;				// Number of states in the TNFA.
	int cflags;				// RE compilation flags.
	int pflags;				// RE property flags.
	int params_depth;
	} tnfa_t;

#ifdef XRE_Debug
extern void print_codes_and_tags(int code_min, int code_max, int *tags);
#if EnableApprox
extern void printParams(bool printLabel, regaparams_t *params);
#endif
extern void printTags(char *label, int *tags, int count);
#endif
extern int compilePat(regex_t *preg, const xchar_t *regex, size_t n, int cflags);
extern void fillMatch(size_t nmatch, regmatch_t pmatch[], int cflags, const tnfa_t *tnfa, int *tags, int match_eo);
extern void refree(regex_t *preg);
extern int runBackref(const tnfa_t *tnfa, const void *string, ssize_t len, xstr_t type, int *match_tags, int eflags,
 int *match_end_ofs);
extern int runParallel(const tnfa_t *tnfa, const void *string, ssize_t len, xstr_t type, int *match_tags, int eflags,
 int *match_end_ofs);

#if EnableApprox
extern int runApprox(const tnfa_t *tnfa, const void *string, ssize_t len, xstr_t type, int *match_tags,
 regamatch_t *match, regaparams_t *params, int eflags, int *match_end_ofs);
#endif

#if EnableReverse
extern int grpcpy(xchar_t **pdest, const xchar_t **psrc, int cflags);
#endif

#ifdef __cplusplus
	}
#endif
#endif
