// xre.h - XRE public API definitions.
//
// (c) Copyright 2022 Richard W. Marinelli
//
// This work is based on TRE ver. 0.7.5 (c) Copyright 2001-2006 Ville Laurikari <vl@iki.fi> and is licensed
// under the GNU Lesser General Public License (LGPLv3).  To view a copy of this license, see the "License.txt"
// file included with this distribution or visit http://www.gnu.org/licenses/lgpl-3.0.en.html.

#ifndef xre_h
#define xre_h

// Configuration options.  Note that enabling multibyte support (EnableMultibyte) requires that wide character support
// (EnableWChar) also be enabled, but EnableWChar can be set without setting EnableMultibyte.
#define HaveHdr_wchar		1	// Have the <wchar.h> header?
#define HaveHdr_wctype		1	// Have the <wctype.h> header?
#define HaveHdr_libutf8		0	// Have the <libutf8.h> header?

#define EnableWChar		1	// Enable wide character (wchar_t) support?
#define EnableMultibyte		1	// Enable multibyte character set support?
#define EnableApprox		1	// Enable approximate matching?
#define EnableReverse		1	// Enable reversed patterns and backward matching?

#define XRE_Version		"1.1.0"

#if EnableMultibyte && !EnableWChar
#undef EnableWChar
#define EnableWChar		1
#endif

#include <sys/types.h>
#if HaveHdr_libutf8
#include <libutf8.h>
#endif
#include <stdlib.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// Since we're not using the system regex.h, we need to define the structs and flags ourselves.
typedef int regoff_t;
typedef struct {
	size_t re_nsub;		// Number of parenthesized subexpressions.
	void *re_cpat;		// For internal use only.
	} regex_t;

typedef struct {
	regoff_t rm_so;
	regoff_t rm_eo;
	} regmatch_t;

// POSIX error codes (in the order listed in the standard).
#define REG_OK			0	// No error.
#define REG_NOMATCH		1	// Match failed.
#define REG_BADPAT		2	// Invalid regular expression.
#define REG_ECOLLATE		3	// Unknown collating element.
#define REG_ECTYPE		4	// Unknown character class name.
#define REG_EESCAPE		5	// Trailing backslash invalid.
#define REG_ESUBREG		6	// Invalid back reference.
#define REG_EBRACK		7	// Brackets '[ ]' not balanced
#define REG_EPAREN		8	// Parentheses '( )' not balanced
#define REG_EBRACE		9	// Braces '{ }' not balanced
#define REG_BADBR		10	// Invalid repetition count(s) in '{ }'.
#define REG_ERANGE		11	// Invalid character range in '[ ]'.
#define REG_ESPACE		12	// Out of memory.
#define REG_BADRPT		13	// Invalid use of repetition operator.
#define REG_EMPTY		14	// Empty (sub)expression.
#define REG_EHEX		15	// Invalid hexadecimal value.
#define REG_STRCHAR		16	// Invalid multibyte character in string.
#define REG_PATCHAR		17	// Invalid multibyte character in pattern.
#define REG_EPARAM		18	// Invalid approximate matching parameter(s).
#define REG_EREGREV		19	// Unsupported element(s) in reversed pattern.

// POSIX regcomp() flags.
#define REG_EXTENDED		0x0000		// For POSIX compatibility -- not used (implicitly set).
#define REG_ICASE		0x0001		// Compile for matching that ignores case.
#define REG_NEWLINE		0x0002		// Compile for newline-sensitive matching.
#define REG_NOSUB		0x0004		// Compile for reporting success or failure only, not what was matched.

// Extra xregcomp() flags.
#define REG_ENHANCED		0x0008		// Recognize enhanced regular expression features.
#define REG_APPROX		0x0010		// Recognize approximate matching parameters inside "{...}".
#define REG_ANY			0x0020		// '.' and "[^...]" match newline when REG_NEWLINE set.
#define REG_LITERAL		0x0040		// Interpret the entire RE literally; that is, as all ordinary characters.
#define REG_RIGHTASSOC		0x0080		// Compile so that concatenation of subpatterns is right associative.
#define REG_UNGREEDY		0x0100		// Use non-greedy (minimal) repetitions instead of the normal greedy ones.
#define REG_REVERSED		0x0200		// Flag RE as a reversed pattern to be used for backward matching.

// Synonym flags.
#define REG_NOSPEC		REG_LITERAL
#define REG_MINIMAL		REG_UNGREEDY

// For internal use 
#define CompTopLevel		0x8000		// Interpret RE as a non-parenthesized (top level) subexpression when reversing.

// POSIX regexec() flags.
#define REG_NOTBOL		0x0001		// The '^' assertion does not match beginning of string.
#define REG_NOTEOL		0x0002		// The '$' assertion does not match end of string.

// Extra xregexec() flags.
#define REG_WORDCHBOS		0x0004		// A word character is assumed to exist immediately before beginning of string
						// (which is at end of scan if REG_REVERSED flag set).
#define REG_WORDCHEOS		0x0008		// A word character is assumed to exist immediately after end of string (which
						// is at start of scan if REG_REVERSED flag set).
#define REG_BESTMATCH		0x0010		// Find best approximate match in string (not necessarily the first).

// For internal use.
#define ExecClassCheck		0x1000		// Check current symbol against transition class(es).

// The maximum number of iterations in a bound expression (POSIX).
#undef RE_DUP_MAX
#define RE_DUP_MAX		255

// The POSIX regexp functions.
extern int xregcomp(regex_t *preg, const char *pat, int cflags);
extern int xregexec(const regex_t *preg, const char *string, size_t nmatch, regmatch_t pmatch[], int eflags);
extern size_t xregerror(int errcode, const regex_t *preg, char *errbuf, size_t errbuf_size);
extern void xregfree(regex_t *preg);

// Versions with a maximum length argument and therefore the capability to handle null characters in the middle of the strings
// (not in POSIX).
extern int xregncomp(regex_t *preg, const char *pat, size_t len, int cflags);
extern int xregnexec(const regex_t *preg, const char *string, size_t len, size_t nmatch, regmatch_t pmatch[], int eflags);

#if EnableWChar
#if HaveHdr_wchar
#include <wchar.h>
#endif

// Wide character versions (not in POSIX).
extern int xregwcomp(regex_t *preg, const wchar_t *wpat, int cflags);
extern int xregwexec(const regex_t *preg, const wchar_t *string, size_t nmatch, regmatch_t pmatch[], int eflags);
extern int xregwncomp(regex_t *preg, const wchar_t *wpat, size_t len, int cflags);
extern int xregwnexec(const regex_t *preg, const wchar_t *string, size_t len, size_t nmatch, regmatch_t pmatch[], int eflags);

typedef wchar_t xchar_t;
typedef wint_t xcint_t;
#else
typedef char xchar_t;
typedef short xcint_t;
#endif

typedef struct {
	bool (*nextchar)(xcint_t *pc, int *plen, void *context);
	void (*rewind)(size_t pos, void *context);
	int (*compare)(size_t pos1, size_t pos2, size_t len, void *context);
	void *context;
	} regusource_t;

// User data version (not in POSIX).
extern int xreguexec(const regex_t *preg, const regusource_t *string, size_t nmatch, regmatch_t pmatch[], int eflags);

// Structure for approximate matching parameters.
typedef struct {
	int cost_ins; 			// Default cost of an inserted character.
	int cost_del; 			// Default cost of a deleted character.
	int cost_subst; 		// Default cost of a substituted character.
	int max_cost; 			// Maximum allowed cost of a match.

	int max_ins;			// Maximum allowed number of inserts.
	int max_del;			// Maximum allowed number of deletes.
	int max_subst;			// Maximum allowed number of substitutes.
	int max_edit;			// Maximum allowed number of edits.
	} regaparams_t;

#if EnableApprox

// Approximate matching result struct.
typedef struct {
	size_t nmatch;			// Length of pmatch[] array.
	regmatch_t *pmatch;		// Submatch data.
	int cost; 			// Cost of the match.
	int num_ins;			// Number of inserts in the match.
	int num_del;			// Number of deletes in the match.
	int num_subst;			// Number of substitutions in the match.
	} regamatch_t;

// Parameter-setting options for xregainit().
#define REG_MODERATE		0		// Moderate-approximation matching (default).
#define REG_EXACT		1		// Exact matching.
#define REG_MAXAPPROX		2		// Maximum-approximation matching.

#define FuzzyParams		{1, 1, 1, INT_MAX, 1, 1, 1, 1}	// Default parameters for "fuzzy" matching.

// Approximate matching functions (not in POSIX).
extern int xregaexec(const regex_t *preg, const char *string, regamatch_t *match, regaparams_t *params, int eflags);
extern int xreganexec(const regex_t *preg, const char *string, size_t len, regamatch_t *match, regaparams_t *params,
 int eflags);

#if EnableWChar
// Wide character approximate matching (not in POSIX).
extern int xregawexec(const regex_t *preg, const wchar_t *string, regamatch_t *match, regaparams_t *params, int eflags);
extern int xregawnexec(const regex_t *preg, const wchar_t *string, size_t len, regamatch_t *match, regaparams_t *params,
 int eflags);
#endif

// User data version (not in POSIXq).
extern int xregauexec(const regex_t *preg, const regusource_t *string, regamatch_t *match, regaparams_t *params, int eflags);

// Sets the parameters to default values according to 'type'.
extern void xregainit(regaparams_t *params, int level);
#endif // EnableApprox.

extern const char *xregmsg(int errcode);

#if EnableReverse
extern int xregrev(char *revpat, const char *pat, int cflags);
extern int xregnrev(char *revpat, const char *pat, size_t n, int cflags);
#if EnableWChar
extern int xregwrev(wchar_t *revpat, const wchar_t *pat, int cflags);
extern int xregwnrev(wchar_t *revpat, const wchar_t *pat, size_t n, int cflags);
#endif
#endif

// Returns information about a compiled pattern.
extern int xreginfo(const regex_t *preg);

#define PatBackrefs		0x0001
#define PatApprox		0x0002

// Returns library configuration flags.
extern int xlibconf(void);

#define ConfigWChar		0x0001
#define ConfigMultibyte		0x0002
#define ConfigApprox		0x0004
#define ConfigReverse		0x0008

// Returns a library version string.
extern char *xrevers(void);

#ifdef __cplusplus
	}
#endif
#endif
