// xreg.c - POSIX ERE-compatible compilation, matching, and error reporting functions plus approximate matching routines.
//
// (c) Copyright 2020 Richard W. Marinelli
//
// This work is based on TRE ver. 0.7.5 (c) Copyright 2001-2006 Ville Laurikari <vl@iki.fi> and is licensed
// under the GNU Lesser General Public License (LGPLv3).  To view a copy of this license, see the "License.txt"
// file included with this distribution or visit http://www.gnu.org/licenses/lgpl-3.0.en.html.

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#if HaveHdr_wchar
#include <wchar.h>
#endif
#if HaveHdr_wctype
#include <wctype.h>
#endif
#if !EnableWChar
#include <ctype.h>
#endif
#include <limits.h>

#include "xre.h"
#include "internal.h"

#ifdef __cplusplus
extern "C" {
#endif

#if EnableWChar

// Convert a single-byte or multibyte RE pattern of length *plen to wide format.  The 'wpat' buffer is null terminated.
static int towide(wchar_t *wpat, const char *pat, size_t *plen) {
	size_t len = *plen;

	// If the current locale uses the standard single byte encoding of characters, we don't do a multibyte string
	// conversion.  If we did, many applications which use the default locale would break since the default "C" locale uses
	// the 7-bit ASCII character set, and all characters with the eighth bit set would be considered invalid.
#if EnableMultibyte
	if(XRE_MB_CUR_MAX == 1)
#endif
		{const char *str, *strz;
		wchar_t *wstr = wpat;

		strz = (str = (const char *) pat) + len;
		while(str < strz)
			*wstr++ = *str++;
		*wstr = L'\0';
		}
#if EnableMultibyte
	else {
		int consumed;
		size_t len1 = len;
		wchar_t *wcptr = wpat;
		mbstate_t state;
		memset(&state, '\0', sizeof(state));
		while(len1 > 0) {
			consumed = mbrtowc(wcptr, pat, len1, &state);

			switch(consumed) {
				case 0:
					// Null byte.
					consumed = 1;
					break;
				case -1:
					DPrint((stderr, "mbrtowc: error %d: %s.\n", errno, strerror(errno)));
					return REG_BADPAT;
				case -2:
					// The last character wasn't complete.  Let's just ignore it and stop here.
					goto Done;
				}
			pat += consumed;
			len1 -= consumed;
			++wcptr;
			}
Done:
		*wcptr = L'\0';
		*plen = wcptr - wpat;
		}
#endif // EnableMultibyte
	return REG_OK;
	}
#endif // EnableWChar

int xregncomp(regex_t *preg, const char *pat, size_t len, int cflags) {
	int status;
#if EnableWChar
	wchar_t wpat[len + 1];

	if((status = towide(wpat, pat, &len)) == REG_OK)
		status = compilePat(preg, wpat, len, cflags);
#else
	status = compilePat(preg, (const xchar_t *) pat, len, cflags);
#endif
	return status;
	}

int xregcomp(regex_t *preg, const char *pat, int cflags) {

	return xregncomp(preg, pat, strlen(pat), cflags);
	}

#if EnableWChar
int xregwncomp(regex_t *preg, const wchar_t *wpat, size_t len, int cflags) {

	return compilePat(preg, wpat, len, cflags);
	}

int xregwcomp(regex_t *preg, const wchar_t *wpat, int cflags) {

	return compilePat(preg, wpat, wcslen(wpat), cflags);
	}
#endif

void xregfree(regex_t *preg) {

	refree(preg);
	}

// Error message strings for error codes listed in 'xre.h'.  This list needs to be in sync with the codes listed there,
// naturally.
static const char *error_messages[] = {
	"No error",					// REG_OK
	"Match failed",					// REG_NOMATCH
	"Invalid regular expression",			// REG_BADPAT
	"Unknown collating element",			// REG_ECOLLATE
	"Unknown character class name",			// REG_ECTYPE
	"Trailing backslash invalid",			// REG_EESCAPE
	"Invalid back reference",			// REG_ESUBREG
	"Brackets '[ ]' not balanced",			// REG_EBRACK
	"Parentheses '( )' not balanced",		// REG_EPAREN
	"Braces '{ }' not balanced",			// REG_EBRACE
	"Invalid repetition count(s) in '{ }'",		// REG_BADBR
	"Invalid character range in '[ ]'",		// REG_ERANGE
	"Out of memory",				// REG_ESPACE
	"Invalid use of repetition operator",		// REG_BADRPT
	"Empty (sub)expression",			// REG_EMPTY
	"Invalid hexadecimal value",			// REG_EHEX
	"Invalid multibyte character in string",	// REG_STRCHAR
	"Invalid multibyte character in pattern",	// REG_PATCHAR
	"Invalid approximate matching parameter(s)",	// REG_EPARAM
	"Unsupported element(s) in reversed pattern"	// REG_EREGREV
	};

// Return an error message like strerror(), but in a thread safe manner.
const char *xregmsg(int errcode) {

	return (errcode >= 0 && errcode < (int) elementsof(error_messages)) ? error_messages[errcode] : "Unknown error";
	}

size_t xregerror(int errcode, const regex_t *preg, char *errbuf, size_t errbuf_size) {
	const char *err;
	size_t errlen;

	err = xregmsg(errcode);
	errlen = strlen(err) + 1;
	if(errbuf_size > 0 && errbuf != NULL) {
		if(errlen > errbuf_size) {
			strncpy(errbuf, err, errbuf_size - 1);
			errbuf[errbuf_size - 1] = '\0';
			}
		else
			strcpy(errbuf, err);
		}
	return errlen;
	}

// RE matching functions.

// Fills the POSIX regmatch_t array from the TNFA tag and match endpoint values.
void fillMatch(size_t nmatch, regmatch_t pmatch[], int cflags, const tnfa_t *tnfa, int *tagpos, int match_eo) {
	submatch_data_t *submatch_data;
	regmatch_t *pmatchi;
	unsigned i;
	int *parent;

#ifdef XRE_Debug
	fprintf(stderr, "fillMatch(): nmatch %lu, cflags %.4X, match_eo %d\n", nmatch, cflags, match_eo);
	if(tnfa->num_submatches > 0) {
		submatch_data_t *sd, *sdz;
		sdz = (sd = tnfa->submatch_data) + tnfa->num_submatches;
		i = 0;
		do {
			fprintf(stderr, "submatch_data[%u] = {so_tag = %d, eo_tag = %d, parents = ",
			 i, sd->so_tag, sd->eo_tag);
			if(sd->parents == NULL)
				fputs("NULL}\n", stderr);
			else {
				printTags("", sd->parents, -1);
				fputs("}\n", stderr);
				}
			} while(++sd < sdz);
		}
	fprintf(stderr, "tagpos (%p)", tagpos);
	printTags("", tagpos, tnfa->num_tags);
	fputc('\n', stderr);
#endif
	i = 0;
	if(match_eo >= 0 && !(cflags & REG_NOSUB)) {

		// Construct submatch offsets from the tag positions (tagpos).
		DPrint((stderr, "end tag = t%d = %d\n", tnfa->end_tag, match_eo));
		submatch_data = tnfa->submatch_data;
		pmatchi = pmatch;
		while(i < tnfa->num_submatches && i < nmatch) {
			pmatchi->rm_so = (submatch_data->so_tag == tnfa->end_tag) ? match_eo :
			 tagpos[submatch_data->so_tag];
			pmatchi->rm_eo = (submatch_data->eo_tag == tnfa->end_tag) ? match_eo :
			 tagpos[submatch_data->eo_tag];

			// If either of the endpoints were not used, this submatch was not part of the match.
			if(pmatchi->rm_so == -1 || pmatchi->rm_eo == -1)
				pmatchi->rm_so = pmatchi->rm_eo = -1;

			DPrint((stderr, "pmatch[%d] = {t%d = %d, t%d = %d}\n", i,
			 submatch_data->so_tag, pmatchi->rm_so, submatch_data->eo_tag, pmatchi->rm_eo));
			++i;
			++submatch_data;
			++pmatchi;
			}

		// Reset all submatches that are not entirely within all of their parent submatches.
		i = 0;
		submatch_data = tnfa->submatch_data;
		pmatchi = pmatch;
		while(i < nmatch) {
			if(i >= tnfa->num_submatches)
				pmatchi->rm_so = pmatchi->rm_eo = -1;
			else {
				DPrint((stderr, "reset check %d: rm_so %d, rm_eo %d\n", i, pmatchi->rm_so, pmatchi->rm_eo));
				if(pmatchi->rm_eo == -1)
					assert(pmatchi->rm_so == -1);
				assert(pmatchi->rm_so <= pmatchi->rm_eo);

				if((parent = submatch_data->parents) != NULL)
					while(*parent >= 0) {
						DPrint((stderr, "pmatch[%d] parent is %d\n", i, *parent));
						if(pmatchi->rm_so < pmatch[*parent].rm_so ||
						 pmatchi->rm_eo > pmatch[*parent].rm_eo)
							pmatchi->rm_so = pmatchi->rm_eo = -1;
						++parent;
						}
				++submatch_data;
				}
			DPrint((stderr, "reset pmatch[%d] = {%d, %d}\n", i, pmatchi->rm_so, pmatchi->rm_eo));
			++i;
			++pmatchi;
			}
		}

	while(i < nmatch) {
		pmatch[i].rm_so = pmatch[i].rm_eo = -1;
		++i;
		}
	}

// Return information about a compiled pattern.
int xreginfo(const regex_t *preg) {
	tnfa_t *tnfa = (tnfa_t *) preg->re_cpat;
	int result = 0;
	if(tnfa->pflags & PropHaveBackrefs)
		result |= PatBackrefs;
	if(tnfa->pflags & PropHaveApprox)
		result |= PatApprox;
	return result;
	}

// Return library configuration flags.
int xlibconf(void) {
	int result = 0;
#if EnableWChar
	result |= ConfigWChar;
#endif
#if EnableMultibyte
	result |= ConfigMultibyte;
#endif
#if EnableApprox
	result |= ConfigApprox;
#endif
#if EnableReverse
	result |= ConfigReverse;
#endif
	return result;
	}

// Return the (static) library version string.
char *xlibvers(void) {
	static char str[32];

	if(str[0] == '\0')
		(void) sprintf(str, "XRE %s (LGPLv3)", XRE_Version);
	return str;
	}

#if EnableApprox
// Initialize approximate matching parameters per specified level.
void xregainit(regaparams_t *params, int level) {
	static regaparams_t fuzzy = FuzzyParams;

	// Initialize parameters so that all individual costs are 1 and all maximums (including maximum cost) are either 0
	// (which is the equivalent of "exact matching" as a starting point - REG_EXACT flag) or INT_MAX (which sets "maximum
	// fuzziness" as a starting point - REG_MAXAPPROX flag).  These settings can then be overridden by the caller and/or
	// settings in the pattern.
	if(level == REG_EXACT || level == REG_MAXAPPROX) {
		params->cost_ins =
		 params->cost_del =
		 params->cost_subst = 1;
		params->max_cost =
		 params->max_ins =
		 params->max_del =
		 params->max_subst =
		 params->max_edit = (level == REG_EXACT) ? 0 : INT_MAX;
		}
	else
		// Neither REG_EXACT or REG_MAXAPPROX value specified (REG_MODERATE assumed).  Initialize parameters to a
		// "moderate" level of fuzziness as the default.
		*params = fuzzy;
	}
#endif

static int matchExact(const tnfa_t *tnfa, const void *string, size_t len, xstr_t type, size_t nmatch, regmatch_t pmatch[],
 int eflags) {
	int status, eo;
	int *tagpos = NULL;

	if(tnfa->num_tags > 0 && nmatch > 0)
		if((tagpos = malloc(sizeof(*tagpos) * tnfa->num_tags)) == NULL)
			return REG_ESPACE;

	// Dispatch to the appropriate matcher.
	if(tnfa->pflags & PropHaveBackrefs) {

		// The pattern has back references; use the back reference matcher.
		if(type == StrUser) {
			const regusource_t *source = string;
			if(source->rewind == NULL || source->compare == NULL)

				// The back reference matcher requires rewind and compare capabilities from the input stream.
				return REG_BADPAT;
			}
		status = runBackref(tnfa, string, (ssize_t) len, type, tagpos, eflags, &eo);
		}
#if EnableApprox
	else if(tnfa->pflags & PropHaveApprox) {

		// The pattern uses approximate matching; use the approximate matcher with "exact matching" as the default.
		// Note that the nmatch and pmatch members of 'match' are ignored by runApprox() and the remaining values in the
		// struct (which are set by that function) are abandoned.  Note also that the exact matching parameters set by
		// xregainit() with REG_EXACT are used as the default for the entire RE and will be overriden by the parameters
		// in the pattern (which usually affect specific subexpressions only).
		regamatch_t match;
		regaparams_t params;
		xregainit(&params, REG_EXACT);
		status = runApprox(tnfa, string, (ssize_t) len, type, tagpos, &match, &params, eflags, &eo);
		}
#endif
	else {
		// Exact matching and no back references; use the parallel matcher.
		status = runParallel(tnfa, string, (ssize_t) len, type, tagpos, eflags, &eo);
		}

	if(status == REG_OK)
		// A match was found -- fill in the submatch array.
		fillMatch(nmatch, pmatch, tnfa->cflags, tnfa, tagpos, eo);
	if(tagpos != NULL)
		free(tagpos);
	return status;
	}

int xregnexec(const regex_t *preg, const char *string, size_t len, size_t nmatch, regmatch_t pmatch[], int eflags) {
	tnfa_t *tnfa = (tnfa_t *) preg->re_cpat;
	return matchExact(tnfa, string, len, (XRE_MB_CUR_MAX == 1) ? StrByte : StrMBS, nmatch, pmatch, eflags);
	}

int xregexec(const regex_t *preg, const char *string, size_t nmatch, regmatch_t pmatch[], int eflags) {

	return xregnexec(preg, string, strlen(string), nmatch, pmatch, eflags);
	}

#if EnableWChar

int xregwnexec(const regex_t *preg, const wchar_t *string, size_t len, size_t nmatch, regmatch_t pmatch[], int eflags) {
	tnfa_t *tnfa = (tnfa_t *) preg->re_cpat;
	return matchExact(tnfa, string, len, StrWide, nmatch, pmatch, eflags);
	}

int xregwexec(const regex_t *preg, const wchar_t *string, size_t nmatch, regmatch_t pmatch[], int eflags) {

	return xregwnexec(preg, string, wcslen(string), nmatch, pmatch, eflags);
	}

#endif // EnableWChar.

int xreguexec(const regex_t *preg, const regusource_t *string, size_t nmatch, regmatch_t pmatch[], int eflags) {
	tnfa_t *tnfa = (tnfa_t *) preg->re_cpat;
	return matchExact(tnfa, string, (size_t) -1, StrUser, nmatch, pmatch, eflags);
	}

#if EnableApprox

// Wrapper functions for approximate regexp matching.

static int matchApprox(const tnfa_t *tnfa, const void *string, size_t len, xstr_t type, regamatch_t *match,
 regaparams_t *params, int eflags) {
	int status, eo;
	int *tagpos = NULL;

	// Check if approximate matching parameters are valid.
	{int *pi, *piz;

	piz = (pi = (int *) params) + sizeof(*params) / sizeof(pi);
	do {
		if(*pi++ < 0)
			return REG_EPARAM;
		} while(pi < piz);
	}

	// If the RE doesn't use approximate matching features and the maximum cost is zero, use the exact matcher instead.
	if(!(tnfa->pflags & PropHaveApprox) && params->max_cost == 0)
		return matchExact(tnfa, string, len, type, match->nmatch, match->pmatch, eflags);

	// Back references are not supported by the approximate matcher.
	if(tnfa->pflags & PropHaveBackrefs)
		return REG_ESUBREG;

	// Create tag array if needed and run the matcher routine.
	if(tnfa->num_tags > 0 && match->nmatch > 0)
		if((tagpos = malloc(sizeof(*tagpos) * tnfa->num_tags)) == NULL)
			return REG_ESPACE;
	if((status = runApprox(tnfa, string, (ssize_t) len, type, tagpos, match, params, eflags, &eo)) == REG_OK)
		fillMatch(match->nmatch, match->pmatch, tnfa->cflags, tnfa, tagpos, eo);
	if(tagpos != NULL)
		free(tagpos);
	return status;
	}

int xreganexec(const regex_t *preg, const char *string, size_t len, regamatch_t *match, regaparams_t *params, int eflags) {
	tnfa_t *tnfa = (tnfa_t *) preg->re_cpat;
	return matchApprox(tnfa, string, len, (XRE_MB_CUR_MAX == 1) ? StrByte : StrMBS, match, params, eflags);
	}

int xregaexec(const regex_t *preg, const char *string, regamatch_t *match, regaparams_t *params, int eflags) {

	return xreganexec(preg, string, strlen(string), match, params, eflags);
	}

#if EnableWChar

int xregawnexec(const regex_t *preg, const wchar_t *string, size_t len, regamatch_t *match, regaparams_t *params, int eflags) {
	tnfa_t *tnfa = (tnfa_t *) preg->re_cpat;
	return matchApprox(tnfa, string, len, StrWide, match, params, eflags);
	}

int xregawexec(const regex_t *preg, const wchar_t *string, regamatch_t *match, regaparams_t *params, int eflags) {

	return xregawnexec(preg, string, wcslen(string), match, params, eflags);
	}

#endif // EnableWChar.

int xregauexec(const regex_t *preg, const regusource_t *string, regamatch_t *match, regaparams_t *params, int eflags) {
	tnfa_t *tnfa = (tnfa_t *) preg->re_cpat;
	return matchApprox(tnfa, string, (size_t) -1, StrUser, match, params, eflags);
	}

#endif // EnableApprox.

#if EnableReverse

// Copy an XRE regular expression from 'pat' to 'revpat' with all atoms reversed and return status code.  'revpat' is assumed
// to point to a buffer that is at least "length of pat + 1".  If REG_ENHANCED is specified in 'cflags', 'pat' is assumed to
// point to an Enhanced Extended RE; otherwise, an Extended RE.  Note that the converted RE can subsequently be compiled with
// the REG_REVERSED flag and used for backward matches as long as it does not contain back references or stand-alone options.
int xregnrev(char *revpat, const char *pat, size_t len, int cflags) {
	int status;
#if EnableWChar
	xchar_t wpat[len + 1];

	// Convert RE pattern to wide characters and create temporary wide buffer in which to place reversed pattern.
	if((status = towide(wpat, pat, &len)) == REG_OK) {
		xchar_t *wpat1 = wpat;
		xchar_t revpat0[len + 1];
		xchar_t *revpat1 = revpat0 + len;
		*revpat1 = L'\0';
		status = grpcpy(&revpat1, (const xchar_t **) &wpat1, cflags | CompTopLevel);
		if(status != REG_OK)
			*revpat = '\0';
		else {
			// Reversal succeeded.  Convert wide result buffer back to multibyte.
			assert(revpat1 == revpat0);
#if EnableMultibyte
			if(XRE_MB_CUR_MAX == 1)
#endif
				{xchar_t *wstr, *wstrz;
				char *str = revpat;
				wstrz = (wstr = revpat1) + len;
				while(wstr < wstrz)
					*str++ = *wstr++;
				*str = '\0';
				}
#if EnableMultibyte
			else if(wcstombs(revpat, revpat1, len + 1) == (size_t) -1) {
				DPrint((stderr, "wcstombs: error %d: %s.\n", errno, strerror(errno)));
				*revpat = '\0';
				status = REG_BADPAT;
				}
#endif // EnableMultibyte
			}
		}
#else // !EnableWChar
	xchar_t *revpat1 = revpat + len;
	*revpat1 = '\0';
	if((status = grpcpy(&revpat1, (const char **) &pat, cflags | CompTopLevel)) != REG_OK)
		*revpat = '\0';
	else
		assert(revpat1 == revpat);
#endif
	return status;
	}

int xregrev(char *revpat, const char *pat, int cflags) {

	return xregnrev(revpat, pat, strlen(pat), cflags);
	}

#if EnableWChar
int xregwnrev(wchar_t *revpat, const wchar_t *pat, size_t len, int cflags) {
	int status;
	wchar_t *revpat1 = revpat + len;
	*revpat1 = L'\0';
	if((status = grpcpy(&revpat1, &pat, cflags | CompTopLevel)) != REG_OK)
		*revpat = L'\0';
	return status;
	}

int xregwrev(wchar_t *revpat, const wchar_t *pat, int cflags) {

	return xregwnrev(revpat, pat, wcslen(pat), cflags);
	}
#endif

#endif // EnableReverse

#ifdef __cplusplus
	}
#endif
