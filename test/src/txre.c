// (c) Copyright 2022 Richard W. Marinelli
//
// This work is licensed under the GNU General Public License (GPLv3).  To view a copy of this license, see the
// "License.txt" file included with this distribution or visit http://www.gnu.org/licenses/gpl-3.0.en.html.
//
// Program to demonstrate how the XRE library works.

#include "stdos.h"
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <locale.h>
#include <wchar.h>
#include "tutil.h"

// Types and functions needed for xreguexec().
typedef struct {
	char *str;
	regoff_t pos, len;
	bool multibyte;
	} ScanInfo;

#if 0
// Comparison function that ignores case.  Return zero if a match; otherwise, non-zero.
static int casecmp(const void *s1, const void *s2, regoff_t len) {
	const char *str1 = s1;
	const char *str2 = s2;

	while(len-- > 0)
		if(tolower(*str1++) != tolower(*str2++))
			return 1;
	return 0;
	}
#endif

// Get next input character -- callback routine for xreguexec().  Set *pc to the value of the next character in string, and set
// *plen to number of bytes advanced.  Return true if end of string reached; otherwise, false.
static bool cNextChar(xint_t *pc, unsigned int *plen, void *context) {
	ScanInfo *pscan = (ScanInfo *) context;
	char *strpos;

	if(pscan->pos == pscan->len) {
		*pc = (xint_t) -1;
		return true;
		}
	strpos = pscan->str + pscan->pos;
	if(pscan->multibyte) {
		wchar_t wc;
		int len;

		if((len = mbtowc(&wc, strpos, pscan->len - pscan->pos)) < 0) {

			// Bad multibyte character.  Treat it as 8-bit.
			goto RawChar;
			}
		*pc = wc;
		*plen = len;
		pscan->pos += len;
		}
	else {
RawChar:
		*pc = (xint_t) *strpos;
		*plen = 1;
		++pscan->pos;
		}
	return false;
	}

// Rewind input -- callback routine for xreguexec().  Reset the current position in the input string.
static void cRewind(regoff_t pos, void *context) {
	ScanInfo *pscan = (ScanInfo *) context;
	pscan->pos = pos;
	}

// Compare strings -- callback routine for xreguexec().  Compare two substrings in the input and return 0 if the substrings
// are equal, or a nonzero value if not.
static int cCompare(regoff_t pos1, regoff_t pos2, size_t len, void *context) {
	ScanInfo *pscan = (ScanInfo *) context;
	return memcmp(pscan->str + pos1, pscan->str + pos2, len);
	}

// Reverse a byte string in place and return it.
static void *bsrev(void *str, size_t len) {

	if(len > 1) {
		short c;
		char *str1 = (char *) str;
		char *strEnd = str1 + len;
		do {
			c = *str1;
			*str1++ = *--strEnd;
			*strEnd = c;
			} while(strEnd > str1 + 1);
		}
	return str;
	}

// Reverse a multibyte string in place.  Return NULL if successful, otherwise error message.
static char *mrev(char *str, size_t len) {

	if(len > 1) {
		mbstate_t mbstate;
		size_t n;
		char *strEnd, *str1;

		// First, find and reverse any multibyte sequences within the string.
		strEnd = (str1 = str) + len;
		memset(&mbstate, '\0', sizeof(mbstate));
		do {
			if((*str1 & 0x80) == 0)
				goto Incr;
			if((n = mbrlen(str1, strEnd - str1, &mbstate)) == (size_t) -1 || n == (size_t) -2) {
				(void) asprintf(&strEnd, "Invalid multibyte character in string (at offset %lu)", str1 - str);
				return strEnd;
				}
			else if(n == 0)
Incr:
				++str1;
			else {
				bsrev(str1, n);
				str1 += n;
				}
			} while(str1 < strEnd);

		// Now reverse the whole string.
		bsrev(str, len);
		}

	return NULL;
	}

// Convert any "\n", "\t", or "\0" in string to a newline, tab, or null in place and return new length.  If nullOnly is true,
// convert "\0" only.
static size_t strconv(char *str, bool nullOnly) {
	char *s1, *s2;

	s1 = s2 = str;
	while(*s1 != '\0') {
		if(s1[0] == '\\' && (s1[1] == '0' || (!nullOnly && (s1[1] == 'n' || s1[1] == 't')))) {
			*s2++ = (s1[1] == '0') ? '\0' : (s1[1] == 'n') ? '\n' : '\t';
			s1 += 2;
			}
		else
			*s2++ = *s1++;
		}
	*s2 = '\0';
	return s2 - str;
	}

// Parse command-line arguments and process them.  Write results to standard output.
int main(int argc, char *argv[]) {
	const char *Version = "1.2.0";
	int status, cflags = 0, eflags = 0;
	char *Myself, *execName;
	char *pat, *str1, *str2, *locale = NULL;
	size_t grp, slen, plen;
	regex_t re;
	bool aFuncs = false;
	bool regExact = false;
	bool nFuncs = false;
	bool uFuncs = false;
	char wkbuf[80];

	// Check arguments and set flags.
	Myself = *argv++;
	--argc;
	while(argc > 0) {
		str2 = *argv;
		if(str2[0] == '-') {
			if(str2[1] == '\0')
				break;
			if(str2[2] != '\0')
				goto Usage;
			switch(str2[1]) {
				case 'A':
					cflags |= REG_ANY;
					break;
				case 'a':
					aFuncs = true;
					break;
				case 'B':
					eflags |= REG_BESTMATCH;
					break;
				case 'E':
					cflags |= REG_ENHANCED;
					break;
				case 'F':
					cflags |= REG_APPROX;
					break;
				case 'I':
					cflags |= REG_ICASE;
					break;
				case 'l':
					locale = setlocale(LC_ALL, "");
					break;
				case 'N':
					cflags |= REG_NEWLINE;
					break;
				case 'n':
					nFuncs = true;
					break;
				case 'R':
					cflags |= REG_REVERSE;
					break;
				case 'U':
					cflags |= REG_UNGREEDY;
					break;
				case 'u':
					uFuncs = true;
					break;
				case 'X':
					regExact = true;
					break;
				default:
					goto Usage;
				}
			}
		else
			break;
		++argv;
		--argc;
		}
	if(argc < 2) {
Usage:
		fprintf(stderr,
		 "%s %s (GPLv3) [linked with %s]\n", Myself, Version, xrevers());
		xconf();
		fprintf(stderr,
		 "Usage:\n"
		 "    %s [-A] [-a] [-B] [-E] [-F] [-I] [-l] [-N] [-n] [-U] [-u] [-X] str pat ...\n"
		 "Switches:\n"
		 "    -A  Set REG_ANY compilation flag.\n"
		 "    -a  Use approximate execution functions.\n"
		 "    -B  Set REG_BESTMATCH execution flag.\n"
		 "    -E  Set REG_ENHANCED compilation flag.\n"
		 "    -F  Set REG_APPROX (fuzzy) compilation flag.\n"
		 "    -I  Set REG_ICASE compilation flag.\n"
		 "    -l  Set native locale.\n"
		 "    -N  Set REG_NEWLINE compilation flag.\n"
		 "    -n  Use \"n\" function variants for compilation and execution.\n"
		 "    -R  Set REG_REVERSE compilation flag and scan str argument backward during execution.\n"
		 "    -U  Set REG_UNGREEDY compilation flag.\n"
		 "    -u  Use \"u\" function variants for execution.\n"
		 "    -X  Force REG_EXACT xregainit() flag for all patterns.\n"
		 "Notes:\n"
		 " 1. Any \"\\n\", \"\\t\", or \"\\0\" in str argument is converted to a newline, tab,\n"
		 "    or null character, respectively.  Any \"\\0\" in a pat argument is converted\n"
		 "    to a null character.\n"
		 " 2. If -a switch is specified and -X switch is not specified, xregainit() is\n"
		 "    called with REG_EXACT flag if a pattern contains approximate matching\n"
		 "    features; otherwise, REG_MODERATE.\n"
		 "Example:\n"
		 "    %s -N -E 'abc \\txyxyz!\\n' '\\h+(\\l+)\\b' '^([^\\t]+)\\t?(\\w+)\\2' '^\\s*$'\n",
		Myself, Myself);
		exit(1);
		}

	regaparams_t aparams;
	regamatch_t amatch;
	ScanInfo context;
	regusource_t ustr = {cNextChar, cRewind, cCompare, (void *) &context};
	regmatch_t *match;
	if(aFuncs && regExact)
		xregainit(&aparams, REG_EXACT);

	context.str = str1 = *argv++;
	--argc;
	progHdr(Myself, Version);
	if(locale == NULL)
		context.multibyte = false;
	else {
		context.multibyte = (xlibconf() & ConfigMultibyte) && MB_CUR_MAX > 1;
		printf("Locale: %s\n", locale);
		}
	printf("Multibyte enabled: %s\n", trueFalse(context.multibyte));

	// Convert any "\n", "\t", or "\0" in string to a newline, tab, or null.
	context.len = slen = strconv(str1, false);

	// Print string in visible form.
	fputs("String: '", stdout);
	if(slen > 0)
		fvizstr(str1, slen, stdout, true);
	printf("' (%lu)\n", slen);
#if 0
fputs("Forward string: '", stdout);
fvizstr(str1, slen, stdout, false);
fputs("'\n", stdout);
#endif

	// Reverse string if REG_REVERSE flag specified.
	if(cflags & REG_REVERSE) {
		if(!context.multibyte)
			bsrev(str1, slen);
		else if((str2 = mrev(str1, slen)) != NULL) {
			fprintf(stderr, "Error: %s\n", str2);
			exit(1);
			}
#if 0
fputs("Reversed string: '", stdout);
fvizstr(str1, slen, stdout, false);
fputs("'\n", stdout);
#endif
		}

	// Loop through the patterns.
	do {
		// Compile next pattern.
		plen = strconv(pat = *argv++, true);
		fputs("----- Pattern: /", stdout);
		if(plen > 0)
			fvizstr(pat, plen, stdout, true);
		printf("/\n  Compilation: xreg%scomp(..., %.4X)", nFuncs ? "n" : "", cflags);
		if((status = (nFuncs ? xregncomp(&re, pat, plen, cflags) : xregcomp(&re, pat, cflags))) != 0) {
			xregerror(status, &re, wkbuf, sizeof(wkbuf));
			printf(" -> Error: (%d) %s\n", status, wkbuf);
			}
		else {
			regmatch_t groups[re.re_nsub + 1];
			if(aFuncs) {
				amatch.nmatch = re.re_nsub + 1;
				amatch.pmatch = groups;
				}

			// Match the pattern.
			printf(", re_nsub: %lu, regical: %s, escLitChar: %s, backref: %s, approx: %s\n    Execution: ",
			 re.re_nsub, trueFalse(re.pflags & PropHaveRegical), trueFalse(re.pflags & PropHaveEscLit),
			 trueFalse(re.pflags & PropHaveBackref), trueFalse(re.pflags & PropHaveApprox));

			if(aFuncs) {
				int iflags = regExact || (re.pflags & PropHaveApprox) ? REG_EXACT : REG_MODERATE;
				printf("xregainit(..., %d), ", iflags);
				if(!regExact)
					xregainit(&aparams, iflags);
				if(uFuncs) {
					context.pos = 0;
					execName = "au";
					status = xregauexec(&re, &ustr, &amatch, &aparams, eflags);
					}
				else if(nFuncs) {
					execName = "an";
					status = xreganexec(&re, str1, slen, &amatch, &aparams, eflags);
					}
				else {
					execName = "a";
					status = xregaexec(&re, str1, &amatch, &aparams, eflags);
					}
				}
			else if(uFuncs) {
				context.pos = 0;
				execName = "u";
				status = xreguexec(&re, &ustr, re.re_nsub + 1, groups, eflags);
				}
			else if(nFuncs) {
				execName = "n";
				status = xregnexec(&re, str1, slen, re.re_nsub + 1, groups, eflags);
				}
			else {
				execName = "";
				status = xregexec(&re, str1, re.re_nsub + 1, groups, eflags);
				}

			printf("xreg%sexec(..., %.4X) -> ", execName, eflags);
			if(status != 0) {
				if(status == REG_NOMATCH)
					fputs("NO MATCH\n", stdout);
				else {
					xregerror(status, &re, wkbuf, sizeof(wkbuf));
					printf("Error: (%d) %s\n", status, wkbuf);
					}
				}
			else {
				// Success.
				regoff_t mlen;
				fputs("Success\n", stdout);
				match = groups;
				for(grp = 0; grp <= re.re_nsub; ++grp) {
					printf("\tGroup %lu: '", grp);
					if((mlen = match->rm_eo - match->rm_so) > 0)
						fvizstr(str1 + match->rm_so, mlen, stdout, true);
					if(match->rm_so == -1)
						fputs("' (-1)\n", stdout);
					else
						printf("' (%ld-%ld)\n", (long) match->rm_so, (long) match->rm_eo);
					++match;
					}
				if(aFuncs)
					printf("\tCost %d, num_ins %d, num_del %d, num_subst %d\n",
					 amatch.cost, amatch.num_ins, amatch.num_del, amatch.num_subst);
				}
			xregfree(&re);
			}
		} while(--argc > 0);

	return 0;
	}
