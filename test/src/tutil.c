// (c) Copyright 2025 Richard W. Marinelli
//
// This work is licensed under the GNU General Public License (GPLv3).  To view a copy of this license, see the
// "License.txt" file included with this distribution or visit http://www.gnu.org/licenses/gpl-3.0.en.html.
//
// Utility functions for XRE test program (txre.c).

#include "stdos.h"
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "xre.h"

// Return character c as a string, converting to visible form if non-text, as follows:
//	<NL>	012	Newline.
//	<CR>	015	Carriage return.
//	<ESC>	033	Escape.
//	<SPC>	040	Space (if VSpace flag is set).
//	^X		Non-printable 7-bit character.
//	<NN>		Ordinal value of 8-bit character in hexadecimal.
//	c		Character c as a string, if none of the above.
static char *vizc(short c) {
	static char lit[6];

	// Expose character.
	switch(c) {
		case '\n':
			return "<NL>";
		case '\r':
			return "<CR>";
		case 033:
			return "<ESC>";
		default:
			if(c >= ' ' && c <= '~') {
				lit[0] = c;
				lit[1] = '\0';
				break;
				}
			if(c < 0 || c > 0xFF)
				return "<?>";
			else {
				char *fmt;
				if(c <= 0x7F) {
					fmt = "^%c";
					c ^= 0x40;
					}
				else
					fmt = "<%.2X>";
				sprintf(lit, fmt, c);
				}
		}

	return lit;
	}

// Write 'str' to given file, exposing all 7-bit characters which are invisible.  If 'pass' is false, 8-bit characters are also
// exposed; otherwise, they are passed through unchanged.  If 'len' is zero, 'str' is assumed to be null-terminated; otherwise,
// exactly 'len' bytes are written.  Return EOF if error; otherwise, 0.
int fvizstr(const char *str, size_t len, FILE *file, bool pass) {
	const char *str1;
	short c;
	size_t n = (len == 0) ? strlen(str) : len;

	for(str1 = str; n > 0; --n) {
		c = *str1++;
		if(pass && c & 0x80) {
			if(fputc(c, file) == EOF)
				return EOF;
			}
		else if(fputs(vizc(c), file) == EOF)
			return EOF;
		}
	return 0;
	}

// Reverse a byte string in place and return it.
void *memrev(void *str, size_t len) {

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
char *mbrev(char *str, size_t len) {

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
				memrev(str1, n);
				str1 += n;
				}
			} while(str1 < strEnd);

		// Now reverse the whole string.
		memrev(str, len);
		}

	return NULL;
	}

// Return Boolean value as a literal.
char *trueFalse(bool val) {

	return val ? "true" : "false";
	}

// Print XRE configuration.
void xconf(void) {
	int flags = xlibconf();

	printf("WideChar: %s, Multibyte: %s, ApproxMatch: %s, Reverse: %s, MB_CUR_MAX: %lu\n",
	 trueFalse(flags & ConfigWChar), trueFalse(flags & ConfigMultibyte),
	 trueFalse(flags & ConfigApprox), trueFalse(flags & ConfigReverse), (size_t) MB_CUR_MAX);
	}

// Print program header.
void progHdr(const char *name, const char *version) {

	printf("%s %s -- Testing %s\n", name, version, xrevers());
	xconf();
	}
