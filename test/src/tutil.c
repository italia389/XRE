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
