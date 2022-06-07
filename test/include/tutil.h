// (c) Copyright 2020 Richard W. Marinelli
//
// This work is licensed under the GNU General Public License (GPLv3).  To view a copy of this license, see the
// "License.txt" file included with this distribution or visit http://www.gnu.org/licenses/gpl-3.0.en.html.
//
// Function declarations for XRE test programs.

#include <stdlib.h>
#include "xre.h"

extern int fvizstr(const char *str, size_t len, FILE *file, bool pass);
extern void progHdr(const char *name, const char *version);
extern char *trueFalse(bool val);
extern void xconf(void);
