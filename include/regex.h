// regex.h - XRE legacy API.
//
// (c) Copyright 2020 Richard W. Marinelli
//
// This work is based on TRE ver. 0.7.5 (c) Copyright 2001-2006 Ville Laurikari <vl@iki.fi> and is licensed
// under the GNU Lesser General Public License (LGPLv3).  To view a copy of this license, see the "License.txt"
// file included with this distribution or visit http://www.gnu.org/licenses/lgpl-3.0.en.html.

// This header is for source level compatibility with existing code using the <regex.h> header.  New code should
// include "xre.h" instead and use the ereg*() function names.

#ifndef xre_regex_h
#define xre_regex_h

#include "xre.h"

#define regcomp			xregcomp
#define regexec			xregexec
#define regerror		xregerror
#define regfree			xregfree

#endif
