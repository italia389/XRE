Introduction
============
XRE is a lightweight, robust, and mostly POSIX-compliant regular expression matching library.  It has some special
features such as approximate (fuzzy) matching and backward matching.  It also provides an "enhanced" mode, which enables
additional features such as `\x` shortcuts (for example, `\w` for a word character, or `\n` for a newline), `(?:...)`
non-capturing groups, and `\1` ... `\9` back referencing.

XRE is based on the TRE library, vers. 0.7.5., with many changes, fixes, and improvements.  Extended Regular Expressions (EREs)
are the default and Basic Regular Expressions (BREs), which are obsolete, are not supported.

As with TRE, the matching algorithm used in XRE uses linear worst-case time in the length of the text being searched, and
quadratic worst-case time in the length of the regular expression.  In other words, the time complexity of the algorithm is
`O(m^2n)`, where `m` is the length of the regular expression and `n` is the length of the text.  The memory that is used is also
quadratic on the length of the regular expression, but does not depend on the searched string.  This quadratic behaviour occurs
only on pathological cases which are probably very rare in practice.

Installation
============
Installation is very straightforward.  Change to the root of the source directory and do one of the following:

    On Linux:
        $ sudo ./xre-1.1.0.sh

    On macOS:
        $ open xre-1.1.0.pkg

This will install two versions of the XRE (static) library, C headers, and man pages into `/usr/local`.  One of the installed
libraries supports wide characters and multibyte characters (which is the default) and the other supports 8-bit characters only.

Features
========
XRE has many features, some of which are not available in other implementations.  A partial list follows.

Arbitrary String Matching
-------------------------
In addition to matching character strings, pattern matching can be done over arbitrary user-defined data structures by using the
`xreguexec()` function and specifying a callback function to provide the input characters.

Backward Matching
-----------------
XRE provides support for matching an RE while scanning backward through a string or user data structures.
This is accomplished by performing the following steps in your program:

1. Reverse the RE string pattern with `xregrev()`.
2. Compile the reversed pattern with the REG_REVERSED flag.
3. Optionally specify flag(s) at execution time which control how line and word boundaries are matched.
4. Provide a reversed string to `xregexec()`, or provide elements of a collection in reversed order to the callback function
   associated with `xreguexec()`.

The only limitation to this feature is that the RE pattern cannot contain back references or stand-alone options.

Approximate Matching
--------------------
Approximate pattern matching is supported, which allows matches to be approximate; that is, matches may be close to the RE
pattern by some measure.  XRE uses the edit-distance measure (also known as the Levenshtein distance) where characters can be
inserted, deleted, or substituted in the pattern in order to get an exact match.

The maximum number of insertions, deletions, or substitutions can be specified to control the "fuzziness" of matches.  This may
be done directly in the pattern (which affects all or just a portion of the pattern), or via an `xregaexec()` call, or both.
Additionally, the edits can be individually weighted and the "cost of a match" can be used to further refine which matches are
reported.

Conformance to Standards
------------------------
XRE is in strict compliance with the Open Group Base Specifications Issue 7, 2018 edition, IEEE Std 1003.1-2017 (commonly
referred to as POSIX) regarding the `regcomp()` API and RE syntax, except that obsolete BREs are not supported and EREs are the
default.  The standard can be found online [here](https://pubs.opengroup.org/onlinepubs/9699919799).

XRE always returns the correct matches for subpatterns, as specified in the standard, in contrast to many other RE
implementations which do not.

Predictable Matching Speed
--------------------------
Because of the matching algorithm used in XRE, the maximum time consumed by any `xregexec()` call is always directly
proportional to the length of the searched string.  There is one exception: if back references are used, the matching may take
time that grows exponentially with the length of the string.  This is because matching back references is an NP complete
problem, and almost certainly requires exponential time to match in the worst case.

Predictable and Modest Memory Consumption
-----------------------------------------
An `xregexec()` call never allocates memory from the heap.  XRE allocates all the memory it needs during a `xregcomp()` call,
and some temporary working space from the stack frame for the duration of the `xregexec()` call.  The amount of temporary space
needed is constant during matching and does not depend on the searched string.  For REs of reasonable size, XRE needs less than
50K of dynamically allocated memory during the `xregcomp()` call, less than 20K for the compiled pattern buffer, and less than
2K of temporary working space from the stack frame during a `xregexec()` call.  There is no time/memory tradeoff.  XRE is also
small in code size; statically linking with XRE increases the executable size by about 60K.

Wide Character and Multibyte Character Set Support
--------------------------------------------------
XRE supports multibyte character sets.  This makes it possible to use REs seamlessly with, for example, Japanese locales.  XRE
also provides a wide character API.

Thread Safe
-----------
XRE is completely thread safe.  All the public API functions are re-entrant, and a single compiled RE object can be used
simultaneously in multiple contexts; e.g., in main() and a signal handler, or in several threads of a multithreaded application.

Runs On macOS and Linux
-----------------------
XRE can be used on macOS and all Linux platforms.  It contains precompiled libraries for both.  It should also be portable to
other Unix platforms as well with little or no modifications.  Depending on the platform, you may need to install `libutf8` to
get wide character and multibyte character set support.

Free
----
XRE is released under the GNU Lesser General Public License (LGPLv3).
See the file `License.txt` for details.

Distribution
============
The current distribution of the XRE library may be obtained at `https://github.com/italia389/XRE.git`.

Contact and Feedback
====================
User feedback is welcomed and encouraged.  If you have the time and interest, please contact Rick Marinelli
<italian389@yahoo.com> with your questions, comments, bug reports, likes, dislikes, or whatever you feel is worth mentioning.
Questions and feature requests are welcomed as well.  You may also report a bug by opening an issue on the GitHub page.

Notes
=====
This distribution of the XRE library is version 1.1.0.  Installer packages containing 64-bit binaries are included for Linux
platforms and macOS ver. 10.12 and later.  The sources can be compiled instead if desired; however, the build process has not
been tested on other Unix platforms and there may be some (hopefully minor) issues which will need to be resolved.  If you are
compiling the sources and encounter any problems, please contact the author with the details.

Credits
=======
The XRE library (c) Copyright 2022 Richard W. Marinelli is an extensively modified and enhanced derivative of TRE
ver. 0.7.5 (c) Copyright 2001-2006 Ville Laurikari <vl@iki.fi> and was created by Rick Marinelli <italian389@yahoo.com>.
