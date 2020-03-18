// match.c - XRE parallel, back-reference, and approximate RE matching engines.
//
// (c) Copyright 2020 Richard W. Marinelli
//
// This work is based on TRE ver. 0.7.5 (c) Copyright 2001-2006 Ville Laurikari <vl@iki.fi> and is licensed
// under the GNU Lesser General Public License (LGPLv3).  To view a copy of this license, see the "License.txt"
// file included with this distribution or visit http://www.gnu.org/licenses/lgpl-3.0.en.html.

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#if HaveHdr_wchar
#include <wchar.h>
#endif
#if HaveHdr_wctype
#include <wctype.h>
#endif
#if !EnableWChar
#include <ctype.h>
#endif

#include "xre.h"
#include "internal.h"
#include "mem.h"

#ifdef __cplusplus
extern "C" {
#endif

// XRE matcher helper definitions.

typedef struct {
	int pos;
	xcint_t prev_c;
	xcint_t next_c;
	} scan_pos_t;

#define str_source		((const regusource_t *) string)

// "Get next character" macros.  After a character is retrieved:
//	spos.prev_c	contains the previous character (or -1 if at beginning).
//	spos.next_c	contains the current character (or -1 if at end).
//	spos.pos	contains offset of current character from beginning of string (zero for first character).
//	next_c_len	contains length of current character in "input stream units" (usually bytes) if type is StrMBS or
//			StrUser.

#if EnableWChar
#if EnableMultibyte

// Wide character and multibyte support.
#define GetNextChar()\
	spos.prev_c = spos.next_c;\
	if(type == StrByte)\
		spos.next_c = (++spos.pos == len) ? (xcint_t) -1 : (xcint_t)(*str_byte++);\
	else if(type == StrWide)\
		spos.next_c = (++spos.pos == len) ? (xcint_t) -1 : *str_wide++;\
	else if(type == StrMBS) {\
		size_t w;\
		size_t max = len - spos.pos;\
		if(max == 0) {\
			spos.next_c = (xcint_t) -1;\
			next_c_len = 0;\
			}\
		else {\
			spos.pos += next_c_len;\
			w = mbrtowc(&spos.next_c, str_byte, max, &mbstate);\
			if(w == (size_t) -1 || w == (size_t) -2) {\
				status = REG_STRCHAR;\
				goto Retn;\
				}\
			if(w == 0) {\
				spos.next_c = L'\0';\
				w = 1;\
				}\
			next_c_len = w;\
			str_byte += w;\
			}\
		}\
	else {\
		spos.pos += next_c_len;\
		str_user_end = str_source->nextchar(&spos.next_c, &next_c_len, str_source->context);\
		}
#else // !EnableMultibyte.

// Wide character support, no multibyte support.
#define GetNextChar()\
	spos.prev_c = spos.next_c;\
	if(type == StrByte)\
		spos.next_c = (++spos.pos == len) ? (xcint_t) -1 : (xcint_t)(*str_byte++);\
	else if(type == StrWide)\
		spos.next_c = (++spos.pos == len) ? (xcint_t) -1 : *str_wide++;\
	else {\
		spos.pos += next_c_len;\
		str_user_end = str_source->nextchar(&spos.next_c, &next_c_len, str_source->context);\
		}
#endif // !EnableMultibyte.
#else // !EnableWChar.

// No wide character or multibyte support.
#define GetNextChar()\
	spos.prev_c = spos.next_c;\
	if(type == StrByte)\
		spos.next_c = (++spos.pos == len) ? (xcint_t) -1 : (xcint_t)(*str_byte++);\
	else {\
		spos.pos += next_c_len;\
		str_user_end = str_source->nextchar(&spos.next_c, &next_c_len, str_source->context);\
		}
#endif // !EnableWChar.

#define IsWordChar(c)  ((c) == L'_' || xisalnum(c))

// Returns true if tag positions in 'tagpos1' take precedence over those in 'tagpos2'; otherwise, false.
static bool betterTags(int num_tags, tag_direction_t *tag_directions, int *tagpos1, int *tagpos2) {
	int i;

	for(i = 0; i < num_tags; ++i) {
		if(tagpos1[i] < tagpos2[i])
			return tag_directions[i] == TagMinimize;
		if(tagpos1[i] > tagpos2[i])
			return tag_directions[i] != TagMinimize;
		}

	return false;
	}

// Find active assertion (including a class or negated class list if ExecClassCheck flag set in eflags) and return true if it
// fails to match at current position; otherwise, false.  Note that more than one positional assertion may be set, so all are
// checked; that is, all must match or 'true' (failure) is returned.
static bool assertfail(tnfa_transition_t *trans, scan_pos_t *spos, const tnfa_t *tnfa, int eflags) {
	int assertions = trans->assertions;

	// Check line assertions.
	DPrint((stderr, "assertfail(%.4X,[%d,%d,%d],%.4X,%.4X) ", assertions, spos->pos, (int) spos->prev_c, (int) spos->next_c,
	 tnfa->cflags, eflags));
	if(assertions & AssertAtBOL) {
		if(tnfa->cflags & REG_REVERSED) {
			if((spos->next_c != (xcint_t) -1 || eflags & REG_NOTBOL) &&
			 (!(tnfa->cflags & REG_NEWLINE) || spos->next_c != L'\n'))
				return true;
			}
		else if((spos->pos > 0 || eflags & REG_NOTBOL) && (!(tnfa->cflags & REG_NEWLINE) || spos->prev_c != L'\n'))
			return true;
		}
	if(assertions & AssertAtEOL) {
		if(tnfa->cflags & REG_REVERSED) {
			if((spos->pos > 0 || eflags & REG_NOTEOL) && (!(tnfa->cflags & REG_NEWLINE) || spos->prev_c != L'\n'))
				return true;
			}
		else if((spos->next_c != (xcint_t) -1 || eflags & REG_NOTEOL) &&
		 (!(tnfa->cflags & REG_NEWLINE) || spos->next_c != L'\n'))
			return true;
		}

	// Check word assertions.
	int wordChBOS = eflags & (tnfa->cflags & REG_REVERSED ? REG_WORDCHEOS : REG_WORDCHBOS);
	int wordChEOS = eflags & (tnfa->cflags & REG_REVERSED ? REG_WORDCHBOS : REG_WORDCHEOS);
	bool prevIsWC = (spos->pos == 0 && tnfa->cflags & REG_ENHANCED && wordChBOS) || IsWordChar(spos->prev_c);
	bool nextIsWC = (spos->next_c == (xcint_t) -1 && tnfa->cflags & REG_ENHANCED && wordChEOS) || IsWordChar(spos->next_c);
	if(tnfa->cflags & REG_REVERSED) {
		bool tempIsWC = prevIsWC;
		prevIsWC = nextIsWC;
		nextIsWC = tempIsWC;
		}
	if(assertions & AssertAtBOW && (prevIsWC || !nextIsWC))
		return true;
	if(assertions & AssertAtEOW && (!prevIsWC || nextIsWC))
		return true;
	if(assertions & AssertAtWB && (prevIsWC == nextIsWC))
		return true;
	if(assertions & AssertAtNegWB && (prevIsWC != nextIsWC))
		return true;

	// Check classes if requested.
	if(eflags & ExecClassCheck) {
		xcint_t wc = spos->prev_c;
		int icase = tnfa->cflags & REG_ICASE;
		if(assertions & AssertCC) {
			if(icase) {
				if(!xisctype(xtolower(wc), trans->u.class) && !xisctype(xtoupper(wc), trans->u.class))
					return true;
				}
			else if(!xisctype(wc, trans->u.class))
				return true;
			}
		else if(assertions & AssertNegCC) {

			// 'newline' does not match any negated character class if REG_NEWLINE set and REG_ANY not set.
			if(wc == L'\n' && (tnfa->cflags & (REG_NEWLINE | REG_ANY)) == REG_NEWLINE)
				return true;

			// Check the negated class list for a match.
			xctype_t *class = trans->neg_classes;
			do {
				if(icase) {
					if(xisctype(xtoupper(wc), *class) || xisctype(xtolower(wc), *class))
						return true;
					}
				else if(xisctype(wc, *class))
					return true;
				} while(*++class != (xctype_t) 0);
			}
		}
	return false;
	}

// Copy a block of integers from src to dest.  "size" may be zero.
static void intcpy(int *dest, int *src, int size) {

	if(size > 0) {
		int *srcz = src + size;
		do {
			*dest++ = *src++;
			} while(src < srcz);
		}
	}

// Compute tag positions after a transition.  "num_tags" is assumed to be greater than zero.
static void settags(int *dest, int *src, int num_tags, tnfa_transition_t *trans, scan_pos_t *spos) {

	if(src == NULL) {
		int *dest1 = dest;
		int *destz = dest + num_tags;
		do {
			*dest1++ = -1;
			} while(dest1 < destz);
		}
	else
		intcpy(dest, src, num_tags);

	if(trans->tags != NULL) {
		for(int *t = trans->tags; *t >= 0; ++t)
			if(*t < num_tags) {
#ifdef XRE_Debug
				if(src == NULL)
					fprintf(stderr, "Setting reach tagpos[t%d] to %d from %p\n",
					 *t, spos->pos, (void *) trans);
#endif
				dest[*t] = spos->pos;
				}
		}
	}

// Parallel matching.  This algorithm (implemented in runParallel() function) searches for matches basically by reading
// characters in the searched string one by one, starting at the beginning.  All matching paths in the TNFA are traversed in
// parallel.  When two or more paths reach the same state, exactly one is chosen according to tag ordering rules; if returning
// submatches is not required, it does not matter which path is chosen.
//
// The worst case time required for finding the leftmost and longest match, or determining that there is no match, is always
// linearly dependent on the length of the text being searched.
//
// This algorithm cannot handle TNFAs with back-referencing nodes.  See next section for that.

typedef struct {
	tnfa_transition_t *state;	// -1 terminated array of current transitions.
	int *tagpos;			// Array of current tag positions.
	} reach_t;

typedef struct {
	int pos;
	int **ptagpos;			// Pointer to array of current tag positions (in appropriate reach object).
	} reach_pos_t;

#ifdef XRE_Debug
static void printReach(reach_t *reach, int num_tags) {
	int tag;
	bool first = true;

	while(reach->state != NULL) {
		if(!first)
			fputs("\t\t", stderr);
		fprintf(stderr, " %p", (void *) reach->state);
		if(num_tags > 0) {
			char *delim = "/";
			tag = 0;
			do {
				fprintf(stderr, "%s%d:%d", delim, tag, reach->tagpos[tag]);
				delim = ", ";
				} while(++tag < num_tags);
			fputs("/\n", stderr);
			}
		++reach;
		first = false;
		}
	}
#endif

int runParallel(const tnfa_t *tnfa, const void *string, ssize_t len, xstr_t type, int *match_tagpos, int eflags,
 int *match_end_off) {

	// State variables required by GetNextChar macro.
	scan_pos_t spos = {-1, (xcint_t) -1, (xcint_t) -1};
	const char *str_byte = string;
	int next_c_len = 1;
#if EnableWChar
	const wchar_t *str_wide = string;
#ifdef UseMBState
	mbstate_t mbstate;
#endif
#endif
	bool str_user_end = false;
	int status = REG_OK;

	// Other local variables.
	char *buf;
	tnfa_transition_t *trans;
	reach_t *reach, *reach_next, *reachi, *reach_nexti;
	reach_pos_t *reach_pos;
	int num_tags, i;

	int match_eo = -1;		 // End offset of match (-1 if no match found yet).
	bool new_match = false;
	int *tmp_tagpos = NULL;
	int *tmp_iptr;

#ifdef UseMBState
	memset(&mbstate, '\0', sizeof(mbstate));
#endif // UseMBState.

	DPrint((stderr, "runParallel, input type %d\n", type));

	num_tags = (match_tagpos == NULL) ? 0 : tnfa->num_tags;

	// Allocate memory for temporary data required for matching.  This needs to be done for every matching operation to be
	// thread safe.  This allocates everything in a single large block using malloc().
	{char *buf1;
	size_t reachBytes = sizeof(*reach) * (tnfa->num_states + 1);	// Space needed for one reach table.
	size_t posBytes = sizeof(*reach_pos) * tnfa->num_states;	// Space needed for one reach position table.
	size_t tagBytes = sizeof(*tmp_tagpos) * num_tags;		// Space needed for one array of tag positions.
	size_t totalBytes = (reachBytes + tagBytes * tnfa->num_states) * 2 + posBytes + tagBytes;	// Total space needed.

	// Allocate the memory, zeroed.
	if((buf = calloc(1, totalBytes)) == NULL)
		return REG_ESPACE;

	// Set the various pointers within buf1 (properly aligned by setting pointers first, then integers).
	reach = (reach_t *) buf;
	buf1 = buf + reachBytes;
	reach_next = (reach_t *) buf1;
	buf1 += reachBytes;
	reach_pos = (reach_pos_t *) buf1;
	buf1 += posBytes;
	tmp_tagpos = (int *) buf1;
	buf1 += tagBytes;
	for(i = 0; i < tnfa->num_states; ++i) {
		reach[i].tagpos = (int *) buf1;
		buf1 += tagBytes;
		reach_next[i].tagpos = (int *) buf1;
		buf1 += tagBytes;
		reach_pos[i].pos = -1;
		}
	assert(buf1 == buf + totalBytes);
	}

	// If only one character can start a match, find it first.
	if(tnfa->first_char >= 0 && type == StrByte) {
		const char *origStr = str_byte;
		if((str_byte = memchr(origStr, tnfa->first_char, (size_t) len)) == NULL)
			goto Retn;	// Not found.

		DPrint((stderr, "skipped %lu chars\n", (unsigned long)(str_byte - origStr)));
		if(str_byte > origStr)
			spos.prev_c = (xcint_t) str_byte[-1];
		spos.next_c = (xcint_t) *str_byte;
		spos.pos = str_byte - origStr;
		++str_byte;
		}

	// Multiple characters can start a match.  Find first one if table available.
	else if(tnfa->firstpos_chars != NULL && type == StrByte) {
		char *chars = tnfa->firstpos_chars;
		spos.pos = 0;
#ifdef XRE_Debug
		int origPos = spos.pos;
#endif
		while(spos.pos < len) {
			spos.next_c = (xcint_t) *str_byte++;
			if(chars[spos.next_c])
				break;
			spos.prev_c = spos.next_c;
			++spos.pos;
			}

		DPrint((stderr, "skipped %d chars\n", spos.pos - origPos));
		if(spos.pos == len)
			goto Retn;	// No match.
		}
	else {
		GetNextChar();
		}

	DPrint((stderr, "length: %ld\n", len));
	DPrint((stderr, "===============+======================================\n"));
	DPrint((stderr, "pos:chr/code   | reach state/tag:pos/...\n"));
	DPrint((stderr, "---------------+--------------------------------------\n"));

	reach_nexti = reach_next;
	for(;;) {
		// If no match found yet, add the initial states to 'reach_next'.
		if(match_eo < 0) {
			DPrint((stderr, " INIT at spos %d\n", spos.pos));
			for(trans = tnfa->initial; trans->state != NULL; ++trans) {
				DPrint((stderr, "    examining trans %p -> %p (rpos %d)",
				 (void *) trans, (void *) trans->state, reach_pos[trans->state_id].pos));
				if(reach_pos[trans->state_id].pos < spos.pos) {
					if(trans->assertions && assertfail(trans, &spos, tnfa, eflags)) {
						DPrint((stderr, " > assertion failed.\n"));
						continue;
						}
					DPrint((stderr, " +\n"));
					reach_nexti->state = trans->state;
					if(num_tags > 0)
						settags(reach_nexti->tagpos, NULL, num_tags, trans, &spos);
					if(reach_nexti->state == tnfa->final) {
						DPrint((stderr, "  found empty match\n"));
						match_eo = spos.pos;
						new_match = true;
						intcpy(match_tagpos, reach_nexti->tagpos, num_tags);
						}
					reach_pos[trans->state_id].pos = spos.pos;
					reach_pos[trans->state_id].ptagpos = &reach_nexti->tagpos;
					++reach_nexti;
					}
#ifdef XRE_Debug
				else
					fputc('\n', stderr);
#endif
				}
			DPrint((stderr, "\n"));
			reach_nexti->state = NULL;
			}
		else if(num_tags == 0 || reach_nexti == reach_next)
			// Match found.
			break;

		// Check for end of string.
		if(type == StrUser) {
			if(str_user_end)
				break;
			}
		else if(spos.pos == len)
			break;

		GetNextChar();
#ifdef XRE_Debug
		fprintf(stderr, "%3d:%2c/%05d   |", spos.pos - 1, (int) spos.prev_c, (int) spos.prev_c);
		printReach(reach_next, num_tags);
		fprintf(stderr, "%3d:%2c/%05d   |\n", spos.pos, (int) spos.next_c, (int) spos.next_c);
#endif
		// Swap 'reach' and 'reach_next'.
		reachi = reach;
		reach = reach_next;
		reach_next = reachi;

		// For each state in 'reach', weed out states that don't fulfill the minimal matching conditions (by adding the
		// ones that do to 'reach_next' temporarily, then swapping 'reach' and 'reach_next').
		if(tnfa->num_minimals > 0 && new_match) {
			int beginTag, endTag;

			DPrint((stderr, "Have minimal tags and match found: weeding out non-minimal states\n"));
			new_match = false;
			reach_nexti = reach_next;
			for(reachi = reach; reachi->state != NULL; ++reachi) {
				for(i = 0; tnfa->minimal_tags[i] >= 0; i += 2) {
					endTag = tnfa->minimal_tags[i];
					beginTag = tnfa->minimal_tags[i + 1];
					DPrint((stderr, "  examining trans %p: minimal beginTag %d, endTag %d\n",
					 (void *) reachi->state, beginTag, endTag));

					// Does end tag exist?
					if(endTag >= num_tags) {
						DPrint((stderr, "  Throwing %p out because end t%d does not exist.\n",
						 reachi->state, endTag));
						goto Onward;
						}

					// Tagged length less than last match?  If so, skip this state -- the current, longer
					// match takes precedence.
					if(reachi->tagpos[beginTag] == match_tagpos[beginTag] &&
					 reachi->tagpos[endTag] < match_tagpos[endTag]) {
						DPrint((stderr, "  Throwing %p out because end t%d pos %d < match_tagpos %d\n",
						 reachi->state, endTag, reachi->tagpos[endTag], match_tagpos[endTag]));
						goto Onward;
						}
					}
				reach_nexti->state = reachi->state;
				tmp_iptr = reach_nexti->tagpos;
				reach_nexti->tagpos = reachi->tagpos;
				reachi->tagpos = tmp_iptr;
				++reach_nexti;
Onward:;
				}
			reach_nexti->state = NULL;

			// Swap 'reach' and 'reach_next'.
			reachi = reach;
			reach = reach_next;
			reach_next = reachi;
			}

		// For each state in 'reach', see if there is a transition leaving with the current input symbol to a state not
		// yet in 'reach_next' and if so, add its destination states.
		reach_nexti = reach_next;
		for(reachi = reach; reachi->state != NULL; ++reachi) {
			for(trans = reachi->state; trans->state != NULL; ++trans) {

				// Does this transition match the input symbol?
				if(trans->code_min <= (xcint_t) spos.prev_c && trans->code_max >= (xcint_t) spos.prev_c) {
					if(trans->assertions && assertfail(trans, &spos, tnfa, eflags | ExecClassCheck)) {
						DPrint((stderr, "assertion failed\n"));
						continue;
						}

					// Compute the tags after this transition.
					if(num_tags > 0)
						settags(tmp_tagpos, reachi->tagpos, num_tags, trans, &spos);
					if(reach_pos[trans->state_id].pos < spos.pos) {

						// Found an unvisited node.
						reach_nexti->state = trans->state;
						tmp_iptr = reach_nexti->tagpos;
						reach_nexti->tagpos = tmp_tagpos;
						tmp_tagpos = tmp_iptr;
						reach_pos[trans->state_id].pos = spos.pos;
						reach_pos[trans->state_id].ptagpos = &reach_nexti->tagpos;

						if(reach_nexti->state == tnfa->final && (match_eo == -1 || (num_tags > 0 &&
						 reach_nexti->tagpos[0] <= match_tagpos[0]))) {
							DPrint((stderr, "  found match %p\n", trans->state));
							match_eo = spos.pos;
							new_match = true;
							intcpy(match_tagpos, reach_nexti->tagpos, num_tags);
							}
						++reach_nexti;
						}
					else {
						assert(reach_pos[trans->state_id].pos == spos.pos);

						// Another path has also reached this state.  We choose the winner by examining
						// the tag positions for both paths.
						if(betterTags(num_tags, tnfa->tag_directions, tmp_tagpos,
						 *reach_pos[trans->state_id].ptagpos)) {

							// The new path wins.
							tmp_iptr = *reach_pos[trans->state_id].ptagpos;
							*reach_pos[trans->state_id].ptagpos = tmp_tagpos;
							if(trans->state == tnfa->final) {
								DPrint((stderr, "  found better match\n"));
								match_eo = spos.pos;
								new_match = true;
								intcpy(match_tagpos, tmp_tagpos, num_tags);
								}
							tmp_tagpos = tmp_iptr;
							}
						}
					}
				}
			}
		reach_nexti->state = NULL;
		}

	DPrint((stderr, "match end offset = %d\n", match_eo));

	*match_end_off = match_eo;
Retn:
	free(buf);
	return status != REG_OK ? status : match_eo >= 0 ? REG_OK : REG_NOMATCH;
	}

// Backreference matching.  This algorithm (implemented in runBackref() function) is for regexps that use back
// referencing.  Regexp matching with back referencing is an NP-complete problem on the number of back references.  The easiest
// way to match them is to use a backtracking routine which basically goes through all possible paths in the TNFA and chooses
// the one which results in the best (leftmost and longest) match.  This can be spectacularly expensive and may run out of stack
// space, but there really is no better known generic algorithm.  Quoting Henry Spencer from comp.compilers
// <URL: http://compilers.iecc.com/comparch/article/93-03-102>:
//
//	"POSIX.2 REs require longest match, which is really exciting to implement since the obsolete ("basic") variant
//	also includes \<digit>.  I haven't found a better way of tackling this than doing a preliminary match using a
//	DFA (or simulation) on a modified RE that just replicates subREs for \<digit>, and then doing a backtracking
//	match to determine whether the subRE matches were right.  This can be rather slow, but I console myself with the
//	thought that people who use \<digit> deserve very slow execution.  (Pun unintentional but very appropriate.)"

typedef struct {
	int pos;
	const char *str_byte;
#if EnableWChar
	const wchar_t *str_wide;
#endif
	tnfa_transition_t *state;
	int state_id;
	xcint_t next_c;
	int *tagpos;			// Tag positions.
#ifdef UseMBState
	mbstate_t mbstate;
#endif
	} backtrack_item_t;

typedef struct backtrack {
	backtrack_item_t item;
	struct backtrack *prev;
	struct backtrack *next;
	} backtrack_t;

#if EnableWChar
#define BTStackWideIn(_str_wide) stack->item.str_wide = (_str_wide)
#define BTStackWideOut	(str_wide) = stack->item.str_wide
#else
#define BTStackWideIn(_str_wide)
#define BTStackWideOut
#endif

#ifdef UseMBState
#define BTStackMBStateIn	stack->item.mbstate = (mbstate)
#define BTStackMBStateOut	(mbstate) = stack->item.mbstate
#else
#define BTStackMBStateIn
#define BTStackMBStateOut
#endif

#define BTStackPush(_pos, _str_byte, _str_wide, _state, _state_id, _next_c, _tagpos, _mbstate)\
	if(stack->next == NULL) {\
		backtrack_t *s;\
		if((s = mem_alloc(mem, sizeof(*s))) == NULL) {\
			mem_free(mem);\
			if(tagpos != NULL)\
				free(tagpos);\
			if(pmatch != NULL)\
				free(pmatch);\
			if(states_seen != NULL)\
				free(states_seen);\
			return REG_ESPACE;\
			}\
		s->prev = stack;\
		s->next = NULL;\
		if((s->item.tagpos = mem_alloc(mem, sizeof(*tagpos) * tnfa->num_tags)) == NULL) {\
			mem_free(mem);\
			if(tagpos != NULL)\
				free(tagpos);\
			if(pmatch != NULL)\
				free(pmatch);\
			if(states_seen != NULL)\
				free(states_seen);\
			return REG_ESPACE;\
			}\
		stack->next = s;\
		stack = s;\
		}\
	else\
		stack = stack->next;\
	stack->item.pos = (_pos);\
	stack->item.str_byte = (_str_byte);\
	BTStackWideIn(_str_wide);\
	stack->item.state = (_state);\
	stack->item.state_id = (_state_id);\
	stack->item.next_c = (_next_c);\
	intcpy(stack->item.tagpos, _tagpos, tnfa->num_tags);\
	BTStackMBStateIn;

#define BTStackPop()\
	assert(stack->prev != NULL);\
	spos.pos = stack->item.pos;\
	if(type == StrUser)\
		str_source->rewind(spos.pos + next_c_len, str_source->context);\
	str_byte = stack->item.str_byte;\
	BTStackWideOut;\
	state = stack->item.state;\
	spos.next_c = stack->item.next_c;\
	intcpy(tagpos, stack->item.tagpos, tnfa->num_tags);\
	BTStackMBStateOut;\
	stack = stack->prev;

#undef Min
#define Min(a, b) ((a) <= (b) ? (a) : (b))

#if 0
// Comparison function that ignores case.  Return zero if a match; otherwise, non-zero.
static int casecmp(const void *s1, const void *s2, int n, xstr_t type) {
	const char *str1 = s1;
	const char *str2 = s2;
#if EnableWChar
	const wchar_t *wstr1 = s1;
	const wchar_t *wstr2 = s2;

	if(type == StrWide) {
		while(n-- > 0)
			if(xtolower(*wstr1++) != xtolower(*wstr1++))
				return 1;
		}
	else
#endif
		while(n-- > 0)
			if(tolower(*str1++) != tolower(*str2++))
				return 1;
	return 0;
	}
#endif

int runBackref(const tnfa_t *tnfa, const void *string, ssize_t len, xstr_t type, int *match_tagpos, int eflags,
 int *match_end_off) {

	// State variables required by GetNextChar macro.
	scan_pos_t spos = {0, (xcint_t) -1, (xcint_t) -1};
	const char *str_byte = string;
	int next_c_len = 1;
#if EnableWChar
	const wchar_t *str_wide = string;
#ifdef UseMBState
	mbstate_t mbstate;
#endif
#endif
	bool str_user_end = false;
	int status = REG_OK;

	// These are used to remember the necessary values of the above variables to return to the position where the current
	// search started from.
	xcint_t next_c_start;
	const char *str_byte_start;
	int pos_start = -1;
#if EnableWChar
	const wchar_t *str_wide_start;
#endif
#ifdef UseMBState
	mbstate_t mbstate_start;
#endif
	int match_eo = -1;		// End offset of best match so far, or -1 if no match found yet.
	int *next_tags, *tagpos = NULL;	// Tag arrays.
	tnfa_transition_t *state;	// Current TNFA state.
	int *states_seen = NULL;
	memhdr_t *mem;			// Memory allocator for allocating the backtracking stack.
	backtrack_t *stack;		// The backtracking stack.
	tnfa_transition_t *trans;
	regmatch_t *pmatch = NULL;

#ifdef UseMBState
	memset(&mbstate, '\0', sizeof(mbstate));
#endif
	if((mem = mem_new()) == NULL)
		return REG_ESPACE;
	if((stack = mem_alloc(mem, sizeof(*stack))) == NULL)
		goto ErrExit;
	stack->prev = NULL;
	stack->next = NULL;

	DPrint((stderr, "runBackref, input type %d\n", type));
	DPrint((stderr, "len = %ld\n", len));

	if(tnfa->num_tags > 0) {
		if((tagpos = malloc(sizeof(*tagpos) * tnfa->num_tags)) == NULL)
			goto ErrExit;
		}
	if(tnfa->num_submatches > 0) {
		if((pmatch = malloc(sizeof(*pmatch) * tnfa->num_submatches)) == NULL)
			goto ErrExit;
		}
	if(tnfa->num_states > 0) {
		if((states_seen = malloc(sizeof(*states_seen) * tnfa->num_states)) == NULL)
			goto ErrExit;
		}
Retry:
	for(int i = 0; i < tnfa->num_tags; ++i) {
		tagpos[i] = -1;
		if(match_tagpos != NULL)
			match_tagpos[i] = -1;
		}
	if(tnfa->num_states > 0)
		memset(states_seen, 0, sizeof(*states_seen) * tnfa->num_states);

	state = NULL;
	spos.pos = pos_start;
	if(type == StrUser)
		str_source->rewind(spos.pos + next_c_len, str_source->context);
	GetNextChar();
	pos_start = spos.pos;
	next_c_start = spos.next_c;
	str_byte_start = str_byte;
#if EnableWChar
	str_wide_start = str_wide;
#endif
#ifdef UseMBState
	mbstate_start = mbstate;
#endif

	// Handle initial states.
	next_tags = NULL;
	for(trans = tnfa->initial; trans->state != NULL; ++trans) {
		DPrint((stderr, "> init %p, prev_c %c\n", trans->state, (int) spos.prev_c));
		if(trans->assertions && assertfail(trans, &spos, tnfa, eflags)) {
			DPrint((stderr, "assertion failed\n"));
			continue;
			}
		if(state == NULL) {

			// Start from this state.
			state = trans->state;
			next_tags = trans->tags;
			}
		else {
			// Backtrack to this state.
			DPrint((stderr, "saving state %d for backtracking\n", trans->state_id));
			BTStackPush(spos.pos, str_byte, str_wide, trans->state, trans->state_id, spos.next_c, tagpos,
			 mbstate);
			{int *tmp = trans->tags;
			if(tmp != NULL)
				while(*tmp >= 0)
					stack->item.tagpos[*tmp++] = spos.pos;
			}
			}
		}

	if(next_tags != NULL)
		for(; *next_tags >= 0; ++next_tags)
			tagpos[*next_tags] = spos.pos;

	DPrint((stderr, "entering match loop, pos %d, str_byte %p\n", spos.pos, str_byte));
	DPrint((stderr, "pos:chr/code | state and tags\n"));
	DPrint((stderr, "-------------+------------------------------------------------\n"));

	if(state == NULL)
		goto Backtrack;

	for(;;) {
		tnfa_transition_t *next_state;
		int empty_br_match;

		DPrint((stderr, "start loop\n"));
		if(state == tnfa->final) {
			DPrint((stderr, "  match found, %d %d\n", match_eo, spos.pos));
			if(match_eo < spos.pos || (match_eo == spos.pos && match_tagpos != NULL &&
			 betterTags(tnfa->num_tags, tnfa->tag_directions, tagpos, match_tagpos))) {

				// This match beats the previous match.
				DPrint((stderr, "  win previous\n"));
				match_eo = spos.pos;
				if(match_tagpos != NULL)
					intcpy(match_tagpos, tagpos, tnfa->num_tags);
				}

			// TNFAs never have transitions leaving from the final state, so we jump right to backtracking.
			goto Backtrack;
			}
#ifdef XRE_Debug
		fprintf(stderr, "%3d:%2c/%05d | %p ", spos.pos, (int) spos.next_c, (int) spos.next_c, state);
		for(int i = 0; i < tnfa->num_tags; ++i)
			fprintf(stderr, "%d%s", tagpos[i], i < tnfa->num_tags - 1 ? ", " : "");
		fputc('\n', stderr);
#endif

		// Go to the next character in the input string.
		empty_br_match = 0;
		trans = state;
		if(trans->state && trans->assertions & AssertBackref) {

			// This is a back reference state.  All transitions leaving from this state have the same back reference
			// "assertion".  Instead of reading the next character, we match the back reference.
			int so, eo, bt_len, result;
			int bt = trans->u.backref;

			DPrint((stderr, " should match back reference %d\n", bt));

			// Get the substring we need to match against.  Remember to turn off REG_NOSUB temporarily.
			fillMatch(bt + 1, pmatch, tnfa->cflags & ~REG_NOSUB, tnfa, tagpos, spos.pos);
			so = pmatch[bt].rm_so;
			eo = pmatch[bt].rm_eo;
			bt_len = eo - so;
#ifdef XRE_Debug
			{int slen = (len < 0) ? bt_len : Min(bt_len, len - spos.pos);
			if(type == StrByte) {
				fprintf(stderr, "  substring (len %d) is [%d, %d]: '%.*s'\n", bt_len, so, eo, bt_len,
				 (char *) string + so);
				fprintf(stderr, "  current string is '%.*s'\n", slen, str_byte - 1);
				}
#if EnableWChar
			else if(type == StrWide) {
				DPrint((stderr, "  substring (len %d) is [%d, %d]: '%.*" StrF "'\n", bt_len, so, eo, bt_len,
				 (wchar_t *) string + so));
				DPrint((stderr, "  current string is '%.*" StrF "'\n", slen, str_wide - 1));
				}
#endif
			}
#endif
			if(len < 0)
				result = str_source->compare((size_t) so, (size_t) spos.pos, (size_t) bt_len,
				 str_source->context);
			else if(len - spos.pos < bt_len)
				result = 1;
#if EnableWChar
			else if(type == StrWide)
				result = wmemcmp((const wchar_t *) string + so, str_wide - 1, (size_t) bt_len);
#endif
			else
				result = memcmp((const char *) string + so, str_byte - 1, (size_t) bt_len);

			if(result == 0) {

				// Back reference matched.  Check for infinite loop.
				if(bt_len == 0)
					empty_br_match = 1;
				if(empty_br_match && states_seen[trans->state_id]) {
					DPrint((stderr, "  avoid loop\n"));
					goto Backtrack;
					}

				states_seen[trans->state_id] = empty_br_match;

				// Advance in input string and resync 'prev_c', 'next_c' and 'pos'.
				DPrint((stderr, "  back reference matched\n"));
				if(bt_len == 0) {
					--str_byte;
#if EnableWChar
					--str_wide;
#endif
					--spos.pos;
					bt_len = 1;
					}
				do {
					GetNextChar();
					} while(--bt_len > 0);
				DPrint((stderr, "  pos now %d\n", spos.pos));
				}
			else {
				DPrint((stderr, "  back reference did not match\n"));
				goto Backtrack;
				}
			}
		else {
			// Check for end of string.
			if(type == StrUser) {
				if(str_user_end)
					goto Backtrack;
				}
			else if(spos.pos == len)
				goto Backtrack;

			// Read the next character.
			GetNextChar();
			}

		next_state = NULL;
		for(trans = state; trans->state != NULL; ++trans) {
			DPrint((stderr, "  transition %d-%d (%c-%c) %d to %d\n",
			 trans->code_min, trans->code_max,
			 trans->code_min, trans->code_max,
			 trans->assertions, trans->state_id));
			if(trans->code_min <= (xcint_t) spos.prev_c && trans->code_max >= (xcint_t) spos.prev_c) {
				if(trans->assertions && assertfail(trans, &spos, tnfa, eflags | ExecClassCheck)) {
					DPrint((stderr, "assertion failed\n"));
					continue;
					}
				if(next_state == NULL) {

					// First matching transition.
					DPrint((stderr, "  Next state is %d\n", trans->state_id));
					next_state = trans->state;
					next_tags = trans->tags;
					}
				else {
					// Second matching transition.  We may need to backtrack here to take this transition
					// instead of the first one, so we push this transition in the backtracking stack so we
					// can jump back here if needed.
					DPrint((stderr, "  saving state %d for backtracking\n", trans->state_id));
					BTStackPush(spos.pos, str_byte, str_wide, trans->state, trans->state_id,
					 spos.next_c, tagpos, mbstate);
					for(int *tmp = trans->tags; tmp != NULL && *tmp >= 0; ++tmp)
						stack->item.tagpos[*tmp] = spos.pos;
#if 0
					// XXX - it's important not to look at all transitions here to keep the stack small!
					break;
#endif
					}
				}
			}

		if(next_state != NULL) {

			// Matching transitions were found.  Take the first one.
			state = next_state;

			// Update the tag positions.
			if(next_tags != NULL)
				while(*next_tags >= 0)
					tagpos[*next_tags++] = spos.pos;
			}
		else {
Backtrack:
			// A matching transition was not found.  Try to backtrack.
			if(stack->prev) {
				DPrint((stderr, "  backtracking\n"));
				if(stack->item.state->assertions & AssertBackref) {
					DPrint((stderr, "  states_seen[%d] = 0\n", stack->item.state_id));
					states_seen[stack->item.state_id] = 0;
					}

				BTStackPop();
				}
			else if(match_eo < 0) {

				// Backtrack failed.  Try starting from a later position in the input string.

				// Check for end of string.
				if(type == StrUser) {
					if(str_user_end)
						goto EOS;
					}
				else if(spos.pos == len) {
EOS:
					DPrint((stderr, "end of string.\n"));
					break;
					}

				DPrint((stderr, "restarting from next start position\n"));
				spos.next_c = next_c_start;
#ifdef UseMBState
				mbstate = mbstate_start;
#endif
				str_byte = str_byte_start;
#if EnableWChar
				str_wide = str_wide_start;
#endif
				goto Retry;
				}
			else {
				DPrint((stderr, "finished\n"));
				break;
				}
			}
		}

	status = match_eo >= 0 ? REG_OK : REG_NOMATCH;
	*match_end_off = match_eo;
	goto Retn;
ErrExit:
	status = REG_ESPACE;
Retn:
	mem_free(mem);
	if(tagpos != NULL)
		free(tagpos);
	if(pmatch != NULL)
		free(pmatch);
	if(states_seen != NULL)
		free(states_seen);
	return status;
	}

#if EnableApprox

// Approximate matching.

#define __USE_STRING_INLINES
#undef __NO_INLINE__

// Indices into cost arrays.
#define IdxCost			0
#define IdxNumIns		1
#define IdxNumDel		2
#define IdxNumSubst		3
#define IdxNumEdit		4
#define CostArraySize		5

#define MaxDepth		3

typedef struct {
	tnfa_transition_t *state;	// State in the TNFA transition table.
	int pos;			// Position in input string.
	int *tagpos;			// Current tag positions.
	params_t params;		// Matching parameters and nesting depth of parameters, the latter of which is used as
					// an index into the 'costs' array.
	int costs[MaxDepth + 1][CostArraySize];	// Cost and limit values for different parameter nesting depths.
	} approx_reach_t;

#ifdef XRE_Debug

// Prints the 'reach' array in a readable fashion.
static void print_approx_reach(const tnfa_t *tnfa, approx_reach_t *reach, int pos, int num_tags) {
	int id;

	// Print each state on one line.
	fputs("  reach:\n", stderr);
	for(id = 0; id < tnfa->num_states; ++id) {
		int i, *pi;
		char *delim1;
		approx_reach_t *reachi = reach + id;

		if(reachi->pos < pos)
			continue;  // Not reached.
		fprintf(stderr, "    %03d, costs ", id);
		delim1 = "[";
		for(i = 0; i <= reachi->params.depth; ++i) {
			int j;
			char *delim2 = "";
			pi = reachi->costs[i];
			fputs(delim1, stderr);
			for(j = 0; j < CostArraySize; ++j) {
				fprintf(stderr, "%s%2d", delim2, *pi++);
				delim2 = ", ";
				}
			fputc(']', stderr);
			delim1 = ", [";
			}
		fputs("\n    tags ", stderr);
		delim1 = "";
		pi = reachi->tagpos;
		for(i = 0; i < num_tags; ++i) {
			fprintf(stderr, "%s%02d", delim1, *pi++);
			delim1 = ", ";
			}
		fputc('\n', stderr);
		}
	fputc('\n', stderr);
	}
#endif

#define SetParamsDebug		0

#if defined(XRE_Debug) && SetParamsDebug
#include <stdio.h>
#endif

// Sets the matching parameters in 'reach', depending on those in 'transParams' (which are from a transition) and those in
// 'defaultParams' (which are from the regaexec() call).  For each parameter in 'transParams', if it is set, that value is used;
// otherwise, if it specifies a default value, the value from 'defaultParams' is used; otherwise, the parameter in 'reach' is
// left as is.
static void setParams(approx_reach_t *reach, params_t *transParams, regaparams_t *defaultParams) {
	int value;
	int n = sizeof(*defaultParams) / sizeof(defaultParams->cost_ins);
	int *reachParam = (int *) &reach->params.pa;
	int *transParam = (int *) &transParams->pa;
	int *defaultParam = (int *) defaultParams;

#if defined(XRE_Debug) && SetParamsDebug
	// Print approximate matching parameters.
	fputs("\nsetParams(): BEGIN\n\ttrans: ", stderr);
	printParams(false, &transParams->pa);
	fputs("\n\tdeflt: ", stderr);
	printParams(false, defaultParams);
	fputs("\n\treach: ", stderr);
	printParams(false, &reach->params.pa);
	fputc('\n', stderr);
#endif
	// If depth is increased, reset costs and counters to zero for the new levels.
	value = transParams->depth;
	assert(value <= MaxDepth);
	if(value > reach->params.depth)
		for(int i = reach->params.depth + 1; i <= value; ++i)
			memset(reach->costs[i], 0, sizeof(reach->costs[0][0]) * CostArraySize);
	reach->params.depth = value;

	// Set parameters in reach object.
	do {
		value = *transParam++;
		if(value == ParamDefault)
			*reachParam = *defaultParam;
		else if(value != ParamUnset)
			*reachParam = value;
		++reachParam;
		++defaultParam;
		} while(--n > 0);

#if defined(XRE_Debug) && SetParamsDebug
	fputs("setParams(): END\n\treach: ", stderr);
	printParams(false, &reach->params.pa);
	fputc('\n', stderr);
#endif
	}

// Find first match (or best match if REG_BESTMATCH flag set in eflags) that is within edit and cost constraints, and has the
// lowest cost.
int runApprox(const tnfa_t *tnfa, const void *string, ssize_t len, xstr_t type, int *match_tagpos, regamatch_t *match,
 regaparams_t *params, int eflags, int *match_end_off) {

	// State variables required by GetNextChar macro.
	scan_pos_t spos = {-1, (xcint_t) -1, (xcint_t) -1};
	const char *str_byte = string;
	int next_c_len = 1;
#if EnableWChar
	const wchar_t *str_wide = string;
#ifdef UseMBState
	mbstate_t mbstate;
#endif
#endif
	bool str_user_end = false;
	int status = REG_OK;

	// Other local variables.
	int prev_pos;
	int num_tags;					// Number of tags.
	approx_reach_t *reach, *reach_next;		// The reach tables.
	approx_reach_t *reachi, *reach_nexti;
	tnfa_transition_t *trans;
	int *tmp_tagpos;				// Tag array for temporary use.
	int match_eo = -1;				// End offset of best match so far, or -1 if no match found yet.
	int match_costs[CostArraySize];			// Costs of the match.
	char *buf;					// Space for temporary data required for matching.
	int i, id;

	num_tags = (match_tagpos == NULL) ? 0 : tnfa->num_tags;
#ifdef UseMBState
	memset(&mbstate, '\0', sizeof(mbstate));
#endif
	DPrint((stderr, "runApprox, input type %d, len %ld, eflags %d, match_tagpos %p\n", type, len, eflags, match_tagpos));
	DPrint((stderr, "max cost %d, ins %d, del %d, subst %d\n",
	 params->max_cost, params->cost_ins, params->cost_del, params->cost_subst));

	// Allocate memory for temporary data required for matching.  This needs to be done for every matching operation to be
	// thread safe.  This allocates everything in a single large block using malloc().
	{char *buf1;
	size_t reachBytes = sizeof(*reach) * tnfa->num_states;		// Space needed for one reach table.
	size_t tagBytes = sizeof(*tmp_tagpos) * num_tags;		// Space needed for one array of tag positions.
	size_t totalBytes = reachBytes * 2 + (tnfa->num_states * 2 + 1) * tagBytes;	// Total space needed.

	// Allocate the memory, zeroed.
	if((buf = calloc(1, totalBytes)) == NULL)
		return REG_ESPACE;

	// Set the various pointers within buf1 (properly aligned by setting pointers first, then integers).
	reach = (approx_reach_t *) buf;
	buf1 = buf + reachBytes;
	reach_next = (approx_reach_t *) buf1;
	buf1 += reachBytes;
	tmp_tagpos = (int *) buf1;
	buf1 += tagBytes;

	for(i = 0; i < tnfa->num_states; ++i) {
		reach[i].tagpos = (int *) buf1;
		buf1 += tagBytes;
		reach_next[i].tagpos = (int *) buf1;
		buf1 += tagBytes;
		}
	assert(buf1 == buf + totalBytes);
	}

	for(i = 0; i < CostArraySize; ++i)
		match_costs[i] = INT_MAX;

	// Mark the reach arrays empty.
	for(i = 0; i < tnfa->num_states; ++i)
		reach[i].pos = reach_next[i].pos = -2;

	prev_pos = spos.pos;
	GetNextChar();

	// Match the string.
	for(;;) {
		DPrint((stderr, "%03d:%2c/%05o\n", spos.pos, (int) spos.next_c, (int) spos.next_c));

		// Add initial states to 'reach_next' if an exact match has not yet been found.
		if(match_costs[IdxCost] > 0) {
			DPrint((stderr, "  init"));
			for(trans = tnfa->initial; trans->state != NULL; ++trans) {
				reach_nexti = reach_next + trans->state_id;

				// If this state is not currently in 'reach_next', add it there.
				if(reach_nexti->pos < spos.pos) {
					if(trans->assertions && assertfail(trans, &spos, tnfa, eflags)) {

						// Assertions failed, don't add this state.
						DPrint((stderr, " !%d (assertion)", trans->state_id));
						continue;
						}
					DPrint((stderr, " %d%s", trans->state_id,
					 (trans->state == tnfa->final) ? "/final" : ""));
					reach_nexti->state = trans->state;
					reach_nexti->pos = spos.pos;

					// Compute tag positions after this transition.
					if(num_tags > 0)
						settags(reach_nexti->tagpos, NULL, num_tags, trans, &spos);

					// Set the parameters, depth, and costs.
					reach_nexti->params.pa = *params;
					reach_nexti->params.depth = 0;
					memset(reach_nexti->costs[0], 0, sizeof(reach_nexti->costs[0][0]) * CostArraySize);
					if(trans->params != NULL)
						setParams(reach_nexti, trans->params, params);

					// If this is the final state, save the exact match parameters and return.  We're done.
					if(trans->state == tnfa->final) {
						match_eo = spos.pos;
						intcpy(match_tagpos, reach_nexti->tagpos, num_tags);
						memset(match_costs, 0, sizeof(match_costs[0]) * CostArraySize);
						goto Retn;
						}
					}
				}
			DPrint((stderr, "\n"));
			}

		// Handle inserts.  This is done by pretending there's an epsilon transition from each state in 'reach' back to
		// the same state.  We don't need to worry about the final state here; this will never give a better match than
		// what we already have.
		for(id = 0; id < tnfa->num_states; ++id) {
			int depth;
			int cost, cost0;

			reachi = reach + id;
			reach_nexti = reach_next + id;
			if(reachi->pos != prev_pos) {
				DPrint((stderr, "  insert: %d not reached\n", id));
				continue;  // Not reached.
				}
			depth = reachi->params.depth;

			// Compute and check costs and edit limits at current depth.
			assert(reachi->params.pa.cost_ins != ParamUnset);
			if(reachi->costs[depth][IdxNumIns] >= reachi->params.pa.max_ins ||
			 reachi->costs[depth][IdxNumEdit] >= reachi->params.pa.max_edit ||
			 (cost = reachi->costs[depth][IdxCost] + reachi->params.pa.cost_ins) > reachi->params.pa.max_cost)
				continue;  // Too many edits or cost too high.

			// Compute overall cost.
			cost0 = (depth == 0) ? cost : reachi->costs[0][IdxCost] + reachi->params.pa.cost_ins;
			DPrint((stderr, "  insert: from %d to %d, cost %d: ", id, id, reachi->costs[depth][IdxCost]));
			if(reach_nexti->pos == spos.pos && cost0 >= reach_nexti->costs[0][IdxCost]) {
				DPrint((stderr, "lose\n"));
				continue;
				}
			DPrint((stderr, "win\n"));

			// Copy state, position, tags, parameters, and depth.
			reach_nexti->state = reachi->state;
			reach_nexti->pos = spos.pos;
			intcpy(reach_nexti->tagpos, reachi->tagpos, num_tags);
			reach_nexti->params = reachi->params;

			// Set the costs after this transition.
			intcpy((int *) reach_nexti->costs, (int *) reachi->costs, CostArraySize * (depth + 1));
			reach_nexti->costs[depth][IdxCost] = cost;
			++reach_nexti->costs[depth][IdxNumIns];
			++reach_nexti->costs[depth][IdxNumEdit];
			if(depth > 0) {
				reach_nexti->costs[0][IdxCost] = cost0;
				++reach_nexti->costs[0][IdxNumIns];
				++reach_nexti->costs[0][IdxNumEdit];
				}
			}

		// Handle deletes.  This is done by traversing through the whole TNFA pretending that all transitions are
		// epsilon transitions, until no more states can be reached with better costs.

		// XXX - dynamic ringbuffer size.
		approx_reach_t *ringbuffer[512];
		approx_reach_t **deque_start, **deque_end;

		deque_start = deque_end = ringbuffer;

		// Add all states in 'reach_next' that match current position to the deque.
		{approx_reach_t *reachz;
		reachz = (reach_nexti = reach_next) + tnfa->num_states;
		do {
			if(reach_nexti->pos == spos.pos) {
				*deque_end++ = reach_nexti;
				assert(deque_end != deque_start + elementsof(ringbuffer));
				}
			} while(++reach_nexti < reachz);
		}

		// Repeat until the deque is empty.
		while(deque_start != deque_end) {
			int depth;
			int cost, cost0;
			tnfa_transition_t *trans;

			// Pop the next item off the deque.
			reachi = *deque_start;
			id = reachi - reach_next;
			depth = reachi->params.depth;

			// Compute and check costs and edit limits at current depth.
			assert(reachi->params.pa.cost_del != ParamUnset);
			if(reachi->costs[depth][IdxNumDel] >= reachi->params.pa.max_del ||
			 reachi->costs[depth][IdxNumEdit] >= reachi->params.pa.max_edit ||
			 (cost = reachi->costs[depth][IdxCost] + reachi->params.pa.cost_del) > reachi->params.pa.max_cost) {

				// Too many edits or cost too high.
				DPrint((stderr, "  delete: from %03d: cost too high\n", id));
				goto Onward;
				}

			// Compute overall cost.
			cost0 = (depth == 0) ? cost : reachi->costs[0][IdxCost] + reachi->params.pa.cost_del;

			for(trans = reachi->state; trans->state != NULL; ++trans) {
				int dest_id = trans->state_id;
				DPrint((stderr, "  delete: from %03d to %03d, cost %d (%d): ", id, dest_id, cost0,
				 reachi->params.pa.max_cost));

				if(trans->assertions && assertfail(trans, &spos, tnfa, eflags)) {
					DPrint((stderr, "assertion failed\n"));
					continue;
					}

				// Compute tag positions after this transition.
				if(num_tags > 0)
					settags(tmp_tagpos, reachi->tagpos, num_tags, trans, &spos);

				// If another path has also reached this state, choose the one with the smallest cost or best
				// tags if costs are equal.
				reach_nexti = reach_next + dest_id;
				if(reach_nexti->pos == spos.pos && (cost0 > reach_nexti->costs[0][IdxCost] ||
				 (cost0 == reach_nexti->costs[0][IdxCost] && (!match_tagpos || !betterTags(num_tags,
				 tnfa->tag_directions, tmp_tagpos, reach_nexti->tagpos))))) {
					DPrint((stderr, "lose, cost0 %d, have %d\n",
					 cost0, reach_nexti->costs[0][IdxCost]));
					continue;
					}
				DPrint((stderr, "win\n"));

				// Copy state, position, tags, parameters, depth, and costs.
				reach_nexti->state = trans->state;
				reach_nexti->pos = spos.pos;
				intcpy(reach_nexti->tagpos, tmp_tagpos, num_tags);

				reach_nexti->params = reachi->params;
				if(trans->params != NULL)
					setParams(reach_nexti, trans->params, params);
				reach_nexti->params.depth = reachi->params.depth;

				intcpy((int *) reach_nexti->costs, (int *) reachi->costs, CostArraySize * (depth + 1));
				reach_nexti->costs[depth][IdxCost] = cost;
				++reach_nexti->costs[depth][IdxNumDel];
				++reach_nexti->costs[depth][IdxNumEdit];
				if(depth > 0) {
					reach_nexti->costs[0][IdxCost] = cost0;
					++reach_nexti->costs[0][IdxNumDel];
					++reach_nexti->costs[0][IdxNumEdit];
					}

				if(trans->state == tnfa->final && (match_eo < 0 || cost0 < match_costs[IdxCost] ||
				 (cost0 == match_costs[IdxCost] && (num_tags > 0 && tmp_tagpos[0] <= match_tagpos[0])))) {
					DPrint((stderr, "  setting new match at %d, cost %d\n", spos.pos, cost0));
					match_eo = spos.pos;
					intcpy(match_costs, reach_nexti->costs[0], CostArraySize);
					intcpy(match_tagpos, tmp_tagpos, num_tags);
					}

				// Add to the end of the deque.
				*deque_end = reach_nexti;
				if(++deque_end == (ringbuffer + elementsof(ringbuffer)))
					deque_end = ringbuffer;
				assert(deque_end != deque_start);
				}
Onward:
			if(++deque_start == (ringbuffer + elementsof(ringbuffer)))
				deque_start = ringbuffer;
			}
#ifdef XRE_Debug
		print_approx_reach(tnfa, reach_next, spos.pos, num_tags);
#endif
		// If any match was found at this point and REG_BESTMATCH flag not set, we're done.
		if(match_eo >= 0 && !(eflags & REG_BESTMATCH))
			break;

		// Check for end of string.
		if(type == StrUser) {
			if(str_user_end)
				break;
			}
		else if(spos.pos == len)
			break;

		prev_pos = spos.pos;
		GetNextChar();

		// Swap 'reach' and 'reach_next'.
		reachi = reach;
		reach = reach_next;
		reach_next = reachi;

		// Handle exact matches and substitutions.
		for(id = 0; id < tnfa->num_states; ++id) {
			tnfa_transition_t *trans;

			reachi = reach + id;
			if(reachi->pos < prev_pos)
				continue;  // Not reached.
			for(trans = reachi->state; trans->state != NULL; ++trans) {
				int dest_id;
				int depth;
				int cost, cost0;
				bool exact;

				if(trans->assertions && assertfail(trans, &spos, tnfa, eflags | ExecClassCheck)) {
					DPrint((stderr, "  exact,  from %d: assertion failed\n", id));
					continue;
					}

				depth = reachi->params.depth;
				dest_id = trans->state_id;

				cost = reachi->costs[depth][IdxCost];
				cost0 = reachi->costs[0][IdxCost];

				if(trans->code_min > (xcint_t) spos.prev_c || trans->code_max < (xcint_t) spos.prev_c) {

					// Handle substitutions.  The required character was not in the string, so match it in
					// place of whatever was supposed to be there and increase costs accordingly.
					exact = false;

					// Compute and check costs and edit limits at current depth.
					assert(reachi->params.pa.cost_subst != ParamUnset);
					if(reachi->costs[depth][IdxNumSubst] >= reachi->params.pa.max_subst ||
					 reachi->costs[depth][IdxNumEdit] >= reachi->params.pa.max_edit ||
					 (cost = reachi->costs[depth][IdxCost] + reachi->params.pa.cost_subst) >
					 reachi->params.pa.max_cost)
						continue; // Cost too high.

					// Compute overall cost.
					cost0 = (depth == 0) ? cost : reachi->costs[0][IdxCost] + reachi->params.pa.cost_subst;
					DPrint((stderr, "  subst,  from %03d to %03d, cost %d: ", id, dest_id, cost0));
					}
				else {
					exact = true;
					DPrint((stderr, "  exact,  from %03d to %03d, cost %d: ", id, dest_id, cost0));
					}

				// Compute tag positions after this transition.
				if(num_tags > 0)
					settags(tmp_tagpos, reachi->tagpos, num_tags, trans, &spos);

				// If another path has also reached this state, choose the one with the smallest cost or best
				// tags if costs are equal.
				reach_nexti = reach_next + dest_id;
				if(reach_nexti->pos == spos.pos && (cost0 > reach_nexti->costs[0][IdxCost] ||
				 (cost0 == reach_nexti->costs[0][IdxCost] && !betterTags(num_tags, tnfa->tag_directions,
				 tmp_tagpos, reach_nexti->tagpos)))) {
					DPrint((stderr, "lose\n"));
					continue;
					}
				DPrint((stderr, "win %d %d\n", reach_nexti->pos, reach_nexti->costs[0][IdxCost]));

				// Copy state, position, tags, and depth.
				reach_nexti->state = trans->state;
				reach_nexti->pos = spos.pos;
				intcpy(reach_nexti->tagpos, tmp_tagpos, num_tags);

				// Set parameters.
				reach_nexti->params = reachi->params;
				if(trans->params != NULL)
					setParams(reach_nexti, trans->params, params);
				reach_nexti->params.depth = reachi->params.depth;

				// Set the costs after this transition.
				intcpy((int *) reach_nexti->costs, (int *) reachi->costs, CostArraySize * (depth + 1));
				reach_nexti->costs[depth][IdxCost] = cost;
				if(!exact) {
					++reach_nexti->costs[depth][IdxNumSubst];
					++reach_nexti->costs[depth][IdxNumEdit];
					}
				if(depth > 0) {
					reach_nexti->costs[0][IdxCost] = cost0;
					if(!exact) {
						++reach_nexti->costs[0][IdxNumSubst];
						++reach_nexti->costs[0][IdxNumEdit];
						}
					}

				if(trans->state == tnfa->final && (match_eo < 0 || cost0 < match_costs[IdxCost] ||
				 (cost0 == match_costs[IdxCost] && num_tags > 0 && tmp_tagpos[0] <= match_tagpos[0]))) {
					DPrint((stderr, " setting new match at %d, cost %d\n", spos.pos, cost0));
					match_eo = spos.pos;
					intcpy(match_costs, reach_nexti->costs[0], CostArraySize);
					intcpy(match_tagpos, tmp_tagpos, num_tags);
					}
				}
			}

		// If any match was found at this point and REG_BESTMATCH flag not set, or an exact match was found, we're done.
		if(match_eo >= 0 && (!(eflags & REG_BESTMATCH) || match_costs[IdxCost] == 0))
			break;
		}
Retn:
	DPrint((stderr, "match end offset = %d, match cost = %d\n", match_eo, match_costs[IdxCost]));

	free(buf);
	match->cost = match_costs[IdxCost];
	match->num_ins = match_costs[IdxNumIns];
	match->num_del = match_costs[IdxNumDel];
	match->num_subst = match_costs[IdxNumSubst];
	*match_end_off = match_eo;

	return status != REG_OK ? status : match_eo >= 0 ? REG_OK : REG_NOMATCH;
	}
#endif // EnableApprox

#ifdef __cplusplus
	}
#endif
