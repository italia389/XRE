// compile.c - XRE regexp compiler.
//
// (c) Copyright 2022 Richard W. Marinelli
//
// This work is based on TRE ver. 0.7.5 (c) Copyright 2001-2006 Ville Laurikari <vl@iki.fi> and is licensed
// under the GNU Lesser General Public License (LGPLv3).  To view a copy of this license, see the "License.txt"
// file included with this distribution or visit http://www.gnu.org/licenses/lgpl-3.0.en.html.

// TODO:
//  - Change ast_to_tnfa() to recurse using a stack instead of recursive function calls.

#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "xre.h"
#include "parse.h"

#ifdef __cplusplus
extern "C" {
#endif

// Algorithms to set up tags so that submatch addressing can be done.

typedef enum {
	ATRecurse,
	ATAfterIteration,
	ATAfterUnionLeft,
	ATAfterUnionRight,
	ATAfterCatLeft,
	ATAfterCatRight,
	ATSetSubmatchEnd
	} addtags_symbol_t;

typedef struct {
	int tag;
	int next_tag;
	} tag_states_t;

typedef enum {
	leftfork, rightfork
	} fork_t;

// Insert a concatenation node into the root of the tree given by 'node'.  A new tag with number 'tag_id' is added as the left
// or right child, and the old left or right child becomes the old root.
static int addCatTag(fork_t child, memhdr_t *mem, ast_node_t *node, int tag_id) {
	ast_cat_t *cnode;
	ast_node_t **pnode1, **pnode2;

	DPrintf((stderr, "addCatTag: %s, tag %d\n", child == leftfork ? "left" : "right", tag_id));

	if((cnode = mem_alloc(mem, sizeof(*cnode))) == NULL)
		return REG_ESPACE;
	if(child == leftfork) {
		pnode1 = &cnode->left;
		pnode2 = &cnode->right;
		}
	else {
		pnode1 = &cnode->right;
		pnode2 = &cnode->left;
		}
	if((*pnode1 = ast_newLit(mem, LitTag, tag_id, -1)) == NULL || (*pnode2 = mem_alloc(mem, sizeof(ast_node_t))) == NULL)
		return REG_ESPACE;

	(*pnode2)->obj = node->obj;
	(*pnode2)->type = node->type;
	(*pnode2)->nullable = -1;
	(*pnode2)->submatch_id = -1;
	(*pnode2)->firstpos = NULL;
	(*pnode2)->lastpos = NULL;
	(*pnode2)->num_tags = 0;
	node->obj = cnode;
	node->type = AST_Cat;

	return 0;
	}

// Go through 'regset' and set given tag in submatch data in proper place.
static void regsetPurge(int *regset, tnfa_t *tnfa, int tag) {
	int i;

	for(i = 0; regset[i] >= 0; ++i) {
		int id = regset[i] / 2;
		bool start = !(regset[i] % 2);
		DPrintf((stderr, "  Using tag %d for %s offset of submatch %d\n", tag, start ? "start" : "end", id));
		if(start)
			tnfa->submatch_data[id].so_tag = tag;
		else
			tnfa->submatch_data[id].eo_tag = tag;
		}
	regset[0] = -1;
	}

// Find terminator slot (-1) in given integer array and return its index.
static int termidx(int *ary) {
	int *iptr = ary;
	while(*iptr >= 0)
		++iptr;
	return iptr - ary;
	}

// Add minimal tag pair (end tag, begin tag) to array in TNFA object.
static void add_min_tag(tnfa_t *tnfa, int tag, int *minTag) {
	int *minTags = tnfa->minimal_tags;
	while(*minTags >= 0)
		++minTags;
	*minTags++ = tag;
	*minTags++ = *minTag;
	*minTags = *minTag = -1;
	}

// Adds tags to appropriate locations in the given parse tree (if mem is not NULL) so that capturing-group subexpressions can be
// traced.  tnfa->end_tag and tnfa->num_tags are set to number of tags needed, and tnfa->num_minimals is set to number of
// minimal tags.  Also, if mem is not NULL, the 'parents' members of all submatch_data_t objects (in tnfa->submatch_data array)
// are determined and set.
static int addTags(memhdr_t *mem, xstack_t *stack, ast_node_t *tree, tnfa_t *tnfa) {
	int status = 0;
	ast_node_t *node = tree;			// Tree node we are currently looking at.
	int bottom = xstack_num_objects(stack);
	bool firstPass = (mem == NULL);
	int *regset, *regset0;
	int num_tags = 0;				// Total number of tags.
	int num_minimals = 0;				// Number of special minimal tags.
	int tag = 0;					// The tag that is to be added next.
	int next_tag = 1;				// Next tag to use after this one.
	int *parents;					// Stack of submatches the current submatch is contained in.
	int minimal_tag = -1;				// Tag that marks the beginning of a minimal match.
	tag_states_t *saved_states;
	tag_direction_t direction = TagMinimize;

	DPrintf((stderr, "addTags: begin %s pass\n", firstPass ? "first" : "second"));
	if(!firstPass) {
		tnfa->end_tag = 0;
		tnfa->minimal_tags[0] = -1;
		}

	if((regset0 = regset = malloc(sizeof(*regset) * ((tnfa->num_submatches + 1) * 2))) == NULL)
		return REG_ESPACE;
	regset[0] = -1;

	if((parents = malloc(sizeof(*parents) * (tnfa->num_submatches + 1))) == NULL) {
		free(regset);
		return REG_ESPACE;
		}
	parents[0] = -1;

	if((saved_states = malloc(sizeof(*saved_states) * (tnfa->num_submatches + 1))) == NULL) {
		free(regset);
		free(parents);
		return REG_ESPACE;
		}
	else {
		unsigned int i;
		for(i = 0; i <= tnfa->num_submatches; ++i)
			saved_states[i].tag = -1;
		}

	StackPushR(stack, voidptr, node);
	StackPushR(stack, int, ATRecurse);

	while(xstack_num_objects(stack) > bottom) {
		switch((addtags_symbol_t) xstack_pop_int(stack)) {
			case ATSetSubmatchEnd:
				{int id = xstack_pop_int(stack);

				// Add end of this submatch to regset.
				int i = termidx(regset);
				regset[i] = id * 2 + 1;
				regset[i + 1] = -1;

				// Pop this submatch from the parents stack.
				i = termidx(parents);
				parents[i - 1] = -1;
				}
				break;
			case ATRecurse:
				node = xstack_pop_voidptr(stack);
				if(node->submatch_id >= 0) {
					int id = node->submatch_id;
					int i;

					// Add start of this submatch to regset.
					i = termidx(regset);
					regset[i] = id * 2;
					regset[i + 1] = -1;

					if(!firstPass) {
						i = termidx(parents);
						tnfa->submatch_data[id].parents = NULL;
						if(i > 0) {
							int *p = malloc(sizeof(*p) * (i + 1));
							if(p == NULL) {
								status = REG_ESPACE;
								goto Retn;
								}
							assert(tnfa->submatch_data[id].parents == NULL);
							tnfa->submatch_data[id].parents = p;
							for(i = 0; parents[i] >= 0; ++i)
								p[i] = parents[i];
							p[i] = -1;
							}
						}

					// Add end of this submatch to regset after processing this node.
					StackPushR(stack, int, node->submatch_id);
					StackPushR(stack, int, ATSetSubmatchEnd);
					}

				switch(node->type) {
					case AST_Lit:
						{ast_lit_t *lit = node->obj;
						if(!IsSpecial(lit) || IsBackref(lit)) {
							DPrintf((stderr, "Literal %d-%d\n", (int) lit->code_min,
							 (int) lit->code_max));
							if(regset[0] >= 0) {

								// Regset is not empty, so add a tag before the literal or
								// backref.
								if(!firstPass) {
									if((status = addCatTag(leftfork, mem, node,
									 tag)) != 0)
										goto Retn;
									tnfa->tag_directions[tag] = direction;
									if(minimal_tag >= 0) {
										DPrintf((stderr, "Minimal %d, %d\n", minimal_tag,
										 tag));
										add_min_tag(tnfa, tag, &minimal_tag);
										++num_minimals;
										}
									regsetPurge(regset, tnfa, tag);
									}
								else {
									DPrintf((stderr, "  num_tags = 1\n"));
									node->num_tags = 1;
									}

								DPrintf((stderr, "  ++num_tags\n"));
								regset[0] = -1;
								tag = next_tag;
								++num_tags;
								++next_tag;
								}
							}
						else {
							assert(!IsTag(lit));
							}
						}
						break;
					case AST_Cat:
						{ast_cat_t *cat = node->obj;
						ast_node_t *left = cat->left;
						ast_node_t *right = cat->right;
						int reserved_tag = -1;
						DPrintf((stderr, "Catenation, next_tag = %d\n", next_tag));

						// After processing right child.
						StackPushR(stack, voidptr, node);
						StackPushR(stack, int, ATAfterCatRight);

						// Process right child.
						StackPushR(stack, voidptr, right);
						StackPushR(stack, int, ATRecurse);

						// After processing left child.
						StackPushR(stack, int, next_tag + left->num_tags);
						DPrintf((stderr, "  Pushing %d for after left\n",
						 next_tag + left->num_tags));
						if(left->num_tags > 0 && right->num_tags > 0) {

							// Reserve the next tag to the right child.
							DPrintf((stderr, "  Reserving next_tag %d to right child\n", next_tag));
							reserved_tag = next_tag;
							++next_tag;
							}
						StackPushR(stack, int, reserved_tag);
						StackPushR(stack, int, ATAfterCatLeft);

						// Process left child.
						StackPushR(stack, voidptr, left);
						StackPushR(stack, int, ATRecurse);
						}
						break;
					case AST_Iter:
						{ast_iter_t *iter = node->obj;
						DPrintf((stderr, "Iteration\n"));
						if(firstPass) {
							StackPushR(stack, int, regset[0] >= 0 || iter->minimal);
							}
						else {
							StackPushR(stack, int, tag);
							StackPushR(stack, int, iter->minimal);
							}
						StackPushR(stack, voidptr, node);
						StackPushR(stack, int, ATAfterIteration);
						StackPushR(stack, voidptr, iter->sub);
						StackPushR(stack, int, ATRecurse);

						// Regset is not empty, so add a tag here.
						if(regset[0] >= 0 || iter->minimal) {
							if(!firstPass) {
								if((status = addCatTag(leftfork, mem, node, tag)) != 0)
									goto Retn;
								tnfa->tag_directions[tag] = iter->minimal ? TagMinimize :
								 direction;
								if(minimal_tag >= 0) {
									DPrintf((stderr, "Minimal %d, %d\n", minimal_tag, tag));
									add_min_tag(tnfa, tag, &minimal_tag);
									++num_minimals;
									}
								regsetPurge(regset, tnfa, tag);
								}

							DPrintf((stderr, "  ++num_tags\n"));
							regset[0] = -1;
							tag = next_tag;
							++num_tags;
							++next_tag;
							}
						direction = TagMinimize;
						}
						break;
					case AST_Union:
						{ast_union_t *uni = node->obj;
						ast_node_t *left = uni->left;
						ast_node_t *right = uni->right;
						int left_tag;
						int right_tag;

						if(regset[0] >= 0) {
							left_tag = next_tag;
							right_tag = next_tag + 1;
							}
						else {
							left_tag = tag;
							right_tag = next_tag;
							}

						DPrintf((stderr, "Union\n"));

						// After processing right child.
						StackPushR(stack, int, right_tag);
						StackPushR(stack, int, left_tag);
						StackPushR(stack, voidptr, regset);
						StackPushR(stack, int, regset[0] >= 0);
						StackPushR(stack, voidptr, node);
						StackPushR(stack, voidptr, right);
						StackPushR(stack, voidptr, left);
						StackPushR(stack, int, ATAfterUnionRight);

						// Process right child.
						StackPushR(stack, voidptr, right);
						StackPushR(stack, int, ATRecurse);

						// After processing left child.
						StackPushR(stack, int, ATAfterUnionLeft);

						// Process left child.
						StackPushR(stack, voidptr, left);
						StackPushR(stack, int, ATRecurse);

						// Regset is not empty, so add a tag here.
						if(regset[0] >= 0) {
							if(!firstPass) {
								if((status = addCatTag(leftfork, mem, node, tag)) != 0)
									goto Retn;
								tnfa->tag_directions[tag] = direction;
								if(minimal_tag >= 0) {
									DPrintf((stderr, "Minimal %d, %d\n", minimal_tag, tag));
									add_min_tag(tnfa, tag, &minimal_tag);
									++num_minimals;
									}
								regsetPurge(regset, tnfa, tag);
								}

							DPrintf((stderr, "  ++num_tags\n"));
							regset[0] = -1;
							tag = next_tag;
							++num_tags;
							++next_tag;
							}

						if(node->num_submatches > 0) {

							// The next two tags are reserved for markers.
							++next_tag;
							tag = next_tag;
							++next_tag;
							}
						}
						break;
					}

				if(node->submatch_id >= 0) {

					// Push this submatch on the parents stack.
					int i = termidx(parents);
					parents[i] = node->submatch_id;
					parents[i + 1] = -1;
					}
				break; // End case: ATRecurse.
			case ATAfterIteration:
				{int minimal = 0;
				int enter_tag;

				node = xstack_pop_voidptr(stack);
				if(firstPass) {
					node->num_tags = ((ast_iter_t *) node->obj)->sub->num_tags + xstack_pop_int(stack);
					minimal_tag = -1;
					}
				else {
					minimal = xstack_pop_int(stack);
					enter_tag = xstack_pop_int(stack);
					if(minimal)
						minimal_tag = enter_tag;
					}

				DPrintf((stderr, "After iteration\n"));
				if(!firstPass) {
					DPrintf((stderr, "  Setting direction to %s\n", minimal ? "minimize" : "maximize"));
					direction = minimal ? TagMinimize : TagMaximize;
					}
				}
				break;
			case ATAfterCatLeft:
				{int new_tag = xstack_pop_int(stack);
				next_tag = xstack_pop_int(stack);
				DPrintf((stderr, "After cat left, tag = %d, next_tag = %d\n", tag, next_tag));
				if(new_tag >= 0) {
					DPrintf((stderr, "  Setting tag to %d\n", new_tag));
					tag = new_tag;
					}
				}
				break;
			case ATAfterCatRight:
				DPrintf((stderr, "After cat right\n"));
				node = xstack_pop_voidptr(stack);
				if(firstPass)
					node->num_tags = ((ast_cat_t *) node->obj)->left->num_tags
					 + ((ast_cat_t *) node->obj)->right->num_tags;
				break;
			case ATAfterUnionLeft:
				DPrintf((stderr, "After union left\n"));

				// Lift the bottom of the 'regset' array so that when processing the right operand the items
				// currently in the array are invisible.  The original bottom was saved at ATUnion and will be
				// restored at ATAfterUnionRight below.
				while(*regset >= 0)
					++regset;
				break;
			case ATAfterUnionRight:
				{int added_tags, leftTag, rightTag;
				ast_node_t *left = xstack_pop_voidptr(stack);
				ast_node_t *right = xstack_pop_voidptr(stack);
				DPrintf((stderr, "After union right\n"));
				node = xstack_pop_voidptr(stack);
				added_tags = xstack_pop_int(stack);
				if(firstPass) {
					node->num_tags = ((ast_union_t *) node->obj)->left->num_tags
					 + ((ast_union_t *) node->obj)->right->num_tags + added_tags
					 + ((node->num_submatches > 0) ? 2 : 0);
					}
				regset = xstack_pop_voidptr(stack);
				leftTag = xstack_pop_int(stack);
				rightTag = xstack_pop_int(stack);

				// Add tags after both children, the left child gets a smaller tag than the right child.  This
				// guarantees that we prefer the left child over the right child.
				// XXX - This is not always necessary (if the children have tags which must be seen for every
				// match of that child).
				// XXX - Check if this is the only place where addCatTag(rightfork,...) is used.  If so, use
				// addCatTag(leftfork,...) (putting the tag before the child as opposed after the child) and get
				// rid of leftfork and rightfork -- just use left always.
				if(node->num_submatches > 0) {
					if(!firstPass) {
						if((status = addCatTag(rightfork, mem, left, leftTag)) != 0)
							goto Retn;
						tnfa->tag_directions[leftTag] = TagMaximize;
						if((status = addCatTag(rightfork, mem, right, rightTag)) != 0)
							goto Retn;
						tnfa->tag_directions[rightTag] = TagMaximize;
						}
					DPrintf((stderr, "  num_tags += 2\n"));
					num_tags += 2;
					}
				direction = TagMaximize;
				}
				break;
			default:
				assert(0);
				break;
			}
		}

	if(!firstPass) {
		regsetPurge(regset, tnfa, tag);
		if(minimal_tag >= 0) {
			DPrintf((stderr, "Minimal %d, %d\n", minimal_tag, tag));
			add_min_tag(tnfa, tag, &minimal_tag);
			++num_minimals;
			}
		}

	DPrintf((stderr, "addTags: %s pass complete.  num_tags: %d, num_minimals: %d\n",
	 firstPass ? "first" : "second", num_tags, num_minimals));
	assert(tree->num_tags == num_tags);
	tnfa->end_tag = tnfa->num_tags = num_tags;
	tnfa->num_minimals = num_minimals;
Retn:
	free(regset0);
	free(parents);
	free(saved_states);
	return status;
	}

// AST to TNFA compilation routines.

typedef enum {
	CopyRecurse,
	CopySetResultPtr
	} copyast_symbol_t;

// Flags for copyAST().
#define CopyRemoveTags		1
#define CopyMaximizeFirstTag	2

static int copyAST(memhdr_t *mem, xstack_t *stack, ast_node_t *ast, int flags, int *pos_add, tag_direction_t *tag_directions,
 ast_node_t **copy, int *max_pos) {
	int status;
	int bottom = xstack_num_objects(stack);
	int num_copied = 0;
	bool first_tag = true;
	ast_node_t **result = copy;

	StackPush(stack, voidptr, ast);
	StackPush(stack, int, CopyRecurse);

	while(xstack_num_objects(stack) > bottom) {
		ast_node_t *node;
		switch((copyast_symbol_t) xstack_pop_int(stack)) {
			case CopySetResultPtr:
				result = xstack_pop_voidptr(stack);
				break;
			case CopyRecurse:
				node = xstack_pop_voidptr(stack);
				switch(node->type) {
					case AST_Lit:
						{ast_lit_t *lit = node->obj;
						int pos = lit->position;
						int min = lit->code_min;
						int max = lit->code_max;
						if(!IsSpecial(lit) || IsBackref(lit)) {

							// XXX - e.g. [ab] has only one position but two nodes, so we are
							// creating holes in the state space here.  Not fatal, just wastes
							// memory.
							pos += *pos_add;
							++num_copied;
							}
						else if(IsTag(lit)) {
							if(flags & CopyRemoveTags) {

								// Change this tag to empty.
								min = LitEmpty;
								max = pos = -1;
								}
							else if((flags & CopyMaximizeFirstTag) && first_tag) {

								// Maximize the first tag.
								tag_directions[max] = TagMaximize;
								first_tag = false;
								}
							}
						if((*result = ast_newLit(mem, min, max, pos)) == NULL)
							return REG_ESPACE;
						if(pos > *max_pos)
							*max_pos = pos;
						}
						break;
					case AST_Union:
						{ast_union_t *uni = node->obj;
						ast_union_t *tmp;
						if((*result = ast_newUnion(mem, uni->left, uni->right)) == NULL)
							return REG_ESPACE;
						tmp = (*result)->obj;
						result = &tmp->left;
						StackPush(stack, voidptr, uni->right);
						StackPush(stack, int, CopyRecurse);
						StackPush(stack, voidptr, &tmp->right);
						StackPush(stack, int, CopySetResultPtr);
						StackPush(stack, voidptr, uni->left);
						StackPush(stack, int, CopyRecurse);
						}
						break;
					case AST_Cat:
						{ast_cat_t *cat = node->obj;
						ast_cat_t *tmp;
						if((*result = ast_newCat(mem, cat->left, cat->right)) == NULL)
							return REG_ESPACE;
						tmp = (*result)->obj;
						tmp->left = NULL;
						tmp->right = NULL;
						result = &tmp->left;

						StackPush(stack, voidptr, cat->right);
						StackPush(stack, int, CopyRecurse);
						StackPush(stack, voidptr, &tmp->right);
						StackPush(stack, int, CopySetResultPtr);
						StackPush(stack, voidptr, cat->left);
						StackPush(stack, int, CopyRecurse);
						}
						break;
					case AST_Iter:
						{ast_iter_t *iter = node->obj;
						StackPush(stack, voidptr, iter->sub);
						StackPush(stack, int, CopyRecurse);
						if((*result = ast_newIter(mem, iter->sub, iter->min, iter->max,
						 iter->minimal)) == NULL)
							return REG_ESPACE;
						iter = (*result)->obj;
						result = &iter->sub;
						}
						break;
					default:
						assert(0);
					}
				break;
			default:
				assert(0);
			}
		}

	*pos_add += num_copied;
	return 0;
	}

typedef enum {
	ExpandRecurse,
	ExpandAfterIter
	} expand_ast_symbol_t;

// Expands each iteration node that has a finite, non-zero minimum or maximum iteration count to a catenated sequence of copies
// of the node.
static int expandAST(memhdr_t *mem, xstack_t *stack, ast_node_t *ast, int *position, tag_direction_t *tag_directions,
 int *max_depth) {
	int status;
	int bottom = xstack_num_objects(stack);
	int pos_add = 0;
	int pos_add_total = 0;
	int max_pos = 0;				// For sanity check.
	int iter_depth = 0;
	int params_depth = 0;				// Approximate parameter nesting level.
	params_t params = {				// Current approximate matching parameters.
		{ParamDefault, ParamDefault, ParamDefault, ParamDefault,
		 ParamDefault, ParamDefault, ParamDefault, ParamDefault},
		 ParamDefault};

	StackPush(stack, voidptr, ast);
	StackPush(stack, int, ExpandRecurse);
	while(xstack_num_objects(stack) > bottom) {
		ast_node_t *node;
		expand_ast_symbol_t symbol;

		DPrintf((stderr, "pos_add %d\n", pos_add));

		symbol = (expand_ast_symbol_t) xstack_pop_int(stack);
		node = xstack_pop_voidptr(stack);
		switch(symbol) {
			case ExpandRecurse:
				switch(node->type) {
					case AST_Lit:
						{ast_lit_t *lit= node->obj;
						if(!IsSpecial(lit) || IsBackref(lit)) {
							lit->position += pos_add;
							if(lit->position > max_pos)
								max_pos = lit->position;
							}
						}
						break;
					case AST_Union:
						{ast_union_t *uni = node->obj;
						StackPush(stack, voidptr, uni->right);
						StackPush(stack, int, ExpandRecurse);
						StackPush(stack, voidptr, uni->left);
						StackPush(stack, int, ExpandRecurse);
						}
						break;
					case AST_Cat:
						{ast_cat_t *cat = node->obj;
						StackPush(stack, voidptr, cat->right);
						StackPush(stack, int, ExpandRecurse);
						StackPush(stack, voidptr, cat->left);
						StackPush(stack, int, ExpandRecurse);
						}
						break;
					case AST_Iter:
						{ast_iter_t *iter = node->obj;
						StackPush(stack, int, pos_add);
						StackPush(stack, voidptr, node);
						StackPush(stack, int, ExpandAfterIter);
						StackPush(stack, voidptr, iter->sub);
						StackPush(stack, int, ExpandRecurse);

						// If we are going to expand this node at ExpandAfterIter, then don't increase
						// the 'pos' fields of the nodes now -- it will get done when expanding.
						if(iter->min > 1 || iter->max > 1)
							pos_add = 0;
						++iter_depth;
						DPrintf((stderr, "iter\n"));
						}
						break;
					default:
						assert(0);
						break;
					}
				break;
			case ExpandAfterIter:
				{ast_iter_t *iter = node->obj;
				int pos_add_last;

				pos_add = xstack_pop_int(stack);
				pos_add_last = pos_add;
				if(iter->min > 1 || iter->max > 1) {
					ast_node_t *copy;
					ast_node_t *seq1 = NULL, *seq2 = NULL;
					int j, flags;
					int pos_add_save = pos_add;

					// Create a catenated sequence of copies of the node.
					for(j = 0; j < iter->min; ++j) {

						// Remove tags from all but the last copy.
						flags = ((j + 1 < iter->min) ? CopyRemoveTags : CopyMaximizeFirstTag);
						DPrintf((stderr, "  iter copy %d, pos_add %d, flags %d\n", j, pos_add, flags));
						pos_add_save = pos_add;
						if((status = copyAST(mem, stack, iter->sub, flags, &pos_add, tag_directions,
						 &copy, &max_pos)) != 0)
							return status;
						seq1 = (seq1 == NULL) ? copy : ast_newCat(mem, seq1, copy);
						if(seq1 == NULL)
							return REG_ESPACE;
						}

					if(iter->max == -1) {

						// No upper limit.
						pos_add_save = pos_add;
						if((status = copyAST(mem, stack, iter->sub, 0, &pos_add, NULL, &seq2,
						 &max_pos)) != 0)
							return status;
						if((seq2 = ast_newIter(mem, seq2, 0, -1, 0)) == NULL)
							return REG_ESPACE;
						}
					else {
						for(j = iter->min; j < iter->max; ++j) {
							ast_node_t *tmp, *copy;
							pos_add_save = pos_add;
							if((status = copyAST(mem, stack, iter->sub, 0, &pos_add, NULL, &copy,
							 &max_pos)) != 0)
								return status;
							seq2 = (seq2 == NULL) ? copy : ast_newCat(mem, copy, seq2);
							if(seq2 == NULL)
								return REG_ESPACE;
							if((tmp = ast_newLit(mem, LitEmpty, -1, -1)) == NULL ||
							 (seq2 = ast_newUnion(mem, tmp, seq2)) == NULL)
								return REG_ESPACE;
							}
						}

					pos_add = pos_add_save;
					if(seq1 == NULL)
						seq1 = seq2;
					else if(seq2 != NULL)
						seq1 = ast_newCat(mem, seq1, seq2);
					if(seq1 == NULL)
						return REG_ESPACE;
					node->obj = seq1->obj;
					node->type = seq1->type;
					}

				--iter_depth;
				pos_add_total += pos_add - pos_add_last;
				if(iter_depth == 0)
					pos_add = pos_add_total;

				// If approximate parameters are specified, surround the result with two parameter-setting
				// nodes.  The one on the left sets the specified parameters, and the one on the right restores
				// the old parameters (which are usually the default ones specified in the xregaexec() call).
				if(iter->params != NULL) {
					ast_node_t *tmp_l, *tmp_r, *tmp_node, *node_copy;
					params_t *old_params;

					if((tmp_l = ast_newLit(mem, LitParam, 0, -1)) == NULL ||
					 (tmp_r = ast_newLit(mem, LitParam, 0, -1)) == NULL ||
					 (old_params = mem_alloc(mem, sizeof(*old_params))) == NULL)
						return REG_ESPACE;
					((ast_lit_t *) tmp_l->obj)->u.params = iter->params;
					iter->params->depth = params_depth + 1;
					*old_params = params;
					((ast_lit_t *) tmp_r->obj)->u.params = old_params;
					old_params->depth = params_depth;

					if((node_copy = mem_alloc(mem, sizeof(ast_node_t))) == NULL)
						return REG_ESPACE;
					memcpy(node_copy, node, sizeof(*node));
					if((tmp_node = ast_newCat(mem, tmp_l, node_copy)) == NULL ||
					 (tmp_node = ast_newCat(mem, tmp_node, tmp_r)) == NULL)
						return REG_ESPACE;

					// Replace the contents of 'node' with 'tmp_node'.
					memcpy(node, tmp_node, sizeof(*node));

					if(++params_depth > *max_depth)
						*max_depth = params_depth;
					}
				}
				break;
			default:
				assert(0);
				break;
			}
		}

	// 'max_pos' should never be larger than '*position' if the above code works.
	DPrintf((stderr, "Setting *position to %d + %d = %d, max_pos %d\n",
	 *position, pos_add_total, *position + pos_add_total, max_pos));
	*position += pos_add_total;
	assert(max_pos <= *position);

#if XRE_Debug
	printAST(ast, "post-expansion");
	fprintf(stderr, "*position %d, max_pos %d\n", *position, max_pos);
#endif
	return 0;
	}

static void setEmpty(pos_and_tags_t *p) {

	p->position = p->code_min = p->code_max = -1;
	}

static pos_and_tags_t *makeEmpty(memhdr_t *mem) {
	pos_and_tags_t *p;

	if((p = mem_calloc(mem, 1, sizeof(*p))) == NULL)
		return NULL;
	setEmpty(p);
	return p;
	}

static pos_and_tags_t *makePT(memhdr_t *mem, int position, int code_min, int code_max, xctype_t class, xctype_t *neg_classes,
 int backref) {
	pos_and_tags_t *new_set;

	// Create set of two objects; set first to passed values and second one empty.
	if((new_set = mem_calloc(mem, 2, sizeof(*new_set))) == NULL)
		return NULL;

	new_set->position = position;
	new_set->code_min = code_min;
	new_set->code_max = code_max;
	new_set->class = class;
	new_set->neg_classes = neg_classes;
	new_set->backref = backref;

	setEmpty(new_set + 1);

	return new_set;
	}

// Copy parameter to 'newParams' if set in 'oldParams'.
static void copy_if_set(params_t *newParams, params_t *oldParams) {
	int *new = (int *) newParams;
	int *old = (int *) oldParams;
	int *oldz = old + sizeof(*oldParams) / sizeof(oldParams->depth);

	do {
		if(*old != ParamUnset)
			*new = *old;
		++new;
		} while(++old < oldz);
	}

static pos_and_tags_t *makeUnion(memhdr_t *mem, pos_and_tags_t *set1, pos_and_tags_t *set2, int *tags, int assertions,
 params_t *params) {
	int s1, s2, i, j;
	pos_and_tags_t *new_set;
	int *new_tags;
	int num_tags = (tags == NULL) ? 0 : termidx(tags);

	for(s1 = 0; set1[s1].position >= 0; ++s1);
	for(s2 = 0; set2[s2].position >= 0; ++s2);
	if((new_set = mem_calloc(mem, s1 + s2 + 1, sizeof(*new_set))) == NULL)
		return NULL;

	for(s1 = 0; set1[s1].position >= 0; ++s1) {
		new_set[s1].position = set1[s1].position;
		new_set[s1].code_min = set1[s1].code_min;
		new_set[s1].code_max = set1[s1].code_max;
		new_set[s1].assertions = set1[s1].assertions | assertions;
		new_set[s1].class = set1[s1].class;
		new_set[s1].neg_classes = set1[s1].neg_classes;
		new_set[s1].backref = set1[s1].backref;

		if(set1[s1].tags == NULL && tags == NULL)
			new_set[s1].tags = NULL;
		else {
			i = (set1[s1].tags == NULL) ? 0 : termidx(set1[s1].tags);
			if((new_tags = mem_alloc(mem, (sizeof(*new_tags) * (i + num_tags + 1)))) == NULL)
				return NULL;
			for(j = 0; j < i; ++j)
				new_tags[j] = set1[s1].tags[j];
			for(i = 0; i < num_tags; ++i)
				new_tags[j + i] = tags[i];
			new_tags[j + i] = -1;
			new_set[s1].tags = new_tags;
			}

		if(set1[s1].params != NULL)
			new_set[s1].params = set1[s1].params;
		if(params != NULL) {
			if(new_set[s1].params == NULL)
				new_set[s1].params = params;
			else {
				if((new_set[s1].params = mem_alloc(mem, sizeof(*params))) == NULL)
					return NULL;
				copy_if_set(new_set[s1].params,params);
				}
			}
		}

	for(s2 = 0; set2[s2].position >= 0; ++s2) {
		new_set[s1 + s2].position = set2[s2].position;
		new_set[s1 + s2].code_min = set2[s2].code_min;
		new_set[s1 + s2].code_max = set2[s2].code_max;

		// XXX - why not | assertions here as well?
		new_set[s1 + s2].assertions = set2[s2].assertions;
		new_set[s1 + s2].class = set2[s2].class;
		new_set[s1 + s2].neg_classes = set2[s2].neg_classes;
		new_set[s1 + s2].backref = set2[s2].backref;

		if(set2[s2].tags == NULL)
			new_set[s1 + s2].tags = NULL;
		else {
			i = termidx(set2[s2].tags);
			if((new_tags = mem_alloc(mem, sizeof(*new_tags) * (i + 1))) == NULL)
				return NULL;
			memcpy(new_tags, set2[s2].tags, sizeof(*new_tags) * i);
			new_tags[i] = -1;
			new_set[s1 + s2].tags = new_tags;
			}

		if(set2[s2].params != NULL)
			new_set[s1 + s2].params = set2[s2].params;
		if(params != NULL) {
			if(new_set[s1 + s2].params == NULL)
				new_set[s1 + s2].params = params;
			else {
				if((new_set[s1 + s2].params = mem_alloc(mem, sizeof(*params))) == NULL)
					return NULL;
				copy_if_set(new_set[s1 + s2].params,params);
				}
			}
		}
	new_set[s1 + s2].position = -1;
	return new_set;
	}

// Finds the empty path through 'node' (which is the one that should be taken according to POSIX rules) and adds the tags on
// that path to 'tags'. 'tags' may be NULL.  If 'num_tags_seen' is not NULL, it is set to the number of tags seen on the path.
static int matchEmpty(xstack_t *stack, ast_node_t *node, int *tags, int *assertions, params_t *params, int *num_tags_seen,
 bool *params_seen) {
	ast_lit_t *lit;
	ast_union_t *uni;
	ast_cat_t *cat;
	ast_iter_t *iter;
	int i, status;
	int bottom = xstack_num_objects(stack);

	if(num_tags_seen != NULL)
		*num_tags_seen = 0;
	if(params_seen != NULL)
		*params_seen = false;

	if((status = xstack_push_voidptr(stack, node)) != 0)
		return status;

	// Walk through the tree recursively.
	while(xstack_num_objects(stack) > bottom) {
		node = xstack_pop_voidptr(stack);
		switch(node->type) {
			case AST_Lit:
				lit = (ast_lit_t *) node->obj;
				switch(lit->code_min) {
					case LitTag:
						if(lit->code_max >= 0) {
							if(tags != NULL) {

								// Add the tag to 'tags'.
								for(i = 0; tags[i] >= 0; ++i)
									if(tags[i] == lit->code_max)
										break;
								if(tags[i] < 0) {
									tags[i] = lit->code_max;
									tags[i + 1] = -1;
									}
								}
							if(num_tags_seen != NULL)
								++(*num_tags_seen);
							}
						break;
					case LitAssert:
						assert(lit->code_max >= 1 || lit->code_max <= AssertLast);
						if(assertions != NULL)
							*assertions |= lit->code_max;
						break;
					case LitParam:
						if(params != NULL)
							*params = *lit->u.params;
						if(params_seen != NULL)
							*params_seen = true;
						break;
					case LitEmpty:
						break;
					default:
						assert(0);
						break;
					}
				break;
			case AST_Union:
				// Subexpressions starting earlier take priority over ones starting later, so we prefer the left
				// subexpression over the right subexpression.
				uni = (ast_union_t *) node->obj;
				if(uni->left->nullable) {
					StackPush(stack, voidptr, uni->left);
					}
				else if(uni->right->nullable) {
					StackPush(stack, voidptr, uni->right);
					}
				else
					assert(0);
				break;
			case AST_Cat:
				// The path must go through both children.
				cat = (ast_cat_t *) node->obj;
				assert(cat->left->nullable);
				assert(cat->right->nullable);
				StackPush(stack, voidptr, cat->left);
				StackPush(stack, voidptr, cat->right);
				break;
			case AST_Iter:
				// A match with an empty string is preferred over no match at all, so we go through the argument
				// if possible.
				iter = (ast_iter_t *) node->obj;
				if(iter->sub->nullable)
					StackPush(stack, voidptr, iter->sub);
				break;
			default:
				assert(0);
				break;
			}
		}

	return 0;
	}

// Create tags, assertions, and parameters from a child of a catenation node.
static int makeTAP(memhdr_t *mem, xstack_t *stack, ast_node_t *node, int **ptags, int *passertions, params_t **pparams) {
 	int status, num_tags, *tags;
	bool params_seen;
	params_t *params;

	// Make a first pass with matchEmpty() to get the number of tags and parameters.
	if((status = matchEmpty(stack, node, NULL, NULL, NULL, &num_tags, &params_seen)) != 0)
		return status;

	// Allocate arrays for the tags and parameters.
	if((tags = malloc(sizeof(int) * (num_tags + 1))) == NULL)
		return REG_ESPACE;
	tags[0] = -1;
	*passertions = 0;
	if(!params_seen)
		params = NULL;
	else if((params = mem_alloc(mem, sizeof(*params))) == NULL) {
		free(tags);
		return REG_ESPACE;
		}

	// Make a second pass with matchEmpty() to get the list of tags and parameters.
	if((status = matchEmpty(stack, node, tags, passertions, params, NULL, NULL)) != 0) {
		free(tags);
		return status;
		}

	// Return results.
	*ptags = tags;
	*pparams = params;
	return 0;
 	}

typedef enum {
	NFLRecurse,
	NFLPostUnion,
	NFLPostCatenation,
	NFLPostIteration
	} nfl_stack_symbol_t;

// Computes and fills in the fields 'nullable', 'firstpos', and 'lastpos' in the nodes of the AST 'tree'.
static int computeNFL(memhdr_t *mem, xstack_t *stack, ast_node_t *tree) {
	int status;
	int bottom = xstack_num_objects(stack);

	StackPush(stack, voidptr, tree);
	StackPush(stack, int, NFLRecurse);

	while(xstack_num_objects(stack) > bottom) {
		nfl_stack_symbol_t symbol;
		ast_node_t *node;

		symbol = (nfl_stack_symbol_t) xstack_pop_int(stack);
		node = xstack_pop_voidptr(stack);
		switch(symbol) {
			case NFLRecurse:
				switch(node->type) {
					case AST_Lit:
						{ast_lit_t *lit = (ast_lit_t *) node->obj;
						if(IsBackref(lit)) {

							// Back references: nullable = false, firstpos = {i}, lastpos = {i}.
							node->nullable = 0;
							if((node->firstpos = makePT(mem, lit->position, 0, XRE_CHAR_MAX, 0,
							 NULL, -1)) == NULL || (node->lastpos = makePT(mem, lit->position, 0,
							 XRE_CHAR_MAX, 0, NULL, (int) lit->code_max)) == NULL)
								return REG_ESPACE;
							}
						else if(lit->code_min < 0) {

							// Tags, empty strings, params, and zero width assertions:
							// nullable = true, firstpos = {}, and lastpos = {}.
							node->nullable = 1;
							if((node->firstpos = makeEmpty(mem)) == NULL ||
							 (node->lastpos = makeEmpty(mem)) == NULL)
								return REG_ESPACE;
							}
						else {
							// Literal at position i: nullable = false, firstpos = {i},
							// lastpos = {i}.
							node->nullable = 0;
							if((node->firstpos = makePT(mem, lit->position, (int) lit->code_min,
							 (int) lit->code_max, 0, NULL, -1)) == NULL || (node->lastpos =
							 makePT(mem, lit->position, (int) lit->code_min, (int) lit->code_max,
							 lit->u.class, lit->neg_classes, -1)) == NULL)
								return REG_ESPACE;
							}
						}
						break;
					case AST_Union:
						// Compute the attributes for the two subtrees, and after that for this node.
						StackPush(stack, voidptr, node);
						StackPush(stack, int, NFLPostUnion);
						StackPush(stack, voidptr, ((ast_union_t *) node->obj)->right);
						StackPush(stack, int, NFLRecurse);
						StackPush(stack, voidptr, ((ast_union_t *) node->obj)->left);
						StackPush(stack, int, NFLRecurse);
						break;
					case AST_Cat:
						// Compute the attributes for the two subtrees, and after that for this node.
						StackPush(stack, voidptr, node);
						StackPush(stack, int, NFLPostCatenation);
						StackPush(stack, voidptr, ((ast_cat_t *) node->obj)->right);
						StackPush(stack, int, NFLRecurse);
						StackPush(stack, voidptr, ((ast_cat_t *) node->obj)->left);
						StackPush(stack, int, NFLRecurse);
						break;
					case AST_Iter:
						// Compute the attributes for the subtree, and after that for this node.
						StackPush(stack, voidptr, node);
						StackPush(stack, int, NFLPostIteration);
						StackPush(stack, voidptr, ((ast_iter_t *) node->obj)->sub);
						StackPush(stack, int, NFLRecurse);
						break;
					}
				break; // End case: NFLRecurse.
			case NFLPostUnion:
				{ast_union_t *uni = (ast_union_t *) node->obj;
				node->nullable = uni->left->nullable || uni->right->nullable;
				if((node->firstpos = makeUnion(mem, uni->left->firstpos, uni->right->firstpos, NULL, 0,
				 NULL)) == NULL || (node->lastpos = makeUnion(mem, uni->left->lastpos, uni->right->lastpos,
				 NULL, 0, NULL)) == NULL)
					return REG_ESPACE;
				}
				break;
			case NFLPostIteration:
				{ast_iter_t *iter = (ast_iter_t *) node->obj;

				// If zero iterations of the node is the only match, make the null path the only path.
				if(iter->min == 0 && iter->max == 0) {
					node->nullable = 1;
					if((node->firstpos = makeEmpty(mem)) == NULL ||
					 (node->lastpos = makeEmpty(mem)) == NULL)
						return REG_ESPACE;
					}
				else {
					node->nullable = iter->min == 0 || iter->sub->nullable;
					node->firstpos = iter->sub->firstpos;
					node->lastpos = iter->sub->lastpos;
					}
				}
				break;
			case NFLPostCatenation:
				{int status, assertions, *tags;
				params_t *params;
				ast_cat_t *cat = node->obj;

				node->nullable = cat->left->nullable && cat->right->nullable;

				// Compute firstpos.
				if(cat->left->nullable) {

					// The left side matches the empty string.  Get tags, assertions, and parameters from
					// the node and create a union.
					if((status = makeTAP(mem, stack, cat->left, &tags, &assertions, &params)) != 0)
						return status;
					node->firstpos = makeUnion(mem, cat->right->firstpos, cat->left->firstpos, tags,
					 assertions, params);
					free(tags);
					if(!node->firstpos)
						return REG_ESPACE;
					}
				else
					node->firstpos = cat->left->firstpos;

				// Compute lastpos.
				if(cat->right->nullable) {

					// The right side matches the empty string.  Get tags, assertions, and parameters from
					// the node and create a union.
					if((status = makeTAP(mem, stack, cat->right, &tags, &assertions, &params)) != 0)
						return status;
					node->lastpos = makeUnion(mem, cat->left->lastpos, cat->right->lastpos, tags,
					 assertions, params);
					free(tags);
					if(!node->lastpos)
						return REG_ESPACE;
					}
				else
					node->lastpos = cat->right->lastpos;
				}
				break;
			default:
				assert(0);
				break;
			}
		}

	return 0;
	}

#if XRE_Debug
// Print list of tag IDs enclosed in brackets [ ] with given label.
void printTags(char *label, int *tags, int count) {

	if(tags != NULL) {
		int *tagsz = tags + count;
		char *prefix = "";
		fprintf(stderr, "%s [", label);
		while((count >= 0 && tags < tagsz) || (count < 0 && *tags >= 0)) {
			fprintf(stderr, "%s%d", prefix, *tags++);
			prefix = ", ";
			}
		fputc(']', stderr);
		}
	}

// Print codepoint ranges and tag IDs.
void print_codes_and_tags(int code_min, int code_max, int *tags) {

	fprintf(stderr, "%3d", code_min);
	if(code_max != code_min)
		fprintf(stderr, " - %3d", code_max);
	printTags(", tags", tags, -1);
	}
#endif

// Add a transition from each position in 'p1' to each position in 'p2'.
static int makeTrans(pos_and_tags_t *p1, pos_and_tags_t *p2, tnfa_transition_t *transitions, int *counts, int *offs) {
	pos_and_tags_t *orig_p2 = p2;
	tnfa_transition_t *trans;
	int i, j, k, l, dup, prev_p2_pos;

	if(transitions != NULL)
		while(p1->position >= 0) {
			p2 = orig_p2;
			prev_p2_pos = -1;
			while(p2->position >= 0) {

				// Optimization: if this position was already handled, skip it.
				if(p2->position == prev_p2_pos) {
					++p2;
					continue;
					}
				prev_p2_pos = p2->position;

				// Set 'trans' to point to the next unused transition from position 'p1->position'.
				trans = transitions + offs[p1->position];
#if 0
				while(trans->state != NULL) {
					// If we find a previous transition from 'p1->position' to 'p2->position', it is
					// overwritten.  This can happen only if there are nested loops in the regexp, like in
					// "((a)*)*".  In POSIX, repetition using the outer loop is always preferred over using
					// the inner loop.  Therefore the transition for the inner loop is useless and can be
					// thrown away.
					//
					// XXX - The same position is used for all nodes in a bracket expression, so this
					// optimization cannot be used (it will break bracket expressions) unless I figure out a
					// way to detect it here.
					if(trans->state_id == p2->position) {
						DPrintf((stderr, "*"));
						break;
						}
					++trans;
					}
				if(trans->state == NULL)
					(trans + 1)->state = NULL;
#else
				while(trans->state != NULL)
					++trans;
				(trans + 1)->state = NULL;
#endif
				// Use the character ranges, assertions, etc. from 'p1' for the transition from 'p1' to 'p2'.
				trans->code_min = (xint_t) p1->code_min;
				trans->code_max = (xint_t) p1->code_max;
				trans->state = transitions + offs[p2->position];
				trans->state_id = p2->position;
				trans->assertions = p1->assertions | p2->assertions | (p1->class ? AssertCC : 0) |
				 (p1->neg_classes != NULL ? AssertNegCC : 0);
				if(p1->backref >= 0) {
					assert((trans->assertions & AssertCC) == 0);
					assert(p2->backref < 0);
					trans->u.backref = p1->backref;
					trans->assertions |= AssertBackref;
					}
				else
					trans->u.class = p1->class;
				if(p1->neg_classes != NULL) {
					for(i = 0; p1->neg_classes[i] != (xctype_t) 0; ++i);
					if((trans->neg_classes = malloc(sizeof(*trans->neg_classes) * (i + 1))) == NULL)
						return REG_ESPACE;
					for(i = 0; p1->neg_classes[i] != (xctype_t) 0; ++i)
						trans->neg_classes[i] = p1->neg_classes[i];
					trans->neg_classes[i] = (xctype_t) 0;
					}
				else
					trans->neg_classes = NULL;

				// Find out how many tags this transition has.
				i = 0;
				if(p1->tags != NULL)
					while(p1->tags[i] >= 0)
						++i;
				j = 0;
				if(p2->tags != NULL)
					while(p2->tags[j] >= 0)
						++j;

				// If we are overwriting a transition, free the old tag array.
				if(trans->tags != NULL)
					free(trans->tags);
				trans->tags = NULL;

				// If there were any tags, allocate an array and fill it.
				if(i + j > 0) {
					if((trans->tags = malloc(sizeof(*trans->tags) * (i + j + 1))) == NULL)
						return REG_ESPACE;
					i = 0;
					if(p1->tags != NULL)
						while(p1->tags[i] >= 0) {
							trans->tags[i] = p1->tags[i];
							++i;
							}
					l = i;
					j = 0;
					if(p2->tags != NULL)
						while(p2->tags[j] >= 0) {

							// Don't add duplicates.
							dup = 0;
							for(k = 0; k < i; ++k)
								if(trans->tags[k] == p2->tags[j]) {
									dup = 1;
									break;
									}
							if(!dup)
								trans->tags[l++] = p2->tags[j];
							++j;
							}
					trans->tags[l] = -1;
					}

				// Set the parameter array.  If both 'p2' and 'p1' have parameters, the values in 'p2'
				// override those in 'p1'.
				if(p1->params != NULL || p2->params != NULL) {
					if(trans->params == NULL)
						if((trans->params = malloc(sizeof(*trans->params))) == NULL)
							return REG_ESPACE;
					int *tp = (int *) trans->params;
					int *tpz = tp + sizeof(*trans->params) / sizeof(trans->params->depth);
					int *pp1 = (int *) p1->params;
					int *pp2 = (int *) p2->params;
					do {
						*tp = ParamUnset;
						if(pp1 != NULL) {
							if(*pp1 != ParamUnset)
								*tp = *pp1;
							++pp1;
							}
						if(pp2 != NULL) {
							if(*pp2 != ParamUnset)
								*tp = *pp2;
							++pp2;
							}
						} while(++tp < tpz);
					}
				else {
					if(trans->params)
						free(trans->params);
					trans->params = NULL;
					}
#if XRE_Debug
				fprintf(stderr, "  pos %2d -> %2d on char(s) ", p1->position, p2->position);
				print_codes_and_tags(p1->code_min, p1->code_max, trans->tags);
				if(trans->assertions)
					fprintf(stderr, ", assert %d", trans->assertions);
				if(trans->assertions & AssertBackref)
					fprintf(stderr, ", backref %d", trans->u.backref);
				else if(trans->u.class)
					DPrintf((stderr, ", class %ld", (long) trans->u.class));
				if(trans->neg_classes)
					fprintf(stderr, ", neg_classes %p", trans->neg_classes);
#if EnableApprox
				if(trans->params) {
					fputs(", ", stderr);
					printParams(true, &trans->params->pa);
					}
#endif
				fputc('\n', stderr);
#endif
				++p2;
				}
			++p1;
			}
	else
		// Compute a maximum limit for the number of transitions leaving from each state.
		while(p1->position >= 0) {
			p2 = orig_p2;
			while(p2->position >= 0) {
				++counts[p1->position];
				++p2;
				}
			++p1;
			}
	return 0;
	}

// Converts an abstract syntax tree to a TNFA.  All the transitions in the TNFA are labelled with one character range (there are
// no transitions on empty strings).  The TNFA takes O(n^2) space in the worst case, where 'n' is size of the RE.
static int makeTNFA(ast_node_t *node, tnfa_transition_t *transitions, int *counts, int *offs) {
	ast_union_t *uni;
	ast_cat_t *cat;
	ast_iter_t *iter;
	int status = 0;

	// XXX - recurse using a stack!
	switch(node->type) {
		case AST_Lit:
			break;
		case AST_Union:
			uni = (ast_union_t *) node->obj;
			if((status = makeTNFA(uni->left, transitions, counts, offs)) != 0)
				return status;
			status = makeTNFA(uni->right, transitions, counts, offs);
			break;
		case AST_Cat:
			cat = (ast_cat_t *) node->obj;

			// Add a transition from each position in cat->left->lastpos to each position in cat->right->firstpos.
			if((status = makeTrans(cat->left->lastpos, cat->right->firstpos, transitions, counts, offs)) !=
			 0 || (status = makeTNFA(cat->left, transitions, counts, offs)) != 0)
				return status;
			status = makeTNFA(cat->right, transitions, counts, offs);
			break;
		case AST_Iter:
			iter = (ast_iter_t *) node->obj;
			assert(iter->max >= -1 && iter->max <= 1);
			if(iter->max == -1) {
				assert(iter->min == 0 || iter->min == 1);

				// Add a transition from each last position in the iterated expression to each first position.
				if((status = makeTrans(iter->sub->lastpos, iter->sub->firstpos, transitions, counts,
				 offs)) != 0)
					return status;
				}
			status = makeTNFA(iter->sub, transitions, counts, offs);
			break;
		}
	return status;
	}

#if XRE_Debug
static void printTrans(tnfa_transition_t *trans) {

	fprintf(stderr, "    %p: state_id %2d, dest state %p for ", (void *) trans, trans->state_id, (void *) trans->state);
	print_codes_and_tags(trans->code_min, trans->code_max, trans->tags);
	fputc('\n', stderr);
	}

static void printTNFA(const tnfa_t *tnfa) {
	tnfa_transition_t *trans, *transz;

	fprintf(stderr, "TNFA: num_transitions %u, initial %p, final %p\n", tnfa->num_transitions,
	 (void *) tnfa->initial, (void *) tnfa->final);
	fputs("  initial:\n", stderr);
	trans = tnfa->initial;
	while(trans->state != NULL) {
		printTrans(trans);
		++trans;
		}
	fputs("  transitions:\n", stderr);
	transz = (trans = tnfa->transitions) + tnfa->num_transitions;
	while(trans < transz) {
		printTrans(trans);
		++trans;
		}
	fprintf(stderr, "  num_states: %d, num_submatches: %u, num_tags: %d, num_minimals: %d, end_tag: %d",
	 tnfa->num_states, tnfa->num_submatches, tnfa->num_tags, tnfa->num_minimals, tnfa->end_tag);
	printTags(", minimal_tags", tnfa->minimal_tags, -1);
	fputc('\n', stderr);
	}
#endif

// Scan tree and return true if any regular expression indicator found in a node, so that PropHaveRegical flag can be set.
static bool scanAST(ast_node_t *ast, int cflags) {

	if(ast->submatch_id > 0)
		return true;
	switch(ast->type) {
		case AST_Lit:
			{ast_lit_t *lit = ast->obj;
			if(lit->code_min < 0 || lit->code_min != lit->code_max)
				return true;
			}
			break;
		case AST_Cat:
			if(scanAST(((ast_cat_t *) ast->obj)->left, cflags) ||
			 scanAST(((ast_cat_t *) ast->obj)->right, cflags))
				return true;
			break;
		case AST_Union:
			if(!(cflags & REG_ICASE))
				return true;
			else {
				ast_lit_t *lit1, *lit2;
				ast_union_t *u = ast->obj;
				if(u->left->type != AST_Lit || u->left->submatch_id > 0 ||
				 u->right->type != AST_Lit || u->right->submatch_id > 0)
					return true;
				lit1 = u->left->obj;
				lit2 = u->right->obj;
				if(!xisupper(lit1->code_min) || lit1->code_max != lit1->code_min ||
				 lit2->code_min != xtolower(lit1->code_min) || lit2->code_max != lit2->code_min)
					return true;
				}
			break;
		default:
			return true;
		}
	return false;
	}

#define ErrorExit(code)\
	{\
	status = code;\
	goto ErrExit;\
	}

// Convert regular expression pattern to an abstract syntax tree (AST), reverse tree if REG_REVERSE flag specified, set up tags,
// optimize tree, then convert AST to a TNFA (an array of transitions).
int compilePat(regex_t *preg, const xchar_t *pat, size_t len, int cflags) {
	xstack_t *stack;
	ast_node_t *tree, *tmp_ast_l, *tmp_ast_r;
	pos_and_tags_t *p;
	int *counts = NULL;
	int *offs = NULL;				// Array of transition offsets for given positions.
	int i, tran_count;
	tnfa_transition_t *transitions, *initial;
	tnfa_t *tnfa = NULL;
	tag_direction_t *tag_directions = NULL;
	int status;
	memhdr_t *mem;
	parse_ctx_t parse_ctx;

#if XRE_Debug
	(void) setlinebuf(stderr);
#endif
	// Allocate a stack which is used throughout the compilation process for various purposes.
	if((stack = xstack_new(512, 10240, 128)) == NULL)
		return REG_ESPACE;

	// Allocate a fast memory allocator.
	if((mem = mem_new()) == NULL) {
		xstack_free(stack);
		return REG_ESPACE;
		}

	preg->cflags = cflags;
	preg->pflags = 0;
	preg->re_data = NULL;

	// Clear parse context object and initialize non-zero values.
	memset(&parse_ctx, 0, sizeof(parse_ctx));
	parse_ctx.mem = mem;
	parse_ctx.stack = stack;
	parse_ctx.re = pat;
	parse_ctx.len = len;
	parse_ctx.keepfirst = true;
	parse_ctx.cflags = cflags;
	parse_ctx.cur_max = XRE_MB_CUR_MAX;

	// Parse the RE string pattern.
	DPrintf((stderr, "=====\ncompilePat: parsing '%.*" StrF "'\n", (int) len, pat));
	if((status = parsePat(&parse_ctx)) != 0)
		goto ErrExit;

	// Back references and approximate matching or pattern reversal cannot currently be used in the same regexp.
	if(parse_ctx.max_backref > 0) {
		if(parse_ctx.pflags & PropHaveApprox)
			ErrorExit(REG_BADPAT);
		if(cflags & REG_REVERSE)
			ErrorExit(REG_EREGREV);
		}

	preg->re_nsub = parse_ctx.submatch_id - 1;
	tree = parse_ctx.rootnode;
#if XRE_Debug
	printAST(tree, "post-parse");
#endif
	// Referring to non-existent subexpressions is an error.
	if(parse_ctx.max_backref > (int) preg->re_nsub)
		ErrorExit(REG_ESUBREG);

	// Parsing succeeded.  Allocate the TNFA structure and initialize it.  Scan tree and set PropHaveRegical property flag
	// if applicable.
	if((tnfa = calloc(1, sizeof(tnfa_t))) == NULL)
		ErrorExit(REG_ESPACE);
	preg->re_data = (void *) tnfa;
	tnfa->num_submatches = parse_ctx.submatch_id;
	tnfa->cflags = cflags;
	preg->pflags = !(parse_ctx.pflags & PropHaveRegical) && scanAST(tree, cflags) ? parse_ctx.pflags | PropHaveRegical :
	 parse_ctx.pflags;
	if(parse_ctx.max_backref > 0)
		preg->pflags |= PropHaveBackref;

#if EnableReverse
	// Reverse tree if REG_REVERSE specified.
	if(cflags & REG_REVERSE) {
		DPrintf((stderr, "=====\ncompilePat: reversing tree\n"));
		revAST(tree);
#if XRE_Debug
		printAST(tree, "post-reverse");
#endif
		}
#endif
	// Set up tags for submatch addressing.  If REG_NOSUB is set and the RE does not have back references,
	// this can be skipped.
	if(!(cflags & REG_NOSUB) || preg->pflags & PropHaveBackref) {
		DPrintf((stderr, "=====\ncompilePat: setting up tags\n"));

		// Figure out how many tags we will need -- addTags(), first pass.
		if((status = addTags(NULL, stack, tree, tnfa)) != 0)
			goto ErrExit;

		// Allocate tag arrays.
		if(tnfa->num_tags > 0) {
			if((tag_directions = malloc(sizeof(*tag_directions) * tnfa->num_tags)) == NULL)
				ErrorExit(REG_ESPACE);
			tnfa->tag_directions = tag_directions;
			memset(tag_directions, -1, sizeof(*tag_directions) * tnfa->num_tags);
			}
		if((tnfa->minimal_tags = calloc(tnfa->num_tags * 2 + 1, sizeof(*tnfa->minimal_tags))) == NULL ||
		 (tnfa->submatch_data = calloc(parse_ctx.submatch_id, sizeof(*tnfa->submatch_data))) == NULL)
			ErrorExit(REG_ESPACE);

		// Create tag nodes in AST -- addTags(), second pass.
		if((status = addTags(mem, stack, tree, tnfa)) != 0)
			goto ErrExit;
#if XRE_Debug
		for(i = 0; i < parse_ctx.submatch_id; ++i)
			fprintf(stderr, "pmatch[%d] = {t%d, t%d}\n", i,
			 tnfa->submatch_data[i].so_tag, tnfa->submatch_data[i].eo_tag);
		for(i = 0; i < tnfa->num_tags; ++i)
			fprintf(stderr, "t%d is %s\n", i, tag_directions[i] == TagMinimize ? "minimized" : "maximized");
		printAST(tree, "post-tags");
#endif
		}

	// Expand iteration nodes.
	if((status = expandAST(mem, stack, tree, &parse_ctx.position, tag_directions, &tnfa->params_depth)) != 0)
		goto ErrExit;

	// Add a dummy node for the final state.
	// XXX - For certain patterns this dummy node can be optimized away, for example "a*" or "ab*".  Figure out a simple way
	// to detect this possibility.
	tmp_ast_l = tree;
	if((tmp_ast_r = ast_newLit(mem, 0, 0, parse_ctx.position++)) == NULL ||
	 (tree = ast_newCat(mem, tmp_ast_l, tmp_ast_r)) == NULL)
		ErrorExit(REG_ESPACE);

	DPrintf((stderr, "Number of states: %d\n", parse_ctx.position));

	// Prepare to convert AST tree to TNFA.
	if((status = computeNFL(mem, stack, tree)) != 0)
		goto ErrExit;
	if((counts = malloc(sizeof(*counts) * parse_ctx.position)) == NULL ||
	 (offs = malloc(sizeof(*offs) * parse_ctx.position)) == NULL)
		ErrorExit(REG_ESPACE);
	memset(counts, 0, sizeof(*counts) * parse_ctx.position);

	// Compute number of transitions needed and allocate array.
	(void) makeTNFA(tree, NULL, counts, NULL);
	tran_count = 0;
	for(i = 0; i < parse_ctx.position; ++i) {
		offs[i] = tran_count;
		tran_count += counts[i] + 1;
		counts[i] = 0;
		}
	if((transitions = calloc((unsigned) tran_count + 1, sizeof(*transitions))) == NULL)
		ErrorExit(REG_ESPACE);
	tnfa->transitions = transitions;
	tnfa->num_transitions = tran_count;

	// Ready... convert tree.
	DPrintf((stderr, "=====\nConverting to TNFA:\n"));
	if((status = makeTNFA(tree, transitions, counts, offs)) != 0)
		goto ErrExit;

	// If in 8-bit mode, compute a table of characters that can be the first character of a match.
	tnfa->first_char = -1;
	if(XRE_MB_CUR_MAX == 1 && !tmp_ast_l->nullable) {
		int count = 0;
		xint_t k, lastk;
		DPrintf((stderr, "Characters that can start a match:"));
		if((tnfa->firstpos_chars = calloc(256, sizeof(char))) == NULL)
			ErrorExit(REG_ESPACE);
		for(p = tree->firstpos; p->position >= 0; ++p) {
			tnfa_transition_t *t = transitions + offs[p->position];
			while(t->state != NULL) {
				for(k = t->code_min; k <= t->code_max && k < 256; ++k) {
					tnfa->firstpos_chars[lastk = k] = 1;
					++count;
					}
				++t;
				}
			}
#if XRE_Debug
		char *s, *sz;
		sz = (s = tnfa->firstpos_chars) + 256;
		do {
			if(*s)
				fprintf(stderr, " %lu", s - tnfa->firstpos_chars);
			} while(++s < sz);
		fputc('\n', stderr);
#endif
		// Check if exactly one character found.
		if(count == 1) {
			DPrintf((stderr, "first char must be %d\n", lastk));
			tnfa->first_char = lastk;
			free(tnfa->firstpos_chars);
			tnfa->firstpos_chars = NULL;
			}
		}
	else
		tnfa->firstpos_chars = NULL;

	p = tree->firstpos;
	i = 0;
	while(p->position >= 0) {
		++i;
#if XRE_Debug
		int *tags;
		fprintf(stderr, "initial: %d/", p->position);
		tags = p->tags;
		if(tags != NULL)
			while(*tags >= 0)
				fprintf(stderr, "%d, ", *tags++);
		fprintf(stderr, "assert %.4x", p->assertions);
#if EnableApprox
		if(p->params) {
			fputs(", ", stderr);
			printParams(true, &p->params->pa);
			}
#endif
		fputc('\n', stderr);
#endif
		++p;
		}

	if((initial = calloc((unsigned) i + 1, sizeof(tnfa_transition_t))) == NULL)
		ErrorExit(REG_ESPACE);
	tnfa->initial = initial;

	i = 0;
	for(p = tree->firstpos; p->position >= 0; ++p) {
		initial[i].state = transitions + offs[p->position];
		initial[i].state_id = p->position;
		initial[i].tags = NULL;

		// Copy the objects p->tags and p->params; they are allocated from a mem object.
		if(p->tags) {
			int j;

			j = termidx(p->tags);
			if((initial[i].tags = malloc(sizeof(*p->tags) * (j + 1))) == NULL)
				ErrorExit(REG_ESPACE);
			memcpy(initial[i].tags, p->tags, sizeof(*p->tags) * (j + 1));
			}
		initial[i].params = NULL;
		if(p->params) {
			if((initial[i].params = malloc(sizeof(*p->params))) == NULL)
				ErrorExit(REG_ESPACE);
			*initial[i].params = *p->params;
			}
		initial[i].assertions = p->assertions;
		++i;
		}
	initial[i].state = NULL;

	tnfa->final = transitions + offs[tree->lastpos[0].position];
	tnfa->num_states = parse_ctx.position;

#if XRE_Debug
	fprintf(stderr, "final state %p\n", (void *) tnfa->final);
	printTNFA(tnfa);
#endif
	mem_free(mem);
	xstack_free(stack);
	free(counts);
	free(offs);
	return 0;
ErrExit:
	// Free everything that was allocated and return the error code.
	mem_free(mem);
	if(stack != NULL)
		xstack_free(stack);
	if(counts != NULL)
		free(counts);
	if(offs != NULL)
		free(offs);
	refree(preg);
	return status;
	}

void refree(regex_t *preg) {
	tnfa_t *tnfa;
	unsigned int i;
	tnfa_transition_t *trans;

	if((tnfa = (tnfa_t *) preg->re_data) == NULL)
		return;

	for(i = 0; i < tnfa->num_transitions; ++i)
		if(tnfa->transitions[i].state != NULL) {
			if(tnfa->transitions[i].tags != NULL)
				free(tnfa->transitions[i].tags);
			if(tnfa->transitions[i].neg_classes != NULL)
				free(tnfa->transitions[i].neg_classes);
			if(tnfa->transitions[i].params != NULL)
				free(tnfa->transitions[i].params);
			}
	if(tnfa->transitions != NULL)
		free(tnfa->transitions);

	if(tnfa->initial != NULL) {
		for(trans = tnfa->initial; trans->state; ++trans) {
			if(trans->tags != NULL)
				free(trans->tags);
			if(trans->params != NULL)
				free(trans->params);
			}
		free(tnfa->initial);
		}

	if(tnfa->submatch_data != NULL) {
		for(i = 0; i < tnfa->num_submatches; ++i)
			if(tnfa->submatch_data[i].parents != NULL)
				free(tnfa->submatch_data[i].parents);
		free(tnfa->submatch_data);
		}

	if(tnfa->tag_directions != NULL)
		free(tnfa->tag_directions);
	if(tnfa->firstpos_chars != NULL)
		free(tnfa->firstpos_chars);
	if(tnfa->minimal_tags != NULL)
		free(tnfa->minimal_tags);
	free(tnfa);
	}

#ifdef __cplusplus
	}
#endif
