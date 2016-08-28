/** @file       code.c
 *  @brief      PL/0 code generation file
 *  @author     Richard Howe (2016)
 *  @license    LGPL v2.1 or Later
 *  @email      howe.r.j.89@gmail.com */

#include "pl0.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static void generate(code_t *c, node_t *n, instruction i)
{
	c->m[c->here++] = i;
	if(c->root)
		c->debug[c->here] = n;
}

static unsigned hole(code_t *c) 
{
	return c->here++;
}

static unsigned newvar(code_t *c)
{
	return c->globals--;
}

static void fix(code_t *c, unsigned hole, unsigned patch)
{
	c->m[hole] = patch;
}

static code_t *new_code(unsigned size, int debug)
{
	assert(size);
	code_t *r = allocate(sizeof(*r)+size*sizeof(r->m[0]));
	if(debug)
		r->debug = allocate(size*sizeof(*r->debug));
	r->size = size;
	r->globals = size - 1; /* data stored at end of core*/
	return r;
}

void free_code(code_t *c)
{
	if(!c)
		return;
	free_node(c->root);
	free(c->debug);
	free(c);
}

static scope_t *new_scope(scope_t *parent)
{
	scope_t *r = allocate(sizeof(*r));
	r->parent = parent;
	return r;
}

static void free_scope(scope_t *s)
{
	if(!s)
		return;
	free(s);
}

static void _code_error(code_t *c, token_t *t, const char *file, const char *func, unsigned line, const char *msg)
{
	fprintf(stderr, "error (%s:%s:%u)\n", file, func, line);
	fprintf(stderr, "identifier '%s' on line %u: %s\n", t->p.id, t->line, msg);
	ethrow(&c->error);
}

#define code_error(CODE, TOKEN, MSG) _code_error((CODE),(TOKEN), __FILE__, __func__, __LINE__, (MSG))

static instruction token2code(code_t *c, token_t *t)
{
	instruction i = IHALT;
	switch(t->type) {
	case LESSEQUAL:    i = ILTE; break;
	case GREATEREQUAL: i = IGTE; break;
	case ODD:          i = IODD; break; /**@todo translate to other instructions */
	case INVERT:       i = IINVERT; break; 
	case NOTEQUAL:     i = INEQ; break;
	case MUL:          i = IMUL; break;
	case SUB:          i = ISUB; break;
	case DIV:          i = IDIV; break;
	case ADD:          i = IADD; break;
	case LESS:         i = ILT;  break;
	case EQUAL:        i = IEQ;  break;
	case GREATER:      i = IGT;  break;
	case AND:          i = IAND; break;
	case OR:           i = IOR;  break;
	case XOR:          i = IXOR; break;
	default:  fprintf(stderr, "invalid conversion");
		  ethrow(&c->error);
	}
	return i;
}

static instruction unary2code(code_t *c, token_t *t)
{
	instruction i = IHALT;
	switch(t->type) {
	case ADD:   i = INOP;    break;
	case SUB:   i = INEGATE; break;
	case ODD:   i = IODD;    break;
	default:  fprintf(stderr, "invalid conversion");
		  ethrow(&c->error);
	}
	return i;
}

static token_t *finder(node_t *n, token_t *t)
{
	if(!n || !n->token)
		return NULL;
	if(!strcmp(n->token->p.id, t->p.id))
		return n->value ? n->value : n->token; /* constant or variable */
	return finder(n->token->procedure ? n->o[1] : n->o[0], t);
}

static token_t *find(scope_t *s, token_t *t)
{
	token_t *found;
	if((found = finder(s->constants, t)))
		return found;
	if((found = finder(s->variables, t)))
		return found;
	if((found = finder(s->functions, t)))
		return found;
	if(s->parent)
		return find(s->parent, t);
	return NULL;
}

static void allocvar(code_t *c, node_t *n, int global)
{
	unsigned v;
	if(!n)
		return;
	v = newvar(c);
	n->token->location = v;
	n->token->located = 1;
	n->token->global = global;
	allocvar(c, n->o[0], global);
}

/** @todo work out how to store variable lookups and scopes */
static void _code(code_t *c, node_t *n, scope_t *parent) 
{
	unsigned hole1, hole2;
	scope_t *current = NULL;
	token_t *found;
	if(!n)
		return;
	if(c->root)
		c->debug[c->here] = n;
	switch(n->type) {
	case PROGRAM: 
		_code(c, n->o[0], NULL); 
		generate(c, n, IHALT); 
		break;
	case BLOCK: 
		current = new_scope(parent);
		_code(c, n->o[0], current); /*constants*/
		_code(c, n->o[1], current); /*variables*/
		if(!parent) {
			generate(c, n, IJMP);
			hole1 = hole(c);
		}
		_code(c, n->o[2], current); /*procedures*/
		if(!parent)
			fix(c, hole1, c->here);
		_code(c, n->o[3], current); /*final statement*/
		free_scope(current);
		break;
	case CONSTLIST: 
		parent->constants = n; 
		break;
	case VARLIST:   
		parent->variables = n; 
		/**@note allocate all vars to globals for now, although
		 * they will be marked correctly as locals */
		allocvar(c, n, parent->parent == NULL); 
		break;
	case PROCLIST: 
		if(!parent->functions)
			parent->functions = n;
		parent->current = n;
		n->token->location = c->here;
		n->token->located = 1;
		_code(c, n->o[0], parent);
		generate(c, n, IRETURN);
		_code(c, n->o[1], parent);
		break;
	case STATEMENT:  /*do nothing, empty statement*/
		break;
	case ASSIGNMENT:
		_code(c, n->o[0], parent);
		if(!(found = find(parent, n->token)))
			code_error(c, n->token, "variable not found"); 
		if(found->procedure || found->constant)
			code_error(c, n->token, "not a variable");
		generate(c, n, ISTORE);
		generate(c, n, found->location);
		break;
	case INVOKE:
		if(!(found = find(parent, n->token))) 
			code_error(c, n->token, "function not found");
		if(!found->procedure)
			code_error(c, n->token, "variable is not a procedure");
		if(!found->located)
			code_error(c, n->token, "forward references not allowed");
		generate(c, n, ICALL); 
		generate(c, n, found->location);
		break;
	case OUTPUT:  
		_code(c, n->o[0], parent); 
		generate(c, n, IWRITE); 
		break;
	case INPUT:       
		if(!(found = find(parent, n->token)))
			code_error(c, n->token, "variable not found");
		if(found->procedure || found->constant)
			code_error(c, n->token, "not a variable");
		generate(c, n, IREAD); 
		generate(c, n, found->location); 
		break;
	case CONDITIONAL: 
		_code(c, n->o[0], parent); 
		generate(c, n, IJZ); 
		hole1 = hole(c);
		_code(c, n->o[1], parent); 
		if(n->o[2]) { /* if ... then ... else */
			generate(c, n, IJMP);
			hole2 = hole(c);
			fix(c, hole1, c->here);
			_code(c, n->o[2], parent);
			fix(c, hole2, c->here);
		} else { /* if ... then */
			fix(c, hole1, c->here); 
		}
		break;
	case CONDITION:   
		if(n->token && n->token->type == ODD) {
			_code(c, n->o[0], parent);
			generate(c, n, IODD);
		} else {
			_code(c, n->o[0], parent);
			_code(c, n->o[1], parent);
			generate(c, n, token2code(c, n->token));
		}
		break;
	case WHILST:      
		hole1 = c->here;
		_code(c, n->o[0], parent);
		generate(c, n, IJZ);
		hole2 = hole(c);
		_code(c, n->o[1], parent);
		generate(c, n, IJMP);
		fix(c, hole(c), hole1);
		fix(c, hole2, c->here);
		break;
	case DOOP:
		hole1 = c->here;
		_code(c, n->o[0], parent);
		_code(c, n->o[1], parent);
		generate(c, n, IJNZ);
		fix(c, hole(c), hole1);
		break;
	case LIST:        
		_code(c, n->o[0], parent); 
		_code(c, n->o[1], parent); 
		break;
	case UNARY_EXPRESSION:
		/**@todo unary expression not handled correctly */
		_code(c, n->o[0], parent);
		_code(c, n->o[1], parent);
		if(n->token) {
			instruction i = unary2code(c, n->token);
			if(i != INOP)
				generate(c, n, i);
		}
		break;
	case EXPRESSION: 
		_code(c, n->o[0], parent);
		generate(c, n, token2code(c, n->token));
		break;
	case TERM:
		 _code(c, n->o[0], parent);
		 _code(c, n->o[1], parent);
		 if(n->token)
			generate(c, n, token2code(c, n->token));
		 break;
	case FACTOR:
		if(!n->token) {
			_code(c, n->o[0], parent);
			return;
		}
		if(n->token->type == NUMBER) {
			generate(c, n, IPUSH);
			generate(c, n, n->token->p.number);
		} else if((found = find(parent, n->token))) {
			if(found->procedure)
				code_error(c, n->token, "not a variable or constant");
			if(found->type == NUMBER) { /* find returns constants value */
				generate(c, n, IPUSH);
				generate(c, n, found->p.number);
			} else {
				assert(found->type == IDENTIFIER);
				generate(c, n, ILOAD); 
				generate(c, n, found->location);
			}
		} else {
			code_error(c, n->token, "variable not found");
		}
		break;
	}
}

code_t *code(node_t *n, size_t size, int debug)
{
	code_t *c = new_code(size, debug);
	if(debug)
		c->root = n;
	c->error.jmp_buf_valid = 1;
	if(setjmp(c->error.j)) {
		free_code(c);
		return NULL;
	}
	_code(c, n, NULL);
	return c;
}

static void scope(scope_t *s, FILE *output)
{
	if(!s)
		return;
	if(s->current && s->current->token)
		fprintf(output, "%s.", s->current->token->p.id);
	scope(s->parent, output);
}

static void printsym(node_t *n, scope_t *parent, FILE *output)
{
	int isprocedure = n->token->procedure;
	fprintf(output, "%03x: %s ", n->token->location, isprocedure ? "func" : "var ");
	scope(isprocedure ? parent->parent : parent, output);
	fprintf(output, "%s\n", n->token->p.id);
}

static void _export(node_t *n, scope_t *parent, FILE *output) 
{
	node_t *x;
	scope_t *current = NULL;
	if(!n)
		return;
	switch(n->type) {
	case PROGRAM: 
		_export(n->o[0], NULL, output); break;
	case BLOCK: 
		current = new_scope(parent);
		_export(n->o[1], current, output); /*variables*/
		_export(n->o[2], current, output); /*procedures*/
		free_scope(current);
		break;
	case VARLIST:   
		parent->variables = n; 
		for(x = n; x; x = x->o[0])
			printsym(x, parent, output);
		break;
	case PROCLIST:  
		if(!parent->functions)
			parent->functions = n;
		parent->current = n;
		printsym(n, parent, output);
		_export(n->o[0], parent, output);
		_export(n->o[1], parent, output);
		break;
	default:
		return;
	}
}

void export(node_t *n, FILE *output)
{ /* export a list of symbols */
	_export(n, NULL, output);
}


