/* see https://en.wikipedia.org/wiki/PL/0 
 *
 * @todo Replace all instances of exit() with a longjmp to interpreter bounds. 
 * @todo Stack frames need added and dealing with
 * @todo Sanity checking (check for redefinitions)
 * @todo Attempt to detect multiple errors (error recovery in parser)
 * @todo Add in assertions, make unit tests
 * @todo Dump symbol table
 * @todo Check that symbols and code to not overwrite each other
 * @todo Virtual machine should be character aligned, push and pop should
 *       also take an amount to push/pop by.
 *
 * The following needs to be added to the language:
 * 	- Function arguments, return values (possibly with multiple return
 * 	values) for nested procedures.
 * 	- Arrays
 * 	- Modules
 *              program = block ("." | EOI ) .
 * 	- Pre and post conditions for functions
 * 	- Multiple return values for functions
 *
 * See: https://www.cs.swarthmore.edu/~newhall/cs75/s05/proj3/proj3.html#intro 
 * for information about stack frames and allocation */
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

#define MAX_ID_LENGTH (256u)
#define MAX_CORE      (1024u)
#define MAX_STACK     (512u)
#define VERSION       (1)

/* EBNF Grammar (from Wikipedia)
 *
 * This grammar needs modifying so procedures can take arguments and calls
 * can be used in expressions. Also the current definition of block allows
 * nested procedures.
 *
 * Variables should also have a default value that can be used, optionally.
 *
 * @todo Add comments, arrays, procedure arguments and return values, etcetera.
 *
 *    program = block ("." | EOI ) .
 *    
 *    block = [ constlist ]
 *            [ varlist ]
 *            { "procedure" ident ";" block ";" } statement .
 *
 *    constlist  = "const" ident "=" number {"," ident "=" number} ";"
 *    varlist    = "var" ident {"," ident} ";"
 *
 *    statement  = [ assignment | invoke | input | output | list | conditional | whilst | doop ].
 *    assignment = ident ":=" unary-expression .
 *    invoke     = "call" ident .
 *    input      = "read" ident .
 *    output     = "write" unary-expression .
 *    list       = "begin" statement {";" statement } "end" .
 *    conditional = "if" condition "then" statement [ "else" statement ] .
 *    whilst     = "while" condition "do" statement .
 *    doop       = "do" statement "while" condition .
 *    
 *    condition = "odd" unary-expression |
 *                unary-expression ("="|"#"|"<"|"<="|">"|">=") unary-expression .
 *
 *    expression = { ("+"|"-") term}.
 *    
 *    unary-expression = ["+"|"-"] term expression.
 *    
 *    term = factor {("*"|"/") factor}.
 *    
 *    factor = ident | number | "(" unary-expression ")". */

typedef enum 
{
	/*these need to go first, in this order*/
	CONST,		/* const */
	VAR,		/* var */
	PROCEDURE,	/* procedure */
	ASSIGN,         /* := */
	CALL,		/* call */
	BEGIN,          /* begin */
	END,		/* end */
	IF,		/* if */
	THEN,		/* then */
	ELSE,           /* else */
	WHILE,		/* while */
	DO,		/* do */
	LESSEQUAL,      /* <= */
	GREATEREQUAL,   /* >= */
	ODD,            /* odd */
	WRITE,          /* write */
	READ,           /* read */
	LAST_KEY_WORD,  /* not an actual token */

	ERROR,          /* invalid token, not an actual token */
	IDENTIFIER,     /* [a-Z_][0-9a-Z]* */
	NUMBER,         /* [0-9]+ */

	FIRST_SINGLE_CHAR_TOKEN, /* marker, not a token */
	/* @warning these need to be in ASCII order */
	NOTEQUAL     =  '#',
	LPAR         =  '(',
	RPAR         =  ')',
	MUL          =  '*',
	SUB          =  '-',
	DIV          =  '/',
	ADD          =  '+',
	COMMA        =  ',',
	DOT          =  '.',
	SEMICOLON    =  ';',
	LESS         =  '<',
	EQUAL        =  '=',
	GREATER      =  '>',
	LAST_SINGLE_CHAR_TOKEN, /* marker, not a token */
	EOI          =  EOF
} token_e;

static const char *keywords[] = 
{
	[CONST]         =  "const",
	[VAR]           =  "var",
	[PROCEDURE]     =  "procedure",
	[ASSIGN]        =  ":=",
	[CALL]          =  "call",
	[BEGIN]         =  "begin",
	[END]           =  "end",
	[IF]            =  "if",
	[THEN]          =  "then",
	[ELSE]          =  "else",
	[WHILE]         =  "while",
	[DO]            =  "do",
	[LESSEQUAL]     =  "<=",
	[GREATEREQUAL]  =  ">=",
	[ODD]           =  "odd",
	[WRITE]         =  "write",
	[READ]          =  "read",
	NULL
};

typedef struct {
	int error;
	int jmp_buf_valid;
	jmp_buf j;
} error_t;

typedef struct {
	union {
		char *id;
		int number;
	} p;
	token_e  type;
	unsigned location; /* location in code (for variables, functions) */
	unsigned line;     /* line token encountered */
	unsigned global    :1,  /* global data */
		 constant  :1,  /* constant variable */
		 procedure :1,  /* is a procedure */
		 located   :1;  /* has this function been emitted as code already? */
} token_t;

typedef struct {
	FILE *input;
	unsigned line;
	int c;
	char id[MAX_ID_LENGTH];
	int number;
	int debug;
	token_t *token;
	token_t *accepted;
	error_t error;
} lexer_t;

void *allocate(size_t sz)
{
	errno = 0;
	void *r = calloc(sz, 1);
	if(!r) {
		fprintf(stderr, "allocation failed: %s", errno ? strerror(errno) : "unknown reason");
		exit(EXIT_FAILURE);
	}
	return r;
}

char *duplicate(const char *str)
{
	errno = 0;
	char *r = malloc(strlen(str)+1);
	if(!r) {
		fprintf(stderr, "duplicate failed: %s", errno ? strerror(errno) : "unknown reason");
		exit(EXIT_FAILURE);
	}
	strcpy(r, str);
	return r;
}

error_t *new_error(void)
{
	return allocate(sizeof(error_t));
}

static void indent(FILE *output, char c, unsigned i)
{
	while(i--)
		fputc(c, output);
}

static void print_token_enum(FILE *output, token_e type)
{
	if(type == NUMBER)
		fputs("number\n", output);
	else if(type == IDENTIFIER)
		fputs("identifier\n", output);
	else if(type >= 0 && type < LAST_KEY_WORD)
		fprintf(output, "key-word(%s)\n", keywords[type]);
	else if(type > FIRST_SINGLE_CHAR_TOKEN && type < LAST_SINGLE_CHAR_TOKEN)
		fprintf(output, "token(%c)\n", type);
	else if(type == EOI)
		fputs("EOF\n", output);
	else if(type == ERROR)
		fputs("error token\n", output);
	else
		fprintf(output, "invalid-token(%d)\n", type);
}

static void print_token(FILE *output, token_t *t, unsigned depth)
{
	if(!t)
		return;
	indent(output, ' ', depth);
	if(t->type == NUMBER)
		fprintf(output, "number(%d)\n", t->p.number);
	else if(t->type == IDENTIFIER)
		fprintf(output, "id(%s)\n", t->p.id);
	else
		print_token_enum(output, t->type);
}

void ethrow(error_t *e)
{
	if(e && e->jmp_buf_valid) {
		e->jmp_buf_valid = 0;
		e->error = 1;
		longjmp(e->j, 1);
	}
	exit(EXIT_FAILURE);
}

static int _syntax_error(lexer_t *l, const char *file, const char *func, unsigned line, const char *msg)
{
	fprintf(stderr, "%s:%s:%u\n", file, func, line);
	fprintf(stderr, "  syntax error on line %u of input\n  %s\n", l->line, msg);
	print_token(stderr, l->token, 2);
	ethrow(&l->error);
	return 0;
}

#define syntax_error(L, MSG) _syntax_error((L), __FILE__, __func__, __LINE__, (MSG))

static token_t *new_token(token_e type, unsigned line)
{
	token_t *r = allocate(sizeof(*r));
	r->type = type;
	r->line = line;
	return r;
}

static void free_token(token_t *t)
{
	if(!t)
		return;
	if(t->type == IDENTIFIER)
		free(t->p.id);
	free(t);
}

static lexer_t *new_lexer(FILE *input, int debug)
{
	lexer_t *r = allocate(sizeof(*r));
	r->input = input;
	r->line = 1;
	r->debug = debug;
	return r;
}

void free_lexer(lexer_t *l)
{
	free_token(l->accepted);
	free(l);
}

static int next_char(lexer_t *l)
{
	return fgetc(l->input);
}

static int unget_char(lexer_t *l, int c)
{
	return ungetc(c, l->input);
}

/* process a comment for the lexer, chout is filled with the next 
   character the lexer needs, this function does not return on
   a syntax error */ 
static void comment(lexer_t *l, int *chout)
{ 	
	for(;;) {
		int ch = next_char(l);
		if(ch == '*') {
			ch = next_char(l);
			if(ch == ')') {
				*chout = next_char(l);
				return;
			} else if(ch == EOF) {
				break;
			}
		} else if(ch == EOF) {
			break;
		}
	}
	syntax_error(l, "comment terminated by EOF");
}

static void lexer(lexer_t *l)
{
	int ch = next_char(l);
	l->token = new_token(ERROR, l->line);
again:  switch(ch) {
	case '\n': l->token->line = l->line++;
	case '\t':
	case ' ': ch = next_char(l);
		  goto again;
	case DOT: l->token->type = DOT; return;
	case EOF: l->token->type = EOI; return;
	case COMMA:    case SEMICOLON:   case RPAR:     
	case ADD:      case SUB:         case MUL:      
	case DIV:      case EQUAL:       case NOTEQUAL:
		l->token->type = ch;
		return;
	/* >, >=, <, <=, (, (* and := are special case for now */
	case LPAR:
		ch = next_char(l);
		if(ch == '*') {
			comment(l, &ch);
			goto again;
		} else {
			unget_char(l, ch);
			l->token->type = LPAR;
		}
		return;
	case LESS:
		ch = next_char(l);
		if(ch == '=') {
			l->token->type = LESSEQUAL;
		} else {
			unget_char(l, ch);
			l->token->type = LESS;
		}
		return;
	case GREATER:
		ch = next_char(l);
		if(ch == '=') {
			l->token->type = GREATEREQUAL;
		} else {
			unget_char(l, ch);
			l->token->type = GREATER;
		}
		return;
	case ':':
		ch = next_char(l);
		if(ch != '=')
			syntax_error(l, "expected '=' after ':'");
		l->token->type = ASSIGN;
		return;
	default:
		if(isdigit(ch)) {
			int i = 0;
			while(isdigit(ch)) {
				i = i * 10 + (ch - '0');
				ch = next_char(l);
			}
			l->token->type = NUMBER;
			l->token->p.number = i;
			break;
		}
		if(isalpha(ch)) {
			unsigned i = 0, sym = 0;
			while(isalnum(ch)) {
				l->id[i++] = ch;
				ch = next_char(l);
			}
			l->id[i] = '\0';
			for(sym = CONST; keywords[sym] && strcmp(keywords[sym], l->id); sym++)
				/*do nothing*/;
			if(!keywords[sym]) {
				l->token->type = IDENTIFIER;
				l->token->p.id = duplicate(l->id);
			} else {
				l->token->type = sym;
			}
			break;
		}
		fprintf(stderr, "token: %s (%d)\n", l->id, ch);
		syntax_error(l, "invalid token");
	}
	unget_char(l, ch);
}

typedef enum {
	PROGRAM, 
	BLOCK, 
		CONSTLIST,
		VARLIST,
		PROCLIST,
	STATEMENT,
		ASSIGNMENT,
		INVOKE,
		OUTPUT,
		INPUT,
		CONDITIONAL,
		WHILST,
		DOOP,
		LIST,
	CONDITION, 
	EXPRESSION,
	UNARY_EXPRESSION, 
	TERM, 
	FACTOR,
} parse_e;

static const char *names[] = {
	[PROGRAM]      =  "program",
	[BLOCK]        =  "block",
	[STATEMENT]    =  "statement",
	[CONSTLIST]    =  "constants",
	[VARLIST]      =  "variables",
	[PROCLIST]     =  "procedures",
	[ASSIGNMENT]   =  "assignment",
	[INVOKE]       =  "invocation",
	[OUTPUT]       =  "output",
	[INPUT]        =  "input",
	[CONDITIONAL]  =  "conditional",
	[WHILST]       =  "whilst",
	[DOOP]         =  "do",
	[LIST]         =  "list",
	[CONDITION]    =  "condition",
	[EXPRESSION]   =  "expression",
	[UNARY_EXPRESSION]   =  "unary-expression",
	[TERM]         =  "term",
	[FACTOR]       =  "factor",
};

typedef struct node_t  {
	unsigned type   :8, /* of parse_e type */
		 length :8;
	token_t *token, *value;
	struct node_t *o[];
} node_t;

node_t *new_node(lexer_t *l, parse_e type, unsigned char size)
{
	node_t *r = allocate(sizeof(*r) + sizeof(r->o[0])*size);
	if(l->debug)
		fprintf(stderr, "new> %s\n", names[type]);
	r->length = size;
	r->type = type;
	return r;
}

void free_node(node_t *n)
{
	if(!n)
		return;
	for(unsigned i = 0; i < n->length; i++)
		free_node(n->o[i]);
	free_token(n->token);
	free(n);
}

void print_node(FILE *output, node_t *n, unsigned depth)
{
	if(!n)
		return;
	indent(output, ' ', depth); 
	fprintf(output, "node(%d): %s\n", n->type, names[n->type]);
	print_token(output, n->token, depth);
	print_token(output, n->value, depth);
	for(size_t i = 0; i < n->length; i++)
		print_node(output, n->o[i], depth+1);
}

int accept(lexer_t *l, token_e sym)
{
	if(sym == l->token->type) {
		free_token(l->accepted); /* free token owned by lexer */
		l->accepted = l->token;
		if(sym != EOI && sym != DOT)
			lexer(l);
		return 1;
	}
	return 0;
}

static void use(lexer_t *l, node_t *n)
{ /* move ownership of token from lexer to parse tree */
	n->token = l->accepted;
	l->accepted = NULL;
}

static int _expect(lexer_t *l, token_e sym, const char *file, const char *func, unsigned line)
{
	if(accept(l, sym))
		return 1;
	fprintf(stderr, "%s:%s:%u\n", file, func, line);
	fprintf(stderr, "  Syntax error: unexpected token\n  Got:          ");
	print_token(stderr, l->token, 0);
	fputs("  Expected:     ", stderr);
	print_token_enum(stderr, sym);
	fprintf(stderr, "On line: %u\n", l->line);
	ethrow(&l->error);
	return 0;
}

#define expect(L, SYM) _expect((L), (SYM), __FILE__, __func__, __LINE__)

static node_t *unary_expression(lexer_t *l);

static node_t *factor(lexer_t *l) /* ident | number | "(" unary-expression ")". */
{
	node_t *r = new_node(l, FACTOR, 1);
	if(accept(l, IDENTIFIER) || accept(l, NUMBER)) { 
		use(l, r);
		return r;
	} else if(accept(l, LPAR)) {
		r->o[0] = unary_expression(l);
		expect(l, RPAR);
	} else {
		syntax_error(l, "expected id, number or \"(\" unary-expression \")\"");
	}
	return r;
}

static node_t *term(lexer_t *l) /* factor {("*"|"/") factor}. */
{
	node_t *r = new_node(l, TERM, 2);
	r->o[0] = factor(l);
	if(accept(l, MUL) || accept(l, DIV)) {
		use(l, r);
		r->o[1] = factor(l);
	}
	return r;
}

static node_t *expression(lexer_t *l) /* { ("+"|"-") term}. */
{
	if(accept(l, ADD) || accept(l, SUB)) {
		node_t *r = new_node(l, EXPRESSION, 1);
		use(l, r);
		r->o[0] = term(l);
		return r;
	} else {
		return NULL;
	}
}

static node_t *unary_expression(lexer_t *l) /* [ "+"|"-"] term expression. */
{
	node_t *r = new_node(l, UNARY_EXPRESSION, 2);
	if(accept(l, ADD) || accept(l, SUB))
		use(l, r);
	r->o[0] = term(l);
	r->o[1] = expression(l);
	return r;
}

static node_t *condition(lexer_t *l)
{
	node_t *r = new_node(l, CONDITION, 2);
	if(accept(l, ODD)) { /* "odd" unary_expression */
		use(l, r);
		r->o[0] = unary_expression(l);
	} else { /* unary_expression ("="|"#"|"<"|"<="|">"|">=") unary_expression*/
		r->o[0] = unary_expression(l);
		if(accept(l, EQUAL) || accept(l, GREATER) || accept(l, LESS) 
		|| accept(l, LESSEQUAL) || accept(l, GREATEREQUAL) || accept(l, NOTEQUAL)) { 
			use(l, r);
			r->o[1] = unary_expression(l);
		} else {
			syntax_error(l, "expected condition statement");
		}
	}
	return r;
}

static node_t *statement(lexer_t *l);

static node_t *list(lexer_t *l)
{
	node_t *r = new_node(l, LIST, 2);
	r->o[0] = statement(l);
	if(accept(l, SEMICOLON))
		r->o[1] = list(l);
	return r;
}

static node_t *statement(lexer_t *l)
{
	node_t *r = new_node(l, STATEMENT, 3);
	if(accept(l, IDENTIFIER)) { /* ident ":=" unary_expression */
		use(l, r);
		expect(l, ASSIGN);
		r->o[0] = unary_expression(l);
		r->type = ASSIGNMENT;
	} else if(accept(l, CALL)) { /* "call" ident  */
		expect(l, IDENTIFIER);
		use(l, r);
		r->type = INVOKE;
	} else if(accept(l, READ)) { /* "read" ident */
		expect(l, IDENTIFIER);
		use(l, r);
		r->type = INPUT;
	} else if(accept(l, WRITE)) { /* "write" expression */
		r->o[0] = unary_expression(l);
		r->type = OUTPUT;
	} else if(accept(l, BEGIN)) { /* "begin" statement {";" statement } "end" */
		r->o[0] = list(l);
		expect(l, END);
		r->type = LIST;
	} else if(accept(l, IF)) { /* "if" condition "then" statement [ "else" statement ] */
		r->o[0] = condition(l);
		expect(l, THEN);
		r->o[1] = statement(l);
		if(accept(l, ELSE))
			r->o[2] = statement(l);
		r->type = CONDITIONAL;
	} else if(accept(l, WHILE)) { /*  "while" condition "do" statement */
		r->o[0] = condition(l);
		expect(l, DO);
		r->o[1] = statement(l);
		r->type = WHILST;
	} else if(accept(l, DO)) {
		r->o[0] = statement(l);
		expect(l, WHILE);
		r->o[1] = condition(l);
		r->type = DOOP;
	} else {
		/* statement is optional */
	}
	return r;
}

static node_t *constlist(lexer_t *l) /* "const" ident "=" number {"," ident "=" number} ";" */
{
	node_t *r = new_node(l, CONSTLIST, 1);
	expect(l, IDENTIFIER);
	use(l, r);
	r->token->constant = 1;
	expect(l, EQUAL);
	expect(l, NUMBER);
	r->value = l->accepted;
	r->value->constant = 1;
	l->accepted = NULL;
	if(accept(l, COMMA))
		r->o[0] = constlist(l);
	return r;
}

static node_t *varlist(lexer_t *l) /* "var" ident {"," ident} ";"  */
{
	node_t *r = new_node(l, VARLIST, 1);
	expect(l, IDENTIFIER);
	use(l, r);
	if(accept(l, COMMA))
		r->o[0] = varlist(l);
	return r;
}

static node_t *block(lexer_t *l);

static node_t *proclist(lexer_t *l) /* ident ";" block ";" procedure */
{
	node_t *r = new_node(l, PROCLIST, 2);
	expect(l, IDENTIFIER);
	use(l, r);
	r->token->procedure = 1;
	expect(l, SEMICOLON);
	r->o[0] = block(l);
	expect(l, SEMICOLON);
	if(accept(l, PROCEDURE))
		r->o[1] = proclist(l);
	return r;
}

static node_t *block(lexer_t *l)
{
	node_t *r = new_node(l, BLOCK, 4);
	if(accept(l, CONST)) { /* [ constlist ] */
		r->o[0] = constlist(l);
		expect(l, SEMICOLON);
	}
	if(accept(l, VAR)) { /* [ varlist ] */
		r->o[1] = varlist(l);
		expect(l, SEMICOLON);
	}
	if(accept(l, PROCEDURE)) /* "procedure" proclist */
		r->o[2] = proclist(l);
	r->o[3] = statement(l); /* statement */
	return r;
}

static node_t *program(lexer_t *l)
{
	node_t *r = new_node(l, PROGRAM, 1);
	lexer(l);
	r->o[0] = block(l);
	if(accept(l, EOI))
		return r;
	expect(l, DOT);
	return r;
}

node_t *parse(FILE *input, int debug)
{
	lexer_t *l = new_lexer(input, debug);
	l->error.jmp_buf_valid = 1;
	if(setjmp(l->error.j)) {
		free_lexer(l);
		/** @warning leaks parsed nodes */
		return NULL;
	}
	node_t *n = program(l);
	free_lexer(l);
	return n;
}

typedef struct {
	unsigned here;
	unsigned globals;
	unsigned size;
	error_t error;
	intptr_t m[];
} code_t;

typedef struct scope_t {
	node_t *constants;
	node_t *variables;
	node_t *functions;
	node_t *this;
	struct scope_t *parent;
} scope_t;

typedef enum {
	ILOAD, ISTORE, ICALL, IRETURN, IJ, IJZ, IJNZ, IADD, ISUB, IMUL, IDIV,
	ILTE, IGTE, ILT, IGT, IEQ, INEQ, IODD, IPUSH, IPOP, IWRITE, IREAD, IHALT
} instruction;

static const char *inames[] = {
	[ILOAD]     =  "load",
	[ISTORE]    =  "store",
	[ICALL]     =  "call",
	[IRETURN]   =  "return",
	[IJ]        =  "j",
	[IJZ]       =  "jz",
	[IJNZ]      =  "jnz",
	[IADD]      =  "+",
	[ISUB]      =  "-",
	[IMUL]      =  "*",
	[IDIV]      =  "/",
	[ILTE]      =  "<=",
	[IGTE]      =  ">=",
	[ILT]       =  "<",
	[IGT]       =  ">",
	[IEQ]       =  "=",
	[INEQ]      =  "#",
	[IODD]      =  "odd",
	[IPUSH]     =  "push",
	[IPOP]      =  "pop",
	[IWRITE]    =  "write",
	[IREAD]     =  "read",
	[IHALT]     =  "halt",
};

static void generate(code_t *c, instruction i)
{
	c->m[c->here++] = i;
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

static code_t *new_code(unsigned size)
{
	assert(size);
	code_t *r = allocate(sizeof(*r)+size*sizeof(r->m[0]));
	r->size = size;
	r->globals = size - 1; /* data stored at end of core*/
	return r;
}

void free_code(code_t *c)
{
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
	case NOTEQUAL:     i = INEQ; break;
	case MUL:          i = IMUL; break;
	case SUB:          i = ISUB; break;
	case DIV:          i = IDIV; break;
	case ADD:          i = IADD; break;
	case LESS:         i = ILT;  break;
	case EQUAL:        i = IEQ;  break;
	case GREATER:      i = IGT;  break;
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
	switch(n->type) {
	case PROGRAM: 
		_code(c, n->o[0], NULL); 
		generate(c, IHALT); 
		break;
	case BLOCK: 
		current = new_scope(parent);
		_code(c, n->o[0], current); /*constants*/
		_code(c, n->o[1], current); /*variables*/
		if(!parent) {
			generate(c, IJ);
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
		parent->this = n;
		n->token->location = c->here;
		n->token->located = 1;
		_code(c, n->o[0], parent);
		generate(c, IRETURN);
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
		generate(c, ISTORE);
		generate(c, found->location);
		break;
	case INVOKE:
		if(!(found = find(parent, n->token))) 
			code_error(c, n->token, "function not found");
		if(!found->procedure)
			code_error(c, n->token, "variable is not a procedure");
		if(!found->located)
			code_error(c, n->token, "forward references not allowed");
		generate(c, ICALL); 
		generate(c, found->location);
		break;
	case OUTPUT:  
		_code(c, n->o[0], parent); 
		generate(c, IWRITE); 
		break;
	case INPUT:       
		if(!(found = find(parent, n->token)))
			code_error(c, n->token, "variable not found");
		if(found->procedure || found->constant)
			code_error(c, n->token, "not a variable");
		generate(c, IREAD); generate(c, found->location); 
		break;
	case CONDITIONAL: 
		_code(c, n->o[0], parent); 
		generate(c, IJZ); 
		hole1 = hole(c);
		_code(c, n->o[1], parent); 
		if(n->o[2]) { /* if ... then ... else */
			generate(c, IJ);
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
			generate(c, IODD);
		} else {
			_code(c, n->o[0], parent);
			_code(c, n->o[1], parent);
			generate(c, token2code(c, n->token));
		}
		break;
	case WHILST:      
		hole1 = c->here;
		_code(c, n->o[0], parent);
		generate(c, IJZ);
		hole2 = hole(c);
		_code(c, n->o[1], parent);
		generate(c, IJ);
		fix(c, hole(c), hole1);
		fix(c, hole2, c->here);
		break;
	case DOOP:
		hole1 = c->here;
		_code(c, n->o[0], parent);
		_code(c, n->o[1], parent);
		generate(c, IJNZ);
		fix(c, hole(c), hole1);
		break;
	case LIST:        
		_code(c, n->o[0], parent); 
		_code(c, n->o[1], parent); 
		break;
	case UNARY_EXPRESSION:
		_code(c, n->o[0], parent);
		_code(c, n->o[1], parent);
		if(n->token)
			generate(c, token2code(c, n->token));
		break;
	case EXPRESSION: 
		_code(c, n->o[0], parent);
		generate(c, token2code(c, n->token));
		break;
	case TERM:
		 _code(c, n->o[0], parent);
		 _code(c, n->o[1], parent);
		 if(n->token)
			generate(c, token2code(c, n->token));
		 break;
	case FACTOR:
		if(!n->token) {
			_code(c, n->o[0], parent);
			return;
		}
		if(n->token->type == NUMBER) {
			generate(c, IPUSH);
			generate(c, n->token->p.number);
		} else if((found = find(parent, n->token))) {
			if(found->procedure)
				code_error(c, n->token, "not a variable or constant");
			if(found->type == NUMBER) { /* find returns constants value */
				generate(c, IPUSH);
				generate(c, found->p.number);
			} else {
				assert(found->type == IDENTIFIER);
				generate(c, ILOAD); 
				generate(c, found->location);
			}
		} else {
			code_error(c, n->token, "variable not found");
		}
		break;
	}
}

code_t *code(node_t *n, size_t size)
{
	code_t *c = new_code(size);
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
	if(s->this && s->this->token)
		fprintf(output, "%s.", s->this->token->p.id);
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
		parent->this = n;
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

static int idump(code_t *c, FILE *output, unsigned i)
{ /* print out an instruction and any of its operands */
	instruction op = c->m[i];
	fprintf(output, "%03x: %03x %s\n", i, op, op <= IHALT ? inames[op] : "invalid op");
	if(op == ILOAD || op == ISTORE || op == ICALL || op == IJ || op == IJZ || op == IPUSH || op == IREAD) {
		i++;
		fprintf(output, "%03x: %03"PRIxPTR" data\n", i, c->m[i]);
	}
	return i;
}

void dump(code_t *c, FILE *output)
{ 
	unsigned i;
	fputs("disassembly:\n", output);
	for(i = 0; i < c->here; i++)
		i = idump(c, output, i);
	fputs("symbols defined:\n", output);
	for(i = c->size - 1; i > c->globals; i--) /**@todo lookup variable names */
		fprintf(output, "%03x: %"PRIiPTR"\n", i, c->m[i]);
}

int vm(code_t *c, FILE *input, FILE *output, int debug)
{
	intptr_t stack[MAX_STACK] = { 0 }, 
		 *S = stack, /* variable stack pointer */
		 *pc = &c->m[0], 
		 *m = c->m,
		 f = 0,
		 op;
	for(;;) {
		if(debug)
			idump(c, output, (unsigned)(pc - m));
		switch(op = *pc++) {
		case ILOAD:   *++S = f; f = m[*pc++];  break;
		case ISTORE:  m[*pc++] = f; f = *S--;  break;
		case ICALL:   *++S = f;
			      f = (intptr_t)(pc+1); 
			      pc = m+*pc;              break;
		case IRETURN: pc = (intptr_t*)f; f = *S--; break;
		case IJ:      pc = m+*pc;              break;
		case IJZ:     if(!f) pc = m+*pc; else pc++; f = *S--; break;
		case IJNZ:    if( f) pc = m+*pc; else pc++; f = *S--; break;
		case IADD:    f = *S-- +  f;           break;
		case ISUB:    f = *S-- -  f;           break;
		case IMUL:    f = *S-- *  f;           break;
		case IDIV:    if(!f) {
				      fprintf(stderr, "divide by zero!\n");
				      return -1;
			      }
			      f = *S-- /  f;           break;
		case ILTE:    f = *S-- <= f;           break;
		case IGTE:    f = *S-- >= f;           break;
		case ILT:     f = *S-- <  f;           break;
		case IGT:     f = *S-- >  f;           break;
		case IEQ:     f = *S-- == f;           break;
		case INEQ:    f = *S-- != f;           break;
		case IODD:    f = f & 1;               break;
		case IPUSH:   *++S = f; f = *pc++;     break;
		case IPOP:    f = *S--;                break;
		case IREAD:   fscanf(input, "%"PRIiPTR, &m[*pc++]); break;
		case IWRITE:  fprintf(output, "%"PRIiPTR"\n", f); f = *--S; break;
		case IHALT:   return 0;                break;
		default:
			fprintf(stderr, "illegal operation (%"PRIdPTR")!\n", op);
			return -1;
			break;
		}
	}
}

code_t *process_file(FILE *input, FILE *output, int debug, int symbols)
{
	node_t *n = parse(input, debug);
	if(!n)
		return NULL;
	if(debug)
		print_node(output, n, 0);
	code_t *c = code(n, MAX_CORE);
	if(!c) {
		free_node(n);
		return NULL;
	}
	if(debug)
		dump(c, output);
	if(symbols)
		export(n, output);
	vm(c, stdin, output, debug);
	free_node(n);
	return c;
}

static FILE *fopen_or_die(const char *name, char *mode)
{
	errno = 0;
	FILE *file = fopen(name, mode);
	if(!file) {
		fprintf(stderr, "could not open file \"%s\": %s\"\n", name, errno ? strerror(errno): "unknown");
		exit(EXIT_FAILURE);
	}
	return file;
}

/* Either print out FORTH code, or execute it in a Virtual Machine */
void help(void)
{
	const char *help = "\
PL/0 Compiler: A Toy Compiler\n\n\
\t-h print out a help message and quit\n\
\t-v turn on verbose mode\n\
\t-V print out version information and quit\n\
\t-S print out symbols defined and used\n\
\t-  Stop processing arguments\n\n\
Options must come before files to compile\n\n";
	fputs(help, stderr);
}

void usage(const char *arg0)
{
	fprintf(stderr, "usage: %s [-h] [-v] [-V] [-] files\n", arg0);
}

int main(int argc, char **argv)
{
	int i, verbose = 0, symbols = 0;
	code_t *c;
	for(i = 1; i < argc && argv[i][0] == '-'; i++)
		switch(argv[i][1]) {
		case '\0': goto done; /* stop argument processing */
		case 'h':  usage(argv[0]);
			   help();
			   return -1;
		case 'v': verbose = 1; break;
		case 'S': symbols = 1; break;
		case 'V': fprintf(stderr, "%s version: %d\n", argv[0], VERSION);
			  return -1;
		default:
			fprintf(stderr, "fatal: invalid argument '%s'\n", argv[i]);
			usage(argv[0]);
			return -1;
		}
done:
	if(i == argc) {
		if(verbose)
			fputs("reading from standard in\n", stderr);
		c = process_file(stdin, stdout, verbose, symbols);
		free_code(c);
	} else {
		for(; i < argc; i++) {
			if(verbose)
				fprintf(stderr, "reading from %s\n", argv[i]);
			FILE *in = fopen_or_die(argv[i], "rb");
			c = process_file(in, stdout, verbose, symbols);
			free_code(c);
			fclose(in);
		}
	}
	return 0;
}

