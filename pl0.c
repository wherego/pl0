/* see https://en.wikipedia.org/wiki/PL/0 
 *
 * @todo Replace all instances of exit() with a longjmp to interpreter bounds. */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>

#define MAX_ID_LENGTH (256u)
#define MAX_CORE (1024u)

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
 *    statement  = [ assignment | invoke | input | output | list | conditional | whilst ].
 *    assignment = ident ":=" unary-expression .
 *    invoke     = "call" ident .
 *    input      = "?" ident .
 *    output     = "!" unary-expression .
 *    list       = "begin" statement {";" statement } "end" .
 *    conditional = "if" condition "then" statement .
 *    whilst     = "while" condition "do" statement .
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
 *    factor = ident | number | "(" unary-expression ")".
 *
 */

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
	WHILE,		/* while */
	DO,		/* do */
	LESSEQUAL,      /* <= */
	GREATEREQUAL,   /* >= */
	ODD,            /* odd */
	LAST_KEY_WORD,  /* not an actual token */

	ERROR,          /* invalid token, not an actual token */
	IDENTIFIER,     /* [a-Z_][0-9a-Z]* */
	NUMBER,         /* [0-9]+ */

	FIRST_SINGLE_CHAR_TOKEN, /* marker, not a token */
	/* @warning these need to be in ASCII order */
	EXCLAMATION  =  '!',
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
	QUESTION     =  '?',
	LAST_SINGLE_CHAR_TOKEN, /* marker, not a token */
	EOI          =  EOF
} token_e;

const char *keywords[] = 
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
	[WHILE]         =  "while",
	[DO]            =  "do",
	[LESSEQUAL]     =  "<=",
	[GREATEREQUAL]  =  ">=",
	[ODD]           =  "odd",
	NULL
};

typedef struct {
	token_e type;
	union {
		char *id;
		int number;
	} p;
} token_t;

typedef struct {
	FILE *input;
	unsigned line;
	int c;
	char id[MAX_ID_LENGTH];
	int number;
	token_t *unget;
	token_t *token;
	token_t *accepted;
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

static char *duplicate(const char *str)
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

void indent(FILE *output, char c, unsigned i)
{
	while(i--)
		fputc(c, output);
}

void print_token_enum(FILE *output, token_e type)
{
	if(type == NUMBER) {
		fputs("number\n", output);
	} else if(type == IDENTIFIER) {
		fputs("identifier\n", output);
	} else if(type >= 0 && type < LAST_KEY_WORD) {
		fprintf(output, "key-word(%s)\n", keywords[type]);
	} else if(type > FIRST_SINGLE_CHAR_TOKEN && type < LAST_SINGLE_CHAR_TOKEN) {
		fprintf(output, "token(%c)\n", type);
	} else if(type == EOI) {
		fputs("EOF\n", output);
	} else if(type == ERROR) {
		fputs("error token\n", output);
	} else {
		fprintf(output, "invalid-token(%d)\n", type);
	}
}

void print_token(FILE *output, token_t *t, unsigned depth)
{
	if(!t)
		return;
	indent(output, ' ', depth);
	if(t->type == NUMBER) {
		fprintf(output, "number(%d)\n", t->p.number);
	} else if(t->type == IDENTIFIER) {
		fprintf(output, "id(%s)\n", t->p.id);
	} else {
		print_token_enum(output, t->type);
	}
}

int _syntax_error(lexer_t *l, const char *file, const char *func, unsigned line, const char *msg)
{
	fprintf(stderr, "%s:%s:%u\n", file, func, line);
	fprintf(stderr, "  syntax error on line %u of input\n  %s\n", l->line, msg);
	print_token(stderr, l->token, 2);
	exit(EXIT_FAILURE);
	return 0;
}

#define syntax_error(L, MSG) _syntax_error((L), __FILE__, __func__, __LINE__, (MSG))

lexer_t *new_lexer(FILE *input)
{
	lexer_t *r = allocate(sizeof(*r));
	r->input = input;
	r->line = 1;
	return r;
}

token_t *new_token(token_e type)
{
	token_t *r = allocate(sizeof(*r));
	r->type = type;
	return r;
}

void free_token(token_t *t)
{
	if(!t)
		return;
	if(t->type == IDENTIFIER)
		free(t->p.id);
	free(t);
}

void free_lexer(lexer_t *l)
{
	/* @todo free current and previous */
	free(l);
}

int next_char(lexer_t *l)
{
	return fgetc(l->input);
}

int unget_char(lexer_t *l, int c)
{
	return ungetc(c, l->input);
}

void unget_token(lexer_t *l, token_t *unget)
{
	if(l->unget)
		fprintf(stderr, "%s: too many tokens put back!", __func__);
	l->unget = unget;
}

void lexer(lexer_t *l)
{
	if(l->unget) { /* process put-back of one token */
		l->token = l->unget;
		l->unget = NULL;
		return;
	}
	int ch = next_char(l);
	l->token = new_token(ERROR);
again:  switch(ch) {
	case '\n': l->line++;
	case ' ': ch = next_char(l);
		  goto again;
	case DOT: l->token->type = DOT; return;
	case EOF: l->token->type = EOI; return;
	case QUESTION: case EXCLAMATION: case COMMA: case SEMICOLON:
	case LPAR:     case RPAR:        case ADD:   case SUB:
	case MUL:      case DIV:         case EQUAL: case NOTEQUAL:
		l->token->type = ch;
		return;
	/* >, >=, <, <= and := are special case for now */
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
		syntax_error(l, "invalid token");
	}
	unget_char(l, ch);
}

/** @todo Add equivalent to CALL, IF, LOOP, etc instead of using tokens */
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
		LIST,
	CONDITION, 
	EXPRESSION,
	UNARY_EXPRESSION, 
	TERM, 
	FACTOR,

} parse_e;

const char *names[] = {
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
	[LIST]         =  "list",
	[CONDITION]    =  "condition",
	[EXPRESSION]   =  "expression",
	[UNARY_EXPRESSION]   =  "unary-expression",
	[TERM]         =  "term",
	[FACTOR]       =  "factor",
};

typedef struct node_t {
	parse_e type;
	token_t *token, *value;
	struct node_t *o1, *o2, *o3, *o4;
} node_t;

node_t *new_node(parse_e type)
{
	node_t *r = allocate(sizeof(*r));
	fprintf(stderr, "debug> %s\n", names[type]);
	r->type = type;
	return r;
}

void free_node(node_t *n)
{
	if(!n)
		return;
	free_node(n->o1);
	free_node(n->o2);
	free_node(n->o3);
	free_node(n->o4);
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
	print_node(output, n->o1, depth+1);
	print_node(output, n->o2, depth+1);
	print_node(output, n->o3, depth+1);
	print_node(output, n->o4, depth+1);
}

int accept(lexer_t *l, token_e sym)
{
	if(sym == l->token->type) {
		l->accepted = l->token;
		if(sym != EOI && sym != DOT)
			lexer(l);
		return 1;
	}
	return 0;
}

int _expect(lexer_t *l, token_e sym, const char *file, const char *func, unsigned line)
{
	if(accept(l, sym))
		return 1;
	fprintf(stderr, "%s:%s:%u\n", file, func, line);
	fprintf(stderr, "  Syntax error: unexpected token\n  Got:          ");
	print_token(stderr, l->token, 0);
	fputs("  Expected:     ", stderr);
	print_token_enum(stderr, sym);
	fprintf(stderr, "On line: %u\n", l->line);
	exit(EXIT_FAILURE);
	return 0;
}

#define expect(L, SYM) _expect((L), (SYM), __FILE__, __func__, __LINE__)

/** @todo refactor loops to use recursion, or use void "add(node_t **xs, node_t *x)"*/

node_t *unary_expression(lexer_t *l);

node_t *factor(lexer_t *l) /* ident | number | "(" unary-expression ")". */
{
	node_t *r = new_node(FACTOR);
	if(accept(l, IDENTIFIER) || accept(l, NUMBER)) { 
		r->token = l->accepted;
		return r;
	} else if(accept(l, LPAR)) {
		r->o1 = unary_expression(l);
		expect(l, RPAR);
	} else {
		syntax_error(l, "expected id, number or \"(\" unary-expression \")\"");
	}
	return r;
}

node_t *term(lexer_t *l) /* factor {("*"|"/") factor}. */
{
	node_t *r = new_node(TERM);
	r->o1 = factor(l);
	if(accept(l, MUL) || accept(l, DIV)) {
		r->token = l->accepted;
		r->o2 = factor(l);
	}
	return r;
}

/**@todo check grammar here */
node_t *expression(lexer_t *l) /* { ("+"|"-") term}. */
{
	if(accept(l, ADD) || accept(l, SUB)) {
		node_t *r = new_node(EXPRESSION);
		r->token = l->accepted;
		r->o1 = term(l);
		return r;
	} else {
		return NULL;
	}
}

node_t *unary_expression(lexer_t *l) /* [ "+"|"-"] term expression. */
{
	node_t *r = new_node(UNARY_EXPRESSION);
	if(accept(l, ADD) || accept(l, SUB))
		r->token = l->accepted;
	r->o1 = term(l);
	r->o2 = expression(l);
	return r;
}

node_t *condition(lexer_t *l)
{
	node_t *r = new_node(CONDITION);
	if(accept(l, ODD)) { /* "odd" unary_expression */
		r->o1 = unary_expression(l);
		r->token = l->accepted;
	} else { /* unary_expression ("="|"#"|"<"|"<="|">"|">=") unary_expression*/
		r->o1 = unary_expression(l);
		if(accept(l, EQUAL) || accept(l, GREATER) || accept(l, LESS) 
		|| accept(l, LESSEQUAL) || accept(l, GREATEREQUAL) || accept(l, NOTEQUAL)) { 
			r->token = l->accepted;
			r->o2 = unary_expression(l);
		} else {
			syntax_error(l, "expected condition statement");
		}
	}
	return r;
}

node_t *statement(lexer_t *l);

node_t *list(lexer_t *l)
{
	node_t *r = new_node(LIST);
	r->o1 = statement(l);
	if(accept(l, SEMICOLON))
		r->o2 = list(l);
	return r;
}

node_t *statement(lexer_t *l)
{
	node_t *r = new_node(STATEMENT);
	if(accept(l, IDENTIFIER)) { /* ident ":=" unary_expression */
		r->token = l->accepted;
		expect(l, ASSIGN);
		r->o1 = unary_expression(l);
		r->type = ASSIGNMENT;
	} else if(accept(l, CALL)) { /* "call" ident  */
		expect(l, IDENTIFIER);
		r->token = l->accepted;
		r->type = INVOKE;
	} else if(accept(l, QUESTION)) { /* "?" ident */
		expect(l, IDENTIFIER);
		r->token = l->accepted;
		r->type = INPUT;
	} else if(accept(l, EXCLAMATION)) { /* "!" expression */
		r->o1 = unary_expression(l);
		r->type = OUTPUT;
	} else if(accept(l, BEGIN)) { /* "begin" statement {";" statement } "end" */
		r->o1 = list(l);
		expect(l, END);
		r->type = LIST;
	} else if(accept(l, IF)) { /* "if" condition "then" statement */
		r->token = l->accepted;
		r->o1 = condition(l);
		expect(l, THEN);
		r->o2 = statement(l);
		r->type = CONDITIONAL;
	} else if(accept(l, WHILE)) { /*  "while" condition "do" statement */
		r->token = l->accepted;
		r->o1 = condition(l);
		expect(l, DO);
		r->o2 = statement(l);
		r->type = WHILST;
	} else {
		/* statement is optional */
	}
	return r;
}

node_t *constlist(lexer_t *l) /* "const" ident "=" number {"," ident "=" number} ";" */
{
	node_t *r = new_node(CONSTLIST);
	expect(l, IDENTIFIER);
	r->token = l->accepted;
	expect(l, EQUAL);
	expect(l, NUMBER);
	r->value = l->accepted;
	if(accept(l, COMMA))
		r->o1 = constlist(l);
	return r;
}

node_t *varlist(lexer_t *l) /* "var" ident {"," ident} ";"  */
{
	node_t *r = new_node(VARLIST);
	expect(l, IDENTIFIER);
	r->token = l->accepted;
	if(accept(l, COMMA))
		r->o1 = varlist(l);
	return r;
}

node_t *block(lexer_t *l);

node_t *proclist(lexer_t *l)
{
	node_t *r = new_node(PROCLIST);
	expect(l, IDENTIFIER);
	r->token = l->accepted;
	expect(l, SEMICOLON);
	r->o1 = block(l);
	expect(l, SEMICOLON);
	if(accept(l, PROCEDURE))
		r->o2 = proclist(l);
	return r;
}

node_t *block(lexer_t *l)
{
	node_t *r = new_node(BLOCK);
	if(accept(l, CONST)) { /* [ constlist ] */
		r->o1 = constlist(l);
		expect(l, SEMICOLON);
	}
	if(accept(l, VAR)) { /* [ varlist ] */
		r->o2 = varlist(l);
		expect(l, SEMICOLON);
	}
	/* { "procedure" ident ";" block ";" } statement */
	if(accept(l, PROCEDURE)) {
		r->o3 = proclist(l);
	}
	r->o4 = statement(l);
	return r;
}

node_t *program(lexer_t *l)
{
	node_t *r = new_node(PROGRAM);
	lexer(l);
	r->o1 = block(l);
	if(accept(l, EOI))
		return r;
	expect(l, DOT);
	return r;
}

typedef struct {
	char m[MAX_CORE];
	unsigned index;
} code_t;

typedef struct scope_t {
	node_t *constants;
	node_t *variables;
	node_t *functions;
	struct scope_t *parent;
} scope_t;

typedef enum {
	ILOAD, ISTORE, ICALL, IRETURN, IBRANCH, INBRANCH, IADD, ISUB, IMUL, IDIV,
	ILTE, IGTE, ILT, IGT, IEQ, INEQ, IPUSH, IPOP, IREAD, IWRITE, IHALT
} instruction;

void generate(code_t *c, instruction i)
{
	c->m[c->index++] = i;
}

unsigned hole(code_t *c) 
{
	return c->index++;
}

void fix(code_t *c, unsigned hole, unsigned patch)
{
	c->m[hole] = patch;
}

code_t *new_code(void)
{
	code_t *r = allocate(sizeof(*r));
	return r;
}

scope_t *new_scope(scope_t *parent)
{
	scope_t *r = allocate(sizeof(*r));
	r->parent = parent;
	return r;
}

void free_scope(scope_t *s)
{
	if(!s)
		return;
	free_node(s->constants);
	free_node(s->variables);
	free_node(s->functions);
	free(s);
}

/** @todo work out how to store variable lookups and scopes */
void code(code_t *c, node_t *n, scope_t *parent) {
	unsigned hole1;
	scope_t *current = NULL;
	if(!n)
		return;
	switch(n->type) {
	case PROGRAM: code(c, n->o1, NULL); generate(c, IHALT); 
	case BLOCK: 
		      current = new_scope(parent);
		      code(c, n->o1, current); /*constants*/
		      code(c, n->o2, current); /*variables*/
		      code(c, n->o3, current); /*procedures*/
		      code(c, n->o4, current); /*final statement*/
		      free_scope(current);
		      break;
	case CONSTLIST: parent->constants = n; break;
	case VARLIST:   parent->variables = n; break;
	case PROCLIST:  if(parent->parent) {
				fprintf(stderr, "nested procedures disallowed\n");
				exit(EXIT_FAILURE);
			}
			parent->functions = n;
			code(c, n->o1, parent);
			code(c, n->o2, parent);
		      break;
	case STATEMENT:  /*do nothing, empty statement*/
		      return;
	case ASSIGNMENT:
		      /* find? is variable? emit code */
	case INVOKE:  
	case OUTPUT:  code(c, n->o1, current); generate(c, OUTPUT); break;
	case INPUT:   code(c, n->o1, current); generate(c, INPUT); break;
	case CONDITIONAL: code(c, n->o1, current); generate(c, INBRANCH); hole1 = hole(c);
			  code(c, n->o2, current); fix(c, hole1, c->index); break;
	case CONDITION: 
	case WHILST:
	case LIST:    code(c, n->o1, current); code(c, n->o2, current); break;
	case UNARY_EXPRESSION: 
	case EXPRESSION:
	case TERM: 
	case FACTOR:
		break;
	}
}

/* Either print out FORTH code, or execute it in a Virtual Machine */

int main(void)
{
	print_node(stdout, program(new_lexer(stdin)), 0);
	return 0;
}

