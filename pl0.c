/** @file       pl0.c
 *  @brief      PL/0 driver
 *  @author     Richard Howe (2016)
 *  @license    LGPL v2.1 or Later
 *  @email      howe.r.j.89@gmail.com 
 *
 * see https://en.wikipedia.org/wiki/PL/0 
 *
 * @todo Replace all instances of exit() with a longjmp to interpreter bounds. 
 * @todo Stack frames need added and dealing with
 * @todo Sanity checking (check for redefinitions)
 * @todo Attempt to detect multiple errors (error recovery in parser)
 * @todo Add in assertions, make unit tests
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

#include "pl0.h"
#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

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
 *    block = [ "const" constlist ]
 *            [ "var" varlist ]
 *            { "procedure" ident opt-varlist ";" block ";" } statement .
 *
 *    constlist  = ident "=" number {"," ident "=" number} ";"
 *    varlist    = ident {"," ident} ";"
 *
 *    opt-varlist = [ varlist ] .
 *
 *    statement  = [ assignment | invoke | input | output | list | conditional | whilst | doop ].
 *    assignment = ident ":=" unary-expression .
 *    invoke     = "call" ident opt-varlist .
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
 *    expression = { ("+"|"-"|"and"|"or"|"xor") term}.
 *    
 *    unary-expression = ["+"|"-"] term expression.
 *    
 *    term = factor {("*"|"/") factor}.
 *    
 *    factor = ident | number | "(" unary-expression ")". */


/****************************** driver *************************************/

static code_t *process_file(FILE *input, FILE *output, int debug, int symbols)
{
	node_t *n = parse(input, debug);
	if(!n)
		return NULL;
	if(debug)
		print_node(output, n, 0, 0);
	code_t *c = code(n, MAX_CORE, 1);
	if(!c)
		return NULL;
	if(debug)
		dump(c, output, debug > 1);
	if(symbols)
		export(n, output);
	vm(c, stdin, output, debug);
	/*free_node(n);*/
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

static void help(void)
{
	const char *help = "\
PL/0 Compiler: A Toy Compiler\n\n\
\t-h print out a help message and quit\n\
\t-v increase verbosity levels\n\
\t-V print out version information and quit\n\
\t-S print out symbols defined and used\n\
\t-  Stop processing arguments\n\n\
Options must come before files to compile\n\n";
	fputs(help, stderr);
}

static void usage(const char *arg0)
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
		case 'v': if(verbose < INT_MAX) verbose++; break;
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

