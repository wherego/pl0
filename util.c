/** @file       util.c
 *  @brief      PL/0 generic utilities 
 *  @author     Richard Howe (2016)
 *  @license    LGPL v2.1 or Later
 *  @email      howe.r.j.89@gmail.com */
#include "pl0.h"
#include <stdlib.h>
#include <errno.h>
#include <string.h>

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

void indent(FILE *output, char c, unsigned i)
{
	while(i--)
		fputc(c, output);
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


