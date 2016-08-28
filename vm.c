/** @file       vm.c
 *  @brief      PL/0 virtual machine
 *  @author     Richard Howe (2016)
 *  @license    LGPL v2.1 or Later
 *  @email      howe.r.j.89@gmail.com */

#include "pl0.h"
#include <inttypes.h>

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
			instruction_dump(c, output, debug > 1, (unsigned)(pc - m));
		switch(op = *pc++) {
		case INOP:                             break;
		case ILOAD:   *++S = f; f = m[*pc++];  break;
		case ISTORE:  m[*pc++] = f; f = *S--;  break;
		case ICALL:   *++S = f;
			      f = (intptr_t)(pc+1); 
			      pc = m+*pc;              break;
		case IRETURN: pc = (intptr_t*)f; f = *S--; break;
		case IJMP:    pc = m+*pc;              break;
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
		case IAND:    f = *S-- &  f;           break;     
		case IOR:     f = *S-- |  f;           break;      
		case IXOR:    f = *S-- ^  f;           break;    
		case IINVERT: f = ~f;                  break; 
		case INEGATE: f = -f;                  break; 
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

