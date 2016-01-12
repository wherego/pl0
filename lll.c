/** @file       lll.c
 *  @brief      A small S-Expression based compiler
 *  @author     Richard Howe (2016)
 *  @license    LGPL v2.1 or Later
 *  @email      howe.r.j.89@gmail.com */
#include "lll.h"
#include <liblisp.h>

int main_lll(int argc, char **argv) {
        lisp *l;
        cell *c;
        if(!(l = lisp_init()))
                FATAL("init failed");
        /*@todo custom stages for the semantic analysis and eventual
         *compilation will get inserted between the read and the
         *print*/
        while((c = lisp_read(l, lisp_get_input(l))))
                if(lisp_print(l, c) < 0)
                        FATAL("print failed");
        return 0;
}

