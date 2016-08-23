CFLAGS=-Wall -Wextra -std=c99 -g -O2

all: pl0

test: pl0
	./$< ex1.pas ex2.pas

clean:
	rm -f pl0 
	rm -f core vgcore.*
