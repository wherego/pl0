CFLAGS=-Wall -Wextra -std=c99 -g

all: pl0 pcode

test: pl0
	./$< ex1.pas

clean:
	rm -f pl0 pcode
