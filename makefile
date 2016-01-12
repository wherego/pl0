ECHO=echo
AR=ar
CC=gcc
CFLAGS=-Wall -Wextra -g -pedantic -fPIC -std=c99
TARGET=lll

.PHONY: all clean doxygen

all: shorthelp $(TARGET)

shorthelp:
	@$(ECHO) "Use 'make help' for a list of all options"
help:
	@$(ECHO) ""
	@$(ECHO) "project:      lib$(TARGET)"
	@$(ECHO) "descriptions: A small S-expression based compiler"
	@$(ECHO) ""
	@$(ECHO) "make (option)*"
	@$(ECHO) ""
	@$(ECHO) "	all             create the $(TARGET) libraries and executables"
	@$(ECHO) "	$(TARGET)           create the $(TARGET) executable"
	@$(ECHO) "	unit            create the unit test executable"
	@$(ECHO) "	test            execute the unit tests"
	@$(ECHO) "	doc             make the project documentation"
	@$(ECHO) "	clean           remove generated files"
	@$(ECHO) "	install         (TODO) install the project"
	@$(ECHO) "	uninstall       (TODO) uninstall the project"
	@$(ECHO) "	dist            (TODO) create a distribution archive"
	@$(ECHO) ""

doc: $(TARGET).htm doxygen
$(TARGET).htm: $(TARGET).md
	markdown $^ > $@
doxygen:
	doxygen doxygen.conf

liblisp.a:
	make -C liblisp
	cp liblisp/$@ .
$(TARGET).o: $(TARGET).c $(TARGET).h
	$(CC) $(CFLAGS) -Iliblisp $< -c -o $@
unit: unit.c $(TARGET).o liblisp.a
	$(CC) $(CFLAGS) $^ -o $@
$(TARGET): main.c $(TARGET).o liblisp.a
	$(CC) $(CFLAGS) $^ -o $@

run: $(TARGET)
	./$^
test: unit
	./$^

clean:
	rm -rf $(TARGET) unit *.a *.so *.o *.log *.htm doxygen

