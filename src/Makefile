#
# Makefile for compiling Melon with Gambit
#

CC = gcc

.SUFFIXES: .scm

SRC = lexer.scm sst.scm expand.scm parser.scm interpreter.scm repl.scm
CSRC = $(SRC:.scm=.c)
COBJ = $(CSRC:.c=.o)


all: melon

melon: $(COBJ) repl_.o
	gcc -o $@ $(COBJ) repl_.o $(LDFLAGS) -lgambc

repl_.c: $(SRC)
	gsc -link $(CSRC)

.scm.c:
	gsc -c $<

install: all
	cp -p melon /home/dpa/share/bin

clean:
	-rm -f $(CSRC)
	-rm -f $(COBJ)
	-rm -f repl_.o repl_.c melon

distclean: clean
