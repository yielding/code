# Version
VERSION = 0.0.1

# Compiler flags
CFLAGS = -Wall -W -O2 -s -pipe
CLMFLAGS = -Wall -W -O2 -s -pipe -lm
LFLAGS =  -O2 -s -pipe

all: lemon example1 exp1 example2 exp2 example3 exp3 example4 exp4 lexer example5 exp5 dcalc

exp1: example1.c 
	cat main_part >> example1.c
	g++ -o ex1 $(LFLAGS)  $<

example1: example1.y lemon
	  ./lemon example1.y

exp2: example2.c 
	cat main_part2 >> example2.c
	g++ -o ex2 $(LFLAGS)  $<

example2: example2.y lemon
	  ./lemon example2.y


exp3: example3.c 
	cat main_part3 >> example3.c
	g++ -o ex3 $(LFLAGS)  $<

example3: example3.y lemon
	  ./lemon example3.y


exp4: example4.c 
	cat main_part4 >> example4.c
	g++ -o ex4 $(LFLAGS)  $<

example4: example4.y lemon
	  ./lemon example4.y


exp5: example5.c lexer
	cat main_part5 >> example5.c
	g++ -o ex5 -O2 -s -pipe  example5.c lexer.o -lm

lexer: lexer.l lexglobal.h example5
	flex lexer.l
	test -e lex.yy.c && mv lex.yy.c lexer.c
	gcc -o lexer.o -c lexer.c 


example5: example5.y lemon
	  ./lemon example5.y


lemon: lemon.c
	  gcc -o $@ $(LFLAGS)  $<

dcalc: desktop_calc.cc
	  g++ -o $@ $(CLMFLAGS)  $<








clean:	
	rm  -f  ex1 example1.c example1.h example2.h ex2 example2.c example2.h ex3 example3.c example3.h ex4 example4.c example4.h ex5 example5.c example5.h lexer.c  lemon dcalc a.out *.out


