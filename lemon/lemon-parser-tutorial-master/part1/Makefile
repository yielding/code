shell: main.o shellparser.o
	$(CXX) main.o shellparser.o -o shell

main.o: main.cpp shellparser.hpp

shellparser.cpp: lemonfiles

shellparser.hpp: lemonfiles

.PHONY: lemonfiles
lemonfiles: shellparser.y
	lemon shellparser.y -s
	mv shellparser.c shellparser.cpp

.PHONY: clean
clean:
	rm -f *.o
	rm -f shellparser.cpp
	rm -f shellparser.h
	rm -f shell
	rm -f shellparser.out
