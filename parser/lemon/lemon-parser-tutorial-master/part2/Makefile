CXXFLAGS = -Wall -Wextra -Weffc++ -Wfloat-equal -Wshadow\
	-Wpointer-arith -Wcast-align -Wstrict-overflow=5\
	-Wwrite-strings -Wswitch-default -Wswitch-enum -Wparentheses\
	-Woverloaded-virtual -pedantic

shell: main.o shellparser.o shellscanner.yy.o
	$(CXX) $(CXXFLAGS) main.o shellparser.o shellscanner.yy.o -o shell
 
main.o: main.cpp shellscanner.yy.hpp shellparser.hpp
 
shellparser.cpp: lemonfiles
 
shellparser.hpp: lemonfiles
 
.PHONY: lemonfiles
lemonfiles: shellparser.y
	./lemon shellparser.y -s
	mv shellparser.c shellparser.cpp
	mv shellparser.h shellparser.hpp
 
shellscanner.o: shellscanner.yy.hpp shellscanner.yy.cpp
 
shellscanner.yy.cpp: flexfiles
 
shellscanner.yy.hpp: flexfiles
 
.PHONY: flexfiles
flexfiles: shellscanner.l
	flex --outfile=shellscanner.yy.cpp --header-file=shellscanner.yy.hpp shellscanner.l
 
.PHONY: clean
clean:
	rm -f *.o
	rm -f shellscanner.yy.cpp shellscanner.yy.hpp
	rm -f shellparser.cpp shellparser.hpp shellparser.out
	rm -f shell
