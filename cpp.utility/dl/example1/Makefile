# This file is part of the C++ dlopen mini HOWTO. You can find the complete
# HOWTO and/or updated versions at
# http://www.isotton.com/howtos/C++-dlopen-mini-HOWTO/
#
# Copyright 2002-2006 Aaron Isotton <aaron@isotton.com>
# Licensed under the GNU GPL.

example1: main.cpp hello.dylib
	$(CXX) $(CXXFLAGS) -o example1 main.cpp -ldl

hello.dylib: hello.cpp
	$(CXX) $(CXXFLAGS) -dynamiclib -arch i386 -Wl hello.cpp -o hello.dylib

clean:
	rm -f example1 hello.dylib hello.o

.PHONY: clean
