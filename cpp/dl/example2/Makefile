# This file is part of the C++ dlopen mini HOWTO. You can find the complete
# HOWTO and/or updated versions at
# http://www.isotton.com/howtos/C++-dlopen-mini-HOWTO/
#
# Copyright 2002-2006 Aaron Isotton <aaron@isotton.com>
# Licensed under the GNU GPL.

example2: main.cpp polygon.hpp triangle.dylib
	$(CXX) $(CXXFLAGS) -o example2 main.cpp -ldl

triangle.dylib: triangle.cpp polygon.hpp
	$(CXX) $(CXXFLAGS) -dynamiclib -arch i386 -Wl,-syslibroot,/Developer/SDKs/MacOSX10.5u.sdk triangle.cpp -o triangle.dylib

clean:
	rm -f example2 triangle.dylib *.o

.PHONY: clean
