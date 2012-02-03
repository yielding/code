# This file is part of the C++ dlopen mini HOWTO. You can find the complete
# HOWTO and/or updated versions at
# http://www.isotton.com/howtos/C++-dlopen-mini-HOWTO/
#
# Copyright 2002-2006 Aaron Isotton <aaron@isotton.com>
# Licensed under the GNU GPL.

driver: main.cpp polygon.hpp triangle.dylib
	$(CXX) $(CXXFLAGS) -o driver main.cpp -ldl -DPOSIX

triangle.dylib: triangle.cpp polygon.hpp
	$(CXX) $(CXXFLAGS) -dynamiclib -arch x86_64 -Wl,-syslibroot,/Developer/SDKs/MacOSX10.7.sdk triangle.cpp -o triangle.dylib

clean:
	rm -f driver triangle.dylib *.o

.PHONY: clean
