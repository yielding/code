#
#    Copyright David Abrahams 2003-2004
#    Copyright Aleksey Gurtovoy 2003-2004
#
#    Distributed under the Boost Software License, Version 1.0. 
#    (See accompanying file LICENSE_1_0.txt or copy at 
#    http://www.boost.org/LICENSE_1_0.txt)
#            
#    This file was automatically extracted from the source of 
#    "C++ Template Metaprogramming", by David Abrahams and 
#    Aleksey Gurtovoy.
#
#    It was built successfully with the following command: 
#
#        make edit -f example4.mak
#
%.o: %.cpp
	c++ -c $(CFLAGS) $< -o $@
OBJECTS = main.o kbd.o command.o display.o \
	  insert.o search.o files.o utils.o

edit : $(OBJECTS)
	c++ -o edit $(OBJECTS)

main.o : main.cpp defs.h
kbd.o : kbd.cpp defs.h command.h
command.o : command.cpp defs.h command.h
display.o : display.cpp defs.h buffer.h
insert.o : insert.cpp defs.h buffer.h
search.o : search.cpp defs.h buffer.h
files.o : files.cpp defs.h buffer.h command.h
utils.o : utils.cpp defs.h