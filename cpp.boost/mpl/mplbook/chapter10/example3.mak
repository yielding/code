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
#        make edit -f example3.mak
#
OBJECTS = main.o kbd.o command.o display.o \
	  insert.o search.o files.o utils.o

edit : $(OBJECTS)
	c++ -o edit $(OBJECTS)
main.o : main.cpp defs.h
	c++ -c main.cpp
kbd.o : kbd.cpp defs.h command.h
	c++ -c kbd.cpp
command.o : command.cpp defs.h command.h
	c++ -c command.cpp
display.o : display.cpp defs.h buffer.h
	c++ -c display.cpp
insert.o : insert.cpp defs.h buffer.h
	c++ -c insert.cpp
search.o : search.cpp defs.h buffer.h
	c++ -c search.cpp
files.o : files.cpp defs.h buffer.h command.h
	c++ -c files.cpp
utils.o : utils.cpp defs.h
	c++ -c utils.cpp