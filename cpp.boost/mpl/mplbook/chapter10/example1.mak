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
#        make my_program -f example1.mak
#
my_program: a.cpp b.cpp c.cpp d.cpp
	c++ -o my_program a.cpp b.cpp c.cpp d.cpp