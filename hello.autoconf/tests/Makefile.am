## Makefile.am -- Process this file with automake to produce Makefile.in

INCLUDES = -I$(top_srcdir)/include

TESTS = test1 test2

check_PROGRAMS = test1 test2

test1_SOURCES = test1.cpp
test1_LDADD = $(top_srcdir)/src/libhello.la

test2_SOURCES = test2.cpp
test2_LDADD = $(top_srcdir)/src/libhello.la

MAINTAINERCLEANFILES = Makefile.in
