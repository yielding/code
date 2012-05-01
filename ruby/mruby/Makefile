# makefile description.

# project-specific macros
# extension of the executable-file is modifiable(.exe .out ...)
BASEDIR = ../..
TARGET := helloWorld
RITEVM := $(BASEDIR)/lib/ritevm
MRUBY := $(BASEDIR)/tools/mruby/
ifeq ($(OS),Windows_NT)
EXE := $(TARGET).exe
LIB := $(RITEVM).lib
MRB := $(MRUBY).exe
else
EXE := $(TARGET)
LIB := $(RITEVM).a
MRB := $(MRUBY)
endif
MSRC :=  helloWorld.c
YSRC :=  $(BASEDIR)/src/parse.y
YC :=  $(BASEDIR)/src/y.tab.c
EXCEPT1 := $(YC) $(MSRC)
OBJM := $(patsubst %.c,%.o,$(MSRC))
OBJY := $(patsubst %.c,%.o,$(YC))
OBJ1 := $(patsubst %.c,%.o,$(filter-out $(EXCEPT1),$(wildcard $(BASEDIR)/src/*.c)))
#OBJ2 := $(patsubst %.c,%.o,$(wildcard ext/regex/*.c))
#OBJ3 := $(patsubst %.c,%.o,$(wildcard ext/enc/*.c))
OBJS := $(OBJ1) $(OBJ2) $(OBJ3)
# mruby libraries
EXTC :=  $(BASEDIR)/mrblib/mrblib.c
EXTRB := $(wildcard $(BASEDIR)/mrblib/*.rb)
EXT0 := $(patsubst %.c,%.o,$(BASEDIR)/src/$(EXTC))
# ext libraries
EXTS := $(EXT0)

# libraries, includes
LIBS = $(LIB) -lm
INCLUDES = -I$(BASEDIR)/src -I$(BASEDIR)/include

# compiler, linker (gcc)
CC = gcc
LL = gcc
YACC = bison
DEBUG_MODE = 1
ifeq ($(DEBUG_MODE),1)
CFLAGS = -g
else
CFLAGS = -O3
endif
ALL_CFLAGS = -Wall -Werror-implicit-function-declaration $(CFLAGS)
MAKE_FLAGS = --no-print-directory CC=$(CC) LL=$(LL)

##############################
# generic build targets, rules

all : $(LIB) $(MRB) $(EXE)
	@echo "make: built targets of `pwd`"

# executable constructed using linker from object files
$(EXE) : $(OBJM) $(LIB)
	$(LL) -o $@ $(OBJM) $(LIBS)

-include $(OBJS:.o=.d)

# src compile
$(LIB) : $(EXTS) $(OBJS) $(OBJY)
	$(MAKE) -C $(BASEDIR)/src $(MAKE_FLAGS)

# mruby interpreter compile
$(MRB) : $(EXTS) $(OBJS) $(OBJY)
	$(MAKE) -C $(BASEDIR)/tools/mruby $(MAKE_FLAGS)

# objects compiled from source
$(OBJS) :
	$(MAKE) -C $(BASEDIR)/src $(MAKE_FLAGS) && $(MAKE) -C $(BASEDIR)/tools/mruby $(MAKE_FLAGS)

# extend libraries complile
$(EXTS) : $(EXTRB)
	$(MAKE) -C $(BASEDIR)/mrblib $(MAKE_FLAGS)

# test module compile
$(OBJM) : $(MSRC)
	$(CC) $(ALL_CFLAGS) -MMD $(INCLUDES) -c $(MSRC) -o $(OBJM)

# clean up
clean :
	$(MAKE) clean -C $(BASEDIR)/src $(MAKE_FLAGS)
	$(MAKE) clean -C $(BASEDIR)/tools/mrbc $(MAKE_FLAGS)
	$(MAKE) clean -C $(BASEDIR)/tools/mruby $(MAKE_FLAGS)
	-rm -f $(EXE) $(OBJM)
	-rm -f $(OBJM:.o=.d)
	-rm -f $(patsubst %.c,%.o,$(EXCEPT1)) $(patsubst %.c,%.d,$(EXCEPT1))
	@echo "make: removing targets, objects and depend files of `pwd`"

