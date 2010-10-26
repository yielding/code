OBJS     =example.o rubyeval.o
SRCS     =$(OBJS:.o=.cpp)

WHERE    =/opt/local/lib/ruby/1.8/i686-darwin10
CC       =gcc
CXX      =g++
LINK     =g++
INC      =-I$(WHERE)
CFLAGS   =
CXXFLAGS =-O3
LIBS     =-lruby -ldl
LFLAGS   =-L$(WHERE) 

TAR      = tar -cf
GZIP     = gzip -9f

DIST     = Makefile
TARGET   =c++ruby

##########################################
# Implicit rules

.SUFFIXES: .cpp .cxx .cc .C .c

.cpp.o:
	$(CXX) -c $(CXXFLAGS) $(INC) -o $@ $<

.cxx.o:
	$(CXX) -c $(CXXFLAGS) $(INC) -o $@ $<

.cc.o:
	$(CXX) -c $(CXXFLAGS) $(INC) -o $@ $<

.C.o:
	$(CXX) -c $(CXXFLAGS) $(INC) -o $@ $<

.c.o:
	$(CC)  -c $(CFLAGS)   $(INC) -o $@ $<

##########################################

# Building rules

all: $(TARGET)

clean:
	-rm -f $(OBJS) $(TARGET) 
	-rm -f core *~

dist:
	$(TAR) $(TARGET).tar $(SRCS) $(DIST)
	$(GZIP) $(TARGET).tar

$(TARGET): $(OBJS)
	$(CXX) -o $(TARGET) $(OBJS) $(LIBS)

# Compile
example.o: example.cpp
rubyeval.o: rubyeval.cpp
