OBJS     = spirit.cpp2html.o
SRCS     = $(OBJS:.o=.cpp)

CC       = gcc
CXX      = g++
LINK     = g++
INC      = -I/opt/local/include 
CFLAGS   =
CXXFLAGS =-O3 -DPOSIX
LIBS     = -L/opt/local/lib
LIBS     =
LFLAGS   =

TAR      = tar -cf
GZIP     = gzip -9f

DIST     = 
TARGET   = spirit.cpp2html

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

# all: run_tests $(TARGET)
all: $(TARGET)

clean:
	-rm -f $(OBJS) $(TARGET) runner
	-rm -f *.core *~
	-rm -f runner.cpp

dist:
	$(TAR) $(TARGET).tar $(SRCS) $(DIST)
	$(GZIP) $(TARGET).tar

$(TARGET): $(OBJS)
	$(CXX) -o $(TARGET) $(OBJS) $(LIBS)

# Compile
spirit.cpp2html.o: spirit.cpp2html.cpp 
