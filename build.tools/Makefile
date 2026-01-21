OBJS     = main.o
SRCS     = $(OBJS:.o=.cpp)

CC       = gcc
CXX      = g++-4.2
LINK     = g++-4.2
INC      =
CFLAGS   = 
CXXFLAGS = -O7
LIBS     = -lgtest -lgtest_main
LFLAGS   = 

TAR      = tar -cf
GZIP     = gzip -9f

DIST     = 
TARGET   = main 

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
	./$(TARGET)

clean:
	-rm -f $(OBJS) $(TARGET) 
	-rm -f core *~

dist:
	$(TAR) $(TARGET).tar $(SRCS) $(DIST)
	$(GZIP) $(TARGET).tar

$(TARGET): $(OBJS)
	$(CXX) -o $(TARGET) $(OBJS) $(LIBS)

# Compile
main.o: main.cpp
