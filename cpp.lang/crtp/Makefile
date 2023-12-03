OBJS     = crtp2.o
SRCS     = $(OBJS:.o=.cpp)

CC       = gcc
CXX      = g++
LINK     = g++
INC      = -I/opt/local/include/boost-1_35
CFLAGS   = 
CXXFLAGS = -O7
LIBS     = 
LFLAGS   = 

TAR      = tar -cf
GZIP     = gzip -9f

DIST     = 
TARGET   = crtp2 

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
	strip $(TARGET)

clean:
	-rm -f $(OBJS) $(TARGET) 
	-rm -f core *~

dist:
	$(TAR) crtp2.tar $(SRCS) $(DIST)
	$(GZIP) crtp2.tar

$(TARGET): $(OBJS)
	$(CXX) -o $(TARGET) $(OBJS) $(LIBS)

# Compile
crtp2.o: crtp2.cpp
