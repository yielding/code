OBJS     =main.o
SRCS     =$(OBJS:.o=.cpp)

CC       =gcc
CXX      =g++
LINK     =g++
WHERE    =/opt/local/include
INC      =$(WHERE)
CFLAGS   =-O7
CXXFLAGS =-O7
LIBS     =-ldl -lboost_thread-mt -lboost_system-mt
LFLAGS   =

TAR      = tar -cf
GZIP     = gzip -9f

DIST     = 
TARGET   = main 

##########################################
# Implicit rules

.SUFFIXES: .cpp .cxx .cc .C .c

.cpp.o:
	$(CXX) -c $(CXXFLAGS) -I$(INC) -o $@ $<

.cxx.o:
	$(CXX) -c $(CXXFLAGS) -I$(INC) -o $@ $<

.cc.o:
	$(CXX) -c $(CXXFLAGS) -I$(INC) -o $@ $<

.C.o:
	$(CXX) -c $(CXXFLAGS) -I$(INC) -o $@ $<

.c.o:
	$(CC)  -c $(CFLAGS)   -I$(INC) -o $@ $<

##########################################

# Building rules

all: $(TARGET)

clean:
	-rm -f $(OBJS) $(TARGET) 
	-rm -f core *~

dist:
	$(TAR) main.tar $(SRCS) $(DIST)
	$(GZIP) main.tar

$(TARGET): $(OBJS)
	$(CXX) -o $(TARGET) $(OBJS) $(LIBS)

# Compile
main.o: main.cpp
