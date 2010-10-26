OBJS     =data_model.o main.o rubyeval.o
SRCS     =$(OBJS:.o=.cpp)

CC       =gcc
CXX      =g++
LINK     =g++
WHERE    =/opt/local/lib/ruby/1.8/i686-darwin9.2.2
INC      =$(WHERE)
CFLAGS   =-g
CXXFLAGS =-O2 
LIBS     =-lruby -lobjc -ldl
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
data_model.o: data_model.cpp
rubyeval.o: rubyeval.cpp
main.o: main.cpp
