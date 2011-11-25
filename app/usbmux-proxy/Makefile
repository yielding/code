CC=g++-mp-4.6
CFLAGS=-c -Wall -I/opt/local/include
LDFLAGS=-L/opt/local/lib -lboost_system-mt
SOURCES = usbmux.cpp usbmux-proxy.cpp
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE =usbmux-proxy

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@
