#------------------------------------------------------------------------------*
#                                                                              *
# C + +    H E L L O W O R L D    F O R    M I N G W 3 2    O N    M A C O S X *
#                                                                              *
#   Updated for Release 2                                                      *
#                                                                              *
#------------------------------------------------------------------------------*

TOOL_DIR := /usr/local/i386-mingw32-3.4.5/bin

SOURCES := hello_world.cpp

OBJECTS_DIR := objects

EXECUTABLE := hello_world_in_cpp.exe

ALL_OBJECT_FILES := $(patsubst %, $(OBJECTS_DIR)/%.o, $(SOURCES))

all:$(OBJECTS_DIR) $(EXECUTABLE)

#--- Make object dir
$(OBJECTS_DIR):
	mkdir $(OBJECTS_DIR)

#--- C++ compile : use gcc
$(OBJECTS_DIR)/%.cpp.o:%.cpp
	$(TOOL_DIR)/i386-mingw32-gcc -c $< -o $@

#--- C++ link : use g++
$(EXECUTABLE):$(ALL_OBJECT_FILES)
	$(TOOL_DIR)/i386-mingw32-g++ $< -o $@
	$(TOOL_DIR)/i386-mingw32-strip --strip-all $@

clean:
	rm -f $(EXECUTABLE)
	rm -Rf $(OBJECTS_DIR)/
