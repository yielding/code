CXX = xcrun clang++
SDL = -L/usr/local/lib -lSDL2 -lSDL2_image
# If your compiler is a bit older you may need to change -std=c++11 to -std=c++0x
CXXFLAGS = -Wall -c -std=c++11 -I/usr/local/include
LDFLAGS = $(SDL)
EXE = SDL_Lesson3

all: $(EXE)

$(EXE): main.o
	$(CXX) $(LDFLAGS) $< -o $@

main.o: main.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

clean:
	rm *.o && rm $(EXE)
