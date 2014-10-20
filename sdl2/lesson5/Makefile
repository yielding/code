CXX = clang++
SDL = -L/usr/local/lib -lSDL2 -lSDL2_image
CXXFLAGS = -Wall -c -std=c++11 -I/usr/local/include
LDFLAGS = $(SDL)
EXE = SDL_Lesson5

all: $(EXE)

$(EXE): main.o
	$(CXX) $(LDFLAGS) $< -o $@

main.o: main.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

clean:
	rm *.o && rm $(EXE)
