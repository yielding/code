CC = g++ -std=c++2a -pthread
CFLAGS = -g -Wall
OPENCV = `pkg-config opencv4 --cflags --libs`
LIBS = $(OPENCV)

all:
	echo "compiling srv..."
	$(CC) $(CFLAGS) -o server server.cpp $(LIBS) -lpthread
	echo "compiling client..."
	$(CC) $(CFLAGS) -o client client.cpp $(LIBS)

client:client.cpp
	$(CC) $(CFLAGS) -o client client.cpp $(LIBS)

server:srv.cpp
	$(CC) $(CFLAGS) -o server server.cpp $(LIBS) -lpthread
