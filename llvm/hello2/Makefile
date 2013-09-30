EXECUTABLE=hello

all: hello.ll
	llvm-as-mp-3.3 hello.ll
	llc-mp-3.3 hello.bc
	gcc -o $(EXECUTABLE) hello.s
	./$(EXECUTABLE)

clean:
	rm $(EXECUTABLE)
	rm hello.s
	rm hello.bc
