EXECUTABLE=hello

all: hello.ll
	/usr/local/opt/llvm/bin/llvm-as hello.ll
	/usr/local/opt/llvm/bin/llc hello.bc
	xcrun clang -o $(EXECUTABLE) hello.s
	./$(EXECUTABLE)

clean:
	rm $(EXECUTABLE)
	rm hello.s
	rm hello.bc
