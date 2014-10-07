COMPILE
=======
clang -c -fPIC ffi_fibo.cpp -o ffi_fibo.o
clang -shared -o ffi_test.so ffi_fibo.o
