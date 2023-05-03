BUILD
=====
  dmcs /t:library stack.cs => stack.dll
  dmcs /r:stack.dll stack_test.cs => stack_test.exe

  mono ./stack_test.exe
