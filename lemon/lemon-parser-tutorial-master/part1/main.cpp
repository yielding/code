#include <cstdlib>
#include <iostream>
#include "shellparser.h"
using namespace std;

void* ParseAlloc(void* (*allocProc)(size_t));
void Parse(void*, int, const char*);
void ParseFree(void*, void(*freeProc)(void*));

int main()
{
    void* shellParser = ParseAlloc(malloc);
    // Simulate a command line such as "cat main.cpp | wc"
    Parse(shellParser, ARGUMENT, "cat");
    Parse(shellParser, ARGUMENT, "main.cpp");
    Parse(shellParser, COMMAND_SUBSTITUTION_START, 0);
    Parse(shellParser, ARGUMENT, "find");
    Parse(shellParser, ARGUMENT, ".");
    Parse(shellParser, PIPE, 0);
    Parse(shellParser, ARGUMENT, "grep");
    Parse(shellParser, ARGUMENT, "-i");
    Parse(shellParser, ARGUMENT, "cpp$");
    Parse(shellParser, COMMAND_SUBSTITUTION_END, 0);
    Parse(shellParser, PIPE, 0);
    Parse(shellParser, ARGUMENT, "wc");
    Parse(shellParser, 0, 0); // Signal end of tokens
    ParseFree(shellParser, free);
    return 0;
}
