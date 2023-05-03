#include "stdafx.h"
#include "stkdef.h"

#include <string>

using namespace std;

template Stack<string>;

template Stack<string>::Stack();
template void   Stack<string>::push(string&);
template string Stack<string>::pop();
