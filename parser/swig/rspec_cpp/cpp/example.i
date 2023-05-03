%module example

%{
#include "cexample.h"
#include "CppExample.h"
%}

%include std_string.i

%include "cexample.h"
%include "CppExample.h"

