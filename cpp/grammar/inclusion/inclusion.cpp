// inclusion.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "stk.h"

#include <string>
#include <iostream>

using namespace std;

int _tmain(int argc, _TCHAR* argv[])
{
  string str("leech");
  Stack<string> s;

  s.push(str);
  cout << s.pop();

  return 0;
}
