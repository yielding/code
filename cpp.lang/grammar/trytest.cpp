// trytest.cpp : Defines the entry point for the console application.
//

// #include "stdafx.h"

#include <iostream>

using namespace std;

class Test 
{
public:
  Test(int i);

  int m_i;
};

Test::Test(int i)
try
{
  m_i = i;
  cout << "xxx" << endl;
}
catch(...) 
{
}

int main(int argc, char* argv[])
{
  Test t(1);
  return 0;
}

