#include "CppExample.h"

CppExample::CppExample() : _title("") 
{
}

CppExample::CppExample(const CppExample& foo)
  : _title(foo._title) 
{
}

CppExample::CppExample(const char* title, int flag) 
  : _title(title), _flag(flag) 
{
}

CppExample::~CppExample() 
{
}

std::string CppExample::title() const 
{ 
  return _title;
}

void CppExample::title(std::string name)
{ 
  _title = name; 
}

int  CppExample::flag() const 
{ 
  return _flag; 
}

void CppExample::flag(int value) 
{ 
  _flag = value; 
}

int CppExample::countOfCppExamples() 
{ 
  return 1; 
}
