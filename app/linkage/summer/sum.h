#pragma once

#include <iostream>

using namespace std;

class Sum 
{
public:
  Sum() :m_data(NULL) 
  {
    // m_data = new int[10];
    // cout << "5 byte ints alloc\n";
  }

  ~Sum()
  {
    // delete m_data;
    // cout << "deleting m_data\n";
    // m_data = 0;
  }

    
  void size(int s)
  {
    if (m_data != NULL)
    {
      cout << "deleting m_data\n";
      delete m_data;
    }

    m_data = new int[s];
    cout << "allocating " << s << "bytes\n";
    for (int i=0; i<s; i++)
      m_data[i] = i;
  }

  double sum()
  {
    delete m_data;
    return 10.0;
  }

private:
  int* m_data;
};
