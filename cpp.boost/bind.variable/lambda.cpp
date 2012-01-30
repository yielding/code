//#include "stdafx.h"

#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>

using namespace std;
using namespace boost::lambda;

struct product
{
  product(string name, int id) :m_name(name), m_id(id) 
  {}

  string m_name;
  int    m_id;
};

int main()
{
  std::vector<product> p;

  for (int i=0; i<10; i++) 
    p.push_back(product(" ", i));

  auto it = find_if(p.begin(), p.end(), bind(&product::m_id, _1) == 5);

  it != p.end() 
    ? cout << "found valud: " << it->m_id
    : cout << "not found\n" ;

  return 0;
}
