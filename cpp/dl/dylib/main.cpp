#include "stdafx.h"

#include "polygon.hpp"
#include "dylib.h"

#include <iostream>

#ifdef POSIX
#define DLL_NAME "./triangle.dylib"
#else
#define DLL_NAME "./polygon.dll"
#endif

int main() 
{
  using std::cout;
  using std::cerr;
  using std::endl;

  typedef sys::dylib<polygon, void> dylib_t;
  dylib_t dylib;
  
  if (!dylib.open(DLL_NAME))
  {
    cout << dylib.last_error() << endl;
    return 1;
  }

  dylib_t::creator_t* creator = dylib.get_creator("create");
  if (creator == NULL)
  {
    cout << dylib.last_error() << endl;
    return 1;
  }

  dylib_t::destroyer_t* destroyer = dylib.get_destroyer("destroy");
  if (destroyer == NULL)
  {
    cout << dylib.last_error() << endl;
    return 1;
  }

  void* p = NULL;
  polygon* poly = creator(p);
  poly->set_side_length(7);
  cout << "The area is: " << poly->area() << '\n';
  destroyer(poly);

  return 0;
}
