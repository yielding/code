#include "stdafx.h"
#include "polygon.hpp"

#include <cmath>
#include <iostream>

class triangle : public polygon 
{
public:
  virtual double area() const 
  {
    return side_length_ * side_length_ * sqrt(3.0) / 2;
  }

  virtual ~triangle()
  {
    std::cout << "triangle is deleted\n";
  }
};

// the class factories
extern "C" 
{
  POLYGON_API polygon* create() 
  {
    return new triangle;
  }

  POLYGON_API void destroy(polygon* p) 
  {
    if (p != NULL)
    {
      delete p;
      p = NULL;
    }
  }
}
