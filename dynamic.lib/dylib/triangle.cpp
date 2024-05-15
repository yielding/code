#include "polygon.hpp"

#include <cmath>

class triangle : public polygon 
{
public:
  virtual double area() const 
  {
    return side_length_ * side_length_ * sqrt(3) / 2;
  }
};

// the class factories
extern "C" 
{
  polygon* create(void *)
  {
    return new triangle;
  }

  void destroy(polygon* p) 
  {
    delete p;
  }
}
