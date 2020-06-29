#include <iostream>

#include <boost/format.hpp>

using namespace std;
using namespace boost;

////////////////////////////////////////////////////////////////////////////////
//
// Intent: Decouple an abstraction from its implementation so that the two
//         can vary independently.
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
//
// Implementation
//
////////////////////////////////////////////////////////////////////////////////
// implementor
class drawing_api 
{
public:
  virtual void draw_circle(double x, double y, double radius) = 0;
};

// concrete implementor 1
class cocoa_api: public drawing_api 
{
public:
  virtual void draw_circle(double x, double y, double radius)
  {
    cout << str(format("Cocoa.circle at (%f, %f, %f)\n") % x % y % radius);
  }
};

// concrete implementor 2
class carbon_api: public drawing_api 
{
public:
  virtual void draw_circle(double x, double y, double radius)
  {
    cout << str(format("Carbon.circle at (%f, %f, %f)\n") % x % y % radius);
  }
};

////////////////////////////////////////////////////////////////////////////////
//
// Abstraction
//
////////////////////////////////////////////////////////////////////////////////
class shape
{
public:
  shape(drawing_api* api): m_api(api)
  {}

  virtual void draw() = 0;
  virtual void resize_by_percentage(double pct) = 0;

protected:
  drawing_api* m_api;
};

// refiend abstraction
class circle: public shape
{
public:
  circle(double x, double y, double radius, drawing_api* api)
    : shape(api)
  {
    m_x = x;
    m_y = y;
    m_radius = radius;
  }

  virtual void draw()
  {
    m_api->draw_circle(m_x, m_y, m_radius);
  }

  virtual void resize_by_percentage(double pct)
  {
    m_radius *= pct;
  }

private:
  double m_x, m_y;
  double m_radius;
};

////////////////////////////////////////////////////////////////////////////////
//
// It also let us make very orthogonal design.
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, const char *argv[])
{
  cocoa_api cocoa;
  carbon_api carbon;
  circle s1(1, 2, 3, &cocoa);
  circle s2(2, 3,43, &carbon);

  s1.draw();
  s2.draw();
  
  return 0;
}
