#include <iostream>

using namespace std;

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
class Shape
{
public:
  Shape() {}

  virtual ~Shape() {}

  virtual void   draw_to(ostream&) = 0;
  virtual double area() = 0;
  virtual bool   affine_transformable() { return false; }
};

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
class Circle: public Shape
{
public:
  Circle(int radius)
    : _radius(radius)
  {
    // _radius = radius
  }

  virtual ~Circle()
  {}

  virtual void draw_to(ostream& stream)
  {
  }

  virtual double area()
  {
    return 3.14 * _radius * _radius;
  }

private:
  int _radius;
};

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
class Square: public Shape
{
public:
  Square(int width)
    :_width(width)
  {}

  virtual ~Square()
  {}

  virtual void draw_to(ostream& stream)
  {}

  virtual double area()
  {
    return _width * _width;
  }

  virtual bool affine_transformable() { return true; }

protected:     
  int _width;
};

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
class Rectangle: public Square
{
public:
  Rectangle(int width, int height)
    : Square(width), _height(height)
  {}

  virtual ~Rectangle()
  {}

  virtual void draw_to(ostream& d)
  {}

  virtual double area()
  {
    return _width * _height;
  }

private:
  int _height;
};

class Rectangle2: public Shape
{
public:
  Rectangle2(int width, int height)
    :_width(width), _height(height)
  {}

  virtual ~Rectangle2()
  {}

  virtual void draw_to(ostream& d)
  {}

  virtual double area()
  {
    return _width * _height;
  }

private:
  int _width;
  int _height;
};

//////////////////////////////////////////////////////////////////////////////
//
// IOC (Inversion Of Control), polymorphic function
//
//////////////////////////////////////////////////////////////////////////////
double complex_algoritm_based_on(Shape& s)
{
  double area = 0;
  if (s.affine_transformable())
  {
    area = s.area() * 1243;
    //....
  }
  else
  {
    area = s.area() * 7788;
  }

  return area;
}

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
int main(int argc, const char *argv[])
{
  Rectangle  r1(10, 20);
  Rectangle2 r2(10, 20);
  Square s(10);

  cout << "square : " << s.area()  << endl;
  cout << "rect1  : " << r1.area() << endl;      // okay
  cout << "rect2  : " << r2.area() << endl;      // okay
  
  return 0;
}
