#ifndef POLYGON_HPP
#define POLYGON_HPP

#ifdef WIN32
#ifdef POLYGON_EXPORTS
#define POLYGON_API __declspec(dllexport)
#else
#define POLYGON_API __declspec(dllimport)
#endif
#else
#define POLYGON_API
#endif

class POLYGON_API polygon 
{
protected:
  double side_length_;

public:
  polygon()
    : side_length_(0) 
  {}

  virtual ~polygon() 
  {}

  virtual void set_side_length(double side_length) 
  {
    side_length_ = side_length;
  }

  virtual double area() const = 0;
};

// the types of the class factories
// typedef polygon* create_t();
// typedef void destroy_t(polygon*);

#endif
