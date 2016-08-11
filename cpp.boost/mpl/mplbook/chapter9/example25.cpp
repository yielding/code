/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It was built successfully with Microsoft Visual C++ 6.0 SP6
    using the following command: 

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0  /Fe%TEMP%\metaprogram-chapter9-example25.exe example25.cpp

*/
#include <memory>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


class float_function
{
 private:
    struct impl
    {
        virtual ~impl() {}
        virtual impl* clone() const = 0;
        virtual float operator()(float) const = 0;
    };

    template <class F>
    struct wrapper : impl
    {
        explicit wrapper(F const& f)
          : f(f) {}

        impl* clone() const
        {
            return new wrapper<F>(this->f); // delegate
        }

        float operator()(float x) const
        {
            return f(x);                    // Delegate 
        }

     private:
        F f;
    };

 public:
    // implicit conversion from F
    template <class F>
    float_function(F const& f)
      : pimpl(new wrapper<F>(f)) {}

    float_function(float_function const& rhs)
      : pimpl(rhs.pimpl->clone()) {}

    float_function& operator=(float_function const& rhs)
    {
        this->pimpl = std::auto_ptr<impl>(rhs.pimpl->clone());
        return *this;
    }

    float operator()(float x) const
    {
        return (*this->pimpl)(x);
    }

 private:
    std::auto_ptr<impl> pimpl;
};

  #include <iostream>
  double foo(double x)
  { 
      std::cout << x << std::endl; 
      return 2*x;
  }

  int main()
  {
      float_function f(&foo);
      std::cout << f(3.14) << std::endl;
      return 0;
  }
  




typedef int pixel_map;

class screensaver
{
 public:
    explicit screensaver(float_function f)
      : get_seed(f)
    {}

    pixel_map next_screen()
    {
        float center_pixel_brightness = 3.14;
        float seed = this->get_seed(center_pixel_brightness);
        return 0;
    }

 private:
    float_function get_seed;
    // ...
};
