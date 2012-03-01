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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example29.o example29.cpp

*/

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


#include <cassert>

template <class T> 
struct ordered
{
    bool operator>(T const& rhs) const
    {
        // locate full derived object
        T const& self = static_cast<T const&>(*this);
        return rhs < self;
    }
};

class Int
  : public ordered<Int>
{
 public:
    explicit Int(int x)
      : value(x) {}

    bool operator<(Int const& rhs) const
    {
        return this->value < rhs.value;
    }

    int value;
};

int main()
{
    assert(Int(4) < Int(6));
    assert(Int(9) > Int(6));
    return 0;
}



namespace redefine_Int {

class Int : public ordered<Int> { public: bool operator<(Int) const; };
class bogus : public ordered<Int> {};
bool crash = bogus() > Int();

}

