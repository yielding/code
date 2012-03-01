/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It FAILS to compile, as expected, with Microsoft Visual C++ 6.0 SP6
    using the following command: 

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter3-example36.o example36.cpp

*/

    #include <boost/mpl/int.hpp>
    #include <boost/mpl/vector.hpp>


namespace boost{namespace mpl {}}
namespace mpl = boost::mpl;

#include <boost/static_assert.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/minus.hpp>

#include<boost/mpl/placeholders.hpp>
using namespace mpl::placeholders;

#include <boost/type_traits/add_pointer.hpp>
    #include <boost/static_assert.hpp>
    #include <boost/type_traits/is_same.hpp>
#include <boost/mpl/lambda.hpp>
#include <boost/mpl/apply.hpp>
#include <boost/mpl/plus.hpp>
#include <boost/mpl/multiplies.hpp>

#include <boost/mpl/vector_c.hpp>
namespace {

typedef mpl::vector_c<int,1,0,0,0,0,0,0> mass;
typedef mpl::vector_c<int,0,1,0,0,0,0,0> length; // or position 
typedef mpl::vector_c<int,0,0,1,0,0,0,0> time;
typedef mpl::vector_c<int,0,0,0,1,0,0,0> charge;
typedef mpl::vector_c<int,0,0,0,0,1,0,0> temperature;
typedef mpl::vector_c<int,0,0,0,0,0,1,0> intensity;
typedef mpl::vector_c<int,0,0,0,0,0,0,1> angle;

}



// base dimension:        m l  t ... 
typedef mpl::vector_c<int,0,1,-1,0,0,0,0> velocity;     // l/t
typedef mpl::vector_c<int,0,1,-2,0,0,0,0> acceleration; // l/(t2)
typedef mpl::vector_c<int,1,1,-1,0,0,0,0> momentum;     // ml/t
typedef mpl::vector_c<int,1,1,-2,0,0,0,0> force;        // ml/(t2)


typedef mpl::vector_c<int,0,0,0,0,0,0,0> scalar;



   #include <boost/mpl/equal.hpp>

   template <class T, class Dimensions>
   struct quantity
   {
       explicit quantity(T x)
          : m_value(x)
       {}

       T value() const { return m_value; }
   

template <class OtherDimensions>
quantity(quantity<T,OtherDimensions> const& rhs)
  : m_value(rhs.value())
{
    BOOST_STATIC_ASSERT((
       mpl::equal<Dimensions,OtherDimensions>::type::value
    ));
}

    private:
       T m_value;
   };



quantity<float,length> l( 1.0f );
quantity<float,mass> m( 2.0f );


template <class T, class D>
quantity<T,D>
operator+(quantity<T,D> x, quantity<T,D> y)
{
  return quantity<T,D>(x.value() + y.value());
}

template <class T, class D>
quantity<T,D>
operator-(quantity<T,D> x, quantity<T,D> y)
{
  return quantity<T,D>(x.value() - y.value());
}

void test1() {



quantity<float,length> len1( 1.0f );
quantity<float,length> len2( 2.0f );

len1 = len1 + len2;   // OK

}



template <class Sequence1, class Sequence2, class BinaryOperation>
struct transform;  // returns a Sequence


#include <boost/static_assert.hpp>
#include <boost/mpl/plus.hpp>
#include <boost/mpl/int.hpp>
namespace mpl = boost::mpl;

BOOST_STATIC_ASSERT(( 
    mpl::plus<
        mpl::int_<2>
      , mpl::int_<3>
    >::type::value == 5
));


struct plus_f
{
    template <class T1, class T2>
    struct apply
    {
       typedef typename mpl::plus<T1,T2>::type type;
    };
};


template <class T, class D1, class D2>
quantity< 
    T
  , typename mpl::transform<D1,D2,plus_f>::type  // new dimensions
>
operator*(quantity<T,D1> x, quantity<T,D2> y)
{
    typedef typename mpl::transform<D1,D2,plus_f>::type dim;
    return quantity<T,dim>( x.value() * y.value() );
}



template <class D1,class D2>

typename mpl::transform<D1,D2, mpl::minus<_1,_2> >::type

fff(D1,D2);



template <class D1, class D2>
struct divide_dimensions
{
    typedef typename
      mpl::transform<D1,D2,mpl::minus<_1,_2> >::type
    type;
};

template <class T, class D1, class D2>
quantity<T, typename divide_dimensions<D1,D2>::type>
operator/(quantity<T,D1> x, quantity<T,D2> y)
{
   return quantity<T, typename divide_dimensions<D1,D2>::type>(
      x.value() / y.value());
}



void will_fail(quantity<float,mass> m, quantity<float,acceleration> a)
{ quantity<float,force> f(m*a);

f = f + m * a;

}

