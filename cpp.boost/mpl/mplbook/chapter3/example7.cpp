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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter3-example7.o example7.cpp

*/

    #include <boost/mpl/int.hpp>
    #include <boost/mpl/vector.hpp>


namespace boost{namespace mpl {}}
namespace mpl = boost::mpl;



namespace shield{

template <
    class InputIterator1, class InputIterator2
  , class OutputIterator, class BinaryOperation
>
void transform(
    InputIterator1 start1, InputIterator2 finish1
  , InputIterator2 start2
  , OutputIterator result, BinaryOperation func);

}

