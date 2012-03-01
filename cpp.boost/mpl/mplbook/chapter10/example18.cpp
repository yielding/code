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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -I..\..\boost_prerelease -c /Fo%TEMP%\metaprogram-chapter10-example18.o example18.cpp

*/


      struct slew_tag; struct name_tag; struct score_tag; struct nil_t;
      #define TAG(name)                                                 \
         template <class U>                                             \
         named_params<name##_tag,U const, next> name(U const&); 

      template <class T, class U, class V>
      struct named_params
      {
          typedef named_params next;
          TAG(slew)
          TAG(name)
          TAG(score)
      };

      typedef nil_t next;
      TAG(slew)
      TAG(name)
      TAG(score)

      template <class T>
      int assert_same_type(T,T);

      template <class T>
      T f(T);

void foo()
{
    int test = assert_same_type(
        f(slew(.799).name("z"))
      , named_params<
            name_tag, char const[2]        // .name("z")
          , named_params<
                slew_tag, double const     // slew(.799)
              , nil_t
            >
        >
        ());
}

