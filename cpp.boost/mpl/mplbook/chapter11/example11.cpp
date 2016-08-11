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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter11-example11.o example11.cpp

*/

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

#include <cassert>


    template<class Table, class Event>
    struct generate_dispatcher;

template<class Derived>
class state_machine
{
    // ...
 protected:
    template<
        int CurrentState
      , class Event
      , int NextState
      , void (Derived::*action)(Event const&)
    >
    struct row
    {
        // for later use by our metaprogram
        enum { current_state = CurrentState };
        enum { next_state = NextState };
        typedef Event event;
        typedef Derived fsm_t;

        // do the transition action.
        static void execute(Derived& fsm, Event const& e)
        {
            (fsm.*action)(e);
        }
    };

    ////
    protected:
    int state;
    template <class E>
    static int no_transition(int, E const&);
    ////
  };
