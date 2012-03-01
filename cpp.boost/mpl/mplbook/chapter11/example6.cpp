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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter11-example6.o example6.cpp

*/

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


    struct tag {};

    template <class T>
    tag operator,(tag,T);

    template <class T>
    tag operator|(tag,T);

    struct transition_table
    {
        transition_table& operator[](tag);
    };
    struct fsm
    {
        typedef tag play;
typedef tag open_close;
typedef tag cd_detected;
typedef tag stop;
typedef tag pause;
        static void start_playback(fsm&, play const&);
        static void open_drawer(fsm&, open_close const&);
        static void close_drawer(fsm&, open_close const&);
        static void store_cd_info(fsm&, cd_detected const&);
        static void stop_playback(fsm&, stop const&);
        static void pause_playback(fsm&, pause const&);
        static void resume_playback(fsm&, play const&);
        static void stop_and_open(fsm&, open_close const&);
    };
    enum { Stopped, Playing, Open, Empty, Paused };
    
namespace {typedef tag play;
typedef tag open_close;
typedef tag cd_detected;
typedef tag stop;
typedef tag pause;}


transition_table STT;


        template <int From,class Event,int To,void (*f)(fsm&, tag const&)> 
        struct transition {};
        template <int From,class Event,int To,void (*f)(fsm&, tag const&)> 
        struct row {};
        template <class L1,class L2,class L3=int,class L4=int,class L5=int,class L6=int>
        struct wrap {};
        void test() {

        wrap<

//    Current   Event         Next      Action
//     State                  State             
//  +---------+------------+---------+-----------------------+
row < Stopped , play       , Playing , &fsm::start_playback  >,
row < Stopped , open_close , Open    , &fsm::open_drawer     >,
//  +---------+------------+---------+-----------------------+
row < Paused  , play       , Playing , &fsm::resume_playback >,
row < Paused  , stop       , Stopped , &fsm::stop_playback   >,
row < Paused  , open_close , Open    , &fsm::stop_and_open   >,
//  +---------+------------+---------+-----------------------+

int> x


;}

