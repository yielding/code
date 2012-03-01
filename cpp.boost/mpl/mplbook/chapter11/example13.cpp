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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter11-example13.o example13.cpp

*/
#include <vector>
#include <ctime>
#include <boost/mpl/vector.hpp>

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
      , void (*action)(Derived&, Event const&)
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
            (*action)(fsm, e);
        }
    };

    ////
    protected:
    int state;
    template <class E>
    static int no_transition(int, E const&);
    ////
  };



  struct play {};
  struct open_close {};
  struct cd_detected { 
    cd_detected(char const*, std::vector<clock_t> const&) {}
  };
  #ifdef __GNUC__ // in which pause seems to have a predefined meaning
  # define pause pause_
  #endif
  struct pause {};
  struct stop {};
  

// concrete FSM implementation 
class player : public state_machine<player>
{
 public:
    // the list of FSM states
    enum states {
        Empty, Open, Stopped, Playing, Paused
      , initial_state = Empty
    };

    
    #ifdef __MWERKS__
     public: // Codewarrior bug workaround.  Tested at 0x3202
    #endif
    
    static void start_playback(player&, play const&);
    static void open_drawer(player&, open_close const&);
    static void close_drawer(player&, open_close const&);
    static void store_cd_info(player&, cd_detected const&);
    static void stop_playback(player&, stop const&);
    static void pause_playback(player&, pause const&);
    static void resume_playback(player&, play const&);
    static void stop_and_open(player&, open_close const&);

    
    #ifdef __MWERKS__
       private:
    #endif 
          friend class state_machine<player>;
    typedef player p; // makes transition table cleaner

    // transition table
    struct transition_table : mpl::vector11<

    //    Start     Event              Next      Action
    //  +---------+------------------+---------+------------------+
    row < Stopped , play const       , Playing , &start_playback  >,
    row < Stopped , open_close const , Open    , &open_drawer     >,
    //  +---------+------------------+---------+------------------+
    row < Open    , open_close const , Empty   , &close_drawer    >,
    //  +---------+------------------+---------+------------------+
    row < Empty   , open_close const , Open    , &open_drawer     >,
    row < Empty   , cd_detected const, Stopped , &store_cd_info   >,
    //  +---------+------------------+---------+------------------+
    row < Playing , stop const       , Stopped , &stop_playback   >,
    row < Playing , pause const      , Paused  , &pause_playback  >,
    row < Playing , open_close const , Open    , &stop_and_open   >,
    //  +---------+------------------+---------+------------------+
    row < Paused  , play const       , Playing , &resume_playback >,
    row < Paused  , stop const       , Stopped , &stop_playback   >,
    row < Paused  , open_close const , Open    , &stop_and_open   >
    //  +---------+------------------+---------+------------------+

    > {};
////

// "play" event processor
void process_event(play const& e)
{
    this->state = case_Stopped(e);
}

int case_Stopped(play const& e)
{
    if (this->state == Stopped)
    {
       this->start_playback(*this, e);
       return Playing;
    }
    else return this->case_Paused(e);
}

int case_Paused(play const& e)
{
    if (this->state == Paused)
    {
       this->resume_playback(*this, e);
       return Playing;
    }
    else return this->case_default(e);
}

int case_default(play const& e)
{ 
    return this->no_transition(this->state, e);
}
};

  void player::start_playback(player&, play const&){}
  void player::open_drawer(player&, open_close const&){}
  void player::close_drawer(player&, open_close const&){}
  void player::store_cd_info(player&, cd_detected const&){}
  void player::stop_playback(player&, stop const&){}
  void player::pause_playback(player&, pause const&){}
  void player::resume_playback(player&, play const&){}
  void player::stop_and_open(player&, open_close const&){}
  

