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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0  /Fe%TEMP%\metaprogram-chapter11-example16.exe example16.cpp

*/
#include <boost/mpl/fold.hpp>
#include <boost/mpl/filter_view.hpp>
#include <boost/type_traits/is_same.hpp>
#include <vector>
#include <ctime>
#include <boost/mpl/vector.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

#include <cassert>

template<
    class Transition
  , class Next
>
struct event_dispatcher
{
    typedef typename Transition::fsm_t fsm_t;
    typedef typename Transition::event event;

    static int dispatch(
        fsm_t& fsm, int state, event const& e)
    {
        if (state == Transition::current_state)
        {
            Transition::execute(fsm, e);
            return Transition::next_state;
        }
        else // move on to the next node in the chain.
        {
            return Next::dispatch(fsm, state, e);
        }
    }
};



template <class Derived> class state_machine;

struct default_event_dispatcher
{
    template<class FSM, class Event>
    static int dispatch(
        state_machine<FSM>& m, int state, Event const& e)
    {
        return m.call_no_transition(state, e);
    }
};


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

    
    friend class default_event_dispatcher;
    
    template <class Event>
    int call_no_transition(int state, Event const& e)
    {
        return static_cast<Derived*>(this)  // CRTP downcast
                   ->no_transition(state, e);
    }
    // 
public:

template<class Event>
int process_event(Event const& evt)
{
    // generate the dispatcher type.
    typedef typename generate_dispatcher<
        /*typename*/ Derived::transition_table, Event
    >::type dispatcher;

    // dispatch the event.
    this->state = dispatcher::dispatch(
        *static_cast<Derived*>(this)        // CRTP downcast
      , this->state
      , evt
    );

    // return the new state
    return this->state;
}

// ...
 protected:
    state_machine()
      : state(Derived::initial_state)
    {
    }

 private:
    int state;
// ...

// ...
 public:
    template <class Event>
    int no_transition(int state, Event const& e)
    {
        assert(false);
        return state;
    }
// ...
////
  };


template <class Event> 
struct is_same_event
{
    template <class Transition> struct apply
        : boost::is_same<Event,typename Transition::event>
    {
    };
};

template<class Table, class Event>
struct generate_dispatcher
  : mpl::fold<
        mpl::filter_view<   // select rows triggered by Event
            Table
          , is_same_event<Event>
        >
      , default_event_dispatcher
      , event_dispatcher<_2,_1> 
    >
{};



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

    //    Start     Event               Next      Action
    //  +---------+-------------------+---------+------------------+
    row < Stopped , play const        , Playing , &start_playback  >,
    row < Stopped , open_close const  , Open    , &open_drawer     >,
    //  +---------+-------------------+---------+------------------+
    row < Open    , open_close const  , Empty   , &close_drawer    >,
    //  +---------+-------------------+---------+------------------+
    row < Empty   , open_close const  , Open    , &open_drawer     >,
    row < Empty   , cd_detected const , Stopped , &store_cd_info   >,
    //  +---------+-------------------+---------+------------------+
    row < Playing , stop const        , Stopped , &stop_playback   >,
    row < Playing , pause const       , Paused  , &pause_playback  >,
    row < Playing , open_close const  , Open    , &stop_and_open   >,
    //  +---------+-------------------+---------+------------------+
    row < Paused  , play const        , Playing , &resume_playback >,
    row < Paused  , stop const        , Stopped , &stop_playback   >,
    row < Paused  , open_close const  , Open    , &stop_and_open   >
    //  +---------+-------------------+---------+------------------+

    > {};
typedef
 
event_dispatcher<
    row<Stopped, play const, Playing, &start_playback>
  , event_dispatcher<
        row<Paused, play const, Playing, &resume_playback>
      , default_event_dispatcher
    >
>
 dummy;
};

  void player::start_playback(player&, play const&){}
  void player::open_drawer(player&, open_close const&){}
  void player::close_drawer(player&, open_close const&){}
  void player::store_cd_info(player&, cd_detected const&){}
  void player::stop_playback(player&, stop const&){}
  void player::pause_playback(player&, pause const&){}
  void player::resume_playback(player&, play const&){}
  void player::stop_and_open(player&, open_close const&){}
  



int main()
{
    player p;                      // An instance of the FSM

    p.process_event(open_close()); // user opens CD player
    p.process_event(open_close()); // inserts CD and closes
    p.process_event(               // CD is detected
        cd_detected(
             "louie, louie"
           , std::vector<clock_t>( /* track lengths */ )
        )
    );
    p.process_event(play());       // etc.
    p.process_event(pause());
    p.process_event(play());
    p.process_event(stop());
    return 0;
}
