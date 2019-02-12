#include <boost/function.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/lambda.hpp>

#include <list>
#include <iostream>

using namespace std;
using namespace boost;
using namespace boost::lambda;

/*
   fucntion object를 사용했을 때 확실하게 좋아지는것은
   observer와 subject(or observable)의 cross reference가 이루어지는 해더의 문제가 
   사라진다는것. Why ?

   class Observer {
   public:
   void OnUpdateOf(Subject*) = 0;
   };
   가 더이상 필요 없기 때문
   */

class Subject 
{
public:
  virtual void Attach (function<void (Subject*)>o) { objs_.push_back(o); }
  virtual void Detach (function<void (Subject*)>o) {                     };
  virtual void Notify() 
  {
    for (list<function<void (Subject*)> >::iterator i = objs_.begin(); i!=objs_.end(); ++i)
      (*i)(this);
  }
private:
  list< function<void (Subject *)> > objs_;
};

class Observer 
{
public:
  virtual void OnUpdateOf(Subject*) = 0;
};

class ClockTimer: public Subject 
{
public:
  void Tick() 
  {
    Notify();
  }
};

// Observer가 있어도 그만 없어도 그만인 것을 보여주려함: 상속하는 경우
//
class DigitalClock: public Observer { // still works
public:
  DigitalClock( ClockTimer* t ) : timer_(t)
  { 
    // timer_->Attach( bind1st( mem_fun( &DigitalClock::OnUpdateOf ), this ) ); 
    timer_->Attach( bind( &DigitalClock::OnUpdateOf, this, _1) );
  }

  ~DigitalClock()
  { timer_->Detach( bind1st( mem_fun( &DigitalClock::OnUpdateOf ), this ) ); }

  void OnUpdateOf( Subject* timer ) 
  { 
    cout << "DigitalClock::OnUpdateOf is called" << endl;
  }
private:
  ClockTimer* timer_;
};

// 상속 안하는 경우
class Clock2 {
public:
  Clock2(ClockTimer* t ) : timer_(t)
  { timer_->Attach( bind1st( mem_fun( &Clock2::Ticker ), this ) ); }

  ~Clock2()
  { timer_->Detach( bind1st( mem_fun( &Clock2::Ticker ), this ) ); }
  void Ticker( Subject* timer ) { 
    cout << "Clock2::Ticker is called" << endl;
  }
private:
  ClockTimer* timer_;
};

class Tock {
public:
  void operator()( ClockTimer* timer ) // note, not exact match!
  { 
    cout << " Tock::operator() is called" << endl;
  }
};

void TickMe( Subject* timer ) { 
  cout << " TickMe Function" << endl;
}

int main(int argc, char* argv[])
{
  ClockTimer timer;

  DigitalClock digital(&timer);
  Clock2       c2(&timer);

  timer.Attach(&TickMe);

  timer.Tick();
  return 0;
}
