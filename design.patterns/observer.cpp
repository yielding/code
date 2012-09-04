#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <list>

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
class subject
{
public:
  typedef boost::function<void (subject*)> F;

public:
  virtual ~subject() {}

  virtual void attach (F o) { m_objs.push_back(o); }
  virtual void detach (F o) { /*m_objs.clear(); */ }
  virtual void notify()  
  {
    auto end = m_objs.end();
    for (auto i=m_objs.begin(); i!=end; ++i)
      (*i)(this);
  }   

private:
  std::list<F> m_objs;
};

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
#if 1

#include <iostream>

class ClockTimer: public subject 
{
public:
  void tick() 
  {
    notify();
  }
};

class DigitalClock
{
public:
  DigitalClock (ClockTimer* t)
    : m_timer(t)
  {
    m_timer->attach(boost::bind(&DigitalClock::update_of, this, _1));
  }
  
  virtual ~DigitalClock ()
  {
    m_timer->detach(boost::bind(&DigitalClock::update_of, this, _1));
  }
  
  void update_of(subject* timer)
  {
    std::cout << "DigitalClock::OnUpdateOf is called\n";
  }

private:

  ClockTimer* m_timer;
};

class AnyClock
{
public:
  void update_of1()
  {
    std::cout << "AnyClock::update_of1 is called\n";
  }
  
  void update_of2(subject* timer)
  {
    std::cout << "AnyClock::update_of2 is called\n";
  }
  
};

void TickMe(subject* timer)
{
  std::cout << "TickMe Function\n";
  if (timer == NULL)
	std::cout << "timer is null\n";
}

int main()
{
  ClockTimer timer;

  DigitalClock digital(&timer);
  AnyClock any;
  
  timer.attach(&TickMe);
  timer.attach(boost::bind(&AnyClock::update_of1,  &any));
  timer.attach(boost::bind(&AnyClock::update_of2,  &any, &timer));
  
  timer.tick();
  return 0;
}

#endif
