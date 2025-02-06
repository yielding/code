#include <print>
#include <vector>

using namespace std;

class ControlCenter;

class AirCraft 
{
public:
  AirCraft(ControlCenter& c) : _center(c) {}

  virtual int id() = 0;

  virtual void message_from_center(string const& msg)
  {
    print("{}\n", msg);
  }

protected: 
  ControlCenter& _center; 
};

class ControlCenter
{ 
public:
  virtual void notify(AirCraft& c, int event) = 0;
};

class Airport: public ControlCenter
{
public:
  void notify(AirCraft& c, int event) override
  {
    print("AirCraft(id: {}) took off with {}\n", c.id(), event);

    for (auto plain: _aircrafts)
    {
      if (c.id() != plain->id())
        plain->message_from_center(format("AirCraft {}, be careful", plain->id()));
    }
  }

  void add(AirCraft* c) 
  {
    _aircrafts.push_back(c);
  }

private:
  vector<AirCraft*> _aircrafts;
};

class Drone: public AirCraft
{ 
public:
  Drone(ControlCenter& center) : AirCraft(center) {}

  int id() override { return 1; }

  void take_off() { _center.notify(*this, 1); } 
};  

class AirPlane : public AirCraft
{
public:
  AirPlane(ControlCenter& center) : AirCraft(center) {}

  int id() override { return 2; }

  void take_off() 
  {
    _center.notify(*this, 2); 
  }
};

int main(int argc, char* argv[])
{
  Airport airport;
  Drone c1(airport);
  AirPlane c2(airport);

  airport.add(&c1);
  airport.add(&c2);

  c1.take_off();
  c2.take_off();

  return 0;
}
