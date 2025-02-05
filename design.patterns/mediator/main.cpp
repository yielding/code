#include <print>
#include <vector>

using namespace std;

class ControlCenter;

class AirCraft 
{
public:
  AirCraft(ControlCenter& c) : _center(c) {}
  virtual int id() = 0;

protected: 
  ControlCenter& _center; 
};

class ControlCenter
{ 
public:
  virtual void notify(AirCraft& c, int event) = 0;
};

class Airport : public ControlCenter
{
public:
  void notify(AirCraft& c, int event) override
  {
    for (auto plain: _aircrafts)
    {
      if (c.id() == plain->id())
         print("Drone id: {}, Event: {}\n", c.id(), event);
    }
  }

  void add(AirCraft* c) 
  {
    _aircrafts.push_back(c);
  }

private:
  vector<AirCraft*> _aircrafts;
};

class Drone : public AirCraft
{ 
public:
  Drone(ControlCenter& center) : AirCraft(center) 
  {}

  int id() override 
  { 
    return 1; 
  }

  void take_off() 
  { 
    _center.notify(*this, 1);
  } };  

class AirPlane : public AirCraft
{
public:
  AirPlane(ControlCenter& center) : AirCraft(center) 
  {}

  int id() override { return 2; }

  void take_off() 
  {
    _center.notify(*this, 2); 
  }
};

int main(int argc, char* argv[])
{
  Airport center;
  Drone c1(center);
  AirPlane c2(center);

  center.add(&c1);
  center.add(&c2);

  c1.take_off();
  c2.take_off();

  return 0;
}
