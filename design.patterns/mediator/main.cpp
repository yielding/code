#include <print>

using namespace std;

class IMediator;

class Component 
{
public:
  Component(IMediator& mediator) : mediator(mediator) {}
  virtual int id() = 0;

protected:
  IMediator& mediator;
};

class IMediator
{ 
public:
  virtual void notify(Component& c, int event) = 0;
};

class ConcreteMediator : public IMediator
{
public:
  void notify(Component& c, int event) override
  {
    print("Component id: {}, Event: {}\n", c.id(), event);
  }
};

class Component1 : public Component
{
public:
  Component1(IMediator& mediator) : Component(mediator) {}
  int id() override { return 1; }

  void doIt() { mediator.notify(*this, 1); }

};  

class Component2 : public Component
{
public:
  Component2(IMediator& mediator) : Component(mediator) {}

  int id() override { return 2; }

  void doIt() { mediator.notify(*this, 2); }
};

int main(int argc, char* argv[])
{
  ConcreteMediator mediator;
  Component1 c1(mediator);
  Component2 c2(mediator);

  c1.doIt();
  c2.doIt();

  return 0;
}
