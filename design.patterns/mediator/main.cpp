#include <print>

using namespace std;

class IMediator
{ 
public:
  virtual void notify(int event) = 0;
};

class ConcreteMediator : public IMediator
{
public:
  void notify(int event) override
  {
    print("Event: {}\n", event);
  }
};

class Component
{
public:
  Component(IMediator& mediator) : mediator(mediator) {}

  void doSomething()
  {
    mediator.notify(1);
  }

private:
  IMediator& mediator;
};  

class Component2
{
public:
  Component2(IMediator& mediator) : mediator(mediator) {}

  void doSomething()
  {
    mediator.notify(2);
  }

private:
  IMediator& mediator;
};

int main(int argc, char* argv[])
{
  ConcreteMediator mediator;
  Component component(mediator);
  Component2 component2(mediator);

  component.doSomething();
  component2.doSomething();

  return 0;
}
