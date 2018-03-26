#include <iostream>
#include <functional>

using namespace std;

class Messagener
{
public:
  typedef function<bool(void)> ForwardF;

  template <typename Func>
  void forward_message(Func f) { m_forward_f = f; }

public:
  Messagener() { }

  void fire()  { m_forward_f(); }

private:
  ForwardF m_forward_f;
};

class Tester
{
public:
  bool test(int a)
  {
    cout << "Tester::test() is called" << endl;

    return false;
  }
};

int main(int argc, const char *argv[])
{
  Messagener m;
  Tester t;

  cout << "1\n";
  string msg = "leech babo";
  m.forward_message([&msg]() {

  });

  m.fire();

  return 0;
}
