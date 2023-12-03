#include <iostream>

using namespace std;

class clonable_base
{
public:
  clonable_base* clone() const
  {
    return do_clone();
  }

  virtual ~clonable_base()
  {
    cout << "destructor of clonable_base\n";
  }

private:
  virtual clonable_base* do_clone() const = 0;
};

template<typename Derived, typename Base = clonable_base>
class clonable_impl: Base
{
public:
  Derived* clone() const
  {
    return static_cast<Derived*>(this->do_clone());
  }

  virtual ~clonable_impl()
  {
    cout << "destructor of clonable_impl\n";
  }

private:
  virtual clonable_impl* do_clone() const
  {
    return new Derived(static_cast<Derived const&>(*this));
  }
};

class test: public clonable_impl<test>
{
public:
  virtual ~test()
  {
    cout << "destructor of test\n";
  }

  void print()
  {
    cout << "I'm test" << endl;
  }
};

int main(int argc, char const* argv[])
{
  test* t1 = new test;
  test* t2 = t1->clone();

  t1->print();
  t2->print();

  delete t1;
  delete t2;

  return 0;
}
