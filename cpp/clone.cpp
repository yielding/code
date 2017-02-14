#ifndef __CLONE_HPP__
#define __CLONE_HPP__

#include <memory>
#include <iostream>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
//
////////////////////////////////////////////////////////////////////////////////
class clonable_base
{
public:
  clonable_base* clone() const
  {
    return do_clone();
  }

  virtual ~clonable_base()
  {
    // cout << "destructor of clonable_base\n";
  }

private: virtual clonable_base* do_clone() const = 0;
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
    // cout << "destructor of clonable_impl\n";
  }

private:
  virtual clonable_impl* do_clone() const
  {
    return new Derived(static_cast<Derived const&>(*this));
  }
};

#endif

////////////////////////////////////////////////////////////////////////////////
//
// usage
//
////////////////////////////////////////////////////////////////////////////////

class test: public clonable_impl<test>
{
public:
  test()
  {
    m_id = 0;
  }

  virtual ~test()
  {
    cout << "destructor of test with " << m_id << "\n";
  }

  void set_id(int id)
  {
    m_id = id;
  }

  void print()
  {
    cout << "I'm test with id: " << m_id << endl;
  }

private:
  int m_id;
};

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
int main(int argc, char const* argv[])
{
  auto sp_test1 = shared_ptr<test>(new test);
  auto sp_test2 = shared_ptr<test>(new test(*sp_test1));

  sp_test2->set_id(10);
  sp_test1->print();
  sp_test2->print();

  auto test3 = sp_test2->clone();
  test3->set_id(20);
  test3->print();
  delete test3;

  return 0;
}
