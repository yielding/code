#ifndef __CLONE_HPP__
#define __CLONE_HPP__

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
  {}

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

  virtual ~clonable_impl() {}

private:
  virtual clonable_impl* do_clone() const
  {
    return new Derived(static_cast<Derived const&>(*this));
  }
};

#endif

/*
////////////////////////////////////////////////////////////////////////////////
//
// usage
//
////////////////////////////////////////////////////////////////////////////////
#include <boost/shared_ptr.hpp>

class test: public clonable_impl<test>
{
public:
virtual ~test()
{
  std::cout << "destructor of test\n";
}

void print()
{
std::cout << "I'm test" << endl;
}
};

int main(int argc, char const* argv[])
{
  boost::shared_ptr<test> sp_test1(new test);
  boost::shared_ptr<test> sp_test2(sp_test1);
  sp_test1->print();
  sp_test2->print();

  return 0;
}
*/
