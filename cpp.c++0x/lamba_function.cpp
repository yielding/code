#include <iostream>
#include <boost/shared_ptr.hpp>

using namespace std;
using namespace boost;

class Object
{
public:
  Object()
  {
    m_x = 0;
  }

  virtual ~Object()
  {}

  int increment(int x)
  {
    m_x += x;
    return m_x;
  }

  int result()
  {
    return m_x;
  }

private:
  int m_x;
};

template <typename T> 
class LogicalReader
{
public:
  LogicalReader();
  virtual ~LogicalReader();

  void exec()
  {
    func();
  }

private:
  T func;
};

class Closure
{
public:
  Closure() {}
  virtual ~Closure() { }
  virtual void Execute() = 0;
};

template <typename FuncT> 
class LambdaClosure: public Closure
{
public:
  LambdaClosure(FuncT& f)
    : m_exec(f) 
  {}

  virtual ~LambdaClosure() {}

  virtual void Execute() 
  {
    m_exec();
  }

private:
  FuncT m_exec;
};

template <typename FuncT> 
boost::shared_ptr<Closure> 
make_lambda_closure(FuncT f)
{
  return boost::shared_ptr<Closure>(new LambdaClosure<FuncT>(f));
}

int main(int argc, char const* argv[])
{
  Object o;

  int  x = 5;
  auto f = make_lambda_closure([&o, x]() { o.increment(x); });
  f->Execute();
  f->Execute();
  cout << o.result();
  return 0;
}
