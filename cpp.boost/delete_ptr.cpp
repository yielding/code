#include <iostream>
#include <algorithm>
#include <list>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/construct.hpp>
#include <boost/lambda/bind.hpp>

using namespace std;
using namespace boost;
using namespace boost::lambda;

class printer  // /Users
{
public:
  printer(int id) 
    :m_id(id)
  {}

  virtual ~printer() 
  {
    cout << "printer " << m_id << " is deleted\n";
  }

  void print(int i) 
  {
    cout << i << endl;
  }

  bool has_id(int id)
  {
    return id == m_id;
  }

private:
  int m_id;
};

int main(int argc, char const* argv[])
{
  printer *p1 = new printer(10);
  printer *p2 = new printer(20);

  list<printer*> ps;
  typedef list <printer*>::iterator It;

  ps.push_back(p1);
  ps.push_back(p2);

  It it = find_if(ps.begin(), ps.end(), bind(&printer::has_id, _1, 10));
  if (it != ps.end())
  {
    ps.erase(it);
    delete *it;
  }

  cout << "printer sizee: " << ps.size() << endl;
  
  for_each(ps.begin(), ps.end(), bind(delete_ptr(), _1));

  cout << "printer sizee: " << ps.size() << endl;

  return 0;
}
