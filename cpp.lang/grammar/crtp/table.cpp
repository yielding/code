#include <iostream>
#include <vector>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template<typename Record>
class Table
{
public:
  typedef vector<Record> Records;

  static Records find_all(string const& sql)
  {
    Records s;
    s.push_back(Record());

    return s;
  }

private:
  Record const& self()
  {
    return static_cast<Record const&>(*this);
  }
};

class RFSignal: public Table<RFSignal>
{
public:
  RFSignal() { id = 10; }
  int id;
};

class Student: public Table<Student>
{
public:
  Student() { id = 20; }
  int id;
};
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main()
{
  RFSignal::Records rr1;
  rr1 = RFSignal::find_all("select"); 
  cout << rr1[0].id << endl;

  Student::Records ss1;
  ss1 = Student::find_all("select"); 
  cout << ss1[0].id << endl;

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
