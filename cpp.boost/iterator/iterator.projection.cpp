#include <list>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <string>
#include <boost/iterator/transform_iterator.hpp>

using namespace std;
using namespace boost;

struct personnel_record
{
  personnel_record(string n, int id) : m_name(n), m_ID(id) {}
  string m_name;
  int m_ID;
};

struct select_name
{
  typedef personnel_record argument_type;
  typedef string const& result_type;
  const string& operator()(const personnel_record& r) const 
  {
    return r.m_name;
  }

  string& operator()(personnel_record& r) const 
  {
    return r.m_name;
  }
};

struct select_ID
{
  typedef personnel_record argument_type;
  typedef int& result_type;
  const int& operator()(const personnel_record& r) const
  {
    return r.m_ID;
  }

  int& operator()(personnel_record& r) const 
  {
    return r.m_ID;
  }
};

ostream_iterator<string> sout(cout, "\n");
ostream_iterator<int>    iout(cout, "\n");

int main(int, char*[])
{
  list<personnel_record> personnel_list;

  personnel_list.push_back(personnel_record("Barney", 13423));
  personnel_list.push_back(personnel_record("Fred", 12343));
  personnel_list.push_back(personnel_record("Wilma", 62454));
  personnel_list.push_back(personnel_record("Betty", 20490));

  // Example of using transform_iterator to print out the names in the
  // personnel list using a projection.

  transform_iterator<select_name , list<personnel_record>::iterator>
    personnel_first(personnel_list.begin()),
    personnel_last (personnel_list.end());

  copy(personnel_first, personnel_last, sout);
  cout << endl;

  // Example of using transform_iterator with const_iterators to
  // assign new ID numbers to the personnel.

  transform_iterator<select_ID, list<personnel_record>::iterator> 
    ID_first(personnel_list.begin()),
    ID_last(personnel_list.end());

  int new_id = 0;
  while (ID_first != ID_last) {
    *ID_first = new_id++;
    ++ID_first;
  }

  transform_iterator<select_ID, list<personnel_record>::const_iterator, int const& >
    const_ID_first(personnel_list.begin()),
    const_ID_last(personnel_list.end());

  copy(const_ID_first, const_ID_last, iout);
  cout << endl;
  cout << endl;

  copy(make_transform_iterator<select_name>(personnel_list.begin()) , 
       make_transform_iterator<select_name>(personnel_list.end()) , 
       sout);

  return 0;
}
