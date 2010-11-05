#include <string>
#include <map>
#include <iterator>
#include <iostream>

namespace std {

  typedef map<string, int> months_type;

  //
  // Print out a pair.
  //
  template <class First, class Second>
  ostream& operator<< (ostream& out, const pair<First,Second> & p)
  {
    cout << p.first << " has " << p.second << " days";
    return out;
  }

  //
  // Print out a map.
  //
  ostream& operator<< (ostream& out, const months_type & l)
  {
    copy(l.begin(),l.end(), 
        ostream_iterator<months_type::value_type,char>(cout, "\n"));

    return out;
  }
} 

int main ()
{
  using namespace std;
  //
  // Create a map of months and the number of days in the month.
  //
  months_type months;

  typedef months_type::value_type value_type;
  // 
  // Put the months in the multimap.
  //
  months.insert(value_type(string("January"),   31));
  months.insert(value_type(string("Febuary"),   28));
  months.insert(value_type(string("Febuary"),   29));
  months.insert(value_type(string("March"),     31));
  months.insert(value_type(string("April"),     30));
  months.insert(value_type(string("May"),       31));
  months.insert(value_type(string("June"),      30));
  months.insert(value_type(string("July"),      31));
  months.insert(value_type(string("August"),    31));
  months.insert(value_type(string("September"), 30));
  months.insert(value_type(string("October"),   31));
  months.insert(value_type(string("November"),  30));
  months.insert(value_type(string("December"),  31));
  //
  // Print out the months.  Second Febuary is not present.
  //
  cout << months << endl;
  //
  // Find the Number of days in June.
  //
  months_type::iterator p = months.find(string("June"));
  //
  // Print out the number of days in June.
  //
  if (p != months.end())
    cout << endl << *p << endl;

  return 0;
}
