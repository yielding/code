#include <string>
#include <map>

#include <iostream>

namespace std {
  typedef multimap<int, string> months_type;

  //
  // Print out a pair.
  //
  template <class First, class Second>
  ostream& operator<< (ostream& out, const pair<First,Second>& p)
  {
    out << p.second << " has " << p.first << " days";
    return out;
  }

  //
  // Print out a multimap.
  //
  ostream& operator<< (ostream& out, months_type l)
  {
    copy(l.begin(),l.end(), ostream_iterator
	 <months_type::value_type,char,char_traits<char> >(cout,"\n"));
    return out;
  }
} 

int main ()
{
  using namespace std;

  //
  // Create a multimap of months and the number of days in the month.
  //
  months_type months;

  typedef months_type::value_type value_type;
  // 
  // Put the months in the multimap.
  //
  months.insert(value_type(31, string("January")));
  months.insert(value_type(28, string("Febuary")));
  months.insert(value_type(31, string("March")));
  months.insert(value_type(30, string("April")));
  months.insert(value_type(31, string("May")));
  months.insert(value_type(30, string("June")));
  months.insert(value_type(31, string("July")));
  months.insert(value_type(31, string("August")));
  months.insert(value_type(30, string("September")));
  months.insert(value_type(31, string("October")));
  months.insert(value_type(30, string("November")));
  months.insert(value_type(31, string("December")));
  //
  // Print out the months.
  //
  cout << "All months of the year" << endl << months << endl;
  //
  // Find the Months with 30 days.
  //
  pair<months_type::iterator,months_type::iterator> p = 
  months.equal_range(30);
  //
  // Print out the 30 day months.
  //
  cout << endl << "Months with 30 days" << endl;
  copy(p.first,p.second,ostream_iterator<months_type::value_type,char,char_traits<char> >(cout,"\n"));
  
  return 0;
}


