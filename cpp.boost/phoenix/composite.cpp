#include <iostream>
#include <algorithm>
#include <string>
#include <vector>

#include <boost/phoenix/bind.hpp>
#include <boost/phoenix/core.hpp>
#include <boost/phoenix/operator.hpp>
#include <boost/phoenix/bind/bind_member_function.hpp>

using namespace boost::phoenix;
using boost::phoenix::arg_names::arg1;

using namespace std;

struct Person
{
  Person(string const& f, string const& l) : FirstName(f), LastName(l) {}

  string FirstName;
  string LastName;
};

int main()
{
  vector<Person> people;
  people.push_back(new Person("Fred", "Smith"));
  people.push_back(new Person("Fred2", "1Smith"));

  auto it = find_if(people.begin(), people.end(), 
      bind(&Person::FirstName, arg1) == "Fred");

//  cout << ((it != people.end())
//            ? it->LastName
//            : "Not found");

  return 0;
}
