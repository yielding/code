#include <list>
#include <string>
#include <cstring>
#include <iostream>

using namespace std;

struct student
{
  string name; 
  string email; 
  string addres; 
  bool operator<(const student& other) 
  {
    return strcmp(this->name.c_str(), other.name.c_str()) < 0;
  }
}; 

typedef std::list<student> studentlist; 

int main()
{ 
  student a, b, c;
  studentlist l;

  a.name = "c11"; l.push_back(a);
  b.name = "a2",  l.push_back(b);
  c.name = "b3";  l.push_back(c);
  l.sort();

  for (studentlist::iterator it=l.begin(); it != l.end(); ++it) 
    cout << it->name << endl;

  return 0;
}
