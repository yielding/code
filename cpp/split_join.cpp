#include <iostream>
#include <sstream>
#include <vector>
#include <string>

using namespace std;

void split(const string& in, char delim, vector<string>& elems)
{
  stringstream ss(in);
  string item;
  while (getline(ss, item, delim))
    elems.push_back(item);
}

vector<string> split(string const& s, char delim)
{
  vector<string> elems;
  split(s, delim, elems);
  return elems;
}

template <typename ForwardIterator> 
string join(ForwardIterator f, ForwardIterator l, string const& sep=", ")
{
  stringstream ss;

  ss << *f;
  for (++f; f != l - 1; ++f) 
    ss << sep << *f;


  return ss.str();
}

template <typename ForwardIterator> 
string join(ForwardIterator f, int count, string const& sep=", ")
{
  auto from = f;
  auto to   = f + count;

  if (distance(from, to) <= 0)
    return "";

  return join(from, to, sep);
}

int main(int argc, const char *argv[])
{
  auto ss = split("/var/mobile/Applications/Kakao/Kakao.sqlite", '/');
  cout << join(ss.begin(), 5, "/") << endl;
  
  return 0;
}
