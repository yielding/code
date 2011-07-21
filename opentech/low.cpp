#include <iostream>
#include <string>
#include <fstream>
#include <algorithm>

using namespace std;

class Problem
{
public:
  Problem(string const& line) :m_line(line) 
  {
    m_sc = "";
    m_rc = "";
    m_count = 1;
  }

  bool is_problem();
  void print() { cout << m_line << endl; }

private:
  int m_count;
  string m_line;
  string m_rc;
  string m_sc;
  string m_sisgnal;
};


bool Problem::is_problem()
{
  return 
    m_line.find("|:|") != string::npos ||
    m_line.find("|;|") != string::npos ||
    m_line.find("|<|") != string::npos ||
    m_line.find("|>|") != string::npos ||
    m_line.find("|?|") != string::npos;
}

int main(int argc, char const* argv[])
{
  ifstream in("data.txt");
  // Problems probs;
  string line;
  while(getline(in, line))
  {
    Problem p(line);
    if (p.is_problem()) 
      p.print();
  }

  return 0;
}
