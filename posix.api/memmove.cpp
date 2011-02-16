#include <iostream>
#include <cstring>
#include <string>

using namespace std;

int main(int argc, char const* argv[])
{
  string m_s = "abcdefghijklmnopqrstuvwxyz";

  for (size_t i=0; i<m_s.length(); ++i)
  {
    char c = m_s[0];
    memmove(&m_s[0], &m_s[1], m_s.length());
    m_s[m_s.length()-1] = c;
    // rotate(m_s.begin(), m_s.begin()+1, m_s.end());
    cout << m_s << endl;
  }

  return 0;
}
