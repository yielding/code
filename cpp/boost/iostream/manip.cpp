#include <ios>
#include <iostream>
#include <iomanip>

// http://beoribs.re.pusan.ac.kr/pgi/pgC++_lib/stdlibug/for_5394.htm
using namespace std;

int main()
{
  cout.setf(ios_base::right, ios_base::adjustfield);

  cout << setw(10) << 18 << endl;
  cout << setw(10) << 10 << endl;
  cout << setw(10) << 10 << endl;
  cout << setw(10) << 10 << endl;

  return 0;
}
