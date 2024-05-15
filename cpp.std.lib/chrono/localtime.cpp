#include <iostream>
#include <iomanip>
#include <ctime>
#include <cstdlib> // defines putenv in POSIX

using namespace std;

int main()
{
  time_t t = time(nullptr);
  cout << "UTC:       " << put_time(gmtime(&t), "%c %Z") << '\n';
  cout << "local:     " << put_time(localtime(&t), "%c %Z") << '\n';
  // POSIX-specific:
  string tz = "TZ=Asia/Singapore";
  putenv((char*)tz.c_str());
  cout << "Singapore: " << put_time(localtime(&t), "%c %Z") << '\n';

  return 0;
}
