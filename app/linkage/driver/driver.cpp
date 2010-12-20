#include "stdafx.h"

#include "summer.h"
#include <iostream>
#include <string>
#include <cstdlib>
#include <vector>

using namespace std;

int _tmain(int argc, _TCHAR* argv[])
{
  std::wstring path = L"D:/tmp/linkage/summer/release/summer.dll";
  HMODULE handle = ::LoadLibrary(path.c_str());
  if (handle == NULL)
    cout << "error 1";

  create_summer_t* create_summer = (create_summer_t*)::GetProcAddress(handle, "create_summer");
  CSummer* summer = create_summer();
  if (summer == NULL)
    cout << "error 2";

  std::vector<double> dv;
  for (int j=0; j<=10; j++) dv.push_back(j* 1.0);
  cout << "sum: " << summer->sum(dv) << "\n";

  for (int j=0; j<1000; j++)
  {
    Sum s;
    cout << "sum2: " << summer->sum2(s) << "\n";
  }

  ::FreeLibrary(handle);

  cout << "ok";
  return 0;
}
