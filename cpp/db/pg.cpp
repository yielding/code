#include <iostream>
#include <pqxx/pqxx>

using namespace std;
using namespace pqxx;

int main()
{
  try
  {
    connection C("host=192.168.10.128 user=postgres password=sol123 dbname=o2");
    cout << "Connected to " << C.dbname() << endl;
    work W(C);

    result R = W.exec("SELECT * FROM snmpgets");

    cout << "Found " << R.size() << "employees:" << endl;
    for (result::const_iterator r = R.begin(); r != R.end(); ++r)
    {
      cout << r[0].c_str() << endl;
    }

//    cout << "Doubling all employees' salaries..." << endl;
//    W.exec("UPDATE employee SET salary=salary*2");
//
//    W.exec("UPDATE employee SET salary=salary*3");
//
//    cout << "Making changes definite: ";
    W.commit();
    cout << "ok." << endl;
  }
  catch (const exception &e)
  {
    cerr << e.what() << endl;
    return 1;
  }

  return 0;
}
