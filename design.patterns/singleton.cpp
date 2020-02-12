#include <iostream>
#include <vector>
#include <string>
#include <map>

using namespace std;

class Database // : public boost::noncopyable
{
public:
  static Database& get()
  {
    static Database db;
    return db;
  }

protected:
  Database() 
  {
  }
};

int main(int argc, char *argv[])
{
  return 0;
}
