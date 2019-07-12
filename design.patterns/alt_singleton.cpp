#include <iostream>
#include <vector>
#include <string>
#include <map>

using namespace std;

//
// DON'T use singleton
// It's really nightmare. Dependencies etc.
//
class Database
{
public:
  virtual int get_population(const string& name) = 0;
};

class DummyDatabase : public Database
{
public:
  DummyDatabase()
  {
    capitals["alpha"] = 1;
    capitals["beta"]  = 2;
    capitals["gamma"] = 3;
  }

  auto get_population(const string& name) int override
  {
    return capitals[name];
  }

private:
  map<string, int> capitals;
};

struct ConfigurableRecordFinder 
{
  explicit ConfigurableRecordFinder(Database& db)
    : db{db} {}

  int total_population(vector<string> names)
  {
    int result = 0;

    for (auto& name: names) result += db.get_population(name);

    return result;
  }

private:
  Database& db;
};


int main(int argc, char *argv[])
{
  
  return 0;
}
