#include <fstream>
#include <jsoncons/json.hpp>
#include <jsoncons_ext/jsonpath/json_query.hpp>

using namespace std;
using namespace jsoncons;
using namespace jsoncons::jsonpath;

int main(int argc, char *argv[])
{
  ifstream is("books.json");
  auto books = json::parse(is);

  for (auto& book : books.array_range())
  {
    auto author = book["author"].as<string>();
    
    cout << author << endl;
  }

  return 0;
}
