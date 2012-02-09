#include <iostream>
#include <map>

using namespace std;

int main(int argc, char const* argv[])
{
  map<int, string> m;

  m[1] = "January"; // 키가 존재하면 overwirte 아니면 추가
  m.insert(make_pair(2, "February")); // 키가 존재하면 no op 아니면 추가
  bool is_newly_inserted = m.insert(make_pair(3, "March")).second;

  return 0;
}
