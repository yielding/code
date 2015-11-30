#include <codecvt>
#include <locale>
#include <string>
#include <cassert>

using namespace std;

int main(int argc, char *argv[])
{
  wstring_convert<codecvt_utf8<char32_t>, char32_t> convert;

  string utf8 = convert.to_bytes(0x5e9);
  assert(utf8[0] == '\xD7');
  assert(utf8[1] == '\xA9');
  
  return 0;
}
