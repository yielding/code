#include <iostream>
#include <string>
#include <locale>
#include <codecvt>

using namespace std;

int main(int argc, const char *argv[])
{
  string utf8 = u8"z\u6c34\U0001d10b";

  // U+1d10b, musical sign segno, does not fit in UCS2
  // but fit in UTF-16

  // 1. to utf16
  wstring_convert<codecvt_utf8_utf16<char16_t>, char16_t>utf16conv;
  auto utf16 = utf16conv.from_bytes(utf8);
  cout << "UTF16 conversion produced " << utf16.size() 
    << " code points:\n";

  for (char16_t c : utf16) cout << hex << showbase << c << '\n';


  // 2. to ucs2

  wstring_convert<codecvt_utf8<char16_t>, char16_t> ucs2conv;
  try 
  {
    u16string ucs2 = ucs2conv.from_bytes(utf8);
  }
  catch(range_error& e)
  {
    u16string ucs2 = ucs2conv.from_bytes(utf8.substr(0, ucs2conv.converted()));
    cout << "UCS2 failed after producing " << dec << ucs2.size()
      << " characters:\n";

    for (char16_t c : ucs2) cout << hex << showbase << c << '\n';

  }

  return 0;
}
