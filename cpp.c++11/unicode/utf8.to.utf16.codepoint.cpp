#include <iostream>
#include <fstream>
#include <string>
#include <locale>
#include <iomanip>
#include <string>
#include <codecvt>

using namespace std;

// utility wrapper to adapt locale-bound facets for wstring/wbuffer convert
template<class Facet>
struct deletable_facet : Facet
{
  template<class ...Args>
  deletable_facet(Args&& ...args) : Facet(forward<Args>(args)...) 
  {}

  ~deletable_facet() 
  {}
};

int main()
{
  // UTF-8 narrow multibyte encoding
  string data = u8"z\u00df\u6c34\U0001f34c";
                // or u8"zß水🍌"
                // or "\x7a\xc3\x9f\xe6\xb0\xb4\xf0\x9f\x8d\x8c";

  ofstream("text.txt") << data;

  // using system-supplied locale's codecvt facet
  wifstream fin("text.txt");
  // reading from wifstream will use codecvt<wchar_t, char, mbstate_t>
  // this locale's codecvt converts UTF-8 to UCS4 (on systems such as Linux)
  fin.imbue(locale("en_US.UTF-8"));
  cout << "The UTF-8 file contains the following UCS4 code points: \n";
  for (wchar_t c; fin >> c; )
    cout << "U+" << hex << setw(4) << setfill('0') << c << '\n';

  // using standard (locale-independent) codecvt facet
  wstring_convert<
    deletable_facet<codecvt<char16_t, char, mbstate_t>>, char16_t> conv16;
  u16string str16 = conv16.from_bytes(data);

  cout << "The UTF-8 file contains the following UTF-16 code points: \n";
  for (char16_t c : str16)
    cout << "U+" << hex << setw(4) << setfill('0') << c << '\n';
}
