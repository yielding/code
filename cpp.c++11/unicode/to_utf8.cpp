#include <codecvt>
#include <locale>
#include <string>
#include <cassert>
#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
  wstring_convert<codecvt_utf8<char32_t>, char32_t> convert;
  string utf8 = convert.to_bytes(0x5e9);
  assert(utf8[0] == '\xD7');
  assert(utf8[1] == '\xA9');

  // utf-8 data
  uint8_t u8[] = { 0xec, 0x9d, 0xb4, 0xec, 0xb0, 0xbd, 0xed, 0x95, 0x98 };
  string x(u8, u8+9);
  cout << x << endl;  // 이창하


  // uint8_t u16[] = { 0xff, 0xfe, 0x74, 0xc7, 0x3d, 0xcc, 0x58, 0xd5 };
  uint8_t u16[] =  { 0x74, 0xc7, 0x3d, 0xcc, 0x58, 0xd5 }; // utf16-le data

  // utf16-be -> utf8
  wstring_convert<codecvt_utf8_utf16<char16_t, 0x10ffff, (codecvt_mode)1>, 
                  char16_t> convert2;

  string dest = convert2.to_bytes((char16_t*)u16);
  cout << dest << endl;

  return 0;
}
