#include <codecvt>
#include <locale>
#include <string>
#include <vector>
#include <cassert>
#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
  wstring_convert<codecvt_utf8<char32_t>, char32_t> convert;
  string utf8 = convert.to_bytes(0x5e9);
  assert(utf8[0] == '\xD7');
  assert(utf8[1] == '\xA9');

  uint8_t u8[] // 이창하
    = { 0xec, 0x9d, 0xb4, 0xec, 0xb0, 0xbd, 0xed, 0x95, 0x98 };

  string x(u8, u8+9);
  cout << x << endl;

  // uint8_t u16[] = { 0xff, 0xfe, 0x74, 0xc7, 0x3d, 0xcc, 0x58, 0xd5 };
  // char16_t u16[] = { 0xc774, 0xcc3d, 0xd558 };
  vector<char16_t> u16 { 0xc774, 0xcc3d, 0xd558 };

  // utf16-be -> utf8
  // codecvt_mode 1: little endian, 
  //            def: big endian
  wstring_convert<codecvt_utf8_utf16<char16_t, 0x10ffff, (codecvt_mode)1>, 
                  char16_t> convert2;
  auto beg  = &*u16.begin();
  auto dest = convert2.to_bytes(beg, beg + 3);
  // string dest = convert2.to_bytes(u"\uc774\ucc3d\ud558");
  cout << dest << endl;

  return 0;
}
