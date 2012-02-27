#include "curve25519-donna.h"
#include "ByteBuffer.h"

#include <iostream>
#include <vector>

using namespace std;
using namespace utility::hex;

int main(int argc, char const* argv[])
{
  auto z          = ByteBuffer::from_hexcode("04000000080000000200000048000000000000000000000000000000000000000000000002917dc2542198edeb1078c4d1ebab74d9ca87890657ba02b9825dadf20a002f44360c6f87743fac0236df1f9eedbea801e31677aef3a09adfb4e10a37ae27facf419ab3ea3f39f4");
  auto my_secret  = ByteBuffer::from_hexcode("99b66345829d8c05041eea1ba1ed5b2984c3e5ec7a756ef053473c7f22b49f14").get_buffer();
  // auto my_public  = ByteBuffer::from_hexcode("b1c652786697a5feef36a56f36fde524a21193f4e563627977ab515f600fdb3a").get_buffer();
  auto his_public = z.slice(36, 36+32);

  cout << ByteBuffer::to_hexcode(his_public.get_buffer());



//  uint8_t* secret = &my_secret[0];
//  uint8_t* basepoint = his_public;
//  vector<uint8_t> mypublic(32, 0);
//  curve25519_donna(&mypublic[0], secret, basepoint);

//  auto res = ByteBuffer::to_hexcode(mypublic);

//  cout << res;

  return 0;
}
