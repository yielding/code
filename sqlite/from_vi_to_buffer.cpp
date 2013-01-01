#include <iostream>
#include <cassert>
#include <string>
#include <stdint.h>

using namespace std;

string hex_(uint8_t v)
{
  char buffer[10] = { 0 };
  sprintf(buffer, "%02x", v);
  
  return string(buffer);
}

void to_hex(uint8_t* buffer, size_t size)
{
  for (auto i=0; i<size; i++) 
    cout << hex_(buffer[i]) << " ";

  cout << endl;
}

int sqlite3PutVarint(uint8_t* p, uint64_t v)
{
  int i, j, n;
  uint8_t buf[10];

   // v의 most significant 바이트에 값이 있는지 검사
   // (즉, 가장 큰 바이트에 값이 있을 경우는 8바이트 정수이므로
   // 무조건 9바이트의 varint가 필요)
  if (v & ((uint64_t(0xff000000)) << 32))
  {
    // v의 가장 작은 바이트 값이 p의 9번째 바이트에 assgin
    p[8] = v;

    // v를 1바이트 right shift
    v >>= 8;

    // 루프를 돌며 7비트씩 assign, 상위 1비트는 모두 1로 해줌
    for (i=7; i>=0; i--)
    {
      p[i] = (v & 0x7f) | 0x80;
      v >>= 7;
    }

    /* 바이트 길이(9)를 되돌려 주며 종료) */
    return 9;
  }

  n = 0;

  // 8바이트 크기 정수가 아닌 경우는 첫번째 비트를 1로 하면서
  // 7비트씩 차례로 buffer에 인코딩
  // (현 시점에서는 바이트 길이를 판단하기 힘드므로 가장 마지막 바이트
  // 를 buf[0]에 assign하면서 역순으로 처리)
  do 
  {
    buf[n++] = (v & 0x7f) | 0x80;
    v >>= 7;
  } 
  while (v != 0);

  // 인코딩 종료 후 마지막 바이트의 첫번째 비트를 다시 0으로 만듬
  buf[0] &= 0x7f;
  assert (n <= 9);

  // output 변수에 바이트 순서를 뒤집어서 copy
  for (i=0, j=n-1; j>=0; j--, i++)
    p[i] = buf[j];

  return n;
}

int main(int argc, const char *argv[])
{
  uint8_t buffer[10] = { 0 };

  auto size = sqlite3PutVarint(buffer, 0xff);

  to_hex(buffer, size);

  return 0;
}
