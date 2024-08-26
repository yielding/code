#include <iostream>
#include <charconv>
#include <cassert>

using namespace std;

int main(int argc, char *argv[])
{
  u8string s2{u8"abc가나다"};
  cout << s2.length() << endl;

  u16string s3{u"abc가나다"};
  cout << s3.length() << endl;

  // 11을 10진수 문자열로 변환    
  // 구조화된 바인딩. std::to_chars_result result{std::to_chars()} 와 동일
  char buf[10];
  auto [ptr0, ec0] = to_chars(buf, buf + sizeof(buf), 11, 10);
  if (ec0 == errc{}) 
    assert(string(buf, ptr0 - buf) == "11");

  // 11을 16진수 문자열로 변환
  auto [ptr1, _] = to_chars(buf, buf + sizeof(buf), 11, 16);
  assert(string(buf, ptr1 - buf) == "b");

  char str[]{"11year"}; // 숫자와 일반 문자로 구성됩니다.
  int result{0};

  auto [ptr2, ec2] = from_chars(str, str + sizeof(str), result);
  if (ec2 == errc{}) 
  {
    // 숫자 부분만 잘 변환합니다.
    assert(result == 11); 
  }

  // ptr은 숫자 다음 위치입니다. 즉, 'y' 위치입니다.
  assert(ptr2 == &str[2]); 

  return 0;
}