#include <iostream>
#include <string>
#include <cstdio>
#include <ctime>

using namespace std;

// 현재시간을 string type으로 return하는 함수
string currentDateTime() 
{
  auto now = time(0); //현재 시간을 time_t 타입으로 저장
  auto tstruct = *localtime(&now);
  char buf[80] = { 0 };
  strftime(buf, sizeof(buf), "%Y-%m-%d.%X", &tstruct); // YYYY-MM-DD.HH:mm:ss 형태의 스트링

  return buf;
}

int main() 
{
  cout << "지금 시간은: " << currentDateTime() << endl;

  return 0;
}
