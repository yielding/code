#include <iostream>
#include <cstdlib>
#include <fstream>
#include <string>
#include <cstdint>

using namespace std;

// NOTICE
// intel cpy는 기본적으로 little endian
// 1) little을 big으로 바꾸고 저장
// 2) big을 읽어 little로 바꾸어 화면에 출력
int toggle_endian(int num)
{
  return ((num >> 24) & 0x000000ff) | 
         ((num <<  8) & 0x00ff0000) | 
         ((num >>  8) & 0x0000ff00) | 
         ((num << 24) & 0xff000000);
}

// 함수 작성 규칙
// 1) 한 함수는 "오직" 한가지 일만 한다
// 2) 그 일을 "잘" 해야한다
int read_int4_be(ifstream& ifs)
{
  char buffer[4] = { 0 };
  ifs.read(buffer, 4);

  int res = *(int *)buffer;

  return toggle_endian(res);
}

void write_int4_be(ofstream& ofs, int value)
{
  int res = toggle_endian(value);
  ofs.write((char *)&res, 4);
}

int main(int argc, char *argv[])
{
  string path = "/Users/yielding/Desktop/data.bin";
  ofstream ofs(path, ios_base::binary);

  // 에러 처리는 확실하게
  if (!ofs.good())
  {
    cout << "failed to open data file";
    return EXIT_FAILURE;
  }

  for (int i=1; i<=10; i++)
    write_int4_be(ofs, i);

  // 동일 파일을 다시 open할 예정이므로 반드시 close
  ofs.close();

  ifstream ifs(path, ios_base::binary);
  if (!ifs.good())
  {
    cout << "failed to open data file";
    return EXIT_FAILURE;
  }

  for (int i=0; i<10; i++)
    cout << read_int4_be(ifs) << endl;

  return EXIT_SUCCESS;
}
