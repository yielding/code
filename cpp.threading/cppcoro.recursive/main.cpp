#include <cppcoro/recursive_generator.hpp>
#include <filesystem>
#include <iostream>

using namespace std;
using namespace cppcoro;
      namespace fs = std::filesystem;

// 재귀적으로 파일 이름을 나열하는 제너레이터
auto list_files(const fs::path& dir) -> recursive_generator<fs::path> 
{
  for (auto& entry : fs::directory_iterator(dir)) 
  {
    if (entry.is_directory()) 
      co_yield list_files(entry.path());
    else if (entry.is_regular_file())
      co_yield fs::path(entry.path());
  }
}

int main() 
{
  fs::path root = "."; // 현재 디렉터리 기준
  int count = 0;

  for (auto& file : list_files(root)) 
  {
    cout << file.string() << "\n";
    if (++count >= 20) break; // 처음 20개 파일만 출력
  }

  return 0;
}
