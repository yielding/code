#include <expected>
#include <system_error>
#include <string>
#include <fstream>
#include <iostream>

using namespace std;

//
// 1. 사용자 정의 오류 enum
//
enum class MyError 
{
  NotFound = 1,
  PermissionDenied,
  InvalidFormat
};

//
// 2. error_category 구현
//
class MyErrorCategory : public error_category 
{
public:
  auto name() const noexcept -> const char* override 
  {
    return "MyErrorCategory";
  }

  auto message(int ev) const -> string override
  {
    switch (static_cast<MyError>(ev))
    {
      case MyError::NotFound:
        return "Resource not found";
      case MyError::InvalidFormat:
        return "Invalid data format";
      case MyError::PermissionDenied:
        return "Permission denied";
      default:
        return "Unknown error";
    }
  }
};

//
// 3. error_category 싱글턴 반환 함수
//
auto my_error_category() -> const error_category& 
{
  static MyErrorCategory instance;
  return instance;
}

//
// 4. make_error_code 오버로드
//
namespace std 
{
  template <>
  struct is_error_code_enum<MyError> : true_type {};
}

auto make_error_code(MyError e) -> error_code 
{
  // 정의된 category와 연결
  return {static_cast<int>(e), my_error_category()};
}

auto read_file(const string& path) -> expected<string, error_code> 
{
  ifstream file{path};
  if (!file.is_open())
  {
    return unexpected{make_error_code(MyError::InvalidFormat)};
    //return unexpected{make_error_code(errc::no_such_file_or_directory)};
  }

  string content 
    = string{(istreambuf_iterator<char>(file)), istreambuf_iterator<char>()};

  if (file.bad())
    return unexpected(make_error_code(errc::io_error));

  return content;
}

int main(int argc, char *argv[])
{
  auto result = read_file("example.txt");
  if (result)
    println("파일 내용: {}", *result);
  else
    println("오류 발생: {}", result.error().message());

  return 0;
}
