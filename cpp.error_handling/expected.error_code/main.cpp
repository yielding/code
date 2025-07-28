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

template <typename T>
using Result = expected<T, error_code>;
//
// 2. error_category 구현
//
class MyErrorCategory final : public error_category
{
public:
  auto name() const noexcept -> const char * override { return "MyErrorCategory"; }

  [[nodiscard]]
  auto message(int ev) const -> string override
  {
    switch (static_cast<MyError>(ev))
    {
    case MyError::NotFound:
      return "Resource not found. ㅠㅠ";
    case MyError::InvalidFormat:
      return "Invalid data format. ㅠㅠ";
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
auto my_error_category() -> const error_category &
{
  static MyErrorCategory instance;
  return instance;
}

//
// 4. make_error_code 오버로드
//    Note : 이 specialization이 있어야 MyError가 std::error_code로 자동 변환 가능.
//
template <>
struct std::is_error_code_enum<MyError> : true_type
{
};

auto make_error_code(MyError e) -> error_code
{
  // 정의된 category와 연결
  return {static_cast<int>(e), my_error_category()};
}

auto read_file(const string &path) -> Result<string>
{
  ifstream file{path};

  if (!file.is_open())
    return unexpected{make_error_code(errc::no_such_file_or_directory)};

  if (file.bad())
    return unexpected(make_error_code(errc::io_error));

  return string{(istreambuf_iterator(file)), istreambuf_iterator<char>()};
}

int main(int argc, char *argv[])
{
  if (auto result = read_file("example.txt"))
    println("파일 내용: {}", *result);
  else
    println("오류 발생: {}", result.error().message());

  return 0;
}
