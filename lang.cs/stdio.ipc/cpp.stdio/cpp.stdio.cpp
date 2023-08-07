#include <iostream>
#include <vector>

#include "opencv2/imgproc.hpp"
#include "opencv2/imgcodecs.hpp"

using namespace std;

auto read_image_from_stdin() -> vector<char>
{
  vector<char> buffer;

  unsigned ch;
  while ((ch = getchar_unlocked()) != EOF)  // in Windows, getchar_nolock
    buffer.push_back(ch);

  return buffer;
}

void write_image_to_stdout(vector<uchar> const& buffer)
{
  for (auto const& ch: buffer) 
    putchar_unlocked(ch);
}

cv::Mat read_mat_from_stdio(int color=0)
{
  return cv::imdecode(cv::Mat(read_image_from_stdin()), color);
}

cv::Mat read_mat_from_filename(string const& input)
{
  return cv::imread(input.c_str(), 0);
}

cv::Mat analyze_input(string const&& input)
{
  return (input == "-")
    ? read_mat_from_stdio()
    : read_mat_from_filename(input);
}

int main(int argc, char *argv[])
{
  // 입력 데이터 받기
  auto mat_jpg = (argc > 1)
    ? analyze_input(string(argv[1]))
    : read_mat_from_stdio();

  // 실제 내가 작성한 코드
  cv::Mat blurred;
  cv::GaussianBlur(mat_jpg, blurred, cv::Size(9, 9), 2);

  if (argc == 1) // stdio
  {
    vector<uchar> result;
    cv::imencode(".jpg", blurred, result, vector{ cv::IMWRITE_JPEG_QUALITY, 80 });
    write_image_to_stdout(result);
  }
  else if (argc > 1) // user input
  {
    auto fname = argc > 2 ? string(argv[2]) : "./res.jpg"s;
    cv::imwrite(fname, blurred);
  }

  return 0;
}