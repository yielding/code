#include <iostream>
#include <vector>

#include "opencv2/imgproc.hpp"
#include "opencv2/imgcodecs.hpp"

using namespace std;

auto read_image_from_stdin() -> vector<char>
{
  vector<char> buffer;

  unsigned ch;
  while ((ch = getchar_unlocked()) != EOF)
    buffer.push_back(ch);

  return buffer;
}

void write_image_to_stdout(vector<uchar> const& buffer)
{
  for (auto const& ch: buffer) putchar_unlocked(ch);
}

int main(int argc, char *argv[])
{
  auto mat_jpg = (argc > 1)
    ? cv::imread(argv[1], 0)
    : cv::imdecode(cv::Mat(read_image_from_stdin()), 0);

  cv::Mat blurred;
  cv::GaussianBlur(mat_jpg, blurred, cv::Size(9, 9), 2);

  if (argc == 1)
  {
    vector<uchar> result;
    cv::imencode(".jpg", blurred, result, vector{ cv::IMWRITE_JPEG_QUALITY, 80 });
    write_image_to_stdout(result);
  }
  else if (argc > 1)
  {
    auto fname = argc > 2 ? string(argv[2]) : "./res.jpg"s;
    cv::imwrite(fname, blurred);
  }

  return 0;
}