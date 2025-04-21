#include "cv/mat.serialize.hpp"
#include <print>
#include <cstdlib>

using namespace std;
using namespace cv::serializer;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[])
{
  auto home = getenv("HOME");
  auto file = argc == 1 ? "1.jpeg" : argv[1];
  auto img  = cv::imread(format("{}/Desktop/{}", home, file));
  auto compressed = mat::serialize(img);

  try
  {
    auto img2 = mat::deserialize(compressed);
    cv::imshow("Ex1", img2);
    cv::waitKey(0);
  }
  catch (exception& e)
  {
    println("Error: {}", e.what());
  }

  return 0;
}
