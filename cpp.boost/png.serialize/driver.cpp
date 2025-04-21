#include "cv/png.serialize.hpp"
#include <iostream>

using namespace std;
using namespace cv;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////

int main(int argc, char* argv[])
{
  auto img  = imread("/Users/yielding/Desktop/1.jpeg");
  auto compressed = PngSerializer::serialize(img);

  try
  {
    auto img2 = PngSerializer::deserialize(compressed);
    imshow("Ex1", img2);
    waitKey(0);
  }
  catch (exception& e)
  {
    cout << e.what() << endl;
  }

  return 0;
}
