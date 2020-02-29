#include "mat.serialize.hpp"
#include <iostream>

using namespace std;
using namespace cv;
      namespace io = boost::iostreams;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////

int main(int argc, char* argv[])
{
  auto img  = imread("/Users/yielding/Desktop/IMG_0447.jpeg");
  auto path = string("/Users/yielding/Desktop/matrices.bin");
  auto compressed = MatSerializer::serialize(img);

  try
  {
    auto img2 = MatSerializer::deserialize(compressed);
    imshow("Ex1", img2);
    waitKey(0);
  }
  catch (exception& e)
  {
    cout << e.what() << endl;
  }

  return 0;
}
