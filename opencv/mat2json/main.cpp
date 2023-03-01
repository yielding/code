#include <opencv2/opencv.hpp>
#include <nlohmann/json.hpp>
#include <fstream>
#include <iostream>

using json = nlohmann::json;

using namespace std;

int main()
{
  // create a Mat object
  cv::Mat image = cv::imread("/Users/yielding/code/data/taekwon_v.jpg");

  cout << "1\n";

  // serialize Mat to JSON
  json serializedMat;
  serializedMat["rows"] = image.rows;
  serializedMat["cols"] = image.cols;
  serializedMat["type"] = image.type();
  serializedMat["data"] = std::vector<uchar>(image.data, image.data + image.total() * image.elemSize());
  cout << "2\n";

  // write serializedMat to a file
  ofstream output("image.json");
  output << serializedMat.dump(4) << std::endl;
  cout << "3\n";

  return 0;
}