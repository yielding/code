#include <opencv2/opencv.hpp>
#include <nlohmann/json.hpp>
#include <fstream>
#include <print>
#include <cstdlib>

using json = nlohmann::json;

using namespace std;

int main()
{
  auto path = format("{}/code/data/taekwon_v.jpg", getenv("HOME"));
  // create a Mat object
  cv::Mat image = cv::imread(path);
  println("1");


  // serialize Mat to JSON
  json serializedMat;
  serializedMat["rows"] = image.rows;
  serializedMat["cols"] = image.cols;
  serializedMat["type"] = image.type();
  serializedMat["data"] = std::vector<uchar>(image.data, image.data + image.total() * image.elemSize());
  println("2");

  // write serializedMat to a file
  ofstream output("image.json");
  output << serializedMat.dump(4) << std::endl;
  println("3");

  return 0;
}