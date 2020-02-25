#include <boost/serialization/binary_object.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/iostreams/filtering_streambuf.hpp>
#include <boost/iostreams/filter/zlib.hpp>

#include <opencv2/opencv.hpp>
#include <iostream>

using namespace std;
using namespace cv;
namespace io = boost::iostreams;

namespace boost::serialization {

  template <typename Archive>
  void serialize(Archive& ar, cv::Mat& mat, const unsigned int /*version*/)
  {
    int rows, cols, type;
    bool continuous;

    if (Archive::is_saving::value)
    {
      rows = mat.rows;
      cols = mat.cols;
      type = mat.type();
      continuous = mat.isContinuous();
    }

    ar & BOOST_SERIALIZATION_NVP(rows)
       & BOOST_SERIALIZATION_NVP(cols)
       & BOOST_SERIALIZATION_NVP(type)
       & BOOST_SERIALIZATION_NVP(continuous);

    if (Archive::is_loading::value)
      mat.create(rows, cols, type);

    if (continuous)
    {
      const int data_size = rows * cols * static_cast<int>(mat.elemSize());
      boost::serialization::binary_object mat_data(mat.data, data_size);
      ar & BOOST_SERIALIZATION_NVP(mat_data);
    }
    else
    {
      const int row_size = cols * static_cast<int>(mat.elemSize());
      for (int i = 0; i < rows; i++)
      {
        boost::serialization::binary_object row_data(mat.ptr(i), row_size);
        std::string row_name("mat_data_row_" + std::to_string(i));
        ar & make_nvp(row_name.c_str(), row_data);
      }
    }
  };

}

int main1(int argc, char *argv[])
{
  Mat img;

  ifstream ofs("/Users/yielding/Desktop/matrices.bin", ios::binary);
  boost::archive::binary_iarchive oa(ofs);
  oa >> img;

  if (img.empty())
    return -1;

  imshow("Ex1", img);
  waitKey(0);

  destroyWindow("Ex1");

  return 0;
}

int main(int argc, char *argv[])
{
  auto img = imread("/Users/yielding/Desktop/IMG_0447.jpeg");
  size_t sizeInBytes = img.total() * img.elemSize();
  cout << sizeInBytes << endl;

  if (img.empty())
    return -1;

  imshow("Ex1", img);
  waitKey(0);

  {
    ofstream ofs("/Users/yielding/Desktop/matrices.bin", ios::binary);
    io::filtering_streambuf<io::output> out;
    out.push(io::zlib_compressor(io::zlib::best_speed));
    out.push(ofs);
    boost::archive::binary_oarchive oa(out);
    oa << img;
  }

  destroyWindow("Ex1");

  return 0;
}
