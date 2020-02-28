#include <boost/serialization/binary_object.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/iostreams/filtering_streambuf.hpp>
#include <boost/iostreams/filter/zlib.hpp>

#include <opencv2/opencv.hpp>
#include <iostream>
#include <sstream>

using namespace std;
using namespace cv;
      namespace io = boost::iostreams;

namespace boost::serialization {

  /**/
  template <typename Archive>
  void serialize(Archive& ar, cv::Mat& mat, const unsigned int version)
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
        string row_name("mat_data_row_" + to_string(i));
        ar & make_nvp(row_name.c_str(), row_data);
      }
    }
  }
  /**/
}

cv::Mat deserialize(string compressed, string const& path)
{
  //cout << "compress size: " << compressed.length() << endl;

  Mat img;

  stringstream iss(compressed);
  ifstream ifs(path, ios::binary);
  io::filtering_streambuf<io::input> in;
  in.push(io::zlib_decompressor());
  in.push(iss);  // stream

  boost::archive::binary_iarchive ia(in);
  ia >> img;

  if (img.empty())
    throw logic_error("deserialize error");

  return img;
}

string serialize(cv::Mat& img, string const& path)
{
  size_t sizeInBytes = img.total() * img.elemSize();
  cout << "original size: " << sizeInBytes << endl;

  if (img.empty())
    return "";

  stringstream oss;
  ofstream ofs(path, ios::binary);
  io::filtering_streambuf<io::output> out;
  out.push(io::zlib_compressor());
  out.push(ofs);  // stream

  boost::archive::binary_oarchive oa(out);
  oa << img;

  auto res = oss.str();
  cout << "string stream size: " << res.length() << endl;
  ofstream tmp("/Users/yielding/Desktop/matrices.tmp", ios::binary);
  tmp.write((char *)res.c_str(), res.length());
  return res;
}

int main(int argc, char* argv[])
{
  auto img  = imread("/Users/yielding/Desktop/IMG_0447.jpeg");
  auto path = string("/Users/yielding/Desktop/matrices.bin");
  auto compressed = serialize(img, path);

  try
  {
    auto img2 = deserialize(compressed, path);
    imshow("Ex1", img2);
    waitKey(0);
  }
  catch (exception& e)
  {
    cout << e.what() << endl;
  }

  return 0;
}
