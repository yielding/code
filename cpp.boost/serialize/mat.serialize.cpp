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
}

auto deserialize(string& compressed) -> cv::Mat
{
  cv::Mat result;

  if (!compressed.empty())
  {
    auto mode = ios_base::in | ios_base::out | ios_base::binary;
    stringstream iss(compressed, mode);
    io::filtering_streambuf<io::input> in;
    in.push(io::zlib_decompressor());
    in.push(iss);

    boost::archive::binary_iarchive ia(in);
    ia >> result;
  }

  return result;
}

auto serialize(cv::Mat& img) -> string
{
  stringstream result;

  if (!img.empty())
  {
    io::filtering_streambuf<io::output> out;
    out.push(io::zlib_compressor(io::zlib::best_speed));
    out.push(result);

    boost::archive::binary_oarchive oa(out);
    oa << img;
  }

  return result.str();
}

int main(int argc, char* argv[])
{
  auto img  = imread("/Users/yielding/Desktop/IMG_0447.jpeg");
  auto path = string("/Users/yielding/Desktop/matrices.bin");
  auto compressed = serialize(img);

  try
  {
    auto img2 = deserialize(compressed);
    imshow("Ex1", img2);
    waitKey(0);
  }
  catch (exception& e)
  {
    cout << e.what() << endl;
  }

  return 0;
}
