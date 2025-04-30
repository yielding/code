#include <iostream>
#include <string>
#include "async.video.decoder.hpp"

using namespace std;

int main(int argc, char** argv)
{
  auto filename = "/Users/yielding/Desktop/IMG_0888.MOV"s;
  av::UniqueFormatContext container;
  if (auto res = container.open_input(filename); !res)
  {
    cout << "Input open failed: " << res.error();
    return 1;
  }

  // codec id로 codec을 찾은 후
  // codec context 만들어서 이 context와 codec으로 한 번 더 연다.
  av::VideoDecoder decoder;
  decoder.load(AV_CODEC_ID_H264);
  decoder.open_with(container);


  //av::AsyncVideoDecoder decoder(codec);
  // av::VideoDecoder decoder(codec);
  // if (auto res = decoder.open(); !res)
  // {
  //   cerr << "Decoder open failed: " << res.error() << std::endl;
  //   return 1;
  // }

  /*
  decoder.start();

  for (int i = 0; i < 50; ++i)
  {
    auto frame = decoder.frames().pop();
    cout << "Frame " << i << ": pts = " << frame->pts << ", size = "
         << frame->width << "x" << frame->height << endl;
  }

  decoder.stop();
  */
  return 0;
}