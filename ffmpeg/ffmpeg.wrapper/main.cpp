#include <iostream>
#include <string>
#include "async.video.decoder.hpp"

using namespace std;

int main(int argc, char** argv)
{
  const auto filename = "/Users/yielding/Desktop/IMG_0888.MOV"s;
  av::UniqueFormatContext container;
  if (auto res = container.open_input(filename); !res)
  {
    cout << "Input open failed: " << res.error();
    return 1;
  }

  cout << container.video_codec_name() << endl;
  cout << container.video_codec_name_short() << endl;
  cout << container.video_codec_id() << endl;

  cout << container.audio_codec_name() << endl;
  cout << container.audio_codec_name_short() << endl;
  cout << container.audio_codec_id() << endl;

  av::VideoDecoder decoder;
  // decoder.open_with(container.format_context(), container.video_index());
  // decoder.decode_loop();



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