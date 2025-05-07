#include <print>
#include <string>
#include "video.decoder.hpp"

using namespace std;

int main(const int argc, char** argv)
{
  if (argc != 2) { println("usage: main [filename]"); return 1; }

  const auto filename = string(argv[1]);
  av::InputFormatContext container;
  if (auto res = container.open_input(filename); !res) {
    println("Input open failed: {}", res.error());
    return 1;
  }

  println("video codec info");
  println(" - name: {}", container.video_codec_name());
  println(" - short name: {}", container.video_codec_name_short());
  println(" - id: {}", (int)container.video_codec_id());

  av::VideoDecoder decoder;
  if (auto res = decoder.open_with(container.format_context(), container.video_index()); !res) {
    println("failed to construct decoder: {}", res.error());
    return 1;
  }

  decoder.decode_loop([](auto frame) { 
    println("Decoded frame: pts = {}", frame->pts); 
  });

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