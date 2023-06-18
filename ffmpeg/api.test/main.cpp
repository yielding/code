extern "C" {
#include "libavformat/avformat.h"
#include "libavcodec/avcodec.h"
#include "libavutil/imgutils.h"
#include "libswscale/swscale.h"
}

#include <iostream>
#include <tuple>
#include <format>
#include <scope_guard.hpp>

using namespace std;
using namespace sg;

void print_context_info(AVFormatContext* fmtCtx)
{
  avformat_find_stream_info(fmtCtx, nullptr);

  cout << format("no.stream={}\n", fmtCtx->nb_streams);
  cout << format("duration={}\n", fmtCtx->duration / AV_TIME_BASE);
  cout << format("bitrate={}\n", fmtCtx->bit_rate);
}

void print_all_stream_info(AVFormatContext* ctx, char const* path)
{
  auto r = avformat_find_stream_info(ctx, nullptr);
  if (r != 0)
  {
    cout << "error in print_stream_info" << endl;
    return;
  }

  for (auto i=0; i<ctx->nb_streams; i++)
  {
    auto tp = ctx->streams[i]->codecpar->codec_type;
    if (tp == AVMEDIA_TYPE_AUDIO)
      cout << format("{}th = audio\n", i);
    else if (tp == AVMEDIA_TYPE_VIDEO)
      cout << format("{}th = video\n", i);
    else if (tp == AVMEDIA_TYPE_SUBTITLE)
      cout << format("{}th = subtitle\n", i);

    av_dump_format(ctx, i, path, 0);
  }
}

void print_one_stream_info(AVStream* s)
{
  cout << format("#frames: {}\n", s->nb_frames);
  cout << format("frame rate: {}\n", s->avg_frame_rate.num / s->avg_frame_rate.den);
  cout << format("time base:{}\n", s->time_base.num / s->time_base.den);
  cout << format("width : {}\n", s->codecpar->width);
  cout << format("height: {}\n", s->codecpar->height);
  cout << format("color format: {}\n", s->codecpar->format);
  cout << format("codec: {}\n", (int)s->codecpar->codec_id);
}

void print_codec_info(AVCodec const* codec)
{
  cout << format("id: {}\n", (int)codec->id);
  cout << format("name: {}\n", codec->name);
  cout << format("long name: {}\n", codec->long_name);
  cout << format("capa: {}\n", (int)codec->capabilities);
}

int main(int argc, char** argv)
{
  if (argc != 2) 
  {
    fprintf(stderr, "Usage: %s file\n", argv[0]); 
    exit(1);
  }

  AVFormatContext* fmtCtx = nullptr;
  int r0 = avformat_open_input(&fmtCtx, argv[1], nullptr, nullptr);
  if (r0 != 0) return -1;

  // print_all_stream_info(fmtCtx, argv[1]);
  auto s = fmtCtx->streams[0];
  print_one_stream_info(s);

  auto p = s->codecpar;
  auto codec = avcodec_find_decoder(p->codec_id);
  auto codecCtx = avcodec_alloc_context3(codec);
  avcodec_parameters_to_context(codecCtx, p);
  avcodec_open2(codecCtx, codec, nullptr);
  print_codec_info(codec);

  avcodec_free_context(&codecCtx);
  avformat_close_input(&fmtCtx);

  return 0;
}