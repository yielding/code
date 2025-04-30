extern "C" {
#include "libavformat/avformat.h"
#include "libavcodec/avcodec.h"
#include "libavutil/imgutils.h"
#include "libswscale/swscale.h"
}

#include <iostream>
#include <tuple>
#include <format>
#include <print>
#include <scope_guard.hpp>

using namespace std;
using namespace sg;

void print_context_info(AVFormatContext* fmtCtx)
{
  avformat_find_stream_info(fmtCtx, nullptr);

  println("no.stream={}", fmtCtx->nb_streams);
  println("duration={}", fmtCtx->duration / AV_TIME_BASE);
  println("bitrate={}", fmtCtx->bit_rate);
}

void print_all_stream_info(AVFormatContext* ctx, char const* path)
{
  if (avformat_find_stream_info(ctx, nullptr) < 0)
  {
    println("error in avformat_find_stream_info");
    return;
  }

  for (auto i=0; i<ctx->nb_streams; i++)
  {
    auto tp = ctx->streams[i]->codecpar->codec_type;
    if (tp == AVMEDIA_TYPE_AUDIO)
      println("{}th = audio", i);
    else if (tp == AVMEDIA_TYPE_VIDEO)
      println("{}th = video", i);
    else if (tp == AVMEDIA_TYPE_SUBTITLE)
      println("{}th = subtitle", i);

    av_dump_format(ctx, i, path, 0);
  }
}

void print_one_stream_info(AVStream* s)
{
  println("#frames: {}", s->nb_frames);
  println("frame rate: {}", s->avg_frame_rate.num / s->avg_frame_rate.den);
  println("time base: {}", s->time_base.num / s->time_base.den);
  println("width : {}", s->codecpar->width);
  println("height: {}", s->codecpar->height);
  println("color format: {}", s->codecpar->format);
  println("codec: {}", (int)s->codecpar->codec_id);
}

void print_codec_info(AVCodec const* codec)
{
  println("id: {}", (int)codec->id);
  println("name: {}", codec->name);
  println("long name: {}", codec->long_name);
  println("capa: {}", (int)codec->capabilities);
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