extern "C" {
#include "libavformat/avformat.h"
#include "libavcodec/avcodec.h"
#include "libavutil/imgutils.h"
#include "libswscale/swscale.h"
}

#include <iostream>
#include <fstream>
#include <tuple>
#include <scope_guard.hpp>

using namespace std;
using namespace sg;

auto open_input_file(const char* filename) 
  -> tuple<int, AVFormatContext*, AVCodecContext*>
{
  auto fail = make_tuple(-1, nullptr, nullptr);

  AVFormatContext* fmt_ctx = nullptr;
  if (avformat_open_input(&fmt_ctx, filename, nullptr, nullptr) < 0) 
    return fail;

  if (avformat_find_stream_info(fmt_ctx, nullptr) < 0) 
    return fail;

  // select the video stream
  const AVCodec* dec;
  auto r = av_find_best_stream(fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, &dec, 0);
  if (r < 0) 
    return fail;

  auto video_stream_index = r;

  // create decoding context
  auto decoder_ctx = avcodec_alloc_context3(dec);
  if (decoder_ctx == nullptr)
    return fail;

  avcodec_parameters_to_context(decoder_ctx, 
      fmt_ctx->streams[video_stream_index]->codecpar);

  // init the video decoder
  if (avcodec_open2(decoder_ctx, dec, nullptr) < 0) 
    return fail;

  return make_tuple(video_stream_index, fmt_ctx, decoder_ctx);
}

auto resize_frame(AVFrame* src_frame, AVCodecContext* src_ctx, 
              int dst_width, int dst_height, AVPixelFormat dst_pixfmt) -> AVFrame* 
{
  auto dst_frame = av_frame_alloc();
  if (dst_frame == nullptr)
    return nullptr;

  dst_frame->width  = dst_width;
  dst_frame->height = dst_height;
  dst_frame->format = dst_pixfmt;

  if (av_frame_get_buffer(dst_frame, 0) < 0)
  {
    av_frame_free(&dst_frame);
    return nullptr;
  }

  auto scaler_ctx = sws_getContext(
      src_ctx->width, src_ctx->height, src_ctx->pix_fmt,
      dst_width, dst_height, dst_pixfmt,
      SWS_BICUBIC, nullptr, nullptr, nullptr);

  if (scaler_ctx == nullptr)
  {
    av_frame_free(&dst_frame);
    return nullptr;
  }

  sws_freeContext(scaler_ctx);

  return dst_frame;
}

int save_frame_as_jpeg(AVCodecContext* codec_ctx, AVFrame* frame, int frame_no, int new_width, int new_height)
{
  auto jpeg_codec = avcodec_find_encoder(AV_CODEC_ID_MJPEG);
  if (!jpeg_codec) return -1;

  auto jpeg_ctx = avcodec_alloc_context3(jpeg_codec);
  if (!jpeg_ctx) return -1;

  auto g0 = make_scope_guard([&] { avcodec_free_context(&jpeg_ctx); });

  jpeg_ctx->pix_fmt   = codec_ctx->pix_fmt;
  jpeg_ctx->width     = new_width;
  jpeg_ctx->height    = new_height;
  jpeg_ctx->time_base = AVRational{ 1,10 };
  jpeg_ctx->strict_std_compliance = FF_COMPLIANCE_UNOFFICIAL;

  if (avcodec_open2(jpeg_ctx, jpeg_codec, nullptr) < 0)
    return -1;

  auto dst_frame = resize_frame(frame, codec_ctx, new_width, new_height, codec_ctx->pix_fmt);
  if (dst_frame == nullptr) return -1;

  auto g1 = make_scope_guard([&] { av_frame_free(&dst_frame); });

  if (avcodec_send_frame(jpeg_ctx, dst_frame) < 0)
    return -1;

  auto packet = av_packet_alloc();
  if (packet == nullptr) return -1;

  auto g2 = make_scope_guard([&] { av_packet_free(&packet); });

  if (avcodec_receive_packet(jpeg_ctx, packet) < 0)
    return -1;

  auto ofs = ofstream(to_string(frame_no) + ".jpg", ios::binary);
  if (ofs.good())
    ofs.write((char*)packet->data, packet->size);

  return 0;
}

int main(int argc, char** argv)
{
  if (argc != 2) { fprintf(stderr, "Usage: %s file\n", argv[0]); exit(1); }

  auto frame = av_frame_alloc(); 
  auto packet = av_packet_alloc();

  if (frame == nullptr || packet == nullptr) 
    return 0;

  auto g1 = make_scope_guard([&frame]  { av_frame_free((AVFrame **)&frame); });
  auto g2 = make_scope_guard([&packet] { av_packet_free((AVPacket **)&packet); });

  auto [video_stream_index, fmt_ctx, dec_ctx]
    = open_input_file(argv[1]);

  if (video_stream_index == -1)
    return 0;

  auto g3 = make_scope_guard([&dec_ctx] { avcodec_free_context((AVCodecContext**)&dec_ctx); });
  auto g4 = make_scope_guard([&fmt_ctx] { avformat_close_input((AVFormatContext**)&fmt_ctx); });

  int ret;
  while (true)
  {
    if ((ret = av_read_frame(fmt_ctx, packet)) < 0) break;
    if (packet->stream_index != video_stream_index) 
    {
      av_packet_unref(packet);
      continue;
    }

    ret = avcodec_send_packet(dec_ctx, packet);
    if (ret < 0) 
      break;
    
    while (ret >= 0)
    {
      ret = avcodec_receive_frame(dec_ctx, frame);
      if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF) break;
      if (ret < 0) return 0;

      frame->pts = frame->best_effort_timestamp;

      while (1) 
      {
        int new_width  = 64;
        int new_height = int(frame->height * 64.0 / frame->width);
        cout << "frame_no: " << dec_ctx->frame_num << endl;
        ret = save_frame_as_jpeg(dec_ctx, frame, dec_ctx->frame_num, new_width, new_height);
        if (ret < 0) return 1;

        // TODO without return, we can go on
        //if (ret >= 0) return 1;
        if (ret >= 0) break;
      }

      av_frame_unref(frame);
    }

    av_packet_unref(packet);
  }

  return 0;
}