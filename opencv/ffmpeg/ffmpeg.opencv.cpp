extern "C" {
#include "libavformat/avformat.h"
#include "libavcodec/avcodec.h"
#include "libavutil/avutil.h"
#include "libavutil/pixdesc.h"
#include "libavutil/imgutils.h"
#include "libswscale/swscale.h"
}

#include <opencv2/core.hpp>
#include <opencv2/highgui.hpp>

#include <vector>
#include <cstdlib>
#include <format>

using namespace std;
using namespace cv;

auto main() -> int
{
  auto home = getenv("HOME");
  auto file = "IMG_4164.mov";
  auto path = std::format("{}/Desktop/{}", home, file);
  AVFormatContext* in_fmt_ctx = nullptr;
  if (avformat_open_input(&in_fmt_ctx, path.c_str(), nullptr, nullptr) < 0)
    return -1;

  if (avformat_find_stream_info(in_fmt_ctx, nullptr) < 0)
    return -2;

  const AVCodec* codec;
  auto vsi = av_find_best_stream(in_fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, &codec, 0);
  if (vsi < 0)
    return -3;

  auto decoder_context = avcodec_alloc_context3(codec);
  if (decoder_context == nullptr)
    return -5;

  avcodec_parameters_to_context(decoder_context, in_fmt_ctx->streams[vsi]->codecpar);

  if (avcodec_open2(decoder_context, codec, nullptr) < 0)
    return -6;

  auto w = decoder_context->width;
  auto h = decoder_context->height;
  auto src_pix_fmt = decoder_context->pix_fmt;
  auto dst_pix_fmt = AV_PIX_FMT_BGR24;
  auto sws_ctx = sws_getContext(
    w, h, src_pix_fmt,
    w, h, dst_pix_fmt,
    SWS_BICUBIC, nullptr, nullptr, nullptr);

  if (!sws_ctx)
    return -7;

  Mat image(h, w, CV_8UC3);
  AVFrame mat_frame = { 0 };
  mat_frame.data[0] = (uint8_t *)image.data;
  av_image_fill_arrays(mat_frame.data, mat_frame.linesize, mat_frame.data[0], dst_pix_fmt, w, h, 1);

  AVFrame video_frame = { 0 };
  AVPacket packet = { 0 };

  while (av_read_frame(in_fmt_ctx, &packet) == 0)
  {
    if (packet.stream_index == vsi)
    {
      auto ret = avcodec_send_packet(decoder_context, &packet);
      if (ret != 0) 
      {
        av_packet_unref(&packet);
        continue;
      }

      while (true)
      {
        ret = avcodec_receive_frame(decoder_context, &video_frame);
        if (ret == AVERROR(EAGAIN))
          break;

        sws_scale(sws_ctx,
          video_frame.data, video_frame.linesize, 0, video_frame.height, mat_frame.data, mat_frame.linesize);

        imshow("press ESC to exit", image);
        if (waitKey(1) == 0x1b) break;
      }
    }

    av_packet_unref(&packet);
  }

  av_frame_unref(&video_frame);
  av_frame_unref(&mat_frame);
  avcodec_free_context(&decoder_context);
  avformat_close_input(&in_fmt_ctx);

  return 0;
}
