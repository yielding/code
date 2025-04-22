extern "C" {
#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libavutil/imgutils.h>
#include <libswscale/swscale.h>
}

#include <tuple>
#include <iostream>
#include <fstream>
#include <memory>

using namespace std;

// ----- Smart pointer wrappers for FFmpeg types -----
auto AVFormatContextDeleter = [](AVFormatContext* p) { avformat_close_input(&p); };
auto AVCodecContextDeleter = [](AVCodecContext* p) { avcodec_free_context(&p); };
auto AVFrameDeleter = [](AVFrame* p) { av_frame_free(&p); };
auto AVPacketDeleter = [](AVPacket* p) { av_packet_free(&p); };
auto SwsDeleter = [](SwsContext* p) { sws_freeContext(p); }

using AVFormatContextPtr = unique_ptr<AVFormatContext, decltype(AVFormatContextDeleter)>;
using AVCodecContextPtr = unique_ptr<AVCodecContext, decltype(AVCodecContextDeleter)>;
using AVFramePtr = unique_ptr<AVFrame, decltype(AVFrameDeleter)>;
using AVPacketPtr = unique_ptr<AVPacket, decltype(AVPacketDeleter)>;
using SwsContextPtr = unique_ptr<SwsContext, decltype(SwsDeleter)>;

// ----- Resize frame -----
auto resize_frame(AVFrame* src, AVCodecContext* ctx, int dst_w, int dst_h, AVPixelFormat dst_fmt) -> AVFramePtr 
{
  auto dst = AVFramePtr(av_frame_alloc(), AVFrameDeleter);
  dst->format = dst_fmt;
  dst->width  = dst_w;
  dst->height = dst_h;

  if (av_frame_get_buffer(dst.get(), 32) < 0) 
    return nullptr;

  auto sws_ctx = sws_getContext(
    ctx->width, ctx->height, ctx->pix_fmt,
    dst_w, dst_h, dst_fmt,
    SWS_BICUBIC, nullptr, nullptr, nullptr);

  if (!sws_ctx) 
    return nullptr;

  sws_scale(sws_ctx, src->data, src->linesize, 0, ctx->height, dst->data, dst->linesize);
  sws_freeContext(sws_ctx);

  return dst;
}

int save_frame_as_jpeg(AVCodecContext* src_ctx, AVFrame* frame, int frame_no, int out_w, int out_h) 
{
  const auto jpeg_codec = avcodec_find_encoder(AV_CODEC_ID_MJPEG);
  if (!jpeg_codec) 
      return -1;

  auto jpeg_ctx = AVCodecContextPtr(avcodec_alloc_context3(jpeg_codec), AVCodecContextDeleter);
  jpeg_ctx->pix_fmt = AV_PIX_FMT_YUVJ420P;
  jpeg_ctx->width   = out_w;
  jpeg_ctx->height  = out_h;
  jpeg_ctx->time_base = AVRational{1, 25};

  if (avcodec_open2(jpeg_ctx.get(), jpeg_codec, nullptr) < 0) 
    return -1;

  auto resized = resize_frame(frame, src_ctx, out_w, out_h, jpeg_ctx->pix_fmt);
  if (!resized) 
    return -1;

  if (avcodec_send_frame(jpeg_ctx.get(), resized.get()) < 0) 
    return -1;

  auto pkt = AVPacketPtr(av_packet_alloc(), AVPacketDeleter);
  if (avcodec_receive_packet(jpeg_ctx.get(), pkt.get()) < 0) 
    return -1;

  ofstream ofs(to_string(frame_no) + ".jpg", ios::binary);
  if (!ofs) 
    return -1;

  ofs.write(reinterpret_cast<char*>(pkt->data), pkt->size);

  cout << "Saved frame " << frame_no << " as JPEG" << endl;

  return 0;
}

// ----- Open video file and find decoder -----
auto open_input_file(const char* filename) -> tuple<int, AVFormatContextPtr, AVCodecContextPtr> 
{
  auto fail = make_tuple(-1, nullptr, nullptr);

  AVFormatContext* raw_fmt = nullptr;
  if (avformat_open_input(&raw_fmt, filename, nullptr, nullptr) < 0) 
    return fail;

  auto fmt_ctx = AVFormatContextPtr(raw_fmt, AVFormatContextDeleter);

  if (avformat_find_stream_info(fmt_ctx.get(), nullptr) < 0) 
    return fail;

  const AVCodec* dec = nullptr;
  int idx = av_find_best_stream(fmt_ctx.get(), AVMEDIA_TYPE_VIDEO, -1, -1, &dec, 0);
  if (idx < 0) 
    return fail;

  AVCodecContext* raw_ctx = avcodec_alloc_context3(dec);
  if (!raw_ctx) 
    return fail;

  auto dec_ctx = AVCodecContextPtr(raw_ctx, AVCodecContextDeleter);

  avcodec_parameters_to_context(dec_ctx.get(), fmt_ctx->streams[idx]->codecpar);
  if (avcodec_open2(dec_ctx.get(), dec, nullptr) < 0) 
    return fail;

  return {idx, std::move(fmt_ctx), std::move(dec_ctx)};
}

// ----- Main -----
int main(int argc, char** argv) 
{
  if (argc != 2) 
  {
    cerr << "Usage: " << argv[0] << " input_file" << endl;
    return 1;
  }

  auto [video_stream_index, fmt_ctx, dec_ctx] = open_input_file(argv[1]);
  if (video_stream_index < 0) 
    return 1;

  auto frame  = AVFramePtr(av_frame_alloc(), AVFrameDeleter);
  auto packet = AVPacketPtr(av_packet_alloc(), AVPacketDeleter);

  auto index = 0;
  while (av_read_frame(fmt_ctx.get(), packet.get()) >= 0) 
  {
    if (packet->stream_index != video_stream_index) 
    {
      av_packet_unref(packet.get());
      continue;
    }

    if (avcodec_send_packet(dec_ctx.get(), packet.get()) < 0) 
      break;

    while (avcodec_receive_frame(dec_ctx.get(), frame.get()) >= 0) 
    {
      int new_w = 64;
      int new_h = frame->height * new_w / frame->width;

      frame->pts = frame->best_effort_timestamp;
      save_frame_as_jpeg(dec_ctx.get(), frame.get(), dec_ctx->frame_num, new_w, new_h);
      av_frame_unref(frame.get());
    }

    av_packet_unref(packet.get());

    index++;
    if (index == 300)
      break;
  }

  return 0;
}
