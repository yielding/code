#include "FrameExporter.hpp"
#include <fstream>
#include <stdexcept>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
FrameExporter::FrameExporter(int width, int height, AVPixelFormat pix_fmt) 
{
  const AVCodec* codec = avcodec_find_encoder(AV_CODEC_ID_MJPEG);
  if (!codec) 
      throw std::runtime_error("MJPEG encoder not found");

  _ctx = avcodec_alloc_context3(codec);
  if (!_ctx) 
      throw std::runtime_error("Failed to allocate codec context");

  _ctx->pix_fmt = pix_fmt;
  _ctx->width = width;
  _ctx->height = height;
  _ctx->time_base = {1, 25};

  if (avcodec_open2(_ctx, codec, nullptr) < 0)
    throw std::runtime_error("Failed to open encoder");
}

FrameExporter::~FrameExporter() 
{
  avcodec_free_context(&_ctx);
}

auto FrameExporter::encode_to_file(AVFrame* frame, const std::string& filename) -> bool 
{
  AVPacket* pkt = av_packet_alloc();
  if (!pkt) 
    return false;

  bool success = false;
  if (avcodec_send_frame(_ctx, frame) >= 0 && avcodec_receive_packet(_ctx, pkt) >= 0) 
  {
    std::ofstream ofs(filename, std::ios::binary);
    if (ofs.good()) 
    {
      ofs.write(reinterpret_cast<char*>(pkt->data), pkt->size);
      success = true;
    }
  }

  av_packet_free(&pkt);
  return success;
}

auto FrameExporter::encode_to_memory(AVFrame* frame) -> std::vector<uint8_t> 
{
  std::vector<uint8_t> output;
  AVPacket* pkt = av_packet_alloc();
  if (pkt && avcodec_send_frame(_ctx, frame) >= 0 && avcodec_receive_packet(_ctx, pkt) >= 0) 
    output.assign(pkt->data, pkt->data + pkt->size);

  av_packet_free(&pkt);
  return output;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////