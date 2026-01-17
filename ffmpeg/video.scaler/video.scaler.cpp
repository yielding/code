#include "ffmpeg.hpp"
#include "video.scaler.hpp"
#include <fstream>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace av 
{
  VideoScaler::VideoScaler(int srcW, int srcH, AVPixelFormat srcFmt, int dstW, int dstH, AVPixelFormat dstFmt)
    : _dstW(dstW), _dstH(dstH), _dstFmt(dstFmt),
      _scaler(srcW, srcH, srcFmt, dstW, dstH, dstFmt) 
  {}
  
  VideoScaler::~VideoScaler() = default;
  
  auto VideoScaler::scale_frame(AVFrame* src) -> AVFrame* 
  {
    AVFrame* dst = av_frame_alloc();
    if (!dst) throw runtime_error("Failed to allocate frame");
  
    dst->format = _dstFmt;
    dst->width = _dstW;
    dst->height = _dstH;
  
    if (av_frame_get_buffer(dst, 32) < 0) 
    {
      av_frame_free(&dst);
      throw runtime_error("Failed to allocate frame buffer");
    }
  
    _scaler.scale(src, dst);

    return dst;
  }
  
  bool VideoScaler::scale_to_file(AVFrame* src, const string& filename) 
  {
    auto jpeg_codec = avcodec_find_encoder(AV_CODEC_ID_MJPEG);
    if (!jpeg_codec) return false;
  
    auto jpeg_ctx = avcodec_alloc_context3(jpeg_codec);
    if (!jpeg_ctx) return false;
  
    jpeg_ctx->pix_fmt = _dstFmt;
    jpeg_ctx->width = _dstW;
    jpeg_ctx->height = _dstH;
    jpeg_ctx->time_base = AVRational{1, 25};
  
    if (avcodec_open2(jpeg_ctx, jpeg_codec, nullptr) < 0) 
    {
      avcodec_free_context(&jpeg_ctx);
      return false;
    }
  
    auto scaled = scale_frame(src);
    
    if (!scaled) 
    {
      avcodec_free_context(&jpeg_ctx);
      return false;
    }
  
    if (avcodec_send_frame(jpeg_ctx, scaled) < 0) 
    {
      av_frame_free(&scaled);
      avcodec_free_context(&jpeg_ctx);
      return false;
    }
  
    auto pkt = av_packet_alloc();
    bool result = false;
    if (avcodec_receive_packet(jpeg_ctx, pkt) >= 0) 
    {
      ofstream ofs(filename, ios::binary);
      if (ofs.good()) 
      {
        ofs.write(reinterpret_cast<char*>(pkt->data), pkt->size);
        result = true;
      }
  
      av_packet_unref(pkt);
    }
  
    av_packet_free(&pkt);
    av_frame_free(&scaled);
    avcodec_free_context(&jpeg_ctx);

    return result;
  }
  
  auto VideoScaler::scale_to_vector(AVFrame* src) -> vector<uint8_t> 
  {
    vector<uint8_t> output;
  
    auto jpeg_codec = avcodec_find_encoder(AV_CODEC_ID_MJPEG);
    if (!jpeg_codec) 
      return output;
  
    auto jpeg_ctx = avcodec_alloc_context3(jpeg_codec);
    if (!jpeg_ctx) 
      return output;
  
    jpeg_ctx->pix_fmt = _dstFmt;
    jpeg_ctx->width   = _dstW;
    jpeg_ctx->height  = _dstH;
    jpeg_ctx->time_base = AVRational{1, 25};
  
    if (avcodec_open2(jpeg_ctx, jpeg_codec, nullptr) < 0)
    {
      avcodec_free_context(&jpeg_ctx);
      return output;
    }
  
    AVFrame* scaled = scale_frame(src);
    if (!scaled) 
    {
      avcodec_free_context(&jpeg_ctx);
      return output;
    }
  
    if (avcodec_send_frame(jpeg_ctx, scaled) < 0) 
    {
      av_frame_free(&scaled);
      avcodec_free_context(&jpeg_ctx);
      return output;
    }
  
    AVPacket* pkt = av_packet_alloc();
    if (avcodec_receive_packet(jpeg_ctx, pkt) >= 0) 
    {
      output.assign(pkt->data, pkt->data + pkt->size);
      av_packet_unref(pkt);
    }
  
    av_packet_free(&pkt);
    av_frame_free(&scaled);
    avcodec_free_context(&jpeg_ctx);
  
    return output;
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////