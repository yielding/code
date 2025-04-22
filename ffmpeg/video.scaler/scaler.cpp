#include "scaler.hpp"
#include <stdexcept>

Scaler::Scaler(int srcW, int srcH, AVPixelFormat srcFmt,
    int dstW, int dstH, AVPixelFormat dstFmt,
    int flags) 
{
  _ctx = sws_getContext(
      srcW, srcH, srcFmt,
      dstW, dstH, dstFmt,
      flags, nullptr, nullptr, nullptr);

  if (!_ctx)
    throw std::runtime_error("Failed to initialize SwsContext");
}

Scaler::~Scaler() 
{
  sws_freeContext(_ctx);
}

void Scaler::scale(AVFrame* src, AVFrame* dst) 
{
  sws_scale(
      _ctx,
      src->data, src->linesize,
      0, src->height,
      dst->data, dst->linesize);
}