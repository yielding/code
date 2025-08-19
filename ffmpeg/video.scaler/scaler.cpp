#include "scaler.hpp"
#include <stdexcept>

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
namespace av 
{
  Scaler::Scaler(int sw, int sh, AVPixelFormat sFmt,
    int dw, int dh, AVPixelFormat dFmt, int flags) 
  {
    _ctx = sws_getContext(sw, sh, sFmt, dw, dh, dFmt, flags, nullptr, nullptr, nullptr);
  
    if (!_ctx)
      throw std::runtime_error("Failed to initialize SwsContext");
  }
  
  Scaler::~Scaler() 
  {
    sws_freeContext(_ctx);
  }
  
  void Scaler::scale(AVFrame* src, AVFrame* dst) 
  {
    sws_scale(_ctx,
      src->data, src->linesize, 0, src->height,
      dst->data, dst->linesize);
  }
}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////