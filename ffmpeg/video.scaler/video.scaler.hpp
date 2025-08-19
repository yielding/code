#pragma once

#include "ffmpeg.hpp"
#include "scaler.hpp"
#include <vector>
#include <string>

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
namespace av 
{
  class VideoScaler 
  {
  public:
    VideoScaler(int sw, int sh, AVPixelFormat sFmt, int dw, int dh, AVPixelFormat dFmt);
  
    ~VideoScaler();
  
    auto scale_frame(AVFrame* src) -> AVFrame*;
    auto scale_to_file(AVFrame* src, const std::string& filename) -> bool;
    auto scale_to_vector(AVFrame* src) -> std::vector<uint8_t>;
  
  private:
    int _dstW, _dstH;
    AVPixelFormat _dstFmt;
    Scaler _scaler;
  };
}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////