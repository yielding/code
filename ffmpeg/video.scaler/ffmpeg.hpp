#pragma once

extern "C" {
#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libavutil/frame.h>
#include <libavutil/imgutils.h>
#include <libswscale/swscale.h>
}

#include <expected>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>
#include <functional>

////////////////////////////////////////////////////////////////////////////////
///
/// Internal includes for ffmpeg
///
////////////////////////////////////////////////////////////////////////////////
namespace av 
{
  enum class DecodeResult : int
  {
    FrameAvailable, // 프레임 디코딩 성공
    NeedMoreData,   // EAGAIN
    EndOfStream     // EOF
  };
}

namespace av::detail 
{
  inline auto ffmpeg_error_string(const int err_no) -> std::string
  {
    char buffer[AV_ERROR_MAX_STRING_SIZE] = {};
    av_strerror(err_no, buffer, sizeof(buffer));

    return {buffer};
  }
}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
