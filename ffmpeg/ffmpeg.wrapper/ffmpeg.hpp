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

////////////////////////////////////////////////////////////////////////////////
///
/// Internal include for ffmpeg
///
////////////////////////////////////////////////////////////////////////////////
namespace av::detail {

  inline auto ffmpeg_error_string(int err_no) -> std::string 
  {
    char buffer[AV_ERROR_MAX_STRING_SIZE] = {0};
    av_strerror(err_no, buffer, sizeof(buffer));

    return std::string(buffer);
  }
}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
