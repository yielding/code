#pragma once

#include <memory.hpp>
#include <utility>

#include "ffmpeg.hpp"

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
namespace av::detail {

  struct AVCodecParserDeleter
  {
    void operator()(AVCodecParserContext* parser) const noexcept 
    {
      if (parser)
        av_parser_close(parser);
    }
  };

}

////////////////////////////////////////////////////////////////////////////////
///
/// packet parsing을 담당
/// - container가 없는 단순한 스트림 raw H.264 stream
/// - 프레임의 경계가 명확하지 않은 경우
/// - 네트워크에서 들어오는 연속된 데이터 스트림 처리
///
////////////////////////////////////////////////////////////////////////////////
namespace av {

  using namespace std;

  class CodecParser : public core::memory::move_only<CodecParser>
  {
  public:
    explicit CodecParser(AVCodecID codec_id) 
      : _parser{av_parser_init(codec_id)}
    {
      if (!_parser)
        throw runtime_error("Failed to initialize codec parser");
    }
  
    auto get() const noexcept { return _parser.get(); }
  
    auto parse(AVCodecContext* ctx,
        uint8_t** out_data, int* out_size,
        const uint8_t* in_data, int in_size,
        int64_t pts = AV_NOPTS_VALUE,
        int64_t dts = AV_NOPTS_VALUE,
        int64_t pos = AV_NOPTS_VALUE) -> expected<int, string> 
    {
      // consumed size
      int ret = av_parser_parse2(_parser.get(), ctx, out_data, out_size, in_data, in_size, pts, dts, pos);

      if (ret < 0)
        return unexpected(detail::ffmpeg_error_string(ret));
      
      return ret;  
    }
  
  private:
    unique_ptr<AVCodecParserContext, detail::AVCodecParserDeleter> _parser;
  };

}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
