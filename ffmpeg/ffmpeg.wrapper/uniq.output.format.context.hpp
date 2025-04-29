#pragma once

#include "ffmpeg.hpp"

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
namespace av::detail {

  struct AVOutputFormatContextDeleter 
  {
    void operator()(AVFormatContext* ctx) const noexcept 
    {
        if (ctx) 
        {
        if (!(ctx->oformat->flags & AVFMT_NOFILE) && ctx->pb) 
            avio_closep(&ctx->pb);

        avformat_free_context(ctx);
        }
    }
  };

}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
namespace av {

  using namespace std;

  class UniqueOutputFormatContext 
  {
  public:
    UniqueOutputFormatContext() = default;
  
    explicit UniqueOutputFormatContext(AVFormatContext* ctx)
      : _cts(ctx) {}
  
    ~UniqueOutputFormatContext() = default;
  
    UniqueOutputFormatContext(const UniqueOutputFormatContext&) = delete;
    UniqueOutputFormatContext& operator=(const UniqueOutputFormatContext&) = delete;
  
    UniqueOutputFormatContext(UniqueOutputFormatContext&&) noexcept = default;
    UniqueOutputFormatContext& operator=(UniqueOutputFormatContext&&) noexcept = default;
  
    // 출력용 Context 생성
    void alloc_output(const string& filename, const AVOutputFormat* fmt = nullptr)
    {
      AVFormatContext* tmp_ctx = nullptr;
      int ret = avformat_alloc_output_context2(&tmp_ctx, fmt, nullptr, filename.c_str());
      if (ret < 0 || !tmp_ctx)
        throw runtime_error("Failed to allocate output context: " + filename);
      
      _cts.reset(tmp_ctx);
    }
  
    auto open_output(const string& filename) -> expected<void, string>
    {
      if (!(_cts->oformat->flags & AVFMT_NOFILE)) 
      {
        int ret = avio_open(&_cts->pb, filename.c_str(), AVIO_FLAG_WRITE);
        if (ret < 0)
          return unexpected<string>(detail::ffmpeg_error_string(ret));
      }

      return {};
    }
  
    auto write_header(AVDictionary** options = nullptr) -> expected<void, string>
    {
      if (avformat_write_header(_cts.get(), options) < 0)
        return unexpected<string>("Failed to write header");

      return {};
    }
  
    auto write_frame(AVPacket* pkt) -> expected<void, string>
    {
      if (av_interleaved_write_frame(_cts.get(), pkt) < 0)
        return unexpected<string>("Failed to write frame");

      return {};
    }
  
    auto write_trailer() -> expected<void, string>
    {
      if (av_write_trailer(_cts.get()) < 0)
        return unexpected<string>("Failed to write trailer");

      return {};
    }
  
    auto get() const noexcept { return _cts.get(); }
    auto release() noexcept { return _cts.release(); }
  
  private:
    unique_ptr<AVFormatContext, detail::AVOutputFormatContextDeleter> _cts;
  };

}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
