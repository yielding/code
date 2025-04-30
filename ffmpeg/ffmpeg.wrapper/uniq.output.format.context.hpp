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
        if (ctx->pb && !(ctx->oformat->flags & AVFMT_NOFILE))
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
      : _ctx(ctx) {}
  
    ~UniqueOutputFormatContext() = default;
  
    UniqueOutputFormatContext(const UniqueOutputFormatContext&) = delete;
    UniqueOutputFormatContext& operator=(const UniqueOutputFormatContext&) = delete;
  
    UniqueOutputFormatContext(UniqueOutputFormatContext&&) noexcept = default;
    UniqueOutputFormatContext& operator=(UniqueOutputFormatContext&&) noexcept = default;
  
    // 출력용 Context 생성
    void alloc_output(const string& filename, const AVOutputFormat* fmt = nullptr)
    {
      AVFormatContext* tmp_ctx = nullptr;
      const int ret = avformat_alloc_output_context2(&tmp_ctx, fmt, nullptr, filename.c_str());
      if (ret < 0 || !tmp_ctx)
        throw runtime_error("Failed to allocate output context: " + filename);
      
      _ctx.reset(tmp_ctx);
    }
  
    [[nodiscard]]
    auto open_output(const string& filename) const -> expected<void, string>
    {
      if (!(AVFMT_NOFILE & _ctx->oformat->flags))
      {
        if (const int ret = avio_open(&_ctx->pb, filename.c_str(), AVIO_FLAG_WRITE); ret < 0)
          return unexpected(detail::ffmpeg_error_string(ret));
      }

      return {};
    }
  
    auto write_header(AVDictionary** options = nullptr) const -> expected<void, string>
    {
      if (avformat_write_header(_ctx.get(), options) < 0)
        return unexpected("Failed to write header"s);

      return {};
    }
  
    auto write_frame(AVPacket* pkt) const -> expected<void, string>
    {
      if (av_interleaved_write_frame(_ctx.get(), pkt) < 0)
        return unexpected("Failed to write frame"s);

      return {};
    }
  
    [[nodiscard]]
    auto write_trailer() const -> expected<void, string>
    {
      if (av_write_trailer(_ctx.get()) < 0)
        return unexpected("Failed to write trailer"s);

      return {};
    }
  
    [[nodiscard]]
    auto get() const noexcept { return _ctx.get(); }
    auto release() noexcept { return _ctx.release(); }
  
  private:
    unique_ptr<AVFormatContext, detail::AVOutputFormatContextDeleter> _ctx;
  };

}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
