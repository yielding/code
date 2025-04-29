#pragma once

#include "ffmpeg.hpp"
#include "av.resource.hpp"

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
namespace av::detail {

  struct AVFormatContextDeleter 
  {
    void operator()(AVFormatContext* ctx) const noexcept 
    {
      if (ctx)
        avformat_close_input(&ctx);
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

  class UniqueFormatContext 
  {
  public:
    UniqueFormatContext() = default;

    explicit UniqueFormatContext(AVFormatContext* ctx)
      : _ctx(ctx) {}

    ~UniqueFormatContext() = default;

    UniqueFormatContext(const UniqueFormatContext&) = delete;
    UniqueFormatContext& operator=(const UniqueFormatContext&) = delete;

    UniqueFormatContext(UniqueFormatContext&&) noexcept = default;
    UniqueFormatContext& operator=(UniqueFormatContext&&) noexcept = default;


    auto open_input(const string& filename, const AVInputFormat* fmt=nullptr, AVDictionary** options=nullptr) 
      -> expected<void, string>
    {
      AVFormatContext* tmp_ctx = nullptr;
      int ret = avformat_open_input(&tmp_ctx, filename.c_str(), fmt, options);
      if (ret < 0)
        return unexpected<string>(detail::ffmpeg_error_string(ret));

      _ctx.reset(tmp_ctx);
      return {};
    }

    auto find_stream_info() -> expected<void, string>
    {
      if (avformat_find_stream_info(_ctx.get(), nullptr) < 0)
        return unexpected<string>("Failed to find stream info");

      return {};
    }

    auto read_packet(UniquePacket& packet) -> expected<bool, string> 
    {
      int ret = av_read_frame(_ctx.get(), packet.get());
      if (ret == AVERROR_EOF) 
        return false;

      if (ret < 0)
        return unexpected<string>(detail::ffmpeg_error_string(ret));

      return true;
    }


    auto get() const noexcept { return _ctx.get(); }
    auto release() noexcept { return _ctx.release(); }

  private:
    unique_ptr<AVFormatContext, detail::AVFormatContextDeleter> _ctx;
  };

}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////