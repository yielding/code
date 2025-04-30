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
      // TODO
      // avformat_free_context(&ctx)와 구분하기

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
      if (int ret = avformat_open_input(&tmp_ctx, filename.c_str(), fmt, options); ret < 0)
        return unexpected(detail::ffmpeg_error_string(ret));

      _ctx.reset(tmp_ctx);
      return {};
    }

    auto reset(AVFormatContext* ctx) -> void
    {
      _ctx.reset(ctx);
    }

    [[nodiscard]]
    auto find_stream_info() const -> expected<void, string>
    {
      if (avformat_find_stream_info(_ctx.get(), nullptr) < 0)
        return unexpected("Failed to find stream info"s);

      return {};
    }

    [[nodiscard]]
    auto read_packet(const UniquePacket& packet) const -> expected<bool, string>
    {
      const int ret = av_read_frame(_ctx.get(), packet.get());
      // TODO
      // false도 unexpected로 처리한다.
      if (ret == AVERROR_EOF) 
        return false;

      if (ret < 0)
        return unexpected(detail::ffmpeg_error_string(ret));

      return true;
    }

    [[nodiscard]]
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