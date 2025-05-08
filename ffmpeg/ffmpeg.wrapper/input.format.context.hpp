#pragma once

#include <memory.hpp>
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

  class InputFormatContext : public core::memory::move_only<InputFormatContext>
  {
  public:
    InputFormatContext() = default;

    explicit InputFormatContext(AVFormatContext* ctx)
      : _ctx{ctx} {}

    auto open_input(const string& filename, const AVInputFormat* fmt=nullptr, AVDictionary** options=nullptr)
      -> expected<void, string>
    {
      AVFormatContext* tmp_ctx = nullptr;
      if (const int ret = avformat_open_input(&tmp_ctx, filename.c_str(), fmt, options); ret < 0)
        return unexpected(detail::ffmpeg_error_string(ret));

      if (avformat_find_stream_info(tmp_ctx, nullptr) < 0)
        return unexpected("Failed to find stream info"s);

      _ctx.reset(tmp_ctx);
      const auto ctx = _ctx.get();

      _vindex = av_find_best_stream(ctx, AVMEDIA_TYPE_VIDEO, -1, -1, nullptr, 0);
      _aindex = av_find_best_stream(ctx, AVMEDIA_TYPE_AUDIO, -1, _vindex, nullptr, 0);
      _vcodec = get_codec(ctx, _vindex);
      _acodec = get_codec(ctx, _aindex);

      return {};
    }

    [[nodiscard]] auto format_context() const { return _ctx.get(); }

    [[nodiscard]] auto video_index() const { return _vindex; }
    [[nodiscard]] auto video_codec_name() const { return _vcodec->long_name; }
    [[nodiscard]] auto video_codec_name_short() const { return _vcodec->name; }
    [[nodiscard]] auto video_codec_id() const { return _vcodec->id; }

    [[nodiscard]] auto audio_index() const { return _aindex; }
    [[nodiscard]] auto audio_codec_name() const { return _acodec->long_name; }
    [[nodiscard]] auto audio_codec_name_short() const { return _acodec->name; }
    [[nodiscard]] auto audio_codec_id() const { return _acodec->id; }

    auto reset(AVFormatContext* ctx) { _ctx.reset(ctx); }

    [[nodiscard]]
    auto read_packet(const UniquePacket& packet) const -> expected<bool, string>
    {
      const int ret = av_read_frame(_ctx.get(), packet.get());
      if (ret == AVERROR_EOF) 
        return false;

      if (ret < 0)
        return unexpected(detail::ffmpeg_error_string(ret));

      return true;
    }

    [[nodiscard]]
    auto get() const noexcept { return _ctx.get(); }
    auto release() noexcept   { return _ctx.release(); }

  private:
    static auto get_codec(const AVFormatContext* ctx, const int index) -> const AVCodec*
    {
      const auto p = ctx->streams[index]->codecpar;
      const auto codec = avcodec_find_decoder(p->codec_id);
      auto codec_context = avcodec_alloc_context3(codec);

      avcodec_parameters_to_context(codec_context, p);
      avcodec_open2(codec_context, codec, nullptr);
      avcodec_free_context(&codec_context);

      return codec;
    }

  private:
    unique_ptr<AVFormatContext, detail::AVFormatContextDeleter> _ctx;
    int _vindex{-1}, _aindex{-1};
    const AVCodec *_vcodec{nullptr};
    const AVCodec *_acodec{nullptr};
  };

}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////