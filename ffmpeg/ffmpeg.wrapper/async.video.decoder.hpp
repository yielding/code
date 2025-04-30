#pragma once

#include "ffmpeg.hpp"
#include "video.decoder.hpp"
#include "frame.queue.hpp"
#include "uniq.format.context.hpp"
#include "av.resource.hpp"
#include <thread>
#include <atomic>

#include <iostream>

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
namespace av {

using namespace std;

/*
class AsyncVideoDecoder
{
public:
  explicit AsyncVideoDecoder(const AVCodec *codec)
    : _codec{codec}, _decoder{codec}, _running{false}
  {
  }

  auto open(AVDictionary **options = nullptr) const -> expected <void, string>
  {
    return _decoder.open(_codec, options);
  }

  auto open_input(const string &filename) -> expected <void, string>
  {
    return _input.open_input(filename)
      .and_then([this] { return _input.find_stream_info(); });
  }

  void start()
  {
    _running = true;
    _thread = thread(&AsyncVideoDecoder::decode_loop, this);
  }

  void stop()
  {
    _running = false;

    if (_thread.joinable())
      _thread.join();
  }

  auto &frames() { return _frame_queue; }

private:
  void decode_loop()
  {
    UniquePacket packet;
    UniqueFrame frame;

    while (_running)
    {
      auto r0 = _input.read_packet(packet);
      if (!r0) break;

      if (!r0.value()) break;

      auto r1 = _decoder.decode_next(packet, frame.get());

      if (r1 && r1.value() == DecodeResult::FrameAvailable)
      {
        auto frame_copy = std::make_unique <AVFrame>();
        av_frame_ref(frame_copy.get(), frame.get());
        _frame_queue.push(std::move(frame_copy));
      }
      else
      {
        cout << r1.error() << endl;
      }

      packet.reset();
      frame.reset();
    }

    while (_running && !_decoder.eof())
    {
      if (auto res = _decoder.flush_next(frame.get());
        res && res.value() == DecodeResult::FrameAvailable)
      {
        auto frame_copy = std::make_unique <AVFrame>();
        av_frame_ref(frame_copy.get(), frame.get());
        _frame_queue.push(std::move(frame_copy));
      }

      frame.reset();
    }
  }

private:
  const AVCodec *_codec{nullptr};
  VideoDecoder _decoder;
  UniqueFormatContext _input;
  FrameQueue _frame_queue;
  atomic <bool> _running;
  thread _thread;
};
*/
}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
