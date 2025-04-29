#pragma once

#include "ffmpeg.hpp"
#include <thread>
#include <atomic>
#include "video_decoder.hpp"
#include "frame_queue.hpp"
#include "av.resource.hpp" // UniquePacket, UniqueFrame

namespace av {

  using namespace std;

  class AsyncVideoDecoder 
  {
  public:
    AsyncVideoDecoder(const AVCodec* codec)
      : _decoder(codec), _running(false) 
    {}
  
    auto open(const AVCodec* codec, AVDictionary** options = nullptr) -> expected<void, string> 
    {
      return _decoder.open(codec, options);
    }
  
    auto open_input(const string& filename) -> expected<void, std::string> 
    {
      if (auto res = _input.open_input(filename.c_str()); !res)
        return res.error();
      
      if (auto res = _input.find_stream_info(); !res)
        return res.error();
      
      return {};
    }
  
    void start() 
    {
      _running = true;
      _thread  = std::thread(&AsyncVideoDecoder::decode_loop, this);
    }
  
    void stop() 
    {
      _running = false;

      if (_thread.joinable())
        _thread.join();
    }
  
    auto& frames() { return _frame_queue; }
  
  private:
    void decode_loop() 
    {
      UniquePacket packet;
      UniqueFrame frame;
  
      while (_running && av_read_frame(_input.get(), packet.get()) >= 0) 
      {
        auto res = _decoder.decode_next(packet, frame.get());
        if (res && res.value() == DecodeResult::FrameAvailable) 
        {
          auto frame_copy = std::make_unique<AVFrame>();
          av_frame_ref(frame_copy.get(), frame.get());
          _frame_queue.push(std::move(frame_copy));
        }

        av_packet_unref(packet.get());
        av_frame_unref(frame.get());
      }
  
      // flush phase
      while (_running && !_decoder.eof()) 
      {
        auto res = _decoder.flush_next(frame.get());
        if (res && res.value() == DecodeResult::FrameAvailable) {
          auto frame_copy = std::make_unique<AVFrame>();
          av_frame_ref(frame_copy.get(), frame.get());
          _frame_queue.push(std::move(frame_copy));
        }
        av_frame_unref(frame.get());
      }
    }
  
    av::VideoDecoder _decoder;
    av::UniqueFormatContext _input;
    FrameQueue _frame_queue;
    atomic<bool> _running;
    thread _thread;
  };

} // namespace av
