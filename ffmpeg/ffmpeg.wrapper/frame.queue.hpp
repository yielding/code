#pragma once

#include "ffmpeg.hpp"
#include <queue>
#include <mutex>
#include <condition_variable>

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
namespace av {

  class FrameQueue 
  {
  public:
    explicit FrameQueue(const size_t capacity=30)
      : _capacity(capacity) {}

    void push(unique_ptr<AVFrame> frame) 
    {
      unique_lock lock(_mutex);
      _cond_producer.wait(lock, [this] { return _queue.size() < _capacity; });

      _queue.push(std::move(frame));
      _cond_consumer.notify_one();
    }

    auto pop() -> unique_ptr<AVFrame> 
    {
      unique_lock lock(_mutex);
      _cond_consumer.wait(lock, [this] { return !_queue.empty(); });

      auto frame = std::move(_queue.front());
      _queue.pop();
      _cond_producer.notify_one();

      return frame;
    }

    auto empty() const -> bool
    {
      lock_guard lock(_mutex);
      return _queue.empty();
    }

  private:
    queue<unique_ptr<AVFrame>> _queue;
    size_t _capacity;
    mutable mutex _mutex;
    condition_variable _cond_producer;
    condition_variable _cond_consumer;
  };
}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
