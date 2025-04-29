#pragma once

#include "ffmpeg.hpp"

class AVFrameManager 
{
public:
  AVFrameManager();
  ~AVFrameManager();

  AVFrame* get();
  void unref();
  void ref_from(const AVFrame* other);
  void clone_from(const AVFrame* other);
  void move_from(AVFrameManager& other);

private:
  AVFrame* frame_;
};

