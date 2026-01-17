#include "AVFrameManager.hpp"
#include <stdexcept>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
AVFrameManager::AVFrameManager() 
{
  frame_ = av_frame_alloc();

  if (!frame_)
    throw std::runtime_error("Failed to allocate AVFrame");
}

AVFrameManager::~AVFrameManager() 
{
  av_frame_free(&frame_);
}

auto AVFrameManager::get() -> AVFrame* 
{
  return frame_;
}

void AVFrameManager::unref() 
{
  av_frame_unref(frame_);
}

void AVFrameManager::ref_from(const AVFrame* other)
{
  unref();
  if (av_frame_ref(frame_, other) < 0)
    throw std::runtime_error("av_frame_ref failed");
}

void AVFrameManager::clone_from(const AVFrame* other)
{
  auto clone = av_frame_clone(other);
  if (!clone)
    throw std::runtime_error("av_frame_clone failed");

  av_frame_free(&frame_);
  frame_ = clone;
}

void AVFrameManager::move_from(AVFrameManager& other)
{
  av_frame_unref(frame_);
  av_frame_move_ref(frame_, other.frame_);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
