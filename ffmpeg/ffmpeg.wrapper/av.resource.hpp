#pragma once

#include <memory.hpp>

#include <stdexcept>

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
namespace av::detail {

  template<typename T, void (*FreeFunc)(T**)>
  struct AvDeleter 
  {
    void operator()(T* ptr) const noexcept 
    {
      if (ptr)
        FreeFunc(&ptr);
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

  template<typename T, T* (*AllocFunc)(), void (*UnrefFunc)(T*), void (*FreeFunc)(T**)>
  class AvResource: public core::memory::move_only<AvResource<T, AllocFunc, UnrefFunc, FreeFunc>>
  {
  public:
    AvResource() : _resource(AllocFunc()) 
    {
      if (!_resource)
        throw std::runtime_error("Failed to allocate resource");
    }
  
    void reset() noexcept 
    {
      if (_resource)
        UnrefFunc(_resource.get());
    }
  
    T* get() const noexcept { return _resource.get(); }
    T* release() noexcept { return _resource.release(); }
  
  private:
    unique_ptr<T, detail::AvDeleter<T, FreeFunc>> _resource;
  };
  
  template<typename T, T* (*AllocFunc)(), void (*FreeFunc)(T**), int (*RefFunc)(T* dst, const T* src), void (*UnrefFunc)(T*)>
  class RefAvResource 
  {
  public:
    RefAvResource() : _resource(AllocFunc()) 
    {
      if (!_resource)
        throw std::runtime_error("Failed to allocate resource");
    }
  
    // 복사: ref
    RefAvResource(const RefAvResource& other) : _resource(AllocFunc()) 
    {
      if (!_resource)
        throw std::runtime_error("Failed to allocate resource");

      if (RefFunc(_resource.get(), other._resource.get()) < 0)
        throw std::runtime_error("Failed to ref resource");
    }
  
    auto operator=(const RefAvResource& other) -> RefAvResource& 
    {
      if (this != &other)
        if (RefFunc(_resource.get(), other._resource.get()) < 0)
          throw std::runtime_error("Failed to ref resource");

      return *this;
    }
  
    // move 지원
    RefAvResource(RefAvResource&&) noexcept = default;
    RefAvResource& operator=(RefAvResource&&) noexcept = default;

    void reset() noexcept 
    {
      if (_resource)
        UnrefFunc(_resource.get());
    }
  
    T* get() const noexcept { return _resource.get(); }
  
  private:
    unique_ptr<T, detail::AvDeleter<T, FreeFunc>> _resource;
  };

  using UniquePacket = AvResource<AVPacket, av_packet_alloc, av_packet_unref, av_packet_free>;
  using UniqueFrame = AvResource<AVFrame, av_frame_alloc, av_frame_unref, av_frame_free>;

  using PacketRef = RefAvResource<AVPacket, av_packet_alloc, av_packet_free, av_packet_ref, av_packet_unref>;
  using FrameRef = RefAvResource<AVFrame, av_frame_alloc, av_frame_free, av_frame_ref, av_frame_unref>;

}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
