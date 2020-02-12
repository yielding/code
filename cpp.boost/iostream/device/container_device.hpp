#ifndef BOOST_IOSTREAMS_CONTAINTER_DEVICE_HPP_INCLUDED
#define BOOST_IOSTREAMS_CONTAINTER_DEVICE_HPP_INCLUDED

#include <algorithm>         // copy, min.
#include <cassert>
#include <boost/config.hpp>  // BOOST_NO_STDC_NAMESPACE.
#include <boost/iostreams/categories.hpp>
#include <boost/iostreams/detail/ios.hpp>  // failure.

namespace boost::iostreams::example {

  using namespace std;
  //
  // Model of Source which reads from an STL-compatible sequence
  // whose iterators are random-access iterators.
  //
  template<typename Container>
  class container_source
  {
  public:
    typedef typename Container::value_type  char_type;
    typedef source_tag                      category;

    container_source(Container& container)
      : container_(container), pos_(0)
    { }

    auto read(char_type* s, streamsize n) -> streamsize 
    {
      auto amt    = static_cast<streamsize>(container_.size() - pos_);
      auto result = (min)(n, amt);
      if (result != 0) 
      {
        copy(container_.begin() + pos_, container_.begin() + pos_ + result, s);
        pos_ += result;
        return result;
      } 

      return -1; // EOF
    }

    Container& container() { return container_; }

  private:
    container_source operator=(const container_source&);
    typedef typename Container::size_type   size_type;
    Container&  container_;
    size_type   pos_;
  };

  //
  // Model of Sink which appends to an STL-compatible sequence.
  //
  template<typename Container>
  class container_sink 
  {
  public:
    typedef typename Container::value_type  char_type;
    typedef sink_tag                        category;

    container_sink(Container& container) 
      : container_(container) { }

    auto write(const char_type* s, streamsize n) -> streamsize 
    {
      container_.insert(container_.end(), s, s + n);
      return n;
    }

    Container& container() { return container_; }

  private:
    container_sink operator=(const container_sink&);
    Container& container_;
  };

  //
  // Model of SeekableDevice which accessS an TL-compatible sequence
  // whose iterators are random-access iterators.
  //
  template<typename Container>
  class container_device
  {
  public:
    typedef typename Container::value_type  char_type;
    typedef seekable_device_tag             category;

    container_device(Container& container)
      : container_(container), pos_(0)
    { }

    auto read(char_type* s, streamsize n) -> streamsize 
    {
      auto amt    = static_cast<streamsize>(container_.size() - pos_);
      auto result = (min)(n, amt);
      if (result != 0) 
      {
        copy( container_.begin() + pos_, container_.begin() + pos_ + result, s );
        pos_ += result;
        return result;
      } 

      return -1; // EOF
    }

    auto write(const char_type* s, streamsize n) -> streamsize 
    {
      streamsize result = 0;

      if (pos_ != container_.size()) 
      {
        auto amt = static_cast<streamsize>(container_.size() - pos_);
        result = (min)(n, amt);
        copy(s, s + result, container_.begin() + pos_);
        pos_ += result;
      }

      if (result < n) 
      {
        container_.insert(container_.end(), s, s + n);
        pos_ = container_.size();
      }

      return n;
    }

    auto seek(stream_offset off, BOOST_IOS::seekdir way) -> stream_offset 
    {
      // Determine new value of pos_
      stream_offset next;
      if (way == BOOST_IOS::beg) {
        next = off;
      } else if (way == BOOST_IOS::cur) {
        next = pos_ + off;
      } else if (way == BOOST_IOS::end) {
        next = container_.size() + off - 1;
      } else {
        throw BOOST_IOSTREAMS_FAILURE("bad seek direction");
      }

      // Check for errors
      if (next < 0 || next >= container_.size())
        throw BOOST_IOSTREAMS_FAILURE("bad seek offset");

      pos_ = next;

      return pos_;
    }

    Container& container() { return container_; }

  private:
    container_device operator=(const container_device&);
    typedef typename Container::size_type   size_type;
    Container&  container_;
    size_type   pos_;
  };

} 

#endif
