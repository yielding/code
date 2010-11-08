#ifndef _CODING_H_
#define _CODING_H_

class coding_exception {};

// CRTPed ?
template <typename Derived>
class coder
{
public:
  coder();

  coder& endcode_int1(uint8_t src);
  coder& endcode_int2(uint16_t src);
  coder& endcode_int4(uint32_t src);

  coder& endcode_string(char const* src);
  coder& endcode_string(char* src);
  coder& endcode_stream(uint8_t* src, uint32_t len);
  coder& endcode_binary(uint8_t* src, uint32_t len);

  coder& encode_object(char const* key, coder* obj)
  {
    this->encode_string(name);
    obj->encode_to(this);
  }

  uint8_t  decode_int1();
  uint16_t decode_int2();
  uint32_t decode_int4();
  char*    decode_string();
  uint8_t* decode_binary(uint32_t size);

  coder* decode_object(char const* key)

};

class codable
{
public:
  virtual void decode_from(coder& s) = 0;
  virtual void encode_to(coder& s) = 0;
};

#endif
