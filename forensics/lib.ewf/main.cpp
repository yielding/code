#include <cassert>

#include <libewf.h>
#include <libcerror.h>

int main(int argc, char *argv[])
{
  libcerror_error_t* error = nullptr;
  libewf_handle_t* handle = nullptr;
  int result = 0;

  result = libewf_handle_initialize(&handle, &error);
  assert(result == 1);

  size64_t media_size = 0;
  result = libewf_handle_get_media_size(
      handle,
      &media_size,
      &error);
  
  off64_t offset = 0;
  result = libewf_handle_seek_offset(
      handle,
      0,
      SEEK_SET,
      &error);

  const int BUFFER_SIZE = 0x1000;
  uint8_t buffer[BUFFER_SIZE] = { 0 };
  result = libewf_handle_read_buffer(
      handle,
      buffer,
      (size_t)BUFFER_SIZE,
      &error);

  result = libewf_handle_free(&handle, &error);
  assert(result == 1);

  return 0;
}
