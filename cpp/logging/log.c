#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#ifdef WIN32
#define _vsprintf vsprintf
#endif

#define LOG_MAX_BUFFER_SIZE 256
#define LOG_PATH "change-this-path.log"

inline int LOG(const char* fmt,...)
{
  static char buf[LOG_MAX_BUFFER_SIZE];
  FILE* f = fopen(LOG_PATH, "a+");

  va_list v;
  va_start(v, fmt);
  vsnprintf(buf,LOG_MAX_BUFFER_SIZE,fmt,v);
  va_end(v);

  fwrite(buf, sizeof(char), strlen(buf), f);
  fflush(f);

  printf(buf);
  fflush(stdout);

  fclose(f);
  return 0;
}

int main()
{
  LOG("이창하");
  LOG("바보");
  LOG("윤여광 바보");
}
