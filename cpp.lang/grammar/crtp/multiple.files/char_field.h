#pragma once

#include "crtp2.h"

class char_field: public streamer<char_field>
{
public:
  char_field(char const* name, char const* value);

  ~char_field();

  char* m_name;
  char* m_value;
};
