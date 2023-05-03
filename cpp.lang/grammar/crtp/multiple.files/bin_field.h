#pragma once

#include "crtp2.h"

class bin_field: public streamer<bin_field>
{
public:
  bin_field(char const* name, char const* value);

  ~bin_field();

  char* m_name;
  char* m_value;
};
