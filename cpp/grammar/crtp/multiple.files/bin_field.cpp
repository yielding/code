#include "bin_field.h"

bin_field::bin_field(char const* name, char const* value)
{
  m_name  = strdup(name);
  m_value = strdup(value); 
}

bin_field::~bin_field()
{
  delete m_name;
  delete m_value;
}
