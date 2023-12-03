#include "char_field.h"

char_field::char_field(char const* name, char const* value)
{
  m_name  = strdup(name);
  m_value = strdup(value); 
}

char_field::~char_field()
{
  delete m_name;
  delete m_value;
}
