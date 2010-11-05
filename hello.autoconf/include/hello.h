#ifndef HELLO_H
#define HELLO_H

#include <string>

class Hello
{
 public:
  Hello(std::string const& msg);
  std::string say() const;

 private:
  std::string msg_;
};

#endif
