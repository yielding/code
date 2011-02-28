#ifndef RUBYEVAL_H
#define RUBYEVAL_H

#include "ruby.h"

#include <string>
#include <iostream>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using namespace std;

// REMARK
// 루비는 한 쓰레드당 하나밖에 돌지 않으므로 아래처럼 singleton으로 만들고
// 동기화하는게 옳다.
class RubyEval 
{
public:
  static RubyEval* instance();
  static void      delete_instance();
  ~RubyEval();

	static std::string val2str   (VALUE const rval);
	static int         val2i     (VALUE const rval);
  static std::string strval2str(VALUE const rval);

  void run_file(char const* filename, std::ostream& out = std::cout);

  VALUE eval(char const* code, std::ostream& errout);
  VALUE eval(char const* code);

  void exception_print(std::ostream& errout = std::cerr);
  bool evalOk();

private:
  RubyEval();

  static RubyEval* m_instance;
  int m_status;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
