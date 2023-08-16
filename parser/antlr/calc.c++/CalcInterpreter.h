#pragma once

#include "ExprBaseListener.h"
#include <stack>

class CalcInterpreter: public ExprBaseListener
{
public:
  CalcInterpreter();

  int get_result();

  void exitPlusOp(ExprParser::PlusOpContext * /*ctx*/) override;
  void exitBinaryMinusOp(ExprParser::BinaryMinusOpContext * /*ctx*/) override;
  void exitUnaryMinusOp(ExprParser::UnaryMinusOpContext * /*ctx*/) override;
  void exitMultOp(ExprParser::MultOpContext * /*ctx*/) override;
  void exitDivOp(ExprParser::DivOpContext * /*ctx*/) override;
  void exitInt(ExprParser::IntContext * /*ctx*/) override;

private:
  std::stack<int> operands;
};
