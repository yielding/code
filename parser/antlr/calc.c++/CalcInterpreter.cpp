#include "CalcInterpreter.h"

CalcInterpreter::CalcInterpreter()
{
}

int CalcInterpreter::get_result()
{
  return operands.top();
}

void CalcInterpreter::exitPlusOp(ExprParser::PlusOpContext* ctx)
{
  auto r = operands.top(); operands.pop();
  auto l = operands.top(); operands.pop();

  operands.push(l + r);
}

void CalcInterpreter::exitBinaryMinusOp(ExprParser::BinaryMinusOpContext* ctx)
{
  auto r = operands.top(); operands.pop();
  auto l = operands.top(); operands.pop();

  operands.push(l - r);
}

void CalcInterpreter::exitUnaryMinusOp(ExprParser::UnaryMinusOpContext* ctx)
{
  operands.top() = -operands.top();
}

void CalcInterpreter::exitMultOp(ExprParser::MultOpContext* ctx)
{
  auto r = operands.top(); operands.pop();
  auto l = operands.top(); operands.pop();

  operands.push(l * r);
}

void CalcInterpreter::exitDivOp(ExprParser::DivOpContext* ctx)
{
  auto r = operands.top(); operands.pop();
  auto l = operands.top(); operands.pop();

  operands.push(l / r);
}

void CalcInterpreter::exitInt(ExprParser::IntContext* ctx)
{
  auto value = std::stoi(ctx->getText());
  operands.push(value);
}
