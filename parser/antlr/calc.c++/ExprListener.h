
// Generated from ./Expr.g4 by ANTLR 4.13.0

#pragma once


#include "antlr4-runtime.h"
#include "ExprParser.h"


/**
 * This interface defines an abstract listener for a parse tree produced by ExprParser.
 */
class  ExprListener : public antlr4::tree::ParseTreeListener {
public:

  virtual void enterProg(ExprParser::ProgContext *ctx) = 0;
  virtual void exitProg(ExprParser::ProgContext *ctx) = 0;

  virtual void enterToMultOrDiv(ExprParser::ToMultOrDivContext *ctx) = 0;
  virtual void exitToMultOrDiv(ExprParser::ToMultOrDivContext *ctx) = 0;

  virtual void enterBinaryMinusOp(ExprParser::BinaryMinusOpContext *ctx) = 0;
  virtual void exitBinaryMinusOp(ExprParser::BinaryMinusOpContext *ctx) = 0;

  virtual void enterPlusOp(ExprParser::PlusOpContext *ctx) = 0;
  virtual void exitPlusOp(ExprParser::PlusOpContext *ctx) = 0;

  virtual void enterToAtom(ExprParser::ToAtomContext *ctx) = 0;
  virtual void exitToAtom(ExprParser::ToAtomContext *ctx) = 0;

  virtual void enterMultOp(ExprParser::MultOpContext *ctx) = 0;
  virtual void exitMultOp(ExprParser::MultOpContext *ctx) = 0;

  virtual void enterDivOp(ExprParser::DivOpContext *ctx) = 0;
  virtual void exitDivOp(ExprParser::DivOpContext *ctx) = 0;

  virtual void enterInt(ExprParser::IntContext *ctx) = 0;
  virtual void exitInt(ExprParser::IntContext *ctx) = 0;

  virtual void enterUnaryMinusOp(ExprParser::UnaryMinusOpContext *ctx) = 0;
  virtual void exitUnaryMinusOp(ExprParser::UnaryMinusOpContext *ctx) = 0;

  virtual void enterParenthesisOp(ExprParser::ParenthesisOpContext *ctx) = 0;
  virtual void exitParenthesisOp(ExprParser::ParenthesisOpContext *ctx) = 0;


};

