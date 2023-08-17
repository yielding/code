
// Generated from ./LabeledExpr.g4 by ANTLR 4.13.0

#pragma once


#include "antlr4-runtime.h"
#include "LabeledExprParser.h"


/**
 * This interface defines an abstract listener for a parse tree produced by LabeledExprParser.
 */
class  LabeledExprListener : public antlr4::tree::ParseTreeListener {
public:

  virtual void enterProg(LabeledExprParser::ProgContext *ctx) = 0;
  virtual void exitProg(LabeledExprParser::ProgContext *ctx) = 0;

  virtual void enterPrintExpr(LabeledExprParser::PrintExprContext *ctx) = 0;
  virtual void exitPrintExpr(LabeledExprParser::PrintExprContext *ctx) = 0;

  virtual void enterAssign(LabeledExprParser::AssignContext *ctx) = 0;
  virtual void exitAssign(LabeledExprParser::AssignContext *ctx) = 0;

  virtual void enterBlank(LabeledExprParser::BlankContext *ctx) = 0;
  virtual void exitBlank(LabeledExprParser::BlankContext *ctx) = 0;

  virtual void enterParens(LabeledExprParser::ParensContext *ctx) = 0;
  virtual void exitParens(LabeledExprParser::ParensContext *ctx) = 0;

  virtual void enterMulDiv(LabeledExprParser::MulDivContext *ctx) = 0;
  virtual void exitMulDiv(LabeledExprParser::MulDivContext *ctx) = 0;

  virtual void enterAddSub(LabeledExprParser::AddSubContext *ctx) = 0;
  virtual void exitAddSub(LabeledExprParser::AddSubContext *ctx) = 0;

  virtual void enterId(LabeledExprParser::IdContext *ctx) = 0;
  virtual void exitId(LabeledExprParser::IdContext *ctx) = 0;

  virtual void enterInt(LabeledExprParser::IntContext *ctx) = 0;
  virtual void exitInt(LabeledExprParser::IntContext *ctx) = 0;


};

