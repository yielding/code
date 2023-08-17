
// Generated from ./LabeledExpr.g4 by ANTLR 4.13.0

#pragma once


#include "antlr4-runtime.h"
#include "LabeledExprListener.h"


/**
 * This class provides an empty implementation of LabeledExprListener,
 * which can be extended to create a listener which only needs to handle a subset
 * of the available methods.
 */
class  LabeledExprBaseListener : public LabeledExprListener {
public:

  virtual void enterProg(LabeledExprParser::ProgContext * /*ctx*/) override { }
  virtual void exitProg(LabeledExprParser::ProgContext * /*ctx*/) override { }

  virtual void enterPrintExpr(LabeledExprParser::PrintExprContext * /*ctx*/) override { }
  virtual void exitPrintExpr(LabeledExprParser::PrintExprContext * /*ctx*/) override { }

  virtual void enterAssign(LabeledExprParser::AssignContext * /*ctx*/) override { }
  virtual void exitAssign(LabeledExprParser::AssignContext * /*ctx*/) override { }

  virtual void enterBlank(LabeledExprParser::BlankContext * /*ctx*/) override { }
  virtual void exitBlank(LabeledExprParser::BlankContext * /*ctx*/) override { }

  virtual void enterParens(LabeledExprParser::ParensContext * /*ctx*/) override { }
  virtual void exitParens(LabeledExprParser::ParensContext * /*ctx*/) override { }

  virtual void enterMulDiv(LabeledExprParser::MulDivContext * /*ctx*/) override { }
  virtual void exitMulDiv(LabeledExprParser::MulDivContext * /*ctx*/) override { }

  virtual void enterAddSub(LabeledExprParser::AddSubContext * /*ctx*/) override { }
  virtual void exitAddSub(LabeledExprParser::AddSubContext * /*ctx*/) override { }

  virtual void enterId(LabeledExprParser::IdContext * /*ctx*/) override { }
  virtual void exitId(LabeledExprParser::IdContext * /*ctx*/) override { }

  virtual void enterInt(LabeledExprParser::IntContext * /*ctx*/) override { }
  virtual void exitInt(LabeledExprParser::IntContext * /*ctx*/) override { }


  virtual void enterEveryRule(antlr4::ParserRuleContext * /*ctx*/) override { }
  virtual void exitEveryRule(antlr4::ParserRuleContext * /*ctx*/) override { }
  virtual void visitTerminal(antlr4::tree::TerminalNode * /*node*/) override { }
  virtual void visitErrorNode(antlr4::tree::ErrorNode * /*node*/) override { }

};

