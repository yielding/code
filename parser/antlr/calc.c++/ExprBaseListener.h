
// Generated from ./Expr.g4 by ANTLR 4.13.0

#pragma once


#include "antlr4-runtime.h"
#include "ExprListener.h"


/**
 * This class provides an empty implementation of ExprListener,
 * which can be extended to create a listener which only needs to handle a subset
 * of the available methods.
 */
class  ExprBaseListener : public ExprListener {
public:

  virtual void enterProg(ExprParser::ProgContext * /*ctx*/) override { }
  virtual void exitProg(ExprParser::ProgContext * /*ctx*/) override { }

  virtual void enterToMultOrDiv(ExprParser::ToMultOrDivContext * /*ctx*/) override { }
  virtual void exitToMultOrDiv(ExprParser::ToMultOrDivContext * /*ctx*/) override { }

  virtual void enterBinaryMinusOp(ExprParser::BinaryMinusOpContext * /*ctx*/) override { }
  virtual void exitBinaryMinusOp(ExprParser::BinaryMinusOpContext * /*ctx*/) override { }

  virtual void enterPlusOp(ExprParser::PlusOpContext * /*ctx*/) override { }
  virtual void exitPlusOp(ExprParser::PlusOpContext * /*ctx*/) override { }

  virtual void enterToAtom(ExprParser::ToAtomContext * /*ctx*/) override { }
  virtual void exitToAtom(ExprParser::ToAtomContext * /*ctx*/) override { }

  virtual void enterMultOp(ExprParser::MultOpContext * /*ctx*/) override { }
  virtual void exitMultOp(ExprParser::MultOpContext * /*ctx*/) override { }

  virtual void enterDivOp(ExprParser::DivOpContext * /*ctx*/) override { }
  virtual void exitDivOp(ExprParser::DivOpContext * /*ctx*/) override { }

  virtual void enterInt(ExprParser::IntContext * /*ctx*/) override { }
  virtual void exitInt(ExprParser::IntContext * /*ctx*/) override { }

  virtual void enterUnaryMinusOp(ExprParser::UnaryMinusOpContext * /*ctx*/) override { }
  virtual void exitUnaryMinusOp(ExprParser::UnaryMinusOpContext * /*ctx*/) override { }

  virtual void enterParenthesisOp(ExprParser::ParenthesisOpContext * /*ctx*/) override { }
  virtual void exitParenthesisOp(ExprParser::ParenthesisOpContext * /*ctx*/) override { }


  virtual void enterEveryRule(antlr4::ParserRuleContext * /*ctx*/) override { }
  virtual void exitEveryRule(antlr4::ParserRuleContext * /*ctx*/) override { }
  virtual void visitTerminal(antlr4::tree::TerminalNode * /*node*/) override { }
  virtual void visitErrorNode(antlr4::tree::ErrorNode * /*node*/) override { }

};
