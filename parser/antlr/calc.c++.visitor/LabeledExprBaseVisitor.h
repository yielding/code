
// Generated from ./LabeledExpr.g4 by ANTLR 4.13.0

#pragma once


#include "antlr4-runtime.h"
#include "LabeledExprVisitor.h"


/**
 * This class provides an empty implementation of LabeledExprVisitor, which can be
 * extended to create a visitor which only needs to handle a subset of the available methods.
 */
class  LabeledExprBaseVisitor : public LabeledExprVisitor {
public:

  virtual std::any visitProg(LabeledExprParser::ProgContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitPrintExpr(LabeledExprParser::PrintExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitAssign(LabeledExprParser::AssignContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitBlank(LabeledExprParser::BlankContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitParens(LabeledExprParser::ParensContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitMulDiv(LabeledExprParser::MulDivContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitAddSub(LabeledExprParser::AddSubContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitId(LabeledExprParser::IdContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitInt(LabeledExprParser::IntContext *ctx) override {
    return visitChildren(ctx);
  }


};

