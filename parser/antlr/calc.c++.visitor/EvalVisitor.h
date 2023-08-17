#pragma once

#include "LabeledExprBaseVisitor.h"
#include <map>
#include <string>

using namespace std;

class EvalVisitor: public LabeledExprBaseVisitor
{
public:
  auto visitAssign(LabeledExprParser::AssignContext *ctx)-> any override
  {
    auto id = ctx->ID()->getText();
    auto val = any_cast<int>(visit(ctx->expr()));
    memory[id] = val;

    return val;
  }

  auto visitPrintExpr(LabeledExprParser::PrintExprContext *ctx) -> any override
  {
    cout << any_cast<int>(visit(ctx->expr())) << endl;
    return 0;
  }

  auto visitInt(LabeledExprParser::IntContext *ctx) -> any override
  {
    return stoi(ctx->INT()->getText());
  }

  auto visitId(LabeledExprParser::IdContext *ctx) -> any override
  {
    auto id = ctx->ID()->getText();

    return memory.contains(id)
      ? memory[id]
      : 0;
  }

  auto visitMulDiv(LabeledExprParser::MulDivContext *ctx) -> any override
  {
    auto l = any_cast<int>(visit(ctx->expr(0)));
    auto r = any_cast<int>(visit(ctx->expr(1)));

    return (ctx->op->getType() == LabeledExprParser::MUL)
      ? l * r
      : l / r ;
  }

  auto visitAddSub(LabeledExprParser::AddSubContext *ctx) -> any override
  {
    auto l = any_cast<int>(visit(ctx->expr(0)));
    auto r = any_cast<int>(visit(ctx->expr(1)));

    return (ctx->op->getType() == LabeledExprParser::ADD)
      ? l + r
      : l - r ;
  }

  auto visitParens(LabeledExprParser::ParensContext *ctx) -> any override
  {
    return visit(ctx->expr());
  }

private:
  map<string, int> memory;
};
