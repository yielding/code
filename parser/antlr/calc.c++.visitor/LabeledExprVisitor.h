
// Generated from ./LabeledExpr.g4 by ANTLR 4.13.0

#pragma once


#include "antlr4-runtime.h"
#include "LabeledExprParser.h"



/**
 * This class defines an abstract visitor for a parse tree
 * produced by LabeledExprParser.
 */
class  LabeledExprVisitor : public antlr4::tree::AbstractParseTreeVisitor {
public:

  /**
   * Visit parse trees produced by LabeledExprParser.
   */
    virtual std::any visitProg(LabeledExprParser::ProgContext *context) = 0;

    virtual std::any visitPrintExpr(LabeledExprParser::PrintExprContext *context) = 0;

    virtual std::any visitAssign(LabeledExprParser::AssignContext *context) = 0;

    virtual std::any visitBlank(LabeledExprParser::BlankContext *context) = 0;

    virtual std::any visitParens(LabeledExprParser::ParensContext *context) = 0;

    virtual std::any visitMulDiv(LabeledExprParser::MulDivContext *context) = 0;

    virtual std::any visitAddSub(LabeledExprParser::AddSubContext *context) = 0;

    virtual std::any visitId(LabeledExprParser::IdContext *context) = 0;

    virtual std::any visitInt(LabeledExprParser::IntContext *context) = 0;


};

