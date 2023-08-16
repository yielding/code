
// Generated from ./Expr.g4 by ANTLR 4.13.0

#pragma once


#include "antlr4-runtime.h"




class  ExprParser : public antlr4::Parser {
public:
  enum {
    INT = 1, NL = 2, WS = 3, PLUS = 4, MINUS = 5, MULT = 6, DIV = 7, POPEN = 8, 
    PCLOSE = 9
  };

  enum {
    RuleProg = 0, RulePlusOrMinus = 1, RuleMultOrDiv = 2, RuleAtom = 3
  };

  explicit ExprParser(antlr4::TokenStream *input);

  ExprParser(antlr4::TokenStream *input, const antlr4::atn::ParserATNSimulatorOptions &options);

  ~ExprParser() override;

  std::string getGrammarFileName() const override;

  const antlr4::atn::ATN& getATN() const override;

  const std::vector<std::string>& getRuleNames() const override;

  const antlr4::dfa::Vocabulary& getVocabulary() const override;

  antlr4::atn::SerializedATNView getSerializedATN() const override;


  class ProgContext;
  class PlusOrMinusContext;
  class MultOrDivContext;
  class AtomContext; 

  class  ProgContext : public antlr4::ParserRuleContext {
  public:
    ProgContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    PlusOrMinusContext *plusOrMinus();
    antlr4::tree::TerminalNode *EOF();
    antlr4::tree::TerminalNode *NL();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ProgContext* prog();

  class  PlusOrMinusContext : public antlr4::ParserRuleContext {
  public:
    PlusOrMinusContext(antlr4::ParserRuleContext *parent, size_t invokingState);
   
    PlusOrMinusContext() = default;
    void copyFrom(PlusOrMinusContext *context);
    using antlr4::ParserRuleContext::copyFrom;

    virtual size_t getRuleIndex() const override;

   
  };

  class  ToMultOrDivContext : public PlusOrMinusContext {
  public:
    ToMultOrDivContext(PlusOrMinusContext *ctx);

    MultOrDivContext *multOrDiv();
    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
  };

  class  BinaryMinusOpContext : public PlusOrMinusContext {
  public:
    BinaryMinusOpContext(PlusOrMinusContext *ctx);

    PlusOrMinusContext *plusOrMinus();
    antlr4::tree::TerminalNode *MINUS();
    MultOrDivContext *multOrDiv();
    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
  };

  class  PlusOpContext : public PlusOrMinusContext {
  public:
    PlusOpContext(PlusOrMinusContext *ctx);

    PlusOrMinusContext *plusOrMinus();
    antlr4::tree::TerminalNode *PLUS();
    MultOrDivContext *multOrDiv();
    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
  };

  PlusOrMinusContext* plusOrMinus();
  PlusOrMinusContext* plusOrMinus(int precedence);
  class  MultOrDivContext : public antlr4::ParserRuleContext {
  public:
    MultOrDivContext(antlr4::ParserRuleContext *parent, size_t invokingState);
   
    MultOrDivContext() = default;
    void copyFrom(MultOrDivContext *context);
    using antlr4::ParserRuleContext::copyFrom;

    virtual size_t getRuleIndex() const override;

   
  };

  class  ToAtomContext : public MultOrDivContext {
  public:
    ToAtomContext(MultOrDivContext *ctx);

    AtomContext *atom();
    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
  };

  class  MultOpContext : public MultOrDivContext {
  public:
    MultOpContext(MultOrDivContext *ctx);

    MultOrDivContext *multOrDiv();
    antlr4::tree::TerminalNode *MULT();
    AtomContext *atom();
    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
  };

  class  DivOpContext : public MultOrDivContext {
  public:
    DivOpContext(MultOrDivContext *ctx);

    MultOrDivContext *multOrDiv();
    antlr4::tree::TerminalNode *DIV();
    AtomContext *atom();
    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
  };

  MultOrDivContext* multOrDiv();
  MultOrDivContext* multOrDiv(int precedence);
  class  AtomContext : public antlr4::ParserRuleContext {
  public:
    AtomContext(antlr4::ParserRuleContext *parent, size_t invokingState);
   
    AtomContext() = default;
    void copyFrom(AtomContext *context);
    using antlr4::ParserRuleContext::copyFrom;

    virtual size_t getRuleIndex() const override;

   
  };

  class  ParenthesisOpContext : public AtomContext {
  public:
    ParenthesisOpContext(AtomContext *ctx);

    antlr4::tree::TerminalNode *POPEN();
    PlusOrMinusContext *plusOrMinus();
    antlr4::tree::TerminalNode *PCLOSE();
    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
  };

  class  IntContext : public AtomContext {
  public:
    IntContext(AtomContext *ctx);

    antlr4::tree::TerminalNode *INT();
    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
  };

  class  UnaryMinusOpContext : public AtomContext {
  public:
    UnaryMinusOpContext(AtomContext *ctx);

    antlr4::tree::TerminalNode *MINUS();
    PlusOrMinusContext *plusOrMinus();
    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
  };

  AtomContext* atom();


  bool sempred(antlr4::RuleContext *_localctx, size_t ruleIndex, size_t predicateIndex) override;

  bool plusOrMinusSempred(PlusOrMinusContext *_localctx, size_t predicateIndex);
  bool multOrDivSempred(MultOrDivContext *_localctx, size_t predicateIndex);

  // By default the static state used to implement the parser is lazily initialized during the first
  // call to the constructor. You can call this function if you wish to initialize the static state
  // ahead of time.
  static void initialize();

private:
};

