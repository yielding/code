
// Generated from ./LabeledExpr.g4 by ANTLR 4.13.0


#include "LabeledExprVisitor.h"

#include "LabeledExprParser.h"


using namespace antlrcpp;

using namespace antlr4;

namespace {

struct LabeledExprParserStaticData final {
  LabeledExprParserStaticData(std::vector<std::string> ruleNames,
                        std::vector<std::string> literalNames,
                        std::vector<std::string> symbolicNames)
      : ruleNames(std::move(ruleNames)), literalNames(std::move(literalNames)),
        symbolicNames(std::move(symbolicNames)),
        vocabulary(this->literalNames, this->symbolicNames) {}

  LabeledExprParserStaticData(const LabeledExprParserStaticData&) = delete;
  LabeledExprParserStaticData(LabeledExprParserStaticData&&) = delete;
  LabeledExprParserStaticData& operator=(const LabeledExprParserStaticData&) = delete;
  LabeledExprParserStaticData& operator=(LabeledExprParserStaticData&&) = delete;

  std::vector<antlr4::dfa::DFA> decisionToDFA;
  antlr4::atn::PredictionContextCache sharedContextCache;
  const std::vector<std::string> ruleNames;
  const std::vector<std::string> literalNames;
  const std::vector<std::string> symbolicNames;
  const antlr4::dfa::Vocabulary vocabulary;
  antlr4::atn::SerializedATNView serializedATN;
  std::unique_ptr<antlr4::atn::ATN> atn;
};

::antlr4::internal::OnceFlag labeledexprParserOnceFlag;
#if ANTLR4_USE_THREAD_LOCAL_CACHE
static thread_local
#endif
LabeledExprParserStaticData *labeledexprParserStaticData = nullptr;

void labeledexprParserInitialize() {
#if ANTLR4_USE_THREAD_LOCAL_CACHE
  if (labeledexprParserStaticData != nullptr) {
    return;
  }
#else
  assert(labeledexprParserStaticData == nullptr);
#endif
  auto staticData = std::make_unique<LabeledExprParserStaticData>(
    std::vector<std::string>{
      "prog", "stat", "expr"
    },
    std::vector<std::string>{
      "", "'='", "'('", "')'", "'*'", "'/'", "'+'", "'-'"
    },
    std::vector<std::string>{
      "", "", "", "", "MUL", "DIV", "ADD", "SUB", "ID", "INT", "NEWLINE", 
      "WS"
    }
  );
  static const int32_t serializedATNSegment[] = {
  	4,1,11,43,2,0,7,0,2,1,7,1,2,2,7,2,1,0,4,0,8,8,0,11,0,12,0,9,1,1,1,1,1,
  	1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,21,8,1,1,2,1,2,1,2,1,2,1,2,1,2,1,2,3,2,
  	30,8,2,1,2,1,2,1,2,1,2,1,2,1,2,5,2,38,8,2,10,2,12,2,41,9,2,1,2,0,1,4,
  	3,0,2,4,0,2,1,0,4,5,1,0,6,7,46,0,7,1,0,0,0,2,20,1,0,0,0,4,29,1,0,0,0,
  	6,8,3,2,1,0,7,6,1,0,0,0,8,9,1,0,0,0,9,7,1,0,0,0,9,10,1,0,0,0,10,1,1,0,
  	0,0,11,12,3,4,2,0,12,13,5,10,0,0,13,21,1,0,0,0,14,15,5,8,0,0,15,16,5,
  	1,0,0,16,17,3,4,2,0,17,18,5,10,0,0,18,21,1,0,0,0,19,21,5,10,0,0,20,11,
  	1,0,0,0,20,14,1,0,0,0,20,19,1,0,0,0,21,3,1,0,0,0,22,23,6,2,-1,0,23,30,
  	5,9,0,0,24,30,5,8,0,0,25,26,5,2,0,0,26,27,3,4,2,0,27,28,5,3,0,0,28,30,
  	1,0,0,0,29,22,1,0,0,0,29,24,1,0,0,0,29,25,1,0,0,0,30,39,1,0,0,0,31,32,
  	10,5,0,0,32,33,7,0,0,0,33,38,3,4,2,6,34,35,10,4,0,0,35,36,7,1,0,0,36,
  	38,3,4,2,5,37,31,1,0,0,0,37,34,1,0,0,0,38,41,1,0,0,0,39,37,1,0,0,0,39,
  	40,1,0,0,0,40,5,1,0,0,0,41,39,1,0,0,0,5,9,20,29,37,39
  };
  staticData->serializedATN = antlr4::atn::SerializedATNView(serializedATNSegment, sizeof(serializedATNSegment) / sizeof(serializedATNSegment[0]));

  antlr4::atn::ATNDeserializer deserializer;
  staticData->atn = deserializer.deserialize(staticData->serializedATN);

  const size_t count = staticData->atn->getNumberOfDecisions();
  staticData->decisionToDFA.reserve(count);
  for (size_t i = 0; i < count; i++) { 
    staticData->decisionToDFA.emplace_back(staticData->atn->getDecisionState(i), i);
  }
  labeledexprParserStaticData = staticData.release();
}

}

LabeledExprParser::LabeledExprParser(TokenStream *input) : LabeledExprParser(input, antlr4::atn::ParserATNSimulatorOptions()) {}

LabeledExprParser::LabeledExprParser(TokenStream *input, const antlr4::atn::ParserATNSimulatorOptions &options) : Parser(input) {
  LabeledExprParser::initialize();
  _interpreter = new atn::ParserATNSimulator(this, *labeledexprParserStaticData->atn, labeledexprParserStaticData->decisionToDFA, labeledexprParserStaticData->sharedContextCache, options);
}

LabeledExprParser::~LabeledExprParser() {
  delete _interpreter;
}

const atn::ATN& LabeledExprParser::getATN() const {
  return *labeledexprParserStaticData->atn;
}

std::string LabeledExprParser::getGrammarFileName() const {
  return "LabeledExpr.g4";
}

const std::vector<std::string>& LabeledExprParser::getRuleNames() const {
  return labeledexprParserStaticData->ruleNames;
}

const dfa::Vocabulary& LabeledExprParser::getVocabulary() const {
  return labeledexprParserStaticData->vocabulary;
}

antlr4::atn::SerializedATNView LabeledExprParser::getSerializedATN() const {
  return labeledexprParserStaticData->serializedATN;
}


//----------------- ProgContext ------------------------------------------------------------------

LabeledExprParser::ProgContext::ProgContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<LabeledExprParser::StatContext *> LabeledExprParser::ProgContext::stat() {
  return getRuleContexts<LabeledExprParser::StatContext>();
}

LabeledExprParser::StatContext* LabeledExprParser::ProgContext::stat(size_t i) {
  return getRuleContext<LabeledExprParser::StatContext>(i);
}


size_t LabeledExprParser::ProgContext::getRuleIndex() const {
  return LabeledExprParser::RuleProg;
}


std::any LabeledExprParser::ProgContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<LabeledExprVisitor*>(visitor))
    return parserVisitor->visitProg(this);
  else
    return visitor->visitChildren(this);
}

LabeledExprParser::ProgContext* LabeledExprParser::prog() {
  ProgContext *_localctx = _tracker.createInstance<ProgContext>(_ctx, getState());
  enterRule(_localctx, 0, LabeledExprParser::RuleProg);
  size_t _la = 0;

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(7); 
    _errHandler->sync(this);
    _la = _input->LA(1);
    do {
      setState(6);
      stat();
      setState(9); 
      _errHandler->sync(this);
      _la = _input->LA(1);
    } while ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & 1796) != 0));
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- StatContext ------------------------------------------------------------------

LabeledExprParser::StatContext::StatContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}


size_t LabeledExprParser::StatContext::getRuleIndex() const {
  return LabeledExprParser::RuleStat;
}

void LabeledExprParser::StatContext::copyFrom(StatContext *ctx) {
  ParserRuleContext::copyFrom(ctx);
}

//----------------- BlankContext ------------------------------------------------------------------

tree::TerminalNode* LabeledExprParser::BlankContext::NEWLINE() {
  return getToken(LabeledExprParser::NEWLINE, 0);
}

LabeledExprParser::BlankContext::BlankContext(StatContext *ctx) { copyFrom(ctx); }


std::any LabeledExprParser::BlankContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<LabeledExprVisitor*>(visitor))
    return parserVisitor->visitBlank(this);
  else
    return visitor->visitChildren(this);
}
//----------------- PrintExprContext ------------------------------------------------------------------

LabeledExprParser::ExprContext* LabeledExprParser::PrintExprContext::expr() {
  return getRuleContext<LabeledExprParser::ExprContext>(0);
}

tree::TerminalNode* LabeledExprParser::PrintExprContext::NEWLINE() {
  return getToken(LabeledExprParser::NEWLINE, 0);
}

LabeledExprParser::PrintExprContext::PrintExprContext(StatContext *ctx) { copyFrom(ctx); }


std::any LabeledExprParser::PrintExprContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<LabeledExprVisitor*>(visitor))
    return parserVisitor->visitPrintExpr(this);
  else
    return visitor->visitChildren(this);
}
//----------------- AssignContext ------------------------------------------------------------------

tree::TerminalNode* LabeledExprParser::AssignContext::ID() {
  return getToken(LabeledExprParser::ID, 0);
}

LabeledExprParser::ExprContext* LabeledExprParser::AssignContext::expr() {
  return getRuleContext<LabeledExprParser::ExprContext>(0);
}

tree::TerminalNode* LabeledExprParser::AssignContext::NEWLINE() {
  return getToken(LabeledExprParser::NEWLINE, 0);
}

LabeledExprParser::AssignContext::AssignContext(StatContext *ctx) { copyFrom(ctx); }


std::any LabeledExprParser::AssignContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<LabeledExprVisitor*>(visitor))
    return parserVisitor->visitAssign(this);
  else
    return visitor->visitChildren(this);
}
LabeledExprParser::StatContext* LabeledExprParser::stat() {
  StatContext *_localctx = _tracker.createInstance<StatContext>(_ctx, getState());
  enterRule(_localctx, 2, LabeledExprParser::RuleStat);

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    setState(20);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 1, _ctx)) {
    case 1: {
      _localctx = _tracker.createInstance<LabeledExprParser::PrintExprContext>(_localctx);
      enterOuterAlt(_localctx, 1);
      setState(11);
      expr(0);
      setState(12);
      match(LabeledExprParser::NEWLINE);
      break;
    }

    case 2: {
      _localctx = _tracker.createInstance<LabeledExprParser::AssignContext>(_localctx);
      enterOuterAlt(_localctx, 2);
      setState(14);
      match(LabeledExprParser::ID);
      setState(15);
      match(LabeledExprParser::T__0);
      setState(16);
      expr(0);
      setState(17);
      match(LabeledExprParser::NEWLINE);
      break;
    }

    case 3: {
      _localctx = _tracker.createInstance<LabeledExprParser::BlankContext>(_localctx);
      enterOuterAlt(_localctx, 3);
      setState(19);
      match(LabeledExprParser::NEWLINE);
      break;
    }

    default:
      break;
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ExprContext ------------------------------------------------------------------

LabeledExprParser::ExprContext::ExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}


size_t LabeledExprParser::ExprContext::getRuleIndex() const {
  return LabeledExprParser::RuleExpr;
}

void LabeledExprParser::ExprContext::copyFrom(ExprContext *ctx) {
  ParserRuleContext::copyFrom(ctx);
}

//----------------- ParensContext ------------------------------------------------------------------

LabeledExprParser::ExprContext* LabeledExprParser::ParensContext::expr() {
  return getRuleContext<LabeledExprParser::ExprContext>(0);
}

LabeledExprParser::ParensContext::ParensContext(ExprContext *ctx) { copyFrom(ctx); }


std::any LabeledExprParser::ParensContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<LabeledExprVisitor*>(visitor))
    return parserVisitor->visitParens(this);
  else
    return visitor->visitChildren(this);
}
//----------------- MulDivContext ------------------------------------------------------------------

std::vector<LabeledExprParser::ExprContext *> LabeledExprParser::MulDivContext::expr() {
  return getRuleContexts<LabeledExprParser::ExprContext>();
}

LabeledExprParser::ExprContext* LabeledExprParser::MulDivContext::expr(size_t i) {
  return getRuleContext<LabeledExprParser::ExprContext>(i);
}

tree::TerminalNode* LabeledExprParser::MulDivContext::MUL() {
  return getToken(LabeledExprParser::MUL, 0);
}

tree::TerminalNode* LabeledExprParser::MulDivContext::DIV() {
  return getToken(LabeledExprParser::DIV, 0);
}

LabeledExprParser::MulDivContext::MulDivContext(ExprContext *ctx) { copyFrom(ctx); }


std::any LabeledExprParser::MulDivContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<LabeledExprVisitor*>(visitor))
    return parserVisitor->visitMulDiv(this);
  else
    return visitor->visitChildren(this);
}
//----------------- AddSubContext ------------------------------------------------------------------

std::vector<LabeledExprParser::ExprContext *> LabeledExprParser::AddSubContext::expr() {
  return getRuleContexts<LabeledExprParser::ExprContext>();
}

LabeledExprParser::ExprContext* LabeledExprParser::AddSubContext::expr(size_t i) {
  return getRuleContext<LabeledExprParser::ExprContext>(i);
}

tree::TerminalNode* LabeledExprParser::AddSubContext::ADD() {
  return getToken(LabeledExprParser::ADD, 0);
}

tree::TerminalNode* LabeledExprParser::AddSubContext::SUB() {
  return getToken(LabeledExprParser::SUB, 0);
}

LabeledExprParser::AddSubContext::AddSubContext(ExprContext *ctx) { copyFrom(ctx); }


std::any LabeledExprParser::AddSubContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<LabeledExprVisitor*>(visitor))
    return parserVisitor->visitAddSub(this);
  else
    return visitor->visitChildren(this);
}
//----------------- IdContext ------------------------------------------------------------------

tree::TerminalNode* LabeledExprParser::IdContext::ID() {
  return getToken(LabeledExprParser::ID, 0);
}

LabeledExprParser::IdContext::IdContext(ExprContext *ctx) { copyFrom(ctx); }


std::any LabeledExprParser::IdContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<LabeledExprVisitor*>(visitor))
    return parserVisitor->visitId(this);
  else
    return visitor->visitChildren(this);
}
//----------------- IntContext ------------------------------------------------------------------

tree::TerminalNode* LabeledExprParser::IntContext::INT() {
  return getToken(LabeledExprParser::INT, 0);
}

LabeledExprParser::IntContext::IntContext(ExprContext *ctx) { copyFrom(ctx); }


std::any LabeledExprParser::IntContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<LabeledExprVisitor*>(visitor))
    return parserVisitor->visitInt(this);
  else
    return visitor->visitChildren(this);
}

LabeledExprParser::ExprContext* LabeledExprParser::expr() {
   return expr(0);
}

LabeledExprParser::ExprContext* LabeledExprParser::expr(int precedence) {
  ParserRuleContext *parentContext = _ctx;
  size_t parentState = getState();
  LabeledExprParser::ExprContext *_localctx = _tracker.createInstance<ExprContext>(_ctx, parentState);
  LabeledExprParser::ExprContext *previousContext = _localctx;
  (void)previousContext; // Silence compiler, in case the context is not used by generated code.
  size_t startState = 4;
  enterRecursionRule(_localctx, 4, LabeledExprParser::RuleExpr, precedence);

    size_t _la = 0;

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    unrollRecursionContexts(parentContext);
  });
  try {
    size_t alt;
    enterOuterAlt(_localctx, 1);
    setState(29);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case LabeledExprParser::INT: {
        _localctx = _tracker.createInstance<IntContext>(_localctx);
        _ctx = _localctx;
        previousContext = _localctx;

        setState(23);
        match(LabeledExprParser::INT);
        break;
      }

      case LabeledExprParser::ID: {
        _localctx = _tracker.createInstance<IdContext>(_localctx);
        _ctx = _localctx;
        previousContext = _localctx;
        setState(24);
        match(LabeledExprParser::ID);
        break;
      }

      case LabeledExprParser::T__1: {
        _localctx = _tracker.createInstance<ParensContext>(_localctx);
        _ctx = _localctx;
        previousContext = _localctx;
        setState(25);
        match(LabeledExprParser::T__1);
        setState(26);
        expr(0);
        setState(27);
        match(LabeledExprParser::T__2);
        break;
      }

    default:
      throw NoViableAltException(this);
    }
    _ctx->stop = _input->LT(-1);
    setState(39);
    _errHandler->sync(this);
    alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 4, _ctx);
    while (alt != 2 && alt != atn::ATN::INVALID_ALT_NUMBER) {
      if (alt == 1) {
        if (!_parseListeners.empty())
          triggerExitRuleEvent();
        previousContext = _localctx;
        setState(37);
        _errHandler->sync(this);
        switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 3, _ctx)) {
        case 1: {
          auto newContext = _tracker.createInstance<MulDivContext>(_tracker.createInstance<ExprContext>(parentContext, parentState));
          _localctx = newContext;
          pushNewRecursionContext(newContext, startState, RuleExpr);
          setState(31);

          if (!(precpred(_ctx, 5))) throw FailedPredicateException(this, "precpred(_ctx, 5)");
          setState(32);
          antlrcpp::downCast<MulDivContext *>(_localctx)->op = _input->LT(1);
          _la = _input->LA(1);
          if (!(_la == LabeledExprParser::MUL

          || _la == LabeledExprParser::DIV)) {
            antlrcpp::downCast<MulDivContext *>(_localctx)->op = _errHandler->recoverInline(this);
          }
          else {
            _errHandler->reportMatch(this);
            consume();
          }
          setState(33);
          expr(6);
          break;
        }

        case 2: {
          auto newContext = _tracker.createInstance<AddSubContext>(_tracker.createInstance<ExprContext>(parentContext, parentState));
          _localctx = newContext;
          pushNewRecursionContext(newContext, startState, RuleExpr);
          setState(34);

          if (!(precpred(_ctx, 4))) throw FailedPredicateException(this, "precpred(_ctx, 4)");
          setState(35);
          antlrcpp::downCast<AddSubContext *>(_localctx)->op = _input->LT(1);
          _la = _input->LA(1);
          if (!(_la == LabeledExprParser::ADD

          || _la == LabeledExprParser::SUB)) {
            antlrcpp::downCast<AddSubContext *>(_localctx)->op = _errHandler->recoverInline(this);
          }
          else {
            _errHandler->reportMatch(this);
            consume();
          }
          setState(36);
          expr(5);
          break;
        }

        default:
          break;
        } 
      }
      setState(41);
      _errHandler->sync(this);
      alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 4, _ctx);
    }
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }
  return _localctx;
}

bool LabeledExprParser::sempred(RuleContext *context, size_t ruleIndex, size_t predicateIndex) {
  switch (ruleIndex) {
    case 2: return exprSempred(antlrcpp::downCast<ExprContext *>(context), predicateIndex);

  default:
    break;
  }
  return true;
}

bool LabeledExprParser::exprSempred(ExprContext *_localctx, size_t predicateIndex) {
  switch (predicateIndex) {
    case 0: return precpred(_ctx, 5);
    case 1: return precpred(_ctx, 4);

  default:
    break;
  }
  return true;
}

void LabeledExprParser::initialize() {
#if ANTLR4_USE_THREAD_LOCAL_CACHE
  labeledexprParserInitialize();
#else
  ::antlr4::internal::call_once(labeledexprParserOnceFlag, labeledexprParserInitialize);
#endif
}
