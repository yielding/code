
// Generated from ./Expr.g4 by ANTLR 4.13.0


#include "ExprListener.h"

#include "ExprParser.h"


using namespace antlrcpp;

using namespace antlr4;

namespace {

struct ExprParserStaticData final {
  ExprParserStaticData(std::vector<std::string> ruleNames,
                        std::vector<std::string> literalNames,
                        std::vector<std::string> symbolicNames)
      : ruleNames(std::move(ruleNames)), literalNames(std::move(literalNames)),
        symbolicNames(std::move(symbolicNames)),
        vocabulary(this->literalNames, this->symbolicNames) {}

  ExprParserStaticData(const ExprParserStaticData&) = delete;
  ExprParserStaticData(ExprParserStaticData&&) = delete;
  ExprParserStaticData& operator=(const ExprParserStaticData&) = delete;
  ExprParserStaticData& operator=(ExprParserStaticData&&) = delete;

  std::vector<antlr4::dfa::DFA> decisionToDFA;
  antlr4::atn::PredictionContextCache sharedContextCache;
  const std::vector<std::string> ruleNames;
  const std::vector<std::string> literalNames;
  const std::vector<std::string> symbolicNames;
  const antlr4::dfa::Vocabulary vocabulary;
  antlr4::atn::SerializedATNView serializedATN;
  std::unique_ptr<antlr4::atn::ATN> atn;
};

::antlr4::internal::OnceFlag exprParserOnceFlag;
#if ANTLR4_USE_THREAD_LOCAL_CACHE
static thread_local
#endif
ExprParserStaticData *exprParserStaticData = nullptr;

void exprParserInitialize() {
#if ANTLR4_USE_THREAD_LOCAL_CACHE
  if (exprParserStaticData != nullptr) {
    return;
  }
#else
  assert(exprParserStaticData == nullptr);
#endif
  auto staticData = std::make_unique<ExprParserStaticData>(
    std::vector<std::string>{
      "prog", "plusOrMinus", "multOrDiv", "atom"
    },
    std::vector<std::string>{
      "", "", "", "", "'+'", "'-'", "'*'", "'/'", "'('", "')'"
    },
    std::vector<std::string>{
      "", "INT", "NL", "WS", "PLUS", "MINUS", "MULT", "DIV", "POPEN", "PCLOSE"
    }
  );
  static const int32_t serializedATNSegment[] = {
  	4,1,9,52,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,1,0,1,0,3,0,11,8,0,1,0,1,0,1,
  	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,1,24,8,1,10,1,12,1,27,9,1,1,2,1,2,
  	1,2,1,2,1,2,1,2,1,2,1,2,1,2,5,2,38,8,2,10,2,12,2,41,9,2,1,3,1,3,1,3,1,
  	3,1,3,1,3,1,3,3,3,50,8,3,1,3,0,2,2,4,4,0,2,4,6,0,0,54,0,8,1,0,0,0,2,14,
  	1,0,0,0,4,28,1,0,0,0,6,49,1,0,0,0,8,10,3,2,1,0,9,11,5,2,0,0,10,9,1,0,
  	0,0,10,11,1,0,0,0,11,12,1,0,0,0,12,13,5,0,0,1,13,1,1,0,0,0,14,15,6,1,
  	-1,0,15,16,3,4,2,0,16,25,1,0,0,0,17,18,10,3,0,0,18,19,5,4,0,0,19,24,3,
  	4,2,0,20,21,10,2,0,0,21,22,5,5,0,0,22,24,3,4,2,0,23,17,1,0,0,0,23,20,
  	1,0,0,0,24,27,1,0,0,0,25,23,1,0,0,0,25,26,1,0,0,0,26,3,1,0,0,0,27,25,
  	1,0,0,0,28,29,6,2,-1,0,29,30,3,6,3,0,30,39,1,0,0,0,31,32,10,3,0,0,32,
  	33,5,6,0,0,33,38,3,6,3,0,34,35,10,2,0,0,35,36,5,7,0,0,36,38,3,6,3,0,37,
  	31,1,0,0,0,37,34,1,0,0,0,38,41,1,0,0,0,39,37,1,0,0,0,39,40,1,0,0,0,40,
  	5,1,0,0,0,41,39,1,0,0,0,42,50,5,1,0,0,43,44,5,5,0,0,44,50,3,2,1,0,45,
  	46,5,8,0,0,46,47,3,2,1,0,47,48,5,9,0,0,48,50,1,0,0,0,49,42,1,0,0,0,49,
  	43,1,0,0,0,49,45,1,0,0,0,50,7,1,0,0,0,6,10,23,25,37,39,49
  };
  staticData->serializedATN = antlr4::atn::SerializedATNView(serializedATNSegment, sizeof(serializedATNSegment) / sizeof(serializedATNSegment[0]));

  antlr4::atn::ATNDeserializer deserializer;
  staticData->atn = deserializer.deserialize(staticData->serializedATN);

  const size_t count = staticData->atn->getNumberOfDecisions();
  staticData->decisionToDFA.reserve(count);
  for (size_t i = 0; i < count; i++) { 
    staticData->decisionToDFA.emplace_back(staticData->atn->getDecisionState(i), i);
  }
  exprParserStaticData = staticData.release();
}

}

ExprParser::ExprParser(TokenStream *input) : ExprParser(input, antlr4::atn::ParserATNSimulatorOptions()) {}

ExprParser::ExprParser(TokenStream *input, const antlr4::atn::ParserATNSimulatorOptions &options) : Parser(input) {
  ExprParser::initialize();
  _interpreter = new atn::ParserATNSimulator(this, *exprParserStaticData->atn, exprParserStaticData->decisionToDFA, exprParserStaticData->sharedContextCache, options);
}

ExprParser::~ExprParser() {
  delete _interpreter;
}

const atn::ATN& ExprParser::getATN() const {
  return *exprParserStaticData->atn;
}

std::string ExprParser::getGrammarFileName() const {
  return "Expr.g4";
}

const std::vector<std::string>& ExprParser::getRuleNames() const {
  return exprParserStaticData->ruleNames;
}

const dfa::Vocabulary& ExprParser::getVocabulary() const {
  return exprParserStaticData->vocabulary;
}

antlr4::atn::SerializedATNView ExprParser::getSerializedATN() const {
  return exprParserStaticData->serializedATN;
}


//----------------- ProgContext ------------------------------------------------------------------

ExprParser::ProgContext::ProgContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ExprParser::PlusOrMinusContext* ExprParser::ProgContext::plusOrMinus() {
  return getRuleContext<ExprParser::PlusOrMinusContext>(0);
}

tree::TerminalNode* ExprParser::ProgContext::EOF() {
  return getToken(ExprParser::EOF, 0);
}

tree::TerminalNode* ExprParser::ProgContext::NL() {
  return getToken(ExprParser::NL, 0);
}


size_t ExprParser::ProgContext::getRuleIndex() const {
  return ExprParser::RuleProg;
}

void ExprParser::ProgContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterProg(this);
}

void ExprParser::ProgContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitProg(this);
}

ExprParser::ProgContext* ExprParser::prog() {
  ProgContext *_localctx = _tracker.createInstance<ProgContext>(_ctx, getState());
  enterRule(_localctx, 0, ExprParser::RuleProg);
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
    setState(8);
    plusOrMinus(0);
    setState(10);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ExprParser::NL) {
      setState(9);
      match(ExprParser::NL);
    }
    setState(12);
    match(ExprParser::EOF);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- PlusOrMinusContext ------------------------------------------------------------------

ExprParser::PlusOrMinusContext::PlusOrMinusContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}


size_t ExprParser::PlusOrMinusContext::getRuleIndex() const {
  return ExprParser::RulePlusOrMinus;
}

void ExprParser::PlusOrMinusContext::copyFrom(PlusOrMinusContext *ctx) {
  ParserRuleContext::copyFrom(ctx);
}

//----------------- ToMultOrDivContext ------------------------------------------------------------------

ExprParser::MultOrDivContext* ExprParser::ToMultOrDivContext::multOrDiv() {
  return getRuleContext<ExprParser::MultOrDivContext>(0);
}

ExprParser::ToMultOrDivContext::ToMultOrDivContext(PlusOrMinusContext *ctx) { copyFrom(ctx); }

void ExprParser::ToMultOrDivContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterToMultOrDiv(this);
}
void ExprParser::ToMultOrDivContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitToMultOrDiv(this);
}
//----------------- BinaryMinusOpContext ------------------------------------------------------------------

ExprParser::PlusOrMinusContext* ExprParser::BinaryMinusOpContext::plusOrMinus() {
  return getRuleContext<ExprParser::PlusOrMinusContext>(0);
}

tree::TerminalNode* ExprParser::BinaryMinusOpContext::MINUS() {
  return getToken(ExprParser::MINUS, 0);
}

ExprParser::MultOrDivContext* ExprParser::BinaryMinusOpContext::multOrDiv() {
  return getRuleContext<ExprParser::MultOrDivContext>(0);
}

ExprParser::BinaryMinusOpContext::BinaryMinusOpContext(PlusOrMinusContext *ctx) { copyFrom(ctx); }

void ExprParser::BinaryMinusOpContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterBinaryMinusOp(this);
}
void ExprParser::BinaryMinusOpContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitBinaryMinusOp(this);
}
//----------------- PlusOpContext ------------------------------------------------------------------

ExprParser::PlusOrMinusContext* ExprParser::PlusOpContext::plusOrMinus() {
  return getRuleContext<ExprParser::PlusOrMinusContext>(0);
}

tree::TerminalNode* ExprParser::PlusOpContext::PLUS() {
  return getToken(ExprParser::PLUS, 0);
}

ExprParser::MultOrDivContext* ExprParser::PlusOpContext::multOrDiv() {
  return getRuleContext<ExprParser::MultOrDivContext>(0);
}

ExprParser::PlusOpContext::PlusOpContext(PlusOrMinusContext *ctx) { copyFrom(ctx); }

void ExprParser::PlusOpContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterPlusOp(this);
}
void ExprParser::PlusOpContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitPlusOp(this);
}

ExprParser::PlusOrMinusContext* ExprParser::plusOrMinus() {
   return plusOrMinus(0);
}

ExprParser::PlusOrMinusContext* ExprParser::plusOrMinus(int precedence) {
  ParserRuleContext *parentContext = _ctx;
  size_t parentState = getState();
  ExprParser::PlusOrMinusContext *_localctx = _tracker.createInstance<PlusOrMinusContext>(_ctx, parentState);
  ExprParser::PlusOrMinusContext *previousContext = _localctx;
  (void)previousContext; // Silence compiler, in case the context is not used by generated code.
  size_t startState = 2;
  enterRecursionRule(_localctx, 2, ExprParser::RulePlusOrMinus, precedence);

    

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
    _localctx = _tracker.createInstance<ToMultOrDivContext>(_localctx);
    _ctx = _localctx;
    previousContext = _localctx;

    setState(15);
    multOrDiv(0);
    _ctx->stop = _input->LT(-1);
    setState(25);
    _errHandler->sync(this);
    alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 2, _ctx);
    while (alt != 2 && alt != atn::ATN::INVALID_ALT_NUMBER) {
      if (alt == 1) {
        if (!_parseListeners.empty())
          triggerExitRuleEvent();
        previousContext = _localctx;
        setState(23);
        _errHandler->sync(this);
        switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 1, _ctx)) {
        case 1: {
          auto newContext = _tracker.createInstance<PlusOpContext>(_tracker.createInstance<PlusOrMinusContext>(parentContext, parentState));
          _localctx = newContext;
          pushNewRecursionContext(newContext, startState, RulePlusOrMinus);
          setState(17);

          if (!(precpred(_ctx, 3))) throw FailedPredicateException(this, "precpred(_ctx, 3)");
          setState(18);
          match(ExprParser::PLUS);
          setState(19);
          multOrDiv(0);
          break;
        }

        case 2: {
          auto newContext = _tracker.createInstance<BinaryMinusOpContext>(_tracker.createInstance<PlusOrMinusContext>(parentContext, parentState));
          _localctx = newContext;
          pushNewRecursionContext(newContext, startState, RulePlusOrMinus);
          setState(20);

          if (!(precpred(_ctx, 2))) throw FailedPredicateException(this, "precpred(_ctx, 2)");
          setState(21);
          match(ExprParser::MINUS);
          setState(22);
          multOrDiv(0);
          break;
        }

        default:
          break;
        } 
      }
      setState(27);
      _errHandler->sync(this);
      alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 2, _ctx);
    }
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }
  return _localctx;
}

//----------------- MultOrDivContext ------------------------------------------------------------------

ExprParser::MultOrDivContext::MultOrDivContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}


size_t ExprParser::MultOrDivContext::getRuleIndex() const {
  return ExprParser::RuleMultOrDiv;
}

void ExprParser::MultOrDivContext::copyFrom(MultOrDivContext *ctx) {
  ParserRuleContext::copyFrom(ctx);
}

//----------------- ToAtomContext ------------------------------------------------------------------

ExprParser::AtomContext* ExprParser::ToAtomContext::atom() {
  return getRuleContext<ExprParser::AtomContext>(0);
}

ExprParser::ToAtomContext::ToAtomContext(MultOrDivContext *ctx) { copyFrom(ctx); }

void ExprParser::ToAtomContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterToAtom(this);
}
void ExprParser::ToAtomContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitToAtom(this);
}
//----------------- MultOpContext ------------------------------------------------------------------

ExprParser::MultOrDivContext* ExprParser::MultOpContext::multOrDiv() {
  return getRuleContext<ExprParser::MultOrDivContext>(0);
}

tree::TerminalNode* ExprParser::MultOpContext::MULT() {
  return getToken(ExprParser::MULT, 0);
}

ExprParser::AtomContext* ExprParser::MultOpContext::atom() {
  return getRuleContext<ExprParser::AtomContext>(0);
}

ExprParser::MultOpContext::MultOpContext(MultOrDivContext *ctx) { copyFrom(ctx); }

void ExprParser::MultOpContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterMultOp(this);
}
void ExprParser::MultOpContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitMultOp(this);
}
//----------------- DivOpContext ------------------------------------------------------------------

ExprParser::MultOrDivContext* ExprParser::DivOpContext::multOrDiv() {
  return getRuleContext<ExprParser::MultOrDivContext>(0);
}

tree::TerminalNode* ExprParser::DivOpContext::DIV() {
  return getToken(ExprParser::DIV, 0);
}

ExprParser::AtomContext* ExprParser::DivOpContext::atom() {
  return getRuleContext<ExprParser::AtomContext>(0);
}

ExprParser::DivOpContext::DivOpContext(MultOrDivContext *ctx) { copyFrom(ctx); }

void ExprParser::DivOpContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterDivOp(this);
}
void ExprParser::DivOpContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitDivOp(this);
}

ExprParser::MultOrDivContext* ExprParser::multOrDiv() {
   return multOrDiv(0);
}

ExprParser::MultOrDivContext* ExprParser::multOrDiv(int precedence) {
  ParserRuleContext *parentContext = _ctx;
  size_t parentState = getState();
  ExprParser::MultOrDivContext *_localctx = _tracker.createInstance<MultOrDivContext>(_ctx, parentState);
  ExprParser::MultOrDivContext *previousContext = _localctx;
  (void)previousContext; // Silence compiler, in case the context is not used by generated code.
  size_t startState = 4;
  enterRecursionRule(_localctx, 4, ExprParser::RuleMultOrDiv, precedence);

    

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
    _localctx = _tracker.createInstance<ToAtomContext>(_localctx);
    _ctx = _localctx;
    previousContext = _localctx;

    setState(29);
    atom();
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
          auto newContext = _tracker.createInstance<MultOpContext>(_tracker.createInstance<MultOrDivContext>(parentContext, parentState));
          _localctx = newContext;
          pushNewRecursionContext(newContext, startState, RuleMultOrDiv);
          setState(31);

          if (!(precpred(_ctx, 3))) throw FailedPredicateException(this, "precpred(_ctx, 3)");
          setState(32);
          match(ExprParser::MULT);
          setState(33);
          atom();
          break;
        }

        case 2: {
          auto newContext = _tracker.createInstance<DivOpContext>(_tracker.createInstance<MultOrDivContext>(parentContext, parentState));
          _localctx = newContext;
          pushNewRecursionContext(newContext, startState, RuleMultOrDiv);
          setState(34);

          if (!(precpred(_ctx, 2))) throw FailedPredicateException(this, "precpred(_ctx, 2)");
          setState(35);
          match(ExprParser::DIV);
          setState(36);
          atom();
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

//----------------- AtomContext ------------------------------------------------------------------

ExprParser::AtomContext::AtomContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}


size_t ExprParser::AtomContext::getRuleIndex() const {
  return ExprParser::RuleAtom;
}

void ExprParser::AtomContext::copyFrom(AtomContext *ctx) {
  ParserRuleContext::copyFrom(ctx);
}

//----------------- ParenthesisOpContext ------------------------------------------------------------------

tree::TerminalNode* ExprParser::ParenthesisOpContext::POPEN() {
  return getToken(ExprParser::POPEN, 0);
}

ExprParser::PlusOrMinusContext* ExprParser::ParenthesisOpContext::plusOrMinus() {
  return getRuleContext<ExprParser::PlusOrMinusContext>(0);
}

tree::TerminalNode* ExprParser::ParenthesisOpContext::PCLOSE() {
  return getToken(ExprParser::PCLOSE, 0);
}

ExprParser::ParenthesisOpContext::ParenthesisOpContext(AtomContext *ctx) { copyFrom(ctx); }

void ExprParser::ParenthesisOpContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterParenthesisOp(this);
}
void ExprParser::ParenthesisOpContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitParenthesisOp(this);
}
//----------------- IntContext ------------------------------------------------------------------

tree::TerminalNode* ExprParser::IntContext::INT() {
  return getToken(ExprParser::INT, 0);
}

ExprParser::IntContext::IntContext(AtomContext *ctx) { copyFrom(ctx); }

void ExprParser::IntContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterInt(this);
}
void ExprParser::IntContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitInt(this);
}
//----------------- UnaryMinusOpContext ------------------------------------------------------------------

tree::TerminalNode* ExprParser::UnaryMinusOpContext::MINUS() {
  return getToken(ExprParser::MINUS, 0);
}

ExprParser::PlusOrMinusContext* ExprParser::UnaryMinusOpContext::plusOrMinus() {
  return getRuleContext<ExprParser::PlusOrMinusContext>(0);
}

ExprParser::UnaryMinusOpContext::UnaryMinusOpContext(AtomContext *ctx) { copyFrom(ctx); }

void ExprParser::UnaryMinusOpContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterUnaryMinusOp(this);
}
void ExprParser::UnaryMinusOpContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ExprListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitUnaryMinusOp(this);
}
ExprParser::AtomContext* ExprParser::atom() {
  AtomContext *_localctx = _tracker.createInstance<AtomContext>(_ctx, getState());
  enterRule(_localctx, 6, ExprParser::RuleAtom);

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    setState(49);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ExprParser::INT: {
        _localctx = _tracker.createInstance<ExprParser::IntContext>(_localctx);
        enterOuterAlt(_localctx, 1);
        setState(42);
        match(ExprParser::INT);
        break;
      }

      case ExprParser::MINUS: {
        _localctx = _tracker.createInstance<ExprParser::UnaryMinusOpContext>(_localctx);
        enterOuterAlt(_localctx, 2);
        setState(43);
        match(ExprParser::MINUS);
        setState(44);
        plusOrMinus(0);
        break;
      }

      case ExprParser::POPEN: {
        _localctx = _tracker.createInstance<ExprParser::ParenthesisOpContext>(_localctx);
        enterOuterAlt(_localctx, 3);
        setState(45);
        match(ExprParser::POPEN);
        setState(46);
        plusOrMinus(0);
        setState(47);
        match(ExprParser::PCLOSE);
        break;
      }

    default:
      throw NoViableAltException(this);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

bool ExprParser::sempred(RuleContext *context, size_t ruleIndex, size_t predicateIndex) {
  switch (ruleIndex) {
    case 1: return plusOrMinusSempred(antlrcpp::downCast<PlusOrMinusContext *>(context), predicateIndex);
    case 2: return multOrDivSempred(antlrcpp::downCast<MultOrDivContext *>(context), predicateIndex);

  default:
    break;
  }
  return true;
}

bool ExprParser::plusOrMinusSempred(PlusOrMinusContext *_localctx, size_t predicateIndex) {
  switch (predicateIndex) {
    case 0: return precpred(_ctx, 3);
    case 1: return precpred(_ctx, 2);

  default:
    break;
  }
  return true;
}

bool ExprParser::multOrDivSempred(MultOrDivContext *_localctx, size_t predicateIndex) {
  switch (predicateIndex) {
    case 2: return precpred(_ctx, 3);
    case 3: return precpred(_ctx, 2);

  default:
    break;
  }
  return true;
}

void ExprParser::initialize() {
#if ANTLR4_USE_THREAD_LOCAL_CACHE
  exprParserInitialize();
#else
  ::antlr4::internal::call_once(exprParserOnceFlag, exprParserInitialize);
#endif
}
