
// Generated from ./Expr.g4 by ANTLR 4.13.0


#include "ExprLexer.h"


using namespace antlr4;



using namespace antlr4;

namespace {

struct ExprLexerStaticData final {
  ExprLexerStaticData(std::vector<std::string> ruleNames,
                          std::vector<std::string> channelNames,
                          std::vector<std::string> modeNames,
                          std::vector<std::string> literalNames,
                          std::vector<std::string> symbolicNames)
      : ruleNames(std::move(ruleNames)), channelNames(std::move(channelNames)),
        modeNames(std::move(modeNames)), literalNames(std::move(literalNames)),
        symbolicNames(std::move(symbolicNames)),
        vocabulary(this->literalNames, this->symbolicNames) {}

  ExprLexerStaticData(const ExprLexerStaticData&) = delete;
  ExprLexerStaticData(ExprLexerStaticData&&) = delete;
  ExprLexerStaticData& operator=(const ExprLexerStaticData&) = delete;
  ExprLexerStaticData& operator=(ExprLexerStaticData&&) = delete;

  std::vector<antlr4::dfa::DFA> decisionToDFA;
  antlr4::atn::PredictionContextCache sharedContextCache;
  const std::vector<std::string> ruleNames;
  const std::vector<std::string> channelNames;
  const std::vector<std::string> modeNames;
  const std::vector<std::string> literalNames;
  const std::vector<std::string> symbolicNames;
  const antlr4::dfa::Vocabulary vocabulary;
  antlr4::atn::SerializedATNView serializedATN;
  std::unique_ptr<antlr4::atn::ATN> atn;
};

::antlr4::internal::OnceFlag exprlexerLexerOnceFlag;
#if ANTLR4_USE_THREAD_LOCAL_CACHE
static thread_local
#endif
ExprLexerStaticData *exprlexerLexerStaticData = nullptr;

void exprlexerLexerInitialize() {
#if ANTLR4_USE_THREAD_LOCAL_CACHE
  if (exprlexerLexerStaticData != nullptr) {
    return;
  }
#else
  assert(exprlexerLexerStaticData == nullptr);
#endif
  auto staticData = std::make_unique<ExprLexerStaticData>(
    std::vector<std::string>{
      "INT", "NL", "WS", "PLUS", "MINUS", "MULT", "DIV", "POPEN", "PCLOSE"
    },
    std::vector<std::string>{
      "DEFAULT_TOKEN_CHANNEL", "HIDDEN"
    },
    std::vector<std::string>{
      "DEFAULT_MODE"
    },
    std::vector<std::string>{
      "", "", "", "", "'+'", "'-'", "'*'", "'/'", "'('", "')'"
    },
    std::vector<std::string>{
      "", "INT", "NL", "WS", "PLUS", "MINUS", "MULT", "DIV", "POPEN", "PCLOSE"
    }
  );
  static const int32_t serializedATNSegment[] = {
  	4,0,9,48,6,-1,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,6,
  	2,7,7,7,2,8,7,8,1,0,4,0,21,8,0,11,0,12,0,22,1,1,3,1,26,8,1,1,1,1,1,1,
  	2,4,2,31,8,2,11,2,12,2,32,1,2,1,2,1,3,1,3,1,4,1,4,1,5,1,5,1,6,1,6,1,7,
  	1,7,1,8,1,8,0,0,9,1,1,3,2,5,3,7,4,9,5,11,6,13,7,15,8,17,9,1,0,2,1,0,48,
  	57,2,0,9,9,32,32,50,0,1,1,0,0,0,0,3,1,0,0,0,0,5,1,0,0,0,0,7,1,0,0,0,0,
  	9,1,0,0,0,0,11,1,0,0,0,0,13,1,0,0,0,0,15,1,0,0,0,0,17,1,0,0,0,1,20,1,
  	0,0,0,3,25,1,0,0,0,5,30,1,0,0,0,7,36,1,0,0,0,9,38,1,0,0,0,11,40,1,0,0,
  	0,13,42,1,0,0,0,15,44,1,0,0,0,17,46,1,0,0,0,19,21,7,0,0,0,20,19,1,0,0,
  	0,21,22,1,0,0,0,22,20,1,0,0,0,22,23,1,0,0,0,23,2,1,0,0,0,24,26,5,13,0,
  	0,25,24,1,0,0,0,25,26,1,0,0,0,26,27,1,0,0,0,27,28,5,10,0,0,28,4,1,0,0,
  	0,29,31,7,1,0,0,30,29,1,0,0,0,31,32,1,0,0,0,32,30,1,0,0,0,32,33,1,0,0,
  	0,33,34,1,0,0,0,34,35,6,2,0,0,35,6,1,0,0,0,36,37,5,43,0,0,37,8,1,0,0,
  	0,38,39,5,45,0,0,39,10,1,0,0,0,40,41,5,42,0,0,41,12,1,0,0,0,42,43,5,47,
  	0,0,43,14,1,0,0,0,44,45,5,40,0,0,45,16,1,0,0,0,46,47,5,41,0,0,47,18,1,
  	0,0,0,4,0,22,25,32,1,6,0,0
  };
  staticData->serializedATN = antlr4::atn::SerializedATNView(serializedATNSegment, sizeof(serializedATNSegment) / sizeof(serializedATNSegment[0]));

  antlr4::atn::ATNDeserializer deserializer;
  staticData->atn = deserializer.deserialize(staticData->serializedATN);

  const size_t count = staticData->atn->getNumberOfDecisions();
  staticData->decisionToDFA.reserve(count);
  for (size_t i = 0; i < count; i++) { 
    staticData->decisionToDFA.emplace_back(staticData->atn->getDecisionState(i), i);
  }
  exprlexerLexerStaticData = staticData.release();
}

}

ExprLexer::ExprLexer(CharStream *input) : Lexer(input) {
  ExprLexer::initialize();
  _interpreter = new atn::LexerATNSimulator(this, *exprlexerLexerStaticData->atn, exprlexerLexerStaticData->decisionToDFA, exprlexerLexerStaticData->sharedContextCache);
}

ExprLexer::~ExprLexer() {
  delete _interpreter;
}

std::string ExprLexer::getGrammarFileName() const {
  return "Expr.g4";
}

const std::vector<std::string>& ExprLexer::getRuleNames() const {
  return exprlexerLexerStaticData->ruleNames;
}

const std::vector<std::string>& ExprLexer::getChannelNames() const {
  return exprlexerLexerStaticData->channelNames;
}

const std::vector<std::string>& ExprLexer::getModeNames() const {
  return exprlexerLexerStaticData->modeNames;
}

const dfa::Vocabulary& ExprLexer::getVocabulary() const {
  return exprlexerLexerStaticData->vocabulary;
}

antlr4::atn::SerializedATNView ExprLexer::getSerializedATN() const {
  return exprlexerLexerStaticData->serializedATN;
}

const atn::ATN& ExprLexer::getATN() const {
  return *exprlexerLexerStaticData->atn;
}




void ExprLexer::initialize() {
#if ANTLR4_USE_THREAD_LOCAL_CACHE
  exprlexerLexerInitialize();
#else
  ::antlr4::internal::call_once(exprlexerLexerOnceFlag, exprlexerLexerInitialize);
#endif
}
