
// Generated from ./LabeledExpr.g4 by ANTLR 4.13.0


#include "LabeledExprLexer.h"


using namespace antlr4;



using namespace antlr4;

namespace {

struct LabeledExprLexerStaticData final {
  LabeledExprLexerStaticData(std::vector<std::string> ruleNames,
                          std::vector<std::string> channelNames,
                          std::vector<std::string> modeNames,
                          std::vector<std::string> literalNames,
                          std::vector<std::string> symbolicNames)
      : ruleNames(std::move(ruleNames)), channelNames(std::move(channelNames)),
        modeNames(std::move(modeNames)), literalNames(std::move(literalNames)),
        symbolicNames(std::move(symbolicNames)),
        vocabulary(this->literalNames, this->symbolicNames) {}

  LabeledExprLexerStaticData(const LabeledExprLexerStaticData&) = delete;
  LabeledExprLexerStaticData(LabeledExprLexerStaticData&&) = delete;
  LabeledExprLexerStaticData& operator=(const LabeledExprLexerStaticData&) = delete;
  LabeledExprLexerStaticData& operator=(LabeledExprLexerStaticData&&) = delete;

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

::antlr4::internal::OnceFlag labeledexprlexerLexerOnceFlag;
#if ANTLR4_USE_THREAD_LOCAL_CACHE
static thread_local
#endif
LabeledExprLexerStaticData *labeledexprlexerLexerStaticData = nullptr;

void labeledexprlexerLexerInitialize() {
#if ANTLR4_USE_THREAD_LOCAL_CACHE
  if (labeledexprlexerLexerStaticData != nullptr) {
    return;
  }
#else
  assert(labeledexprlexerLexerStaticData == nullptr);
#endif
  auto staticData = std::make_unique<LabeledExprLexerStaticData>(
    std::vector<std::string>{
      "T__0", "T__1", "T__2", "MUL", "DIV", "ADD", "SUB", "ID", "INT", "NEWLINE", 
      "WS"
    },
    std::vector<std::string>{
      "DEFAULT_TOKEN_CHANNEL", "HIDDEN"
    },
    std::vector<std::string>{
      "DEFAULT_MODE"
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
  	4,0,11,59,6,-1,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
  	6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,1,0,1,0,1,1,1,1,1,2,1,2,1,3,1,3,1,
  	4,1,4,1,5,1,5,1,6,1,6,1,7,4,7,39,8,7,11,7,12,7,40,1,8,4,8,44,8,8,11,8,
  	12,8,45,1,9,3,9,49,8,9,1,9,1,9,1,10,4,10,54,8,10,11,10,12,10,55,1,10,
  	1,10,0,0,11,1,1,3,2,5,3,7,4,9,5,11,6,13,7,15,8,17,9,19,10,21,11,1,0,3,
  	2,0,65,90,97,122,1,0,48,57,2,0,9,9,32,32,62,0,1,1,0,0,0,0,3,1,0,0,0,0,
  	5,1,0,0,0,0,7,1,0,0,0,0,9,1,0,0,0,0,11,1,0,0,0,0,13,1,0,0,0,0,15,1,0,
  	0,0,0,17,1,0,0,0,0,19,1,0,0,0,0,21,1,0,0,0,1,23,1,0,0,0,3,25,1,0,0,0,
  	5,27,1,0,0,0,7,29,1,0,0,0,9,31,1,0,0,0,11,33,1,0,0,0,13,35,1,0,0,0,15,
  	38,1,0,0,0,17,43,1,0,0,0,19,48,1,0,0,0,21,53,1,0,0,0,23,24,5,61,0,0,24,
  	2,1,0,0,0,25,26,5,40,0,0,26,4,1,0,0,0,27,28,5,41,0,0,28,6,1,0,0,0,29,
  	30,5,42,0,0,30,8,1,0,0,0,31,32,5,47,0,0,32,10,1,0,0,0,33,34,5,43,0,0,
  	34,12,1,0,0,0,35,36,5,45,0,0,36,14,1,0,0,0,37,39,7,0,0,0,38,37,1,0,0,
  	0,39,40,1,0,0,0,40,38,1,0,0,0,40,41,1,0,0,0,41,16,1,0,0,0,42,44,7,1,0,
  	0,43,42,1,0,0,0,44,45,1,0,0,0,45,43,1,0,0,0,45,46,1,0,0,0,46,18,1,0,0,
  	0,47,49,5,13,0,0,48,47,1,0,0,0,48,49,1,0,0,0,49,50,1,0,0,0,50,51,5,10,
  	0,0,51,20,1,0,0,0,52,54,7,2,0,0,53,52,1,0,0,0,54,55,1,0,0,0,55,53,1,0,
  	0,0,55,56,1,0,0,0,56,57,1,0,0,0,57,58,6,10,0,0,58,22,1,0,0,0,5,0,40,45,
  	48,55,1,6,0,0
  };
  staticData->serializedATN = antlr4::atn::SerializedATNView(serializedATNSegment, sizeof(serializedATNSegment) / sizeof(serializedATNSegment[0]));

  antlr4::atn::ATNDeserializer deserializer;
  staticData->atn = deserializer.deserialize(staticData->serializedATN);

  const size_t count = staticData->atn->getNumberOfDecisions();
  staticData->decisionToDFA.reserve(count);
  for (size_t i = 0; i < count; i++) { 
    staticData->decisionToDFA.emplace_back(staticData->atn->getDecisionState(i), i);
  }
  labeledexprlexerLexerStaticData = staticData.release();
}

}

LabeledExprLexer::LabeledExprLexer(CharStream *input) : Lexer(input) {
  LabeledExprLexer::initialize();
  _interpreter = new atn::LexerATNSimulator(this, *labeledexprlexerLexerStaticData->atn, labeledexprlexerLexerStaticData->decisionToDFA, labeledexprlexerLexerStaticData->sharedContextCache);
}

LabeledExprLexer::~LabeledExprLexer() {
  delete _interpreter;
}

std::string LabeledExprLexer::getGrammarFileName() const {
  return "LabeledExpr.g4";
}

const std::vector<std::string>& LabeledExprLexer::getRuleNames() const {
  return labeledexprlexerLexerStaticData->ruleNames;
}

const std::vector<std::string>& LabeledExprLexer::getChannelNames() const {
  return labeledexprlexerLexerStaticData->channelNames;
}

const std::vector<std::string>& LabeledExprLexer::getModeNames() const {
  return labeledexprlexerLexerStaticData->modeNames;
}

const dfa::Vocabulary& LabeledExprLexer::getVocabulary() const {
  return labeledexprlexerLexerStaticData->vocabulary;
}

antlr4::atn::SerializedATNView LabeledExprLexer::getSerializedATN() const {
  return labeledexprlexerLexerStaticData->serializedATN;
}

const atn::ATN& LabeledExprLexer::getATN() const {
  return *labeledexprlexerLexerStaticData->atn;
}




void LabeledExprLexer::initialize() {
#if ANTLR4_USE_THREAD_LOCAL_CACHE
  labeledexprlexerLexerInitialize();
#else
  ::antlr4::internal::call_once(labeledexprlexerLexerOnceFlag, labeledexprlexerLexerInitialize);
#endif
}
