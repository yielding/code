#include "antlr4-runtime.h"
#include "LabeledExprLexer.h"
#include "LabeledExprParser.h"
#include "EvalVisitor.h"

#include <cassert>

using namespace std;
using namespace antlr4;

int main(int argc, char* argv[])
{
  assert(argc == 2);

  ifstream ifs(argv[1]);
  if (ifs.good()) 
  {
    auto input  = ANTLRInputStream(ifs);
    auto lexer  = LabeledExprLexer(&input);
    auto tokens = CommonTokenStream(&lexer);
    auto parser = LabeledExprParser(&tokens);

    auto tree = parser.prog();
    EvalVisitor visitor; visitor.visit(tree);
  }

  return 0;
}
