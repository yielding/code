#include "antlr4-runtime.h"
#include "LabeledExprLexer.h"
#include "LabeledExprParser.h"
#include "EvalVisitor.h"

#include <iostream>
#include <fstream>

using namespace std;
using namespace antlr4;

int main(int argc, char* argv[])
{
  if (argc != 2) 
  {
    cout << "xxxxx\n";
    return 0;
  }

  ifstream ifs(argv[1]);
  if (!ifs.good()) 
  {
    cout << "yyyy\n";
    return 0;
  }

  ANTLRInputStream input(ifs);
  LabeledExprLexer lexer(&input);
  CommonTokenStream tokens(&lexer);
  LabeledExprParser parser(&tokens);

  auto tree = parser.prog();
  EvalVisitor visitor;
  visitor.visit(tree);

  return 0;
}
