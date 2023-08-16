#include "antlr4-runtime.h"
#include "tree/IterativeParseTreeWalker.h"
#include "ExprLexer.h"
#include "CalcInterpreter.h"
#include "ExprParser.h"

#include <iostream>
#include <cstdlib>

using namespace std;
using namespace antlr4;

void execute_expression(string const& expr)
{
  // Provide the input text in a stream
  ANTLRInputStream input(expr);

  // Create a lexer from the input
  ExprLexer lexer(&input);
  lexer.removeErrorListeners();

  // Create a token stream from the lexer
  CommonTokenStream tokens(&lexer);

  // Create a parser from the token stream
  ExprParser parser(&tokens);    

  // Display the parse tree
  // cout << parser.expr()->toStringTree() << endl;

  tree::ParseTree* tree = nullptr;
  try
  {
    tree = parser.prog();
  }
  catch (ParseCancellationException& e)
  {
    cout << e.what() << endl;
    exit(EXIT_FAILURE);
  }

  CalcInterpreter interpreter;
  tree::IterativeParseTreeWalker walker;
  walker.walk(&interpreter, tree);

  cout << interpreter.get_result() << endl;
}

int main(int argc, char* argv[])
{
  string prefix = "calc > ";

  cout << prefix;

  string line;
  while (getline(cin, line))
  {
    execute_expression(line);
    cout << prefix;
  }

  return 0;
}