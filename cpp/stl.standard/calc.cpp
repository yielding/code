#include "stlexam.h"

#include <vector>
#include <stack>

#include <iostream>

using namespace std;

//
// Simulate the behavior of a simple integer calculator.
//

class CalculatorEngine 
{
public:
  enum binaryOperator { PLUS, MINUS, TIMES, DIVIDE };
  int  currentMemory ()                { return data.top(); }
  void pushOperand   (int value)       { data.push (value); }
  void doOperator    (binaryOperator);
protected:
  stack< int > data;
};

void CalculatorEngine::doOperator(binaryOperator theOp)
{
  int right = data.top(); data.pop();
  int left  = data.top(); data.pop();
  switch (theOp) {
    case PLUS:   data.push(left + right); break;
    case MINUS:  data.push(left - right); break;
    case TIMES:  data.push(left * right); break;
    case DIVIDE: data.push(left / right); break;
  }
}

int main()
{
  cout << "Calculator example program, from Chapter 8" << endl;

  cout << "Enter a legal RPN expression, end with p q (print and quit)" << endl;
  int intval;
  CalculatorEngine calc;
  char c;

  while (cin >> c) switch (c) {
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      cin.putback(c);
      cin >> intval;
      calc.pushOperand(intval);
      break;
    case '+': 
      calc.doOperator(CalculatorEngine::PLUS); break;
    case '-': calc.doOperator(CalculatorEngine::MINUS); break;
    case '*': calc.doOperator(CalculatorEngine::TIMES); break;
    case '/': calc.doOperator(CalculatorEngine::DIVIDE); break;
    case 'p': cout << calc.currentMemory() << endl;
    case 'q': cout << "End calculator program" << endl;
              return 0; // quit program
  }
  return 0;
}
