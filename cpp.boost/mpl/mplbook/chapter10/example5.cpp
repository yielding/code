/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It was built successfully with the following command: 

        bash -c "yacc -t -o %TEMP%/metaprogram-chapter10-example5_.cpp example5.cpp"

*/

%{ // C++ code to be inserted in the generated source file
  #include <cstdio>
  typedef int YYSTYPE; // the type of all semantic values
  int yylex();                    // forward
  void yyerror(char const* msg);  // forward
%}

%token INTEGER     /* declare a symbolic multi-character token */
%start lines       /* lines is the start symbol */

%% /* grammar rules and actions */

expression : term
           | expression '+' term { $$ = $1 + $3; }
           | expression '-' term { $$ = $1 - $3; }
           ;

term : factor
     | term '*' factor  { $$ = $1 * $3; }
     | term '/' factor  { $$ = $1 / $3; }
     ;

factor : INTEGER
       | group
       ;

group : '(' expression ')' { $$ = $2; }
      ;

lines : lines expression
        { 
          std::printf("= %d\n", $2);   // after every expression
          std::fflush(stdout);    // print its value       
        } 
        '\n' 
      | /* empty */
      ;

%% /* C++ code to be inserted in the generated source file */
#include <cctype>

int yylex()  // tokenizer function
{
  int c;

  // skip whitespace
  do { c = std::getchar(); }
  while (c == ' ' || c == '\t' || c == '\r');

  if (c == EOF)
    return 0;

  if (std::isdigit (c))
  {
      std::ungetc(c, stdin);
      std::scanf("%d", &yylval); // store semantic value
      return INTEGER;
  }
  return c;
}
// standard error handler
void yyerror(char const* msg) { std::fprintf(stderr,msg); }

int main() { int yyparse(); return yyparse(); return 0; }
