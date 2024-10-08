Copyright (GPL) 2004  mmc mchirico@users.sourceforge.net or mchirico@comcast.net
Last Updated: Fri Sep  3 10:18:58 EDT 2004


The latest version of these documents can be downloaded from:
http://prdownloads.sourceforge.net/souptonuts/lemon_examples.tar.gz?download

To get emailed when there are update and additions:
http://sourceforge.net/project/filemodule_monitor.php?filemodule_id=124528




example1.y  

      This is a very simple calulator. To compile the
      example do the following:

         $ make

      Then, to run the example issue the following
      command:

         $ ./ex1

      OK, what is happening: Take a look at the file
      "main_part", then, take a look at "example1.c"
      This main_part is appended to the raw form of
      example1.c in the Makefile. lemon does not create
      a complete program - only the necessary subroutines. 
      So it is necessary to build in the main part of a 
      program.

      If you make your own changes to this example, you
      should make the changes to example1.y or main_part.
      example1.c is a generated file, and will be over
      written every time lemon is run.

      Disecting main_part:

      If you take a look at the documentation at the following
      link:
         http://www.hwaci.com/sw/lemon/lemon.html

      You'll see that these are the  essential functions that 
      must be called. Note, this is a stripped down simple
      version with no error checking or tokenizer.  The 
      tokens are hardwired in so we can see exactly how
      lemon operates.

             void* pParser = ParseAlloc (malloc);
      
      The next 4 lines parse the command  15 DIVIDE 5.

             Parse (pParser, INTEGER, 15);     
	     Parse (pParser, DIVIDE, 0);     
	     Parse (pParser, INTEGER, 5);    
      	     Parse (pParser, 0, 0);          

      INTEGER and  DIVIDE are assigned values in the
      generated file example1.h to be the following;

             #define PLUS                            1
	     #define MINUS                           2
	     #define DIVIDE                          3
    	     #define TIMES                           4
      	     #define INTEGER                         5
     
      Again, this is a generated file, so if any additions
      are made to this file, they'll be over-written when
      re-running lemon.



example2.y

      This example is more interesting. A custom token is 
      created in ex2def.h with the following 4 lines:

       struct Token {                
	       const char *z; 
	       int value;     
 	       unsigned n;    
	     };

      This structure supports both a value and a count.
      You could add many more values, and allocate memory
      to z if you wanted. One, note, you cannot put in 
      C++  strings or other STL containers directly into
      this structure; instead, you would need to have pointers
      to such structures. Note, if memory was allocated for z, you'd
      have to free it. 
               


example3.y         
   
      This shows the behavior of a destructor

             Parse (pParser, 0, t1);
                             ^--^------ only t1 will destruct, since
                          it is called with zero here.


example4.y

       This is the first example that ends the grammer with 
       a newline.  

       In the file example4.y take a look at the following:

          main ::= in.
          in ::= .
          in ::= in state NEWLINE.


       In main_part4 the last statement here can cause the
       output to print. Whereas in examples 1-3 it had to
       end with  " Parse (pParser, 0, t0)";


               Parse (pParser, ID, t0);
               Parse (pParser, PLUS, t0);
               Parse (pParser, ID, t1);
               Parse (pParser, NEWLINE, t1);


example5.y

       This example puts everything together with flex. But,
       take a peek at how lexer.l is complied in the Makefile
       with "flex lexer.l" instead of "flex++ lexer.l". The
       main_part5 for this example is in C++, so it's necessary
       to add the extern "C" directives.  True it could have been
       compiled with flex++; but, many people prefer flex for the
       speed of C in complex projects.


       This example is run interactively, so entering something
       like 3+5 is demonstrated below:

            $ ./ex5
             3+5                             
	     yylex() 6 yylval.dval 3        
     	     yylex() 1 yylval.dval 3        
	     yylex() 6 yylval.dval 5        
	     yylex() 5 yylval.dval 5        
	    In token_destructor t.value= 3  
	    In token_destructor t.n= 0      
	    Result.value=8                  
	    Result.n=4                      


desktop_calc.cc

       This is an example of a C++ calculator that has not been
       written with a parser.






REFERENCES:

(1) Complete source for all examples including the lemon parser generator
http://prdownloads.sourceforge.net/souptonuts/lemon_examples.tar.gz?download

(2) Example desktop calculator from scratch
http://souptonuts.sourceforge.net/code/desktop_calc.cc.html] 

(3) Example of flex and bison parser
http://prdownloads.sourceforge.net/souptonuts/flex_bison.tar.gz?download

(3) Home of the lemon parser generator
http://www.hwaci.com/sw/lemon/

(4) Home of sqlite
http://www.sqlite.org/

(5) Glossory of paser terms
http://www.parsifalsoft.com/gloss.html

(6) Good introduction to parsers
http://www.parsifalsoft.com/isdp.html

(7) GNU flex manual
http://www.gnu.org/software/flex/manual/

(8) GNU bison manual
http://www.gnu.org/software/bison/manual/

(9) The spirit parser
September 2003, C/C++ Users Journal
Powerful parsing made easy via modern template techniques.
Joel de Guzman and Dan Nuffer.
http://spirit.sourceforge.net/

(10)
Getting a C++ Bison parser to use a C Flex lexer
http://www.iunknown.com/000123.html


