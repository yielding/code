%{
    #include "shellparser.hpp"
%}
 
%option reentrant
%option noyywrap
 
%x SINGLE_QUOTED
%x DOUBLE_QUOTED
 
%%
 
"|"    {return PIPE;}
"$("   {return COMMAND_SUBSTITUTION_START;}
")"    {return COMMAND_SUBSTITUTION_END;}
 
[ \t\r\n]    {}
 
[a-zA-Z0-9\-\._/]+  {return FILENAME;}
 
[']                     {BEGIN(SINGLE_QUOTED);}
<SINGLE_QUOTED>[^']+    {}
<SINGLE_QUOTED>[']      {BEGIN(INITIAL); return ARGUMENT;}
<SINGLE_QUOTED><<EOF>>  {return -1;}
 
["]                     {BEGIN(DOUBLE_QUOTED);}
<DOUBLE_QUOTED>[^"]+    {}
<DOUBLE_QUOTED>["]      {BEGIN(INITIAL); return ARGUMENT;}
<DOUBLE_QUOTED><<EOF>>  {return -1;}
 
[^ \t\r\n|'"($\()\)]+   {return ARGUMENT;}
 
%%
