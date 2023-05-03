lexer grammar XMLLexer;

OPEN        : '<'  ->pushMode(INSIDE) ;
COMMENT     : '<!--' .*? '-->';
EntityRef   : '&' [a-z]+ ';' ;
TEXT        : ~('<'|'&')+ ;

mode INSIDE;

CLOSE       : '>'  -> popMode ;
SLASH_CLOSE : '/>' -> popMode ;
EQUALS      : '=';
STRING      : '"' .*? '"' ; 
SlashName   : '/' Name ;
Name        : ALPHA (ALPHA|DIGIT)* ;
S           : [ \t\r\n]           -> skip ;

fragment
ALPHA       : [a-zA-Z] ;

fragment
DIGIT       : [0-9] ;
