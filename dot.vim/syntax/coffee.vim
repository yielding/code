" Vim syntax file
" Language:     CoffeeScript
" Maintainer:   Karl Guertin <grayrest@gr.ayre.st>
" Last Change:  April 5, 2010
" Version:      0.6
"
" The javascript bits adapted from Yi Zhao's <zzlinux AT hotmail DOT com>
" javascript.vim
"
" This version matches the syntax of CoffeeScript 0.6.
"
if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'coffee'
endif

"" dollar sign is permitted anywhere in an identifier
setlocal isident+=$

"" Coffeescript uses # for comments
setlocal commentstring=#%s

syntax case match

"" BEGIN ACTUAL HIGHLIGHTING

syn match   jsSpecial           "\\\d\d\d\|\\x\x\{2\}\|\\u\x\{4\}\|\\." contained
syn keyword jsPrototype         prototype
syn match   jsNumber            /\<-\=\d\+L\=\>\|\<0[xX]\x\+\>/
syn match   jsFloat              /\<-\=\%(\d\+\.\d\+\|\d\+\.\|\.\d\+\)\%([eE][+-]\=\d\+\)\=\>/
syn match   jsIdentifier        "\<this\>\|\<arguments\>"
syn keyword jsSource            import export
syn keyword jsType              const undefined void
syn keyword jsOperator          delete new in instanceof let typeof
syn match   jsOperator          "[><*/+-]\@<![><*/+-][><*/+-]\@!\|[><!=+-]=\|++\|--\|||=\|&&=\|||\|&&\||\|&\|!\|%"
syn keyword jsBoolean           true false
syn keyword jsNull              null

"" Statement Keywords
syn keyword jsConditional       if else
syn keyword jsDeclaration       var
syn keyword jsRepeat            do while for in of
syn keyword jsBranch            break continue switch case default return yield
syn keyword jsStatement         try catch throw with finally

syn keyword jsGlobalObjects     Array Boolean Date Function Infinity JavaArray
    \                           JavaClass JavaObject JavaPackage Math Number
    \                           NaN Object Packages RegExp String
    \                           Undefined java netscape sun
syn match   jsGlobalObjects     "\<exports\>"

syn keyword jsExceptions        Error EvalError RangeError ReferenceError
    \                           SyntaxError TypeError URIError

syn keyword jsFutureKeys        abstract enum int short boolean export
    \                           interface static byte extends long super char
    \                           final native synchronized class float package
    \                           throws const goto private transient debugger
    \                           implements protected volatile double import
    \                           public

syn match   coffeeMaybeInterp   /\$\@<!\$@\?\K\k*/ contained contains=coffeeIdentifier
syn region  coffeeInterp        matchgroup=coffeeInterpolationDelim start="\${" end="}" contained contains=TOP

syn keyword coffeeTodo          TODO FIXME XXX TBD contained
syn match   coffeeComment       "#.*$" display contains=coffeeTodo,@Spell


syn region  coffeeObjLit        matchgroup=coffeeLiteral start="{" end="}" contains=TOP
syn region  coffeeAryLit        matchgroup=coffeeLiteral start="\i\@<!\[" end="\]" contains=TOP,coffeeRange
syn match coffeeIdentifier      "@"

syn region  jsInterpolate       start=+`+  skip=+\\n+  end=+`+ keepend
syn region  coffeeDString        start=+"+  skip=+\\n\|\\\\\|\\"+  end=+"+  contains=jsSpecial,coffeeMaybeInterp,coffeeInterp
syn region  coffeeString        start=+'+  skip=+\\n\|\\\\\|\\'+  end=+'+  contains=jsSpecial keepend
syn region  coffeeString        start=+"""+ end=+"""+ keepend contains=jsSpecial,@Spell
syn region  coffeeString        start=+'''+ end=+'''+ keepend contains=jsSpecial,@Spell

syn match  coffeeAssignment     /\%(@\?\I\i*\%(\(\.\|::\)\I\i*\|\[.\{-}\%(\[.\{-}\].\{-}\)*\]\)*\s*\%(:\@<!::\@!\|=\@<!==\@!\)\)/ contains=coffeeAccessOperator,coffeeArrayAccess,coffeeFunction,jsIdentifier,coffeeIdentifier,coffeeAssignmentOper,jsGlobalObjects
syn region  coffeeArrayAccess     matchgroup=coffeeAccessOperator start="\i\zs\[" end="\]" contains=TOP
syn region  coffeeFunctionCall    matchgroup=coffeeFnCallOperator start="\i\zs(" end=")" contains=TOP

syn keyword coffeeDeclaration   class extends
syn keyword coffeeConditional   unless when then
syn keyword coffeeOperator      and or is isnt not
syn match   coffeeAssignmentOper   ":\@<!::\@!\|=\@<!==\@!"
syn match   coffeeOperator      "\%(||=\|&&=\|?=\|\i\@<=?\|:\@!:\)"
syn match   coffeeAccessOperator /\.\|?\.\|::/
syn match   coffeeSplat         "\.\.\."
"This isn't with the other strings because it needs to override division
syn region  coffeeString        start=+/\(\*\|/\|\s\)\@!+ skip=+\\\\\|\\/+ end=+/[gim]\{,3}+ contains=jsSpecial oneline
syn match   coffeeRange         "\.\.\.\?" containedin=coffeeArrayAccess,coffeeAryLit

"Cofeescript function declaration is distressingly hard to parse with regular
"expressions, mostly because Vim (and most other languages) are built around
"the key atom being before instead after. Initial attempts using syn region
"and whatnot were a mess (see the note at the end of the section :help
"syn-region). The key realization was that the arguments region only extends
"across newlines if the line ends with a comma.

" This handles the true anonymous coffeescript functions ((blah) -> blah).
syn match  coffeeFunction       /(\%(\s*\n\)\?\%([^)]\|,\s*\n\)\{-}\%(\n\s*\)\?)\s*\%(=>\|->\)/
syn match  coffeeFunction       /=>\|->\|<-/

" The entire part before the second \%( is just to match the function name.
" I like this because it 1) makes functions easier to spot and 2) these
" functions are not anonymous, the highlighted part is what the js function
" will be named.
syn match  coffeeFunction       /\I\i*\s*\%(:\@<!:\|\i\@<==\@!<=\)\s*(\%(\s*\n\)\?\%(.\|,\s*\n\)\{-}\%(\n\s*\)\?)\s*\%(=>\|->\)/
syn match  coffeeFunction       /\I\i*\s*\%(:\@<!:\|\i\@<==\@!<=\)\s*\%(=>\|->\)/

if version >= 508 || !exists("did_javascript_syn_inits")
  if version < 508
    let did_javascript_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink coffeeSimpleAssign coffeeAssignment
  HiLink coffeeDestructure  coffeeAssignment
  HiLink coffeeSplat        coffeeOperator
  HiLink coffeeRange        coffeeOperator
  HiLink coffeeAssignmentOper coffeeOperator
  HiLink coffeeAccessOperator coffeeOperator
  HiLink coffeeDString      coffeeString
  HiLink jsIdentifier       coffeeIdentifier
  HiLink jsOperator         coffeeOperator
  HiLink jsBranch           coffeeConditional
  HiLink jsConditional      coffeeConditional
  HiLink jsRepeat           coffeeRepeat
  HiLink jsDeclaration      coffeeDeclaration

  HiLink coffeeComment      Comment
  HiLink coffeeTodo         Todo
  HiLink coffeeString       String
  HiLink jsSpecial          Special
  HiLink coffeeOperator     Operator
  HiLink coffeeAccessOperator     Operator
  HiLink coffeeConditional  Conditional
  HiLink coffeeRepeat       Repeat
  HiLink coffeeDeclaration  Type
  HiLink coffeeLiteral      Structure
  HiLink coffeeInterpolationDelim Delimiter
  HiLink coffeeMaybeInterp  Identifier
  HiLink coffeeFnCallOperator Type
  HiLink jsNumber           Number
  HiLink jsBoolean          Boolean
  HiLink jsType             Type
  HiLink jsNull             Type
  HiLink jsExceptions       Exception
  HiLink coffeeIdentifier   Keyword
  HiLink jsGlobalObjects    Identifier
  HiLink coffeeFunction     Function
  HiLink coffeeAssignment   PreProc
  HiLink jsInterpolate      Include
  delcommand HiLink
endif

let b:current_syntax = "coffee"
if main_syntax == 'coffee'
  unlet main_syntax
endif
