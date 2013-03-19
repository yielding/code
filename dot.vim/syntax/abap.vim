" Vim syntax file
" Language:	SAP/ABAP-4
" Maintainer:	Swapan Sarkar <swapan@yahoo.com>
" Last Change:	2003 June 16
"
" Change Log:
" 2003 June 16    Jeff Woehler <jrwoehler@softhome.net>
"  - Numeric values highlighted as constants
"  - Added many ABAP keywords/statements.
"  - Changed 'syn keyword' to 'syn match' on all words with '-' (word barier).
"

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

" The ABAP reserved words, defined as keywords.

syn keyword abapSpecial 	false null true initial

syn match abapMacro		/rp-low-high/
syn match abapMacro		/rp-provide-from-last/
syn match abapMacro		/rp-provide-from-first/

syn keyword abapEvent		initialization get
syn match abapEvent		/^\s*at/		" used for the 'AT SELECTION-SCREEN' statement
syn match abapEvent		/start-of-selection/
syn match abapEvent		/end-of-selection/
syn match abapEvent		/top-of-page/
syn match abapEvent		/end-of-end/
syn match abapEvent		/selection-screen/
"syn keyword abapEvent		start of selection

syn keyword abapKeyword		report tables infotypes form endform
syn keyword abapKeyword		constants data types class
syn keyword abapKeyword		ranges parameters
syn match abapKeyword		/select-options/
syn match abapKeyword		/field-symbols/

syn keyword abapCond		if else elseif endif case when others endcase
syn keyword abapRepeat		loop endloop while endwhile do enddo
"syn match abapRepeat		/loop\( at\)\@!/

syn keyword abapOperator	not and or lt le gt ge eq ne co ca cs cn na ns cp np 	contained
syn keyword abapOperator	like for in is

syn keyword abapStatement 	write concatenate add subtract multiply divide move translate
syn keyword abapStatement 	delete insert select endselect update modify commit append
syn keyword abapStatement	describe split condense clear
syn keyword abapStatement 	format exit continue reject on single
syn keyword abapStatement	into values from where ref end begin type to
syn keyword abapStatement	definition load block with frame title work and or of
syn match abapStatement		/create object/
syn match abapStatement		/move-corresponding/
syn match abapStatement		/lower case obligatory/
syn match abapStatement		/\(.*\w\+\W\+\)\@<=at/			" used for the 'LOOP AT' and 'SPLIT var AT' statement
syn match abapStatement		/line of/
syn match abapStatement		/line-size/
syn match abapStatement		/line-count/
syn match abapStatement		/no standard page heading/
syn match abapStatement		/message\(-id\)\=/		" 'message' or 'message-id'
syn match abapStatement		/\Wvalue\(-request\)\=\W/		" 'value' or 'value-request'
syn match abapStatement		/defining database/

"syn region abapForm		start=/form/ end=/$/	contains=abapParam,abapComment,abapString,abapNumber
"syn region abapForm		start=/call function/ end=/$/	contains=abapString,abapComment
"syn region abapForm		start=/call method/ end=/[.]/	contains=abapString,abapComment
"syn region abapForm		start=/perform/ end=/$/	contains=abapParam,abapComment,abapString,abapNumber

"syn match abapFunction		/loop at/
syn keyword abapFunction	perform
syn keyword abapFunction	call method function
syn keyword abapFunction	exporting importing receiving changing exceptions
" exporting importing receiving changing exceptions

syn match abapSystem		/sy-[a-z0-9]\+/
syn region abapInclude		start=/include/ end=/$/ contains=abapComment 
syn region abapParen		start='(' end=')' contains=ALL transparent 

"syn keyword abapType	boolean char character date float integer long
"syn keyword abapType	mlslabel number raw rowid varchar varchar2 varray

" Strings and characters:
syn region abapString		start=/'/  skip=/\\\\\|\\'/  end=/'/

"syn match abapIdentifier	/\<\l\+\>/

" Numbers
"syn match abapNumber		/-\=\<\d*\.\=[0-9_]\>/	contained
"integer number, or floating point number without a dot.
"syn match  abapNumber		/\(\(with\)\)\<\d\+\>/
syn match abapNumber		/\(with .*[-]\)\@<!\<\d\+\>/
"floating point number, with dot
syn match  abapNumber		/\<\d\+\.\d*\>/
"floating point number, starting with a dot
syn match  abapNumber		/\.\d\+\>/

" Comments
syn match abapLineComment	/^[*].*/
syn match abapComment	/\".*/

"syn sync ccomment abapComment

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_abap_syn_inits")
  if version < 508
    let did_abap_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink abapComment	Comment
  HiLink abapLineComment	Comment
  HiLink abapMacro	Macro
  HiLink abapInclude	Include
"  HiLink abapEvent	PreProc
  HiLink abapEvent	PreProc
  HiLink abapKeyword	abapSpecial
  HiLink abapOperator	abapStatement
  HiLink abapSpecial	Special
  HiLink abapStatement	Statement
  HiLink abapParam	String
  HiLink abapString	String
  HiLink abapType	Type
  HiLink abapNumber	Number
  HiLink abapChar	Character
  HiLink abapConstant	Constant
  HiLink abapForm	Function
  HiLink abapFunction	Function
  HiLink abapCond	Conditional
  HiLink abapRepeat	Repeat
  HiLink abapSystem	Identifier
  HiLink abapIdentifier	Identifier
  HiLink abapDebug	Debug

  delcommand HiLink
endif

let b:current_syntax = "ABAP"

" vim: ts=4
