" Vim syntax file
" Language:	SWIG
" Maintainer:	Mr.Kissinger <mrkissinger@cmail.cn>
" Last Change:	Mar 20th, 2005

syn match 	directive		"^\s*%\(\w\+\|{\|}\)"
syn match 	macro			"^\s*#\(define\|include\)"
syn match 	cKeyword		"\(int\|char\|void\|unsigned\|struct\)"
syn region	comment			start="//" skip="\\$" end="$"

hi directive	gui=bold guifg='Red'
" hi macro		gui=bold guifg='Magenta'
hi cKeyword		gui=bold guifg='DarkGreen'
hi comment		gui=bold guifg='Blue'

let b:current_syntax = "c"
