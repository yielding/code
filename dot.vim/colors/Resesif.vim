" Vim color file
" Converted from Textmate theme Resesif using Coloration v0.2.5 (http://github.com/sickill/coloration)

set background=dark
highlight clear

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "Resesif"

hi Cursor  guifg=NONE guibg=#ffffff gui=NONE
hi Visual  guifg=NONE guibg=#545d73 gui=NONE
hi CursorLine  guifg=NONE guibg=#333435 gui=NONE
hi CursorColumn  guifg=NONE guibg=#333435 gui=NONE
hi LineNr  guifg=#898684 guibg=#2b2b2b gui=NONE
hi VertSplit  guifg=#4f4e4d guibg=#4f4e4d gui=NONE
hi MatchParen  guifg=#b6c34d guibg=NONE gui=NONE
hi StatusLine  guifg=#e6e1dc guibg=#4f4e4d gui=bold
hi StatusLineNC  guifg=#e6e1dc guibg=#4f4e4d gui=NONE
hi Pmenu  guifg=#e16c5b guibg=#3a2e2d gui=NONE
hi PmenuSel  guifg=NONE guibg=#545d73 gui=NONE
hi IncSearch  guifg=NONE guibg=#595d36 gui=NONE
hi Search  guifg=NONE guibg=#595d36 gui=NONE
hi Directory  guifg=#c29b7a guibg=#393431 gui=NONE
hi Folded  guifg=#8b8b8b guibg=#2b2b2b gui=NONE

hi Normal  guifg=#e6e1dc guibg=#2b2b2b gui=NONE
hi Boolean  guifg=#71bacc guibg=NONE gui=NONE
hi Character  guifg=#c29b7a guibg=#393431 gui=NONE
hi Comment  guifg=#8b8b8b guibg=NONE gui=NONE
hi Conditional  guifg=#b6c34d guibg=NONE gui=NONE
hi Constant  guifg=#c29b7a guibg=#393431 gui=NONE
hi Define  guifg=#b6c34d guibg=NONE gui=NONE
hi ErrorMsg  guifg=#ffffff guibg=#990000 gui=NONE
hi WarningMsg  guifg=#ffffff guibg=#990000 gui=NONE
hi Float  guifg=#b49cda guibg=#353338 gui=NONE
hi Function  guifg=#e16c5b guibg=#3a2e2d gui=NONE
hi Identifier  guifg=#e8bf6a guibg=NONE gui=NONE
hi Keyword  guifg=#b6c34d guibg=NONE gui=NONE
hi Label  guifg=#b0cc66 guibg=#31322d gui=NONE
hi NonText  guifg=#404040 guibg=#333435 gui=NONE
hi Number  guifg=#b49cda guibg=#353338 gui=NONE
hi Operator  guifg=#b6c34d guibg=NONE gui=NONE
hi PreProc  guifg=#b6c34d guibg=NONE gui=NONE
hi Special  guifg=#e6e1dc guibg=NONE gui=NONE
hi SpecialKey  guifg=#404040 guibg=#333435 gui=NONE
hi Statement  guifg=#b6c34d guibg=NONE gui=NONE
hi StorageClass  guifg=#e8bf6a guibg=NONE gui=NONE
hi String  guifg=#b0cc66 guibg=#31322d gui=NONE
hi Tag  guifg=#e16c5b guibg=#3a2e2d gui=NONE
hi Title  guifg=#e6e1dc guibg=NONE gui=bold
hi Todo  guifg=#8b8b8b guibg=NONE gui=inverse,bold
hi Type  guifg=#e16c5b guibg=#3a2e2d gui=NONE
hi Underlined  guifg=NONE guibg=NONE gui=underline
hi rubyClass  guifg=#b6c34d guibg=NONE gui=NONE
hi rubyFunction  guifg=#e16c5b guibg=#3a2e2d gui=NONE
hi rubyInterpolationDelimiter  guifg=NONE guibg=NONE gui=NONE
hi rubySymbol  guifg=#c29b7a guibg=#393431 gui=NONE
hi rubyConstant  guifg=NONE guibg=NONE gui=NONE
hi rubyStringDelimiter  guifg=#b0cc66 guibg=#31322d gui=NONE
hi rubyBlockParameter  guifg=NONE guibg=NONE gui=NONE
hi rubyInstanceVariable  guifg=#dbd76b guibg=NONE gui=NONE
hi rubyInclude  guifg=#b6c34d guibg=NONE gui=NONE
hi rubyGlobalVariable  guifg=#4da6bd guibg=#2b3031 gui=NONE
hi rubyRegexp  guifg=#b0cc66 guibg=#31322d gui=NONE
hi rubyRegexpDelimiter  guifg=#b0cc66 guibg=#31322d gui=NONE
hi rubyEscape  guifg=#3fc172 guibg=NONE gui=NONE
hi rubyControl  guifg=#b6c34d guibg=NONE gui=NONE
hi rubyClassVariable  guifg=NONE guibg=NONE gui=NONE
hi rubyOperator  guifg=#b6c34d guibg=NONE gui=NONE
hi rubyException  guifg=#b6c34d guibg=NONE gui=NONE
hi rubyPseudoVariable  guifg=#dbd76b guibg=NONE gui=NONE
hi rubyRailsUserClass  guifg=NONE guibg=NONE gui=NONE
hi rubyRailsARAssociationMethod  guifg=#d98339 guibg=NONE gui=NONE
hi rubyRailsARMethod  guifg=#d98339 guibg=NONE gui=NONE
hi rubyRailsRenderMethod  guifg=#d98339 guibg=NONE gui=NONE
hi rubyRailsMethod  guifg=#d98339 guibg=NONE gui=NONE
hi erubyDelimiter  guifg=NONE guibg=NONE gui=NONE
hi erubyComment  guifg=#8b8b8b guibg=NONE gui=NONE
hi erubyRailsMethod  guifg=#d98339 guibg=NONE gui=NONE
hi htmlTag  guifg=NONE guibg=NONE gui=NONE
hi htmlEndTag  guifg=NONE guibg=NONE gui=NONE
hi htmlTagName  guifg=NONE guibg=NONE gui=NONE
hi htmlArg  guifg=NONE guibg=NONE gui=NONE
hi htmlSpecialChar  guifg=#c29b7a guibg=#393431 gui=NONE
hi javaScriptFunction  guifg=#e8bf6a guibg=NONE gui=NONE
hi javaScriptRailsFunction  guifg=#d98339 guibg=NONE gui=NONE
hi javaScriptBraces  guifg=NONE guibg=NONE gui=NONE
hi yamlKey  guifg=#e16c5b guibg=#3a2e2d gui=NONE
hi yamlAnchor  guifg=#4da6bd guibg=#2b3031 gui=NONE
hi yamlAlias  guifg=#4da6bd guibg=#2b3031 gui=NONE
hi yamlDocumentHeader  guifg=#b0cc66 guibg=#31322d gui=NONE
hi cssURL  guifg=NONE guibg=NONE gui=NONE
hi cssFunctionName  guifg=#d98339 guibg=NONE gui=NONE
hi cssColor  guifg=#c29b7a guibg=#393431 gui=NONE
hi cssPseudoClassId  guifg=NONE guibg=NONE gui=NONE
hi cssClassName  guifg=NONE guibg=NONE gui=NONE
hi cssValueLength  guifg=#b49cda guibg=#353338 gui=NONE
hi cssCommonAttr  guifg=#bbc94d guibg=NONE gui=NONE
hi cssBraces  guifg=NONE guibg=NONE gui=NONE
