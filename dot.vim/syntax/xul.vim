" Vim synatx file
" Language:     XUL
" Maintainer:   Nikolai Nespor <nikolai.nespor@utanet.at>
" URL:          http://www.unet.univie.ac.at/~a9600989/vim/xul.vim
" Last Change:  2005 02 22
"
" Remarks:      Adds XUL-Highlighting (based on docbk.vim)
"               If you want XUL-Attribute-Highlighting put a line
"               like this:
"               
"               hl_xul_atts = 1
"
"               in your $HOME/.vimrc. Only common attributes are
"               highlighted
"
"

" Backward compatibility
"
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" set filetype to xml (xml highlighting for free ;-)
" 
doau FileType xml

" don't use standard HiLink, it will not work with included syntax files
if version < 508
  command! -nargs=+ XulHiLink hi link <args>
else
  command! -nargs=+ XulHiLink hi def link <args>
endif

" syntax-matching is case-sensitive
"
syn case match

" add XUL-Keywords to XML-Tags
" 
syn cluster xmlTagHook add=xulKW

" if attribute highlighting is enabled
"
if exists("g:hl_xul_atts")
  syn cluster xmlAttribHook add=xulAT
endif

" XUL-Keywords
" 
"
syn keyword xulKW action arrowscrollbox bbox binding contained
syn keyword xulKW bindings box broadcaster broadcasterset contained
syn keyword xulKW browser button caption checkbox contained
syn keyword xulKW colorpicker column columns command contained
syn keyword xulKW commandset conditions content deck description contained
syn keyword xulKW dialog dialogheader editor grid grippy groupbox contained
syn keyword xulKW hbox iframe image key keyset label contained
syn keyword xulKW listbox listcell listcol listcols listhead contained
syn keyword xulKW listheader listitem member menu menubar contained
syn keyword xulKW menuitem menulist menupopup menuseparator observes contained
syn keyword xulKW overlay page popup popupset progressmeter radio contained
syn keyword xulKW radiogroup resizer row rows rule contained
syn keyword xulKW script scrollbar scrollbox contained
syn keyword xulKW separator spacer splitter stack contained
syn keyword xulKW statusbar statusbarpanel stringbundle contained
syn keyword xulKW stringbundleset tab tabbrowser tabbox tabpanel contained
syn keyword xulKW tabpanels tabs template textnode textbox contained
syn keyword xulKW titlebar toolbar toolbarbutton toolbargrippy contained
syn keyword xulKW toolbaritem toolbarpalette toolbarseparator contained
syn keyword xulKW toolbarset toolbarspacer toolbarspring toolbox contained
syn keyword xulKW tooltip tree treecell treechildren treecol treecols contained
syn keyword xulKW treeitem treerow treeseparator triple vbox window contained
syn keyword xulKW wizard wizardpage contained

" XUL-Attributes
"
syn keyword xulAT align allowevents allownegativassertions contained
syn keyword xulAT class coalesceduplicatearcs collapsed container contained
syn keyword xulAT containment context contextmenu datasources debug contained
syn keyword xulAT dir empty equalsize flags flex height hidden id contained
syn keyword xulAT insertafter insertbefore left maxheight contained
syn keyword xulAT maxwidth menu minheight minwidth mousethrough contained
syn keyword xulAT observes ordinal orient pack persist popup position contained
syn keyword xulAT ref removeelement sortDirection sortResource contained
syn keyword xulAT sortResource2 statustext style template tooltip contained
syn keyword xulAT tooltiptext top uri wait-cursor width contained

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_xul_syn_inits")
  if version < 508
    let did_xul_syn_inits = 1
    command -nargs=+ XulHiLink hi link <args>
  else
    command! -nargs=+ XulHiLink hi def link <args>
  endif
  
  XulHiLink xulKW Statement
  XulHiLink xulAT PreProc
  
  delcommand XulHiLink
endif

" finally set syntax
"
let b:current_syntax = "xul"

" vim: ts=8
