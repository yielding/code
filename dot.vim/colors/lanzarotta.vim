" Vim color file
"  Maintainer: Jeff Lanzarotta <jefflanzarotta at yahoo dot com>
" Last Change: 19 March 2003
" grey on black

set background=dark

" Remove all existing highlighting.
hi clear

" Reset syntax hightlighting to the defaults if needed.
if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "lanzarotta"

" GUI
"highlight Comment       gui=none      guifg=#80a0ff
highlight Comment       gui=none      guifg=#4682b4
"highlight Comment       gui=none      guifg=#507080
"highlight Constant     gui=none       guifg=#ff9900       guibg=Black
"highlight Cursor        gui=none      guifg=Black         guibg=Green
highlight Cursor        gui=none      guifg=Black         guibg=Orange
highlight IncSearch     gui=none      guifg=Black         guibg=Yellow
highlight NonText       gui=none      guifg=DarkGrey      guibg=Black
highlight Normal        gui=none      guifg=LightGrey     guibg=Black
highlight Number        gui=none      guifg=LightMagenta  guibg=Black
"highlight Preproc       gui=none      guifg=#ff5577       guibg=Black
highlight Search        gui=none      guifg=Black         guibg=Yellow
highlight Special                     guifg=Orange
highlight Statement     gui=none      guifg=Yellow
"highlight StatusLine    gui=none      guifg=White         guibg=Blue
highlight StatusLine    gui=none      guifg=White         guibg=OrangeRed3
highlight StatusLineNC  gui=none      guifg=Grey          guibg=#333333
highlight Todo          gui=NONE      guifg=Blue          guibg=Yellow
highlight Type          gui=none
highlight Visual        gui=none      guifg=Black         guibg=Gray

" Console
highlight Normal                      ctermfg=LightGrey   ctermbg=Black
highlight Search        cterm=none    ctermfg=Black       ctermbg=Red
highlight Visual        cterm=reverse
highlight Cursor        cterm=bold    ctermfg=Black       ctermbg=Green
highlight Special                     ctermfg=Brown
highlight Comment                     ctermfg=Blue
highlight StatusLine                  ctermfg=blue        ctermbg=white
highlight Statement     cterm=none    ctermfg=Yellow
highlight Type          cterm=none
