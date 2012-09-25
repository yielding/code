"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" 
"
" Last modified : 2012년 6월 27일 수요일 15시 44분 37초 KST by yielding
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"-----------------------------------------------------------------------------
"
" General
"
"-----------------------------------------------------------------------------
set lsp=2

set ru

set autoread

set sr              " shift round

set nu

set nocp

set laststatus=2
set statusline=%<%F%h%m%r%h%w%y\ %=\ ascii:%b,0x%-8B\ col:%c%V\ line:%l\,%L\ %P\ %{VisualSelectionSize()}
set shortmess=a     " avoid to hit continue

set report=0

set wildignore=*.obj,*.bak,*.exe,*.ncb,*.sln,*.aps,*.vcl

set incsearch

set nowrap

set commentstring=//%s

"-----------------------------------------------------------------------------
"
" determine OS
"
"-----------------------------------------------------------------------------
func! MySys()
  let $OS = "mac"
  return $OS
endfun

"-----------------------------------------------------------------------------
"
" Text, Tab and Indent
"
"-----------------------------------------------------------------------------
set ai              " auto indent

set aw              " save file when compiling

set si              " smart indent

set ve=block        " virtual edit

set et              " expand tab

set ts=4 sts=4 sw=4 tw=0
set ts=2 sts=2 sw=2 tw=0

set smarttab

set cindent

set cino=t0,g0

"-----------------------------------------------------------------------------
"
" for persistent undo
"
"-----------------------------------------------------------------------------
try
  if MySys() == "Windows_NT"
    set undodir="c:\Windows\Temp"
  else
    set undodir=~/vim_undo
  endif

  set undofile
  set undolevels=1000
  set undoreload=10000
catch
endtry

"-----------------------------------------------------------------------------
"
" RUMTIME manipulation
"
"-----------------------------------------------------------------------------
"call pathogen#infect()

"-----------------------------------------------------------------------------
"
" Bundle management
"
"-----------------------------------------------------------------------------
set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" My Bundles here:
"
"Bundle 'Syntastic'
Bundle 'Conque-Shell'
Bundle 'comments.vim'
Bundle 'EnhCommentify.vim'
Bundle 'FuzzyFinder'
Bundle 'L9'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'ScrollColors'
Bundle 'ShowMarks7'
Bundle 'Tagbar'
Bundle 'The-NERD-tree'
Bundle 'a.vim'
Bundle 'calendar.vim'
Bundle 'closetag.vim'
Bundle 'increment.vim--Avadhanula'
Bundle 'matchit.zip'
Bundle 'minibufexpl.vim'
Bundle 'rails.vim'
Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
Bundle 'snipMate'

Bundle 'wordlist.vim'
Bundle 'coffee.vim'
Bundle 'vim-coffee-script'
Bundle 'fugitive.vim'
Bundle 'gitv'

filetype plugin indent on     " required! 
"-----------------------------------------------------------------------------
"
" Show Marks
"
"-----------------------------------------------------------------------------
" Enable ShowMarks
let showmarks_enable = 1
" Show which marks
let showmarks_include = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
" Ignore help, quickfix, non-modifiable buffers
let showmarks_ignore_type = "hqm"
" Hilight lower & upper marks
"let showmarks_hlline_lower = 1
"let showmarks_hlline_upper = 1 

"-----------------------------------------------------------------------------
"
" Encoding, file saving
"
"-----------------------------------------------------------------------------
set fencs=utf-8,ucs-bom,euc-kr

if MySys() == "mac"
  set encoding=utf-8 nobomb
  set ffs=unix,dos
else
  set ffs=dos,unix
endif

"-----------------------------------------------------------------------------
"
" VIM GUI
"
"-----------------------------------------------------------------------------
set hid             " change buffer without save
set magic           " set magic on, for regular expressions
set showmatch       " 
set nolazyredraw    " do not redraw while executing macrows
set vb              " visual bell
set wmh=0

if MySys() == "Windows_NT"
  set lines=50
  set columns=90
endif

"-----------------------------------------------------------------------------
"
" Common Dirs for OSX and Windows
"
"-----------------------------------------------------------------------------
let _test  = '~/test/'

if MySys() == "Windows_NT"
  let _vimrc  = '$VIM/_vimrc'
  let _gvimrc = '$VIM/_gvimrc'
  let _ctags  = "c:/windows/system32/ctags "
  map <C-Z> :!start cmd<CR>
else
  let _vimrc  = '~/.vimrc'
  let _gvimrc = '~/.gvimrc'
  let _ctags  = "ctags "
  map <C-Z> :ConqueTermSplit bash<CR>
endif

"-----------------------------------------------------------------------------
"
" for MacVim
"
"-----------------------------------------------------------------------------
if MySys() == "mac"
  set noimd
  set imi=1
  set ims=-1
endif

"-----------------------------------------------------------------------------
"
" using dictionary completion with CRTR-N
"
"-----------------------------------------------------------------------------
set dictionary=

set tags=~/.vim/tags/boost,~/.vim/tags/stl
set tags+=tags;$HOME

set path=.,/usr/include/c++/4.2.1,/opt/local/include,/usr/include
set path+=~/develop/include,~/opensource/mruby/include

set popt=syntax:y,number:y

if MySys() == "mac"
  "set guifont=Menlo\ Regular:h12
  set guifont=Andale\ Mono:h15
  set shell=/opt/local/bin/bash
elseif MySys() == "Windows_NT"
  set guifont=Fixedsys:h12
endif

function! EnhanceCppSyntax() 
  syn match cppFuncDef "::\~\?\zs\h\w*\ze([^)]*\()\s*\(const\)\?\)\?$" 
  hi def link cppFuncDef Special 
endfunction 

autocmd Syntax cpp call EnhanceCppSyntax() 

"------------------- misc
let _project  ='~/develop/app/panther2/p2.project'
let _wordlist ='~/.vim/vimfiles/plugin/wordlist.vim'
map   ,a    :edit <C-R>=_wordlist <CR><CR>    " wordlist 편집
map   ,f    [I
map   ,i    :edit    <C-R>=_viminfo<CR><CR>
map   ,p    :Project <C-R>=_project<CR><CR>
map   ,v    :edit    <C-R>=_vimrc<CR><CR>
map   ,l    :set list!<CR>
map   ,n    :set nu!<CR>

map   ,tag  :!<C-R>=_ctags . "-R --c++-kinds=+p --fields=+iaS --extra=+q ."<CR><CR>
map   ,test :edit <C-R>= _test . 'test.cpp'<CR><CR>
map   ,todo :edit ~/code/work.diary/07.otl<CR>
map   ,w    :w <CR>
map   _W    :cwindow <CR>
map   _L    1G/Last modified : */e+1<CR>D:r!date<CR>kJA by yielding<ESC>
map   _C    1G:0r!date +\%Y-\%m-\%d<CR>A yielding@gamil.com<ESC>
map   ,q    :q <CR>

imap  <C-BS>  <ESC>bvec
imap  _*      <Esc>bi*<Esc>ea*<Space>
map   -       "yyy:@y<cr>
"nmap  ;       :%s/\<<c-r>=expand("<cword>")<cr>\>
map   ;s   :up \| saveas! %:p:r-<C-R>=strftime("%y%m%d")<CR>-bak.txt \| 3sleep \| e #<CR> 

map <F2>    :set makeprg=g++-mp-4.7\ -std=c++11\ %\ -o\ %<<CR>
map <F3>    :set makeprg=rake<CR>
map <F4>    :set makeprg=xcodebuild\ -sdk\ iphonesimulator4.3<CR>
map <F6>    :make<CR>
map <F7>    :!./%<<CR>
map <F8>    :!mono ./%.exe <CR>
map <F9>    :TagbarToggle<CR>
map <F10>   :FufFile<CR>
map <F11>   :FufBuffer<CR>

" Switch on syntax highlighting if it wasn't on yet.
if !exists("syntax_on")
  syntax on
endif

"-----------------------------------------------------------------------------
"
" grep source
"
"-----------------------------------------------------------------------------
nmap ,gc :vimgrep <cword> *.c* *.h *.m *.mm<CR> :copen <CR>
nmap ,gy :vimgrep <cword> *.py <CR> :copen <CR>
nmap ,gr :vimgrep <cword> *.py <CR> :copen <CR>

set grepformat=%f:%l:%m "%f: filename %l: line %m: messgae

"-----------------------------------------------------------------------------
"
" switch highlight search 
"
"-----------------------------------------------------------------------------
set nohlsearch
let flagHlSearch = 0
function! SwitchHlSearch(flag)
  if a:flag == 0
    set hlsearch
    echo "@ Switch HlSearch : ON"
    return 1
  elseif a:flag  == 1
    set nohlsearch
    echo "@ Switch HlSearch : OFF"
    return 0
  else
    set nohlsearch
    echo "@ Syntax Error : SwitchHlSearch(). set nohlsearch."
    return 0
  endif
endfunction
map ,s  :let flagHlSearch = SwitchHlSearch(flagHlSearch)<CR>

"-----------------------------------------------------------------------------
"
" switch syntax
"
"-----------------------------------------------------------------------------
let flagSyntax = 1
function! SwitchSyntax(flag)
  if a:flag == 0
    syntax on
    echo "@ Switch Syntax : ON"
    return 1
  elseif a:flag  == 1
    syntax off
    echo "@ Switch Syntax : OFF"
    return 0
  else
    syntax off
    echo "@ Syntax Error : SwitchSyntax(). syntax off."
    return 0
  endif
endfunction
map ,S :let flagSyntax = SwitchSyntax(flagSyntax)<CR>

set guioptions-=T
if MySys() == "Windows_NT"
  set guioptions-=m
endif

if MySys() == "mac"
  map _M : if &guioptions=~'T' \| set guioptions-=T \| else \| set guioptions+=T \| endif<cr> 
else
  map _M : if &guioptions=~'m' \| set guioptions-=m \| else \| set guioptions+=m \| endif<cr> 
endif

set guioptions-=r  " right scroll bar

"-----------------------------------------------------------------------------
"
" toggle folder
"
"-----------------------------------------------------------------------------
function! ToggleFold()
  if foldlevel('.') == 0
    normal! l
  else
    if foldclosed('.') < 0
      . foldclose
    else
      . foldopen
    endif "
  endif "
  echo
endfun
noremap <space> :call ToggleFold()<CR>

"-----------------------------------------------------------------------------
"
" update .vimrc
"
"-----------------------------------------------------------------------------
map ,u :source <C-R>=_vimrc<CR><CR>

"-----------------------------------------------------------------------------
"
" auto suggest
"
"-----------------------------------------------------------------------------
"function! InsertTabWrapper()
"  let col = col('.') - 1
"  if !col || getline('.')[col-1]!~'\k'
"    return "\<tab>"
"  else
"    if pumvisible()
"    return "\<c-n>"
"  else
"    return "\<c-n>\<c-p>"
"  end
"  endif
"endfunction
"
"inoremap <tab> <c-r>=InsertTabWrapper()<cr>

"hi Pmenu     ctermbg=blue
"hi PmenuSel  ctermbg=yellow ctermfg=black
"hi PmenuSbar ctermbg=blue

"-----------------------------------------------------------------------------
"
" use dictionary
"
"-----------------------------------------------------------------------------
"function! Edic_yahoo()
"  let i = expand("<cword>")
"  new
"  exe "%r! ~/.vim/edic.py ".i
"  set nomod wrap
"  noremap <buffer> q :bd<cr>: echo "" <cr>
"endfunction
"
"map ,ed :call Edic_yahoo() <cr>gg
"
"-----------------------------------------------------------------------------
"
" always current directory
"
"-----------------------------------------------------------------------------
map ,h :cd %:p:h:gs/ /\\ /<CR>

"-----------------------------------------------------------------------------
"
" Syntastic
"
"-----------------------------------------------------------------------------
let b:syntastic_cpp_cflags = ' -I/opt/local/include '
let g:syntastic_cpp_compiler_options = ' -std=c++11'
"let g:syntastic_cpp_auto_refresh_includes = 1

"-----------------------------------------------------------------------------
"
" outline toggle
"
"-----------------------------------------------------------------------------
function! <SID>OutlineToggle() 
  let OldLine = line(".") 
  let OldCol = virtcol(".") 
  if (! exists ("b:outline_mode")) 
    let b:outline_mode = 0 
    let b:OldMarker = &foldmarker 
  endif 

  if (b:outline_mode == 0) 
    let b:outline_mode = 1 
    set foldmethod=marker 
    set foldmarker={,} 
    silent! exec "%s/{<</{<</" 
    silent! exec "%s/{<</}>>/" 
    set foldcolumn=2
  else 
    let b:outline_mode = 0 
    set foldmethod=marker 
    let &foldmarker=b:OldMarker 
    silent! exec "%s/{<</{<</" 
    silent! exec "%s/{<</{<</" 
    set foldcolumn=0 
  endif 

  execute "normal! ".OldLine."G" 
  execute "normal! ".OldCol."|" 
  unlet OldLine 
  unlet OldCol 
  execute "normal! zv" 
endfunction 

command! -nargs=0 OUTLINE call <SID>OutlineToggle() 

"-----------------------------------------------------------------------------
"
" colors 
"
"-----------------------------------------------------------------------------
"color BusyBee
"color carvedwoodcool
"color oceanblack
"color ChocolateLiquor
"color colorful
"color fu
"color ekvoli
"color kellys
"color jellybeans
"color dusk
"color DevC++
if (has('gui_running')) 
  "color peaksea
  "color molokai
  "color jellybeans
  color Twilight2
else
  color jellybeans
endif

"-----------------------------------------------------------------------------
"
" snipMate and complte
"
"-----------------------------------------------------------------------------
set rtp+=~/.vim/snippets

"-----------------------------------------------------------------------------
"
" buffer explorer
"
"-----------------------------------------------------------------------------
let g:miniBufExplMapWindowNavVim    = 1 
let g:miniBufExplMapWindowNavArrows = 1 
let g:miniBufExplMapCTabSwitchBufs  = 1 
let g:miniBufExplModSelTarget       = 1 
let g:bufExplorerDefaultHelp        = 0
let g:bufExplorerShowRelativePath   = 1

let g:bufExplorerSortBy = "name"

autocmd BufRead, BufNew :call UMiniBufExplorer

"-----------------------------------------------------------------------------
"
" ruby
"
"-----------------------------------------------------------------------------
let g:ruby_path = "/opt/local/bin"
au FileType ruby filetype plugin indent on
au FileType ruby set omnifunc=rubycomplete#Complete
au FileType ruby let g:rubycomplete_buffer_loading = 1
au FileType ruby let g:rubycomplete_rails = 1
au FileType ruby let g:rubycomplete_classes_in_global = 1

au FileType ruby noremap ,r :!ruby19 %<CR>
au FileType python noremap ,r :!python %<CR>
au FileType cs noremap ,b :!dmcs %<CR>
au FileType cs noremap ,r :!mono %:r.exe<CR>

"-----------------------------------------------------------------------------
"
" complte, clang complete
"
"-----------------------------------------------------------------------------
set complete-=k complete+=k
set completeopt=menu,menuone
set pumheight=15

let g:SuperTabDefaultCompletionType = "context"

let g:clang_auto_select = 1
let g:clang_complete_auto = 1
let g:clang_complete_copen = 1
let g:clang_user_options ='-fblocks -std=c++11 -isysroot /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.8.sdk'
"-----------------------------------------------------------------------------
"
" 자동 괄호 매칭 off
"
"-----------------------------------------------------------------------------
let loaded_matchparen = 0

"if (has('gui_running')) 
  "set transparency=9
"endif

"-----------------------------------------------------------------------------
"
" syntax match
"
"-----------------------------------------------------------------------------
let NERDTreeIgnore=['\.vim$', '\~$', '.*\.o$', '.*\.pyc$']

"-----------------------------------------------------------------------------
"
" 자주 쓰는 autocmd 
"
"-----------------------------------------------------------------------------
au BufReadCmd file:///* exe "bd!|edit ".substitute(expand("<afile>"),"file:/*","","")

au BufEnter * :syntax sync fromstart
au BufNewFile,BufReadPost *.py         compiler pyunit
au BufNewFile,BufReadPost *.java       compiler javac 
au BufNewFile,BufReadPost *.rb         set ts=2 sw=2
au BufNewFile,BufReadPost *.erb        set ft=eruby 
au BufNewFile,BufReadPost *.md         set ft=markdown 
au BufNewFile,BufReadPost *.m          set ft=objc 
au BufNewFile,BufReadPost *.io         set ft=io 
au BufNewFile,BufReadPost *.hx         set ft=haxe 
au BufNewFile,BufReadPost *.as         set ft=actionscript 
au BufNewFile,BufReadPost *.scala      set ft=scala 
au BufNewFile,BufReadPost *.cpp        set ft=cpp
