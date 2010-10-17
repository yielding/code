""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Last modified : 2009년 1월 17일 토요일 02시 39분 33초 KST by yielding
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
set statusline=%<%F%h%m%r%h%w%y\ %=\ ascii:%b\ col:%c%V\ line:%l\,%L\ %P
set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{CurDir()}%h\ \ \ Line:\ %l/%L%{GitBranch()}


set shortmess=a     " avoid to hit continue

set report=0

set wildignore=*.obj,*.bak,*.exe,*.ncb,*.sln,*.aps,*.vcl

set incsearch

set nowrap

set exrc

set mousemodel=popup

set commentstring=//%s

set clipboard=unnamed

"-----------------------------------------------------------------------------
"
" determine OS
"
"-----------------------------------------------------------------------------
func! MySys()
  let $OS="mac"
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
    set undodir=~"c:\Windows\Temp"
  else
    set undodir=~/.vim/undodir
  endif

  set undofile
  set undolevels=1000
  set undoreload=10000
catch
endtry

"-----------------------------------------------------------------------------
"
" Git
"
"-----------------------------------------------------------------------------
"Git branch
function! GitBranch()
  let branch = system("git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* //'")
  if branch != ''
    return '   Git Branch: ' . substitute(branch, '\n', '', 'g')
  en
  return ''
endfunction

function! CurDir()
  return substitute(getcwd(), '/Users/yielding/', "~/", "g")
endfunction

function! HasPaste()
  if &paste
      return 'PASTE MODE  '
  en
  return ''
endfunction

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

if MySys() != "Windows_NT"
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
"set vb              " visual bell
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
let _test  = '~/tmp/'

if MySys() == "Windows_NT"
  let _vimrc  = '$VIM/.vimrc'
  let _gvimrc = '$VIM/.gvimrc'
  let _ctags   = "c:/windows/system32/ctags "
  map   <C-Z> :!start cmd<CR>
else
  let _vimrc  = '~/.vimrc'
  let _gvimrc = '~/.gvimrc'
  let _ctags  = "ctags "
  map   <C-Z> :!bash<CR>
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
" Visual Mode
"
"-----------------------------------------------------------------------------

" Really useful!
" In visual mode when you press * or # to search for the current selection
vnoremap <silent> * :call VisualSearch('f')<CR>
vnoremap <silent> # :call VisualSearch('b')<CR>

" When you press gv you vimgrep after the selected text
vnoremap <silent> gv :call VisualSearch('gv')<CR>
map <leader>g :vimgrep // **/*.<left><left><left><left><left><left><left>

functio! CmdLine(str)
  exe "menu Foo.Bar :" . a:str
  emenu Foo.Bar
  unmenu Foo
endfunction

" From an idea by Michael Naumann
function! VisualSearch(direction) range
  let l:saved_reg = @"
  execute "normal! vgvy"

  let l:pattern = escape(@", '\\/.*$^~[]')
  let l:pattern = substitute(l:pattern, "\n$", "", "")

  if a:direction == 'b'
    execute "normal ?" . l:pattern . "^M"
  elseif a:direction == 'gv'
    call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.')
  elseif a:direction == 'f'
    execute "normal /" . l:pattern . "^M"
  endif

  let @/ = l:pattern
  let @" = l:saved_reg
endfunction
"-----------------------------------------------------------------------------
"
" using dictionary completion with CRTR-N
"
"-----------------------------------------------------------------------------
set complete-=k complete+=k
set dictionary=

set tags=./tags,/Users/yielding/develop/app/panther2/tags

set path+=.,/usr/include,/usr/include/gcc/darwin/4.0/c++,/opt/local/include
set path+=/Users/yielding/develop/app/panther2/include
set path+=/Users/yielding/develop/app/panther2/common/include
set path+=/Users/yielding/develop/app/panther2/vendor/include

set popt=syntax:y,number:y

if MySys() == "Windows_NT"
  set guifont=Fixedsys:h12
elseif MySys() == "mac"
  set guifont=Menlo\ Regular:h15
  set shell=/opt/local/bin/bash
endif

"set guifont=Andale\ Mono:h11

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
map   ,todo :edit ~/code/work.diary/06.otl<CR>
map   ,w    :w <CR>
map   _W    :cwindow <CR>
map   _L    1G/Last modified : */e+1<CR>D:r!date<CR>kJA by yielding<ESC>
map   _C    1G:0r!date +\%Y-\%m-\%d<CR>A yielding@gamil.com<ESC>
map   ,q    :q <CR>

imap  <C-BS>  <ESC>bvec
imap  _*      <Esc>bi*<Esc>ea*<Space>
map   -       "yyy:@y<cr>
nmap  ;       :%s/\<<c-r>=expand("<cword>")<cr>\>
map   ;s   :up \| saveas! %:p:r-<C-R>=strftime("%y%m%d")<CR>-bak.txt \| 3sleep \| e #<CR> 

map <F9>    :make <CR><CR>

" I like highlighting strings inside C comments
let c_comment_strings=1
let myfiletypefile="~/.vim/vimfiles/filetype.vim"

filetype plugin indent on

" Switch on syntax highlighting if it wasn't on yet.
if !exists("syntax_on")
   syntax on
endif

"-----------------------------------------------------------------------------
"
" grep source
"
"-----------------------------------------------------------------------------
nmap  ,gc :vimgrep <cword> *.c* *.h *.m *.mm<CR> :copen <CR>
nmap  ,gj :vimgrep <cword> *.java<CR> :copen <CR>

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
function! InsertTabWrapper()
  let col = col('.') - 1
  if !col || getline('.')[col-1]!~'\k'
    return "\<tab>"
  else
    if pumvisible()
    return "\<c-n>"
  else
    return "\<c-n>\<c-p>"
  end
  endif
endfunction

"inoremap <tab> <c-r>=InsertTabWrapper()<cr>

hi Pmenu ctermbg=blue
hi PmenuSel ctermbg=yellow ctermfg=black
hi PmenuSbar ctermbg=blue

"-----------------------------------------------------------------------------
"
" use dictionary
"
"-----------------------------------------------------------------------------
function! Edic_yahoo()
  let i = expand("<cword>")
  new
  exe "%r! ~/.vim/edic.py ".i
  set nomod wrap
  noremap <buffer> q :bd<cr>: echo "" <cr>
endfunction

map ,ed :call Edic_yahoo() <cr>gg

"-----------------------------------------------------------------------------
"
" character count under the cursor 
"
"-----------------------------------------------------------------------------
function! CharCount()
  let searchChar = getline(".")[col(".")-1]
  let orgPos = line(".")
  let totalLine = line("$")
  let totalChar = 0
  let i = 0
  while i <= totalLine
    let pos = cursor(i, 0)
    let thisLine = getline(i)
    let lineLength = col("$")
    let k = 0
    while k < lineLength
      if thisLine[k] == searchChar
       let totalChar = totalChar + 1
      endif
      let k = k + 1
    endwhile

    let i = i + 1
  endwhile
  let pos = cursor(orgPos, 0)

  echo "\"" . searchChar . "\" AC °³¼o: " . totalChar
endfunction

map ,cc : call CharCount() <cr>

"-----------------------------------------------------------------------------
"
" always current directory
"
"-----------------------------------------------------------------------------
function! AlwaysCD()
  if bufname("") !~ "^ftp://"
    lcd %:p:h
  endif
endfunction

map ,h :cd %:p:h<CR>

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
    silent! exec "%s/}>>/}>>/" 
    set foldcolumn=2
  else 
    let b:outline_mode = 0 
    set foldmethod=marker 
    let &foldmarker=b:OldMarker 
    silent! exec "%s/{<</{<</" 
    silent! exec "%s/}>>/{<</" 
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
color rainbow_neon
color oceanblack
color inkpot
color oceandeep
color edo_sea
color ChocolateLiquor
color molokai
"color fu
color darkspectrum

"-----------------------------------------------------------------------------
"
" buffer explorer
"
"-----------------------------------------------------------------------------
let g:miniBufExplMapWindowNavVim = 1 
let g:miniBufExplMapWindowNavArrows = 1 
let g:miniBufExplMapCTabSwitchBufs = 1 
let g:miniBufExplModSelTarget = 1 
let g:bufExplorerDefaultHelp=0
let g:bufExplorerShowRelativePath=1

let g:bufExplorerSortBy = "name"

autocmd BufRead,BufNew :call UMiniBufExplorer

"-----------------------------------------------------------------------------
"
" 자동 괄호 매칭 off
"
"-----------------------------------------------------------------------------
let loaded_matchparen = 0

"-----------------------------------------------------------------------------
"
" 자주 쓰는 autocmd 
"
"-----------------------------------------------------------------------------
au BufReadCmd file:///* exe "bd!|edit ".substitute(expand("<afile>"),"file:/*","","")
au BufEnter * :syntax sync fromstart
au BufNewFile,BufReadPost *.py         compiler pyunit
au BufNewFile,BufReadPost *.ipp        set ft=cpp
au BufNewFile,BufReadPost *.pas        set ft=delphi
au BufNewFile,BufReadPost *.java       compiler javac 
au BufNewFile,BufReadPost *.rb         set ts=2 sw=2
au BufNewFile,BufReadPost *.cpp        compiler gcc
au BufNewFile,BufReadPost *.h          compiler gcc
au BufNewFile,BufReadPost *.rhtml      set ft=eruby 
au BufNewFile,BufReadPost *.io         set ft=io 
au BufNewFile,BufReadPost *.hx         set ft=haxe 
au BufNewFile,BufReadPost *.m          set ft=objc 
au BufNewFile,BufReadPost *.as         set ft=actionscript 
au BufNewFile,BufReadPost *.scala      set ft=scala 
