"--------------------------------------------------------------------------------
"
" General
"
"--------------------------------------------------------------------------------
set lsp=4

set ru

set autoread

set sr

set nu

set nocp

set noeol

set laststatus=2

set report=0

set wildignore=*.obj,*.o,*.bak

set incsearch

set nowrap

set commentstring=//%s

set nosm "show match

set nobackup

set updatetime=300

set shortmess=a
set shortmess+=c

set clipboard ^=unnamed,unnamedplus

set signcolumn=no

"--------------------------------------------------------------------------------
"
" Text, Tab and Indent
"
"--------------------------------------------------------------------------------
set ai

set aw

set si

set ve=block

set et

set ts=2 sts=2 sw=2 tw=0

set smarttab

set cindent

set cino=t0,g0


let _vimrc="~/.config/nvim/init.vim"
let _zshrc="~/.zshrc"
let _test="~/tmp/"
let _snippet="~/.config/nvim/plugged/vim-snippets/UltiSnips/"

set path=./usr/include,/usr/local/include,/usr/include/c++/11,~/develop/include,~/opensource/mruby/include
"--------------------------------------------------------------------------------
"
" Persistent undo
"
"--------------------------------------------------------------------------------
" Let's save undo info!
if !isdirectory($HOME."/.vim")
    call mkdir($HOME."/.vim", "", 0770)
endif
if !isdirectory($HOME."/.vim/undo-dir")
    call mkdir($HOME."/.vim/undo-dir", "", 0700)
endif
set undodir=~/.vim/undo-dir
set undofile

let g:python_host_prog="/home/yielding/anaconda3/envs/pytorch/bin/python"

"--------------------------------------------------------------------------------
"
" Plugins
"
"--------------------------------------------------------------------------------
call plug#begin('~/.config/nvim/plugged')

Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'dbakker/vim-projectroot'
Plug 'itchyny/lightline.vim'
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-fugitive'

Plug 'junegunn/fzf', {'dir': '~/.fzf', 'do': './install -all' }
Plug 'junegunn/fzf.vim'
Plug 'vim-scripts/L9'
Plug 'scrooloose/nerdtree'
Plug 'vim-scripts/ack.vim'

Plug 'vim-scripts/FuzzyFinder'
Plug 'vim-scripts/ScrollColors'
Plug 'vim-scripts/ShowMarks7'
Plug 'vim-scripts/TagBar'
Plug 'vim-scripts/a.vim'

Plug 'vim-scripts/closetag.vim'
Plug 'fholgado/minibufexpl.vim'
Plug 'vim-scripts/matchit.zip'

Plug 'morhetz/gruvbox'
Plug 'git://git.wincent.com/command-t.git'
Plug 'rstacruz/sparkup', {'rtp': 'vim/'}
Plug 'bling/vim-airline'
Plug 'tpope/vim-abolish'
Plug 'Lokaltog/vim-easymotion'
Plug 'ronakg/quickr-cscope.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

"Plug 'relastle/vim-nayvy'

Plug 'vim-scripts/wordlist.vim'
Plug 'gmarik/ingretu'
Plug 'vim-scripts/gitv'

call plug#end()

filetype plugin indent on

let g:lightline = { 'colorscheme' : 'wombat' }

"--------------------------------------------------------------------------------
"
" Showmakrs
"
"--------------------------------------------------------------------------------
let showmarks_enable = 0
let showmarks_include = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let showmarks_ignore_type = "hqm"
let showmarks_hlline_lower = 1
let showmarks_hlline_upper = 1

"--------------------------------------------------------------------------------
"
" Encodings
"
"--------------------------------------------------------------------------------
set fencs=utf-8,ucs-bom,euc-kr,cp949

set encoding=utf-8 nobomb
set ffs=unix

"--------------------------------------------------------------------------------
"
" Shortcuts
"
"--------------------------------------------------------------------------------
map ,f  [I
map ,l  :set list!<CR>
map ,n  :set nu!<CR>
map ,u  :source <C-R>=_vimrc<CR><CR>
map ,v  :edit   <C-R>=_vimrc<CR><CR>
map ,w  :w <CR>
map ,q  :edit   :q <CR>
map ,z  :edit   <C-R>=_zshrc<CR><CR>

map ,ec :edit   <C-R>= _snippet . 'cpp.snippets'<CR><CR>
map ,ep :edit   <C-R>= _snippet . 'python.snippets'<CR><CR>
map ,er :edit   <C-R>= _snippet . 'ruby.snippets'<CR><CR>
map ,tc :edit   <C-R>= _test . 'test.cpp'<CR><CR>
map ,tr :edit   <C-R>= _test . 'test.rb'<CR><CR>
map ,tp :edit   <C-R>= _test . 'test.py'<CR><CR>

imap  _*        <Esc>bi*<Esc>ea*<Space>

map <F2>  :set makeprg=g++-11\ -std=c++2a\ %\ -o\ %<<CR>
map <F3>  :set makeprg=rake<CR>
map <F4>  :set makeprg=make<CR>
map <F6>  :make<CR>

map <F7>  :!./%<<CR>
map <F9>  :TagbarToggle<CR>
map <F10> :FufFile<CR>
map <F11> :FufBuffer<CR>

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

"--------------------------------------------------------------------------------
"
" outline toggle
"
"--------------------------------------------------------------------------------
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
    silent! exec "%s/{<</{<</" 
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

if has("gui_running")
  color Tomorrow-Night
else
  color jellybeans
endif


"--------------------------------------------------------------------------------
"
" Ruby
"
"--------------------------------------------------------------------------------
function! Ruby_eval_split() range
  let src = tempname()
  let dst = "Ruby Output"
  silent execute ": " . a:firstline . "," . a:lastline . "w " . src
  silent execute ":below pedit! " . dst
  wincmd P
  setlocal buftype=nofile
  setlocal noswapfile
  setlocal syntax=none
  setlocal bufhidden=delete
  silent execute ":%! ruby " . src . " 2>&1 "
  wincmd p
endfunction

au FileType ruby filetype plugin indent on
au FileType ruby noremap ,r :%call Ruby_eval_split()<CR>
au FileType ruby noremap ,tag :!ripper-tags -R.<CR>

au FileType python noremap ,r :!python %<CR>

"--------------------------------------------------------------------------------
"
" switch highlight search
"
"--------------------------------------------------------------------------------
set nohlsearch
let flagHlsearch = 0
function! SwitchHlSearch(flag)
  if a:flag == 0
    set hlsearch
    echo "@ Switch HlSearch : ON"
    return 1
  elseif a:flag == 1
    set nohlsearch
    echo "@ Switch HlSearch : OFF"
    return 0
  else
    set nohlsearch
    echo "@ Syntax Error : SwitchHlSearch(). set nohlsearch."
    return 0
  endif
endfunction
map ,s :let flagHlsearch = SwitchHlSearch(flagHlsearch)<CR>

"--------------------------------------------------------------------------------
"
" switch syntax search
"
"--------------------------------------------------------------------------------
let flagSyntax = 1
function! SwitchSyntax(flag)
  if a:flag == 0
    syntax on
    echo "@ Switch Syntax : ON"
    return 1
  elseif a:flag == 1
    syntax off
    echo "@ Switch Syntax : OFF"
    return 0
  else
    syntax off
    echo "@ Syntax Error : SwitchSyntax(). syntax off"
    return 0
  endif
endfunction

map ,S :let flagSyntax = SwitchSyntax(flagSyntax)<CR>

"--------------------------------------------------------------------------------
"
" mini buffer explorer
"
"--------------------------------------------------------------------------------
noremap <C-J> <C-W>j
noremap <C-K> <C-W>k
noremap <C-H> <C-W>h
noremap <C-L> <C-W>l

"--------------------------------------------------------------------------------
"
" coc
"
"--------------------------------------------------------------------------------
"set signcolumn=no
"hi coc_err_hi ctermfg=1 ctermbg=15
"sign define coc_err numhl=coc_err_hi
"sign place 1 line=2 name=coc_err

"set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}
"
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

nmap <leader>rn <Plug>(coc-rename)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

"-------------------------------------------------------------------------------
"
" ruby
"
"--------------------------------------------------------------------------------
au FileType ruby filetype plugin indent on

"--------------------------------------------------------------------------------
"
" 괄호 자동 매칭
"
"--------------------------------------------------------------------------------
let loaded_matchparan=0

"--------------------------------------------------------------------------------
"
" run
"
"--------------------------------------------------------------------------------
au BufEnter * :syntax sync fromstart
au BufNewFile,BufReadPost *.rb set ts=2 sw=2 foldmethod=expr
au BufNewFile,BufReadPost *.py compiler pyunit

