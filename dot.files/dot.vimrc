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

set mouse=a

set clipboard+=unnamedplus

set nofixendofline

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

set fillchars+=vert:\ 

"--------------------------------------------------------------------------------
"
" globar variables
"
"--------------------------------------------------------------------------------
let _vimrc="~/.config/nvim/init.vim"
let _plugs="~/.config/nvim/vim-plug/plugins.vim"
let _zshrc="~/.zshrc"
let _test="~/tmp/"
let _snippet="~/.config/nvim/autoload/plugged/vim-snippets/snippets/"
let _mysnippet="~/.config/nvim/UltiSnips/"

set path=~/develop/include,~/opensource/mruby/include

let mapleader = ","
"--------------------------------------------------------------------------------
"
" check macvim
"
"--------------------------------------------------------------------------------
let g:is_nvim = has('nvim')
let g:is_vim8 = v:version >= 800 ? 1 : 0

if !g:is_nvim && g:is_vim8
  set guioptions=
  set guifont=agaveNerdFontCompleteM-r:h19
  set runtimepath+=~/.config/nvim/colors
  let &packpath = &runtimepath
  autocmd! GUIEnter * set vb t_vb=
else
  set termguicolors
  set t_Co=256
  color jellybeans
endif

color Tomorrow-Night-Blue
color Tomorrow-Night-Eighties
color xoria256

"--------------------------------------------------------------------------------
"
" Terminal
"
"--------------------------------------------------------------------------------
if has('nvim')
  tnoremap <Esc> <C-\><C-n>
  tnoremap <M-[> <Esc>
  tnoremap <C-v><Esc> <Esc>
endif

"--------------------------------------------------------------------------------
"
" Persistent undo
"
"--------------------------------------------------------------------------------
let tmp_dir = $HOME."/tmp"
if !isdirectory(tmp_dir)
  call mkdir(tmp_dir, "", 0770)
endif

let tmp_dir .= g:is_nvim ? "/undo_nvim" : "/undo_mvim"

if !isdirectory(tmp_dir)
  call mkdir(tmp_dir, "", 0700)
endif

let &undodir= tmp_dir
set undofile

"--------------------------------------------------------------------------------
"
" python & ruby
"
"--------------------------------------------------------------------------------
let g:python3_host_prog = "/Users/yielding/miniforge/envs/pytorch/bin/python"
"let g:ruby_host_prog="/opt/homebrew/opt/ruby/bin/ruby""
let g:ruby_host_prog="/opt/homebrew/opt/ruby/bin/ruby"
"--------------------------------------------------------------------------------
"
" plugins
"
"--------------------------------------------------------------------------------
source $HOME/.config/nvim/vim-plug/plugins.vim

"--------------------------------------------------------------------------------
"
" 
"
"--------------------------------------------------------------------------------
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
" shortcuts
"
"--------------------------------------------------------------------------------
map ,f  [I
map ,l  :set list!<CR>
map ,n  :set nu!<CR>
map ,p  :edit <C-R>=_plugs<CR><CR>
map ,u  :source <C-R>=_vimrc<CR><CR>
map ,v  :edit <C-R>=_vimrc<CR><CR>
map ,w  :w <CR>
map ,q  :edit   :q <CR>
map ,z  :edit   <C-R>=_zshrc<CR><CR>
map ,ee :CocCommand explorer<CR><CR>

map ,eb :edit   <C-R>= _snippet . 'c.snippets'<CR><CR>
map ,ec :edit   <C-R>= _mysnippet . 'cpp.snippets'<CR><CR>
map ,ep :edit   <C-R>= _snippet . 'python.snippets'<CR><CR>
map ,er :edit   <C-R>= _mysnippet . 'ruby.snippets'<CR><CR>
map ,es :edit   <C-R>= _mysnippet . 'cs.snippets'<CR><CR>
map ,em :edit   <C-R>= _mysnippet . 'cmake.snippets'<CR><CR>
map ,tc :edit   <C-R>= _test . 'test.cpp'<CR><CR>
map ,tm :edit   <C-R>= _test . 'test.md'<CR><CR>
map ,tr :edit   <C-R>= _test . 'test.rb'<CR><CR>
map ,tp :edit   <C-R>= _test . 'test.py'<CR><CR>
map ,tj :edit   <C-R>= _test . 'test.java'<CR><CR>

imap  _*        <Esc>bi*<Esc>ea*<Space>

map <F2>  :set makeprg=g++-11\ -std=c++20\ %\ -o\ %<<CR>
map <F3>  :set makeprg=make<CR>
map <F4>  :CMakeBuild<CR>
map <F5>  :!./.run.sh<CR>
map <F6>  :silent exec "!(./.run.sh) &"<CR>

"map <F7>  :!%<<CR>
map <F9>  :TagbarToggle<CR>
map <F10> :FufFile<CR>
map <F11> :FufBuffer<CR>>

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

"if has("gui_running")
"  color Tomorrow-Night-Blue
"else
"  color jellybeans
"endif
"--------------------------------------------------------------------------------
"
" ruby
"
"--------------------------------------------------------------------------------
function! Ruby_eval_split() range
  let src = tempname()
  let dst = "Ruby Output"
  silent execute ": " . a:firstline . "," . a:lastline . "w " . src
  silent execute ":below pedit! " . dst
  "silent execute ":vert belowright pedit! " . dst
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

filetype off
let &runtimepath .=',~/.config/nvim/autoload/plugged/neoterm'
filetype plugin on

"--------------------------------------------------------------------------------
"
" python
"
"--------------------------------------------------------------------------------
au FileType python noremap ,r :!python %<CR>
au FileType python noremap ,rr :silent exec "!(python %) &"<CR>
au FileType js noremap ,r :!node %<CR>
au FileType cs noremap ,r :!dotnet run<CR>

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
" cd to the buffer
"
"--------------------------------------------------------------------------------
nnoremap <leader>cd :cd %:p:h<CR>:pwd<CR>

"--------------------------------------------------------------------------------
"
" mini buffer explorer
"
"--------------------------------------------------------------------------------
noremap <C-J> <C-W>j
noremap <C-K> <C-W>k
noremap <C-H> <C-W>h
noremap <C-L> <C-W>l

"let g:airline#extensions#tabline#enabled = 1
"let g:airline#extensions#tabline#fnamemod = ':t'
"let g:airline#extensions#tabline#buffer_nr_show = 1
"let g:airline#extensions#tabline#buffer_nr_format = '%s:'
"let g:airline#extensions#tabline#formatter = 'default'
"--------------------------------------------------------------------------------
"
" coc
"
"--------------------------------------------------------------------------------
set signcolumn=number
hi coc_err_hi ctermfg=1 ctermbg=15
sign define coc_err numhl=coc_err_hi
sign place 1 line=2 name=coc_err

let g:coc_global_extensions = [
 \ 'coc-ccls', 
 \ 'coc-pairs', 
 \ 'coc-tsserver', 
 \ 'coc-html', 
 \ 'coc-prettier', 
 \ 'coc-pyright',
 \ 'coc-json',
 \ 'coc-ultisnips',
 \ 'coc-solargraph',
 \ ]

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

"--------------------------------------------------------------------------------
"
" c++ syntax highlight
"
"--------------------------------------------------------------------------------
let g:cpp_class_scope_highlight = 1
let g:cpp_member_variable_highlight = 1
let g:cpp_class_decl_highlight = 1

"--------------------------------------------------------------------------------
"
" typescript
"
"--------------------------------------------------------------------------------
let g:typescript_compiler_binary = 'tsc'
let g:typescript_compiler_options = ''
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

"--------------------------------------------------------------------------------
"
" 괄호 자동 매칭
"
"--------------------------------------------------------------------------------
let loaded_matchparan=0

"--------------------------------------------------------------------------------
"
" cmake
"
"--------------------------------------------------------------------------------
augroup vim-cmake-group
autocmd! User CMakeBuildSucceeded CMakeClose
augroup END

function! GetGenOption(compiler)
  let path = system('which '. a:compiler)[:-2]
  echo path
  let g:cmake_generate_options=["-DCMAKE_CXX_COMPILER=" . path]
endfunction

command! -nargs=* Use call GetGenOption(<f-args>) 

let g:cmake_generate_options=[]
let g:cmake_native_build_options=["-j10"]
nmap <leader>cg <Plug>(CMakeGenerate)
nmap <leader>cb <Plug>(CMakeBuild)
nmap <leader>ci <Plug>(CMakeInstall)
nmap <leader>cs <Plug>(CMakeSwitch)
nmap <leader>cq <Plug>(CMakeClose)

"--------------------------------------------------------------------------------
"
" Ultisnips
"
"--------------------------------------------------------------------------------
let g:UltiSnipsExpandTrigger="<Tab>"
"let g:UltiSnipsJumpForwardTrigger="<Tab>"
"let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"
let g:UltiSnipsEditSplit="vertical"
set runtimepath ^=~/vim/UltiSnips
let g:UltiSnipsSnippetDirectories = ["UltiSnips", '~/.vim/UltiSnips']

"--------------------------------------------------------------------------------
"
" run
"
"--------------------------------------------------------------------------------
au BufEnter * :syntax sync fromstart
au BufNewFile,BufReadPost *.rb set foldmethod=expr
au BufNewFile,BufReadPost *.py set ts=2 sts=2 sw=2 tw=0
au BufNewFile,BufReadPost *.py compiler pyunit
