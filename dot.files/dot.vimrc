" --------------------------------------------------------------------------------
" General 
" --------------------------------------------------------------------------------
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

set notimeout
set ttimeout

set completefunc=emoji#complete

"--------------------------------------------------------------------------------
" Text, Tab and Indent
"--------------------------------------------------------------------------------
set ai

set aw

set si

set ve=block

set et

set ts=4 sts=4 sw=4 tw=0
set ts=2 sts=2 sw=2 tw=0
set ts=8 sts=8 sw=8 tw=0

set smarttab

set cindent

set cino=t0,g0

set fillchars+=vert:┃
set fillchars+=vert:\ 

set splitbelow

"--------------------------------------------------------------------------------
" globar variables
"--------------------------------------------------------------------------------
let _vimrc="~/.config/nvim/init.vim"
let _zedrun="~/.config/zed/custom_runfile.rb"
let _words="~/.config/nvim/autoload/plugged/wordlist.vim/plugin/wordlist.vim"
let _plugs="~/.config/nvim/vim-plug/plugins.vim"
let _zshrc="~/.zshrc"
let _test="~/snippets/"
let _snippet="~/.config/nvim/autoload/plugged/vim-snippets/snippets/"
let _mysnippet="~/.config/nvim/UltiSnips/"

set path=~/develop/include,~/opensource/mruby/include

let mapleader = ","

"--------------------------------------------------------------------------------
" check macvim / nvim
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

color xoria256
color fu
color darkbone
color Tomorrow-Night-Blue
color desertedoceanburnt
color wintersday
color zaibatsu
color jellybeans

"--------------------------------------------------------------------------------
" Terminal
"--------------------------------------------------------------------------------
if has('nvim')
  tnoremap <Esc> <C-\><C-n>
  tnoremap <M-[> <Esc>
  tnoremap <C-v><Esc> <Esc>
endif

"--------------------------------------------------------------------------------
" Persistent undo
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
" python & ruby
"--------------------------------------------------------------------------------
"let g:python3_host_prog = "/opt/homebrew/bin/python3"
let g:python3_host_prog = "/Users/yielding/miniforge/envs/etri/bin/python3"
"let g:ruby_host_prog = "/opt/homebrew/opt/ruby/bin/ruby"
let g:ruby_host_prog = "/opt/homebrew/lib/ruby/gems/3.4.0/bin/neovim-ruby-host"
let g:loaded_perl_provider = 0

set rtp+=/opt/homebrew/opt/fzf
"--------------------------------------------------------------------------------
" plugins
"--------------------------------------------------------------------------------
source $HOME/.config/nvim/vim-plug/plugins.vim

"--------------------------------------------------------------------------------
" spell
"--------------------------------------------------------------------------------
set spelllang=en
set spellfile=$HOME/Dropbox/vim/spell/en.utf-8.add

"--------------------------------------------------------------------------------
"
"--------------------------------------------------------------------------------
let g:lightline = { 'colorscheme' : 'wombat' }

"--------------------------------------------------------------------------------
" Showmakrs
"--------------------------------------------------------------------------------
let showmarks_enable = 0
let showmarks_include = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let showmarks_ignore_type = "hqm"
let showmarks_hlline_lower = 1
let showmarks_hlline_upper = 1

"--------------------------------------------------------------------------------
" Encodings
"--------------------------------------------------------------------------------
set fencs=utf-8,ucs-bom,euc-kr,cp949

set encoding=utf-8 nobomb
set ffs=unix,dos

"--------------------------------------------------------------------------------
" shortcuts
"--------------------------------------------------------------------------------
map ,f  [I
map ,l  :set list!<CR>
map ,n  :set nu!<CR>
map ,p  :edit <C-R>=_plugs<CR><CR>
map ,u  :source <C-R>=_vimrc<CR><CR>
map ,o  :edit <C-R>=_words<CR><CR>
map ,v  :edit <C-R>=_vimrc<CR><CR>
map ,d  :edit <C-R>=_zedrun<CR><CR>
map ,w  :w <CR>
map ,q  :q <CR>
map ,z  :edit <C-R>=_zshrc<CR><CR>
map ,ee :CocCommand explorer<CR>
map ,kk :%s/<C-V><CR>//ge<CR>:w<CR> "모든 ^M 문자 삭제

"nnoremap ,ee :NERDTreeToggle<CR>

" jk | Escaping! 
inoremap jk <esc>
cnoremap jk <C-c>

" below makes vim slow
"xnoremap jk <esc>

"visual enclose, paranthesis
vnoremap ,ss c()<esc>P

" Movement in insert mode
inoremap <a-h> <c-o>h
inoremap <a-l> <c-o>a
inoremap <a-j> <c-o>j
inoremap <a-k> <c-o>k

" qq to record, Q to replay
nnoremap Q @q

map ,eb :edit <C-R>= _snippet . 'c.snippets'<CR><CR>
map ,ec :edit <C-R>= _snippet . 'cpp.snippets'<CR><CR>
map ,ep :edit <C-R>= _snippet . 'python.snippets'<CR><CR>
map ,er :edit <C-R>= _mysnippet . 'ruby.snippets'<CR><CR>
map ,es :edit <C-R>= _mysnippet . 'cs.snippets'<CR><CR>
map ,em :edit <C-R>= _mysnippet . 'cmake.snippets'<CR><CR>
map ,eh :edit <C-R>= _mysnippet . 'haskell.snippets'<CR><CR>
map ,tc :lcd <C-R>= _test<CR><CR> :e <C-R>= _test . 'test.cpp'<CR><CR>
map ,th :edit <C-R>= _test . 'test.hs'<CR><CR>
map ,tm :edit <C-R>= _test . 'test.md'<CR><CR>
map ,tr :edit <C-R>= _test . 'test.rb'<CR><CR>
map ,ts :edit <C-R>= _test . 'test-rs'<CR><CR>
map ,tp :edit <C-R>= _test . 'test.py'<CR><CR>
map ,tj :edit <C-R>= _test . 'Test.java'<CR><CR>
map ,sp :edit <C-R>= '.vimspector.json'<CR><CR>
map ,ls :edit <C-R>= '.ccls'<CR><CR>

imap  _* <Esc>bi*<Esc>ea*<Space> 

map ,3  :set makeprg=g++-14\ -g\ -std=c++2c\ %\ -o\ %<<CR>
map ,4  :execute '!' . './' . expand('%:r')<CR>

map ,9  :TagbarToggle<CR>
map ,22 :FufFile<CR>
map ,11 :FufBuffer<CR>

""-----------------------------------------------------------------------------
"" workspace : visual select를 느리게 만든다.
""-----------------------------------------------------------------------------
"let g:workspace_autocreate = 1
"
""이미 속한 workspace가 있으면 해당 웍스 열기
"let g:workspace_create_new_tabs = 1
"let g:workspace_session_directory = $HOME . '/.vim/sessions/'
"let g:workspace_session_disable_on_args = 1
"nnoremap <leader>c : ToggleWorkspace<CR>

"-----------------------------------------------------------------------------
" grep in gitdir
"-----------------------------------------------------------------------------
nnoremap <leader>gc :Ggrep! -q <cword><CR>

"-----------------------------------------------------------------------------
" toggle folder
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
" outline toggle
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

"--------------------------------------------------------------------------------
" ruby
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
  silent execute ":%! ruby --jit " . src . " 2>&1 "
  wincmd p
endfunction

au FileType ruby filetype plugin indent on
au FileType ruby noremap <c-s-b> :%call Ruby_eval_split()<CR>
au FileType ruby noremap <c-s-r> :%call Ruby_eval_split()<CR>
au FileType ruby noremap ,tag :!ripper-tags -R.<CR>

filetype off
let &runtimepath .=',~/.config/nvim/autoload/plugged/neoterm'
filetype plugin on

"--------------------------------------------------------------------------------
" switch highlight search
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
" switch syntax search
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
" Buffers
"--------------------------------------------------------------------------------
nnoremap ]b :bnext<CR>
nnoremap [b :bprev<CR>
nnoremap <leader>cd :cd %:p:h<CR>:pwd<CR>

"--------------------------------------------------------------------------------
" Tabs
"--------------------------------------------------------------------------------
nnoremap ]t :tabn<CR>
nnoremap [t :tabp<CR>

"--------------------------------------------------------------------------------
" Circular window navigation
"--------------------------------------------------------------------------------
"nnoremap <tab>   <c-w>w
"nnoremap <s-tab> <c-w>W

nnoremap <c-h>   <c-w>h
nnoremap <c-j>   <c-w>j
nnoremap <c-k>   <c-w>k
nnoremap <c-l>   <c-w>l

"--------------------------------------------------------------------------------
" coc
"--------------------------------------------------------------------------------
set signcolumn=number
hi coc_err_hi ctermfg=1 ctermbg=15
sign define coc_err numhl=coc_err_hi
sign place 1 line=2 name=coc_err

" \ 'coc-pairs' # this has eror  i<<>
" \ 'coc-ultisnips',
" \ 'coc-omnisharp',

let g:coc_global_extensions = [
 \ 'coc-ccls',
 \ 'coc-clangd',
 \ 'coc-tsserver',
 \ 'coc-pyright',
 \ 'coc-prettier',
 \ 'coc-html',
 \ 'coc-json', 
 \ 'coc-solargraph',
 \ 'coc-cmake',
 \ 'coc-java',
 \ 'coc-markdownlint',
 \ 'coc-git'
 \ ]           
               
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> gn <Plug>(coc-rename)

autocmd FileType json syntax match Comment +\/\/.\+$+

"This expression seems to be responsible for coc formatting on enter
inoremap <silent><expr> <TAB> coc#pum#visible() ? coc#pum#confirm() : "\<C-g>u\<TAB>"

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


""--------------------------------------------------------------------------------
"" typescript
""--------------------------------------------------------------------------------
"let g:typescript_compiler_binary = 'tsc'
"let g:typescript_compiler_options = ''
"autocmd QuickFixCmdPost [^l]* nested cwindow
"autocmd QuickFixCmdPost    l* nested lwindow

"--------------------------------------------------------------------------------
" c++
"--------------------------------------------------------------------------------
let g:cpp_concepts_highlight = 1

"--------------------------------------------------------------------------------
" 괄호 자동 매칭
"--------------------------------------------------------------------------------
let loaded_matchparan=0

"--------------------------------------------------------------------------------
" cmake
"--------------------------------------------------------------------------------
augroup vim-cmake-group
"autocmd! User CMakeBuildSucceeded CMakeOpen
autocmd! User CMakeBuildSucceeded CMakeClose
augroup END

function! GetGenOption(compiler)
  let path = system('which '. a:compiler)[:-2]
  echo path
  let g:cmake_generate_options=["-DCMAKE_CXX_COMPILER=" . path]
endfunction

command! -nargs=* Use call GetGenOption(<f-args>) 

let g:cmake_root_markers=[]
"let g:cmake_root_markers=['.git', '.svn']
let g:cmake_native_build_options=["-j10"]

nmap <c-s-g>    <Plug>(CMakeGenerate)
nmap <c-s-b>    <Plug>(CMakeBuild)
nmap <c-s-t>    <Plug>(CMakeTest)
nmap <c-s-r>    :execute '!./Debug/' . expand('%:t:r')<CR>
nmap <leader>rr :execute '!./Debug/' . expand('%:t:r')<CR>

nmap <leader>ci <Plug>(CMakeInstall)
nmap <leader>cs <Plug>(CMakeSwitch)
nmap <leader>co <Plug>(CMakeOpen)
nmap <leader>cq <Plug>(CMakeClose)

"--------------------------------------------------------------------------------
" Ultisnips
"--------------------------------------------------------------------------------
nmap <C-S-P> :split \| resize 10 \| terminal <CR>

"--------------------------------------------------------------------------------
" Ultisnips
"--------------------------------------------------------------------------------
let g:UltiSnipsExpandTrigger="<tab>"
"let g:UltiSnipsJumpForwardTrigger="<Tab>"
"let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"
let g:UltiSnipsEditSplit="vertical"

" To reduce so many duplicate, turn off belows
set runtimepath ^=~/.config/nvim/UltiSnips
let g:UltiSnipsSnippetDirectories = ['UltiSnips', '~/.config/nvim/UltiSnips']

"--------------------------------------------------------------------------------
" rust
"--------------------------------------------------------------------------------
let g:rust_recommended_style = 0

"--------------------------------------------------------------------------------
" vimspector
"--------------------------------------------------------------------------------
" terminal does not support shift+F key
let g:vimspector_enable_mappings = "HUMAN"
let g:vimspector_base_dir=$HOME . '/.config/nvim/autoload/plugged/vimspector'

" for normal mode - the word under the cursor
nmap ,di <Plug>VimspectorBalloonEval

" for visual mode, the visually selected text
xmap ,di <Plug>VimspectorBalloonEval

nmap ,df :VimspectorReset<CR>

let g:CommandTPreferredImplementation='ruby'

let g:OmniSharp_server_use_net6 = 1

"--------------------------------------------------------------------------------
" run
"--------------------------------------------------------------------------------
au BufEnter * :syntax sync fromstart
au BufNewFile,BufReadPost *.rb set foldmethod=expr
au BufNewFile,BufReadPost *.py set ts=2 sts=2 sw=2 tw=0
au BufNewFile,BufReadPost *.py compiler pyunit
au BufNewFile,BufReadPost *.g4 set ft=antlr

au FileType python  noremap <c-s-r> :!python3 %<CR>
au FileType cs      noremap <c-s-b> :!dotnet build<CR>
au FileType cs      noremap <c-s-r> :!dotnet run<CR>
au FileType java    noremap <c-s-b> :!javac %<CR>
au FileType java    noremap <c-s-r> :execute '!java ' . expand('%:r')<CR>
au FileType haskell noremap <c-s-r> :!runhaskell %<CR>
au FileType js      noremap <c-s-r> :!node %<CR>
au FileType kotlin  noremap <c-s-4> :execute '!kot.sh ' . expand('%')<CR>
au FileType kotlin  noremap <c-s-r> :execute '!kot.sh ' . expand('%')<CR>

au FileType rust    noremap <c-s-b> :make b<CR>
au FileType rust    noremap <c-s-r> :make r<CR>
au FileType rust    noremap <c-s-t> :make t<CR>