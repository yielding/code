" auto-install vim-plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  "autocmd VimEnter * PlugInstall
  "autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/autoload/plugged')
Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

Plug 'OmniSharp/omnisharp-vim'

Plug 'puremourning/vimspector'
Plug 'neovim/nvim-lspconfig'

" Better Syntax Support
"Plug 'sheerun/vim-polyglot'

"copliot
"Plug 'github/copilot.vim'

Plug 'stevearc/quicker.nvim'
Plug 'EdenEast/nightfox.nvim' " Vim-Plug

"Plug 'romainl/vim-devdocs'
Plug 'gauteh/vim-cppman'

" Rust
Plug 'rust-lang/rust.vim'
Plug 'simrat39/rust-tools.nvim'

" File Explorer
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }
"Plug 'scrooloose/nerdtree'

" Auto pairs for '(' '[' '{'
Plug 'jiangmiao/auto-pairs'

Plug 'https://github.com/leafgarland/typescript-vim'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'dbakker/vim-projectroot'
Plug 'itchyny/lightline.vim'
Plug 'vim-ruby/vim-ruby'

Plug 'junegunn/fzf', {'dir': '~/.fzf', 'do': './install -all' }
Plug 'junegunn/fzf.vim'

Plug 'tpope/vim-fugitive'

Plug 'kristijanhusak/vim-carbon-now-sh'

Plug 'vim-scripts/L9'
Plug 'vim-scripts/ack.vim'

Plug 'vim-scripts/FuzzyFinder'
Plug 'vim-scripts/ScrollColors'
Plug 'vim-scripts/ShowMarks7'
Plug 'vim-scripts/TagBar'
"Plug 'vim-scripts/a.vim'
Plug 'tpope/vim-projectionist'

Plug 'vim-scripts/closetag.vim'
Plug 'vim-scripts/matchit.zip'

Plug 'morhetz/gruvbox'
Plug 'rstacruz/sparkup', {'rtp': 'vim/'}

Plug 'tpope/vim-abolish'
Plug 'Lokaltog/vim-easymotion'
Plug 'ronakg/quickr-cscope.vim'

Plug 'vim-scripts/wordlist.vim'
Plug 'gmarik/ingretu'
Plug 'vim-scripts/gitv'
Plug 'udalov/kotlin-vim'

Plug 'cdelledonne/vim-cmake'
Plug 'tpope/vim-dispatch'

" Haskel
Plug 'nvim-lua/plenary.nvim'

call plug#end()